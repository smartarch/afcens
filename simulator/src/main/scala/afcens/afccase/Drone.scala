package afcens.afccase

import java.time.{Duration, LocalDateTime}

import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.util.Random

object DroneMode extends Enumeration {
  type DroneMode = Value
  val CHARGING, MOVING, RESTING, DEAD, IDLE = Value
}

object Drone {
  val speed = 1 // pos unit / tick
  val visibilityRadius = 50
  val observationTimeout = Duration.ofSeconds(500)
  val chargingThreshold = 0.2
  val flockAtFieldDistanceThreshold = 1

  val energyDrainMovingPerTick = 0.0015
  val energyDrainStayingPerTick = 0.001
  val energyChargePerTick = 0.01

  def props() = Props(new Drone())
}

object ObservedFieldStatus extends Enumeration {
  type ObservedFieldStatus = Value
  val CLEAR, UNDER_THREAT, UNKNOWN = Value
}

import afcens.afccase.ObservedFieldStatus.ObservedFieldStatus
case class ObservedFieldId(time: LocalDateTime, fieldId: FieldId, status: ObservedFieldStatus)

/*
Basic behavior:
- The drone observes an area of radius 50, it looks for birds and occupancy of charging stations
- If the drone has energy level on or below 10 percent, it flies to an available charger. If the availability of chargers
  is not known, it flies to Charger-1. If no chargers are available, it circles over the chargers
  - Once it occupies a charger, it charges to 100% and then it continues
- If the drone has energy level above 10 percent, it flies to the closest bird that is close (in distance <= 1) of some field
  - If no birds are observed, it patrols over the fields in 1..5 order
 */

class Drone() extends Actor {
  val id = self.path.name

  private val log = Logging(context.system, this)
  implicit private val random = new Random(id.hashCode)

  import DroneMode._

  private val startPosId = ScenarioMap.randomFieldId
  private val rndFactor = random.nextDouble() / 5 + 1

  private var mode: DroneMode = _

  private var currentPos: Position = _
  private var targetPos: Position = _
  private var targetChargerId: ChargerId = _

  private var energy: Double = _
  private var chargingInChargerId: Option[ChargerId] = None

  private var observedFieldIds: Map[String, ObservedFieldId] = _

  private def droneState = DroneState(mode, currentPos, energy, chargingInChargerId, observedFieldIds)

  private def processReset(): DroneState = {
    mode = IDLE
    currentPos = startPosId.position
    targetPos = currentPos
    targetChargerId = null
    chargingInChargerId = None
    energy = 1
    observedFieldIds = Map.empty[String, ObservedFieldId]

    droneState
  }

  private def processStandaloneStep(currentTime: LocalDateTime, simulationState: SimulationState): DroneState = {
    val chargerAvailability = Map.empty[ChargerId, Option[Boolean]] ++ (
      for (chargerId <- ScenarioMap.allChargerIds) yield
        chargerId -> (
          if (chargerId.position.distance(currentPos) < Drone.visibilityRadius)
            Some(!simulationState.drones.values.exists(_.chargingInChargerId.getOrElse(null) == chargerId))
          else
            None
        )
    )

    def performObserving(): Unit = {
      val observedFlockPositions =
        if (mode != DEAD && mode != RESTING && mode != CHARGING)
            for (flock <- simulationState.flocks.values if flock.position.distance(currentPos) < Drone.visibilityRadius) yield flock.position
        else
          List()

      for (fieldId <- ScenarioMap.allFieldIds) {
        val existingObservation = observedFieldIds.get(fieldId.toString)

        val status =
          if (fieldId.position.distance(currentPos) >= Drone.visibilityRadius) ObservedFieldStatus.UNKNOWN
          else if (observedFlockPositions.exists(flockPos => fieldId.position.distance(flockPos) <= Drone.flockAtFieldDistanceThreshold)) ObservedFieldStatus.UNDER_THREAT
          else ObservedFieldStatus.CLEAR

        if (
          status != ObservedFieldStatus.UNKNOWN ||
            existingObservation == None ||
            existingObservation.get.time.isBefore(currentTime.minus(Flock.observationTimeout)) ||
            existingObservation.get.status == ObservedFieldStatus.UNKNOWN
          ) {

          observedFieldIds = observedFieldIds + (fieldId.toString -> ObservedFieldId(currentTime, fieldId, status))
        }
      }
    }

    def performCharging(): Unit = {
      mode = CHARGING

      energy += Drone.energyChargePerTick
      if (energy >= 1) {
        energy = 1
        mode = IDLE
        chargingInChargerId = None
      }
    }

    def performGoingToCharge(): Unit = {
      mode = MOVING

      val availableChargers = for {
        (chargerId, availability) <- chargerAvailability if availability == Some(true)
      } yield chargerId

      if (currentPos == targetPos && availableChargers.exists(_ == targetChargerId)) {
        chargingInChargerId = Some(targetChargerId)
        targetChargerId = null
        performCharging()

      } else {
        if (availableChargers.isEmpty) {
          if (currentPos == targetPos && targetChargerId.isInstanceOf[ChargerId]) {
            val nextChargerIdx = (targetChargerId.idx + 1) % ScenarioMap.chargerCount
            targetChargerId = ChargerId(nextChargerIdx)

          } else {
            targetChargerId = ScenarioMap.allChargerIds(0)
          }
        } else {
          targetChargerId = PositionId.getClosestTo(currentPos, availableChargers)
        }

        targetPos = targetChargerId.position
      }
    }

    def performPatrollingOrPursuing(): Unit = {
      mode = MOVING

      if (observedFieldIds.values.forall(_.status != ObservedFieldStatus.UNDER_THREAT)) {
        val fieldCentersWithUnknownStatus = for {
          fieldIdx <- 0 until ScenarioMap.fieldCount
          if observedFieldIds.values.exists(x => x.fieldId.idx == fieldIdx && x.status == ObservedFieldStatus.UNKNOWN)
        } yield FieldIdHelper.centerPosition(fieldIdx)

        if (fieldCentersWithUnknownStatus.isEmpty) {
          targetPos = currentPos
          mode = IDLE
        } else {
          var closestPosition: Position = null
          var closestDistance: Double = 0

          for (position <- fieldCentersWithUnknownStatus) {
            val distance = position.distance(currentPos)
            if (closestPosition == null || distance < closestDistance) {
              closestPosition = position
              closestDistance = distance
            }
          }

          targetPos = closestPosition
        }


      } else {
        val posId = PositionId.getClosestTo(currentPos, observedFieldIds.values.collect{ case field if field.status == ObservedFieldStatus.UNDER_THREAT => field.fieldId })
        targetPos = posId.position
      }
    }

    performObserving()

    if (mode != DEAD) {
      if (mode == IDLE || mode == MOVING) {
        if (energy < Drone.chargingThreshold) {
          performGoingToCharge()
        } else {
          performPatrollingOrPursuing()
        }

      } else if (mode == CHARGING) {
        performCharging()
      }


      if (mode != CHARGING || mode != RESTING) {
        val dx = targetPos.x - currentPos.x
        val dy = targetPos.y - currentPos.y
        val dl = Math.sqrt(dx * dx + dy * dy)

        if (dl >= Drone.speed * rndFactor) {
          energy -= Drone.energyDrainMovingPerTick * rndFactor
          currentPos = Position(currentPos.x + dx * Drone.speed * rndFactor / dl, currentPos.y + dy * Drone.speed * rndFactor / dl)
        } else {
          energy -= Drone.energyDrainStayingPerTick * rndFactor
          currentPos = targetPos
        }

        if (energy <= 0) {
          energy = 0
          mode = DEAD
        }
      }
    }

    droneState
  }

  private def processCoordinatedStep(currentTime: LocalDateTime, simulationState: SimulationState): DroneState = {
    val chargerIdOption = simulationState.tasks.collectFirst{ case ChargeTask(`id`, chargerId) => chargerId}
    val movePositionOption = simulationState.tasks.collectFirst{ case MoveTask(`id`, position: Position) => position}

    def performObserving(): Unit = {
      val observedFlockPositions =
        if (mode != DEAD && mode != RESTING && mode != CHARGING)
          for (flock <- simulationState.flocks.values if flock.position.distance(currentPos) < Drone.visibilityRadius) yield flock.position
        else
          List()

      for (fieldId <- ScenarioMap.allFieldIds) {
        val existingObservation = observedFieldIds.get(fieldId.toString)

        val status =
          if (fieldId.position.distance(currentPos) >= Drone.visibilityRadius) ObservedFieldStatus.UNKNOWN
          else if (observedFlockPositions.exists(flockPos => fieldId.position.distance(flockPos) <= Drone.flockAtFieldDistanceThreshold)) ObservedFieldStatus.UNDER_THREAT
          else ObservedFieldStatus.CLEAR

        if (
          status != ObservedFieldStatus.UNKNOWN ||
            existingObservation == None ||
            existingObservation.get.time.isBefore(currentTime.minus(Flock.observationTimeout)) ||
            existingObservation.get.status == ObservedFieldStatus.UNKNOWN
        ) {

          observedFieldIds = observedFieldIds + (fieldId.toString -> ObservedFieldId(currentTime, fieldId, status))
        }
      }
    }

    def performCharging(): Unit = {
      mode = CHARGING

      energy += Drone.energyChargePerTick
      if (energy >= 1) {
        energy = 1
        mode = IDLE
        chargingInChargerId = None
      }
    }


    performObserving()

    if (mode != DEAD) {
      if (chargerIdOption != None) {
        val chargerId = chargerIdOption.get
        targetPos = chargerId.position

        if (currentPos == targetPos) {
          chargingInChargerId = Some(chargerId)
          performCharging()
        } else {
          mode = MOVING
        }

      } else if (movePositionOption != None) {
        targetPos = movePositionOption.get

        if (targetPos == currentPos) {
          mode = IDLE
        } else {
          mode = MOVING
        }

      } else if (mode == CHARGING) {
        performCharging()

      } else {
        targetPos = currentPos
        mode = IDLE
      }

      if (mode != CHARGING || mode != RESTING) {
        val dx = targetPos.x - currentPos.x
        val dy = targetPos.y - currentPos.y
        val dl = Math.sqrt(dx * dx + dy * dy)

        if (dl >= Drone.speed * rndFactor) {
          energy -= Drone.energyDrainMovingPerTick * rndFactor
          currentPos = Position(currentPos.x + dx * Drone.speed * rndFactor / dl, currentPos.y + dy * Drone.speed * rndFactor / dl)
        } else {
          energy -= Drone.energyDrainStayingPerTick * rndFactor
          currentPos = targetPos
        }

        if (energy <= 0) {
          energy = 0
          mode = DEAD
        }
      }
    }

    droneState
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      //sender() ! DroneAck(msg.uuid, processStandaloneStep(currentTime, simulationState))
      sender() ! DroneAck(msg.uuid, processCoordinatedStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! DroneAck(msg.uuid, processReset())
  }
}
