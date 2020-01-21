package afcens.afccase

import java.time.LocalDateTime

import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.util.Random

object DroneMode extends Enumeration {
  type DroneMode = Value
  val FLYING_TO_CHARGER, CHARGING, PATROLLING, PURSUING, RESTING, DEAD, IDLE = Value
}

object Drone {
  val speed = 1 // pos unit / tick
  val visibilityRadius = 50
  val chargingThreshold = 0.2
  val flockAtFieldDistanceThreshold = 1

  val energyDrainPerTick = 0.001
  val energyChargePerTick = 0.01

  def props() = Props(new Drone())
}

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

  private var mode: DroneMode = _

  private var currentPos: Position = _
  private var targetPos: Position = _
  private var targetPosId: PositionId = _

  private var energy: Double = _
  private var chargingInChargerId: Option[ChargerId] = None

  private def droneState = DroneState(mode, currentPos, energy, chargingInChargerId)

  private def processReset(): DroneState = {
    mode = IDLE
    currentPos = startPosId.position
    targetPos = currentPos
    targetPosId = startPosId
    chargingInChargerId = None
    energy = 1

    droneState
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): DroneState = {
    val observedFlockPositions = for (flock <- simulationState.flocks.values if flock.position.distance(currentPos) < Drone.visibilityRadius) yield flock.position

    val chargerAvailability = Map.empty[ChargerId, Option[Boolean]] ++ (
      for (chargerId <- ScenarioMap.allChargerIds) yield
        chargerId -> (
          if (chargerId.position.distance(currentPos) < Drone.visibilityRadius)
            Some(!simulationState.drones.values.exists(_.chargingInChargerId.getOrElse(null) == chargerId))
          else
            None
        )
    )

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
      mode = FLYING_TO_CHARGER

      val availableChargers = for {
        (chargerId, availability) <- chargerAvailability if availability == Some(true)
      } yield chargerId

      if (currentPos == targetPos && availableChargers.exists(_ == targetPosId)) {
        chargingInChargerId = Some(targetPosId.asInstanceOf[ChargerId])
        performCharging()

      } else {
        if (availableChargers.isEmpty) {
          if (currentPos == targetPos && targetPosId.isInstanceOf[ChargerId]) {
            val nextChargerIdx = (targetPosId.asInstanceOf[ChargerId].idx + 1) % ScenarioMap.chargerCount
            targetPosId = ChargerId(nextChargerIdx)

          } else {
            targetPosId = ScenarioMap.allChargerIds(0)
          }
        } else {
          targetPosId = PositionId.getClosestTo(currentPos, availableChargers)
        }

        targetPos = targetPosId.position
      }
    }

    def performPatrollingOrPursuing(): Unit = {
      val fieldIdsWithFlock = ScenarioMap.allFieldIds.filter(fieldId => observedFlockPositions.exists(flockPos => fieldId.position.distance(flockPos) <= Drone.flockAtFieldDistanceThreshold))

      if (fieldIdsWithFlock.isEmpty) {
        if (mode != PATROLLING) {
          mode = PATROLLING

          targetPosId = PositionId.getClosestTo(currentPos, ScenarioMap.allFieldIds)
          targetPos = targetPosId.position

        } else {
          if (currentPos == targetPos && (targetPosId.isInstanceOf[FieldId])) {
            val nextFieldIdx = (targetPosId.asInstanceOf[FieldId].idx + 1) % ScenarioMap.fieldCount
            targetPosId = FieldId(nextFieldIdx, random.nextInt(ScenarioMap.fieldSizes(nextFieldIdx)))
            targetPos = targetPosId.position
          }
        }

      } else {
        mode = PURSUING

        targetPosId = PositionId.getClosestTo(currentPos, fieldIdsWithFlock)
        targetPos = targetPosId.position
      }
    }

    if (mode != DEAD) {
      if (mode == IDLE || mode == PATROLLING || mode == PURSUING) {
        if (energy < Drone.chargingThreshold) {
          performGoingToCharge()
        } else {
          performPatrollingOrPursuing()
        }

      } else if (mode == CHARGING) {
        performCharging()

      } else if (mode == FLYING_TO_CHARGER) {
        performGoingToCharge()
      }


      val dx = targetPos.x - currentPos.x
      val dy = targetPos.y - currentPos.y
      val dl = Math.sqrt(dx * dx + dy * dy)

      if (dl >= Drone.speed) {
        currentPos = Position(currentPos.x + dx / dl, currentPos.y + dy / dl)
      } else {
        currentPos = targetPos
      }


      if (mode != CHARGING || mode != RESTING) {
        energy -= Drone.energyDrainPerTick
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
      sender() ! DroneAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! DroneAck(msg.uuid, processReset())
  }
}
