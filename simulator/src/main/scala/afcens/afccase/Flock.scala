package afcens.afccase

import java.time.{Duration, LocalDateTime}

import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.util.Random

object FlockMode extends Enumeration {
  type FlockMode = Value
  val FLYING_TO_REST, FLYING_TO_EAT, EATING, RESTING, IDLE = Value
}

object Flock {
  val speed = 1 // pos unit / tick
  val visibilityRadius = 50
  val disturbRadius = 18
  val observationTimeout = Duration.ofSeconds(150)
  val leaveFreeProb = 0.01
  val leaveFieldProb = 0.002
  val chooseFieldProb = 0.8

  def props() = Props(new Flock())
}

/*
Basic behavior:
- If a bird is on FIELD spot, it has each tick the probability 0.2% that it chooses a FREE spot and flies there
- If a bird is on FREE spot, it has each tick the probability 1% that is chooses a FIELD spot and flies there
- The bird observes the area of radius 15 and remembers drones there for 60 seconds
- If a bird is observed (or remembered) in distance of 15 from a target spot, a new target spot is chosen as follows:
  - If the target spot of a bird is FIELD, the closest other spot on the same field is chosen (only those spots are considered
    where no drone has been observed close-by in those 60 second)
  - If the target spot is FIELD, but no other undisturbed spot exists on the same field, the bird select a random undisturbed
    spot
 */

case class ObservedDrone(time: LocalDateTime, position: Position)

class Flock() extends Actor {
  val id = self.path.name

  private val log = Logging(context.system, this)
  implicit private val random = new Random(id.hashCode)

  import FlockMode._

  private val startPosId = ScenarioMap.randomFreeId

  private var mode: FlockMode = _

  private var currentPos: Position = _
  private var targetPos: Position = _
  private var targetPosId: PositionId = _

  private var eatTicks = 0

  private var observedDrones: List[ObservedDrone] = _

  private def flockState = FlockState(mode, currentPos, observedDrones.map(_.position), eatTicks)

  private def processReset(): FlockState = {
    mode = IDLE
    currentPos = startPosId.position
    targetPos = currentPos
    targetPosId = startPosId
    observedDrones = List.empty[ObservedDrone]
    eatTicks = 0

    flockState
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): FlockState = {
    def droneCloseBy(position: Position): Boolean = {
      observedDrones.exists(observedDrone => observedDrone.position.distance(position) < Flock.disturbRadius)
    }

    observedDrones = observedDrones.filter(_.time.isAfter(currentTime.minus(Flock.observationTimeout)))

    for (drone <- simulationState.drones.values if drone.position.distance(currentPos) < Flock.visibilityRadius) {
      observedDrones = ObservedDrone(currentTime, drone.position) :: observedDrones
    }

    if (currentPos == targetPos) {
      if (targetPosId.isInstanceOf[FreeId]) {
        mode = RESTING

        if (random.nextDouble() < Flock.leaveFreeProb) {
          targetPosId = ScenarioMap.randomFieldId
          targetPos = targetPosId.position
          mode = FLYING_TO_EAT
        }

      } else { // FieldId
        mode = EATING

        if (random.nextDouble() < Flock.leaveFieldProb) {
          targetPosId = ScenarioMap.randomFreeId
          targetPos = targetPosId.position
          mode = FLYING_TO_REST
        }
      }
    }

    if (mode == EATING) {
      eatTicks += 1
    }

    while (droneCloseBy(targetPos)) {
      var processed = false

      if (targetPosId.isInstanceOf[FieldId]) {
        val targetFieldId = targetPosId.asInstanceOf[FieldId]
        val undisturbedFieldIds = targetFieldId.sameFieldIds.filter(fieldId => !droneCloseBy(fieldId.position))

        if (!undisturbedFieldIds.isEmpty) {
          targetPosId = PositionId.getClosestTo(targetFieldId.position, undisturbedFieldIds)
          targetPos = targetPosId.position
          mode = FLYING_TO_EAT

          processed = true
        }
      }

      if (!processed) {
        if (random.nextDouble() < Flock.chooseFieldProb) {
          targetPosId = ScenarioMap.randomFieldId
          mode = FLYING_TO_EAT

        } else {
          targetPosId = ScenarioMap.randomFreeId
          mode = FLYING_TO_REST
        }

        targetPos = targetPosId.position
      }
    }

    val dx = targetPos.x - currentPos.x
    val dy = targetPos.y - currentPos.y
    val dl = Math.sqrt(dx * dx + dy * dy)

    if (dl >= Flock.speed) {
      currentPos = Position(currentPos.x + dx / dl, currentPos.y + dy / dl)
    } else {
      currentPos = targetPos
    }

    flockState
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      sender() ! FlockAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! FlockAck(msg.uuid, processReset())
  }
}
