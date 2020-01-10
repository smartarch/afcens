package afcens.afccase

import java.time.LocalDateTime

import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.util.Random

object Flock {
  val speed = 1 // pos unit / tick

  def props(id: String, startPosId: String) = Props(new Flock(id, startPosId))
}

class Flock(val id: String, val startPosId: String) extends Actor {
  private val log = Logging(context.system, this)
  private val random = new Random(startPosId.hashCode)

  private var currentPos: Position = _
  private var targetPos: Position = _

  private def processReset(): FlockState = {
    currentPos = ScenarioMap(startPosId)
    targetPos = currentPos

    FlockState(currentPos)
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): FlockState = {
    if (currentPos == targetPos) {
      val posIdx = random.nextInt(ScenarioMap.fieldCount) + 1
      val posSubIdx = random.nextInt(ScenarioMap.fieldSizes(posIdx)) + 1
      val posId = ScenarioMap.fieldId(posIdx, posSubIdx)

      targetPos = ScenarioMap(posId)
    }

    val dx = targetPos.x - currentPos.x
    val dy = targetPos.y - currentPos.y
    val dl = Math.sqrt(dx * dx + dy * dy)

    if (dl >= Flock.speed) {
      currentPos = Position(currentPos.x + dx / dl, currentPos.y + dy / dl)
    } else {
      currentPos = targetPos
    }

    FlockState(currentPos)
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      sender() ! FlockAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! FlockAck(msg.uuid, processReset())
  }
}
