package afcens.afccase

import java.time.LocalDateTime

import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging

import scala.util.Random

object Drone {
  val speed = 1 // pos unit / tick

  def props(id: String, startPosId: String) = Props(new Drone(id, startPosId))
}

class Drone(val id: String, val startPosId: String) extends Actor {
  private val log = Logging(context.system, this)
  private val random = new Random(startPosId.hashCode)

  private var currentPos: Position = _
  private var targetPos: Position = _

  private def processReset(): DroneState = {
    currentPos = ScenarioMap(startPosId)
    targetPos = currentPos

    DroneState(currentPos)
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): DroneState = {
    if (currentPos == targetPos) {
      val posIdx = random.nextInt(ScenarioMap.freeCount) + 1
      val posSubIdx = random.nextInt(ScenarioMap.freeSizes(posIdx)) + 1
      val posId = ScenarioMap.freeId(posIdx, posSubIdx)

      targetPos = ScenarioMap(posId)
    }

    val dx = targetPos.x - currentPos.x
    val dy = targetPos.y - currentPos.y
    val dl = Math.sqrt(dx * dx + dy * dy)

    if (dl >= Drone.speed) {
      currentPos = Position(currentPos.x + dx / dl, currentPos.y + dy / dl)
    } else {
      currentPos = targetPos
    }

    DroneState(currentPos)
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      sender() ! DroneAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! DroneAck(msg.uuid, processReset())
  }
}
