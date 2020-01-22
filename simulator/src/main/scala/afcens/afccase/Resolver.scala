package afcens.afccase

import java.time.LocalDateTime

import akka.actor.{Actor, Props}
import akka.event.Logging

import afcens.afccase.Simulation.{Reset, SimReset, SimStep}

import scala.collection.mutable

object Resolver {
  def props() = Props(new Resolver())
}

class Resolver() extends Actor {
  import Resolver._

  private val log = Logging(context.system, this)

  private val solverLimitTime = 60000000000L

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): ResolutionResult = {
    log.debug("Resolver processStep")
    val scenario = new Scenario(simulationState)

    log.debug("Resolver started")
    val startTime = System.currentTimeMillis

    val root = scenario.root

    if (root.resolve(solverLimitTime)) {
      val endTime = System.currentTimeMillis

      log.info(s"Solution found in ${endTime - startTime} ms. Utility: " + root.instance.solutionUtility)
      log.info(root.instance.describe)

    } else {
      val endTime = System.currentTimeMillis
      log.error(s"Error. No solution exists. Took ${endTime - startTime} ms to compute.")
    }

    println(scenario.emittedTasks.toList)
    ResolutionResult(scenario.emittedTasks.toList)
  }

  private def processReset(): ResolutionResult = {
    ResolutionResult(List())
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      sender() ! ResolverAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! ResolverAck(msg.uuid, processReset())
  }

}

