package afcens.afccase

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import afcens.MarshallersSupport
import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging
import spray.json._

object Resolver {
  def props(traceFileBase: String) = Props(new Resolver(traceFileBase))
}

class Resolver(val traceFileBase: String) extends Actor with MarshallersSupport {

  val traceFileWriter = if (traceFileBase != null) new PrintWriter(new File(traceFileBase + "-resolver.jsonl" )) else null

  private val log = Logging(context.system, this)

  private val solverLimitTime = 60000000000L

  override def postStop(): Unit = {
    if (traceFileWriter != null) {
      traceFileWriter.close()
    }
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): ResolutionResult = {
    log.debug("Resolver processStep")
    val scenario = new Scenario(simulationState)

    log.debug("Resolver started")
    val startTime = System.currentTimeMillis

    val root = scenario.root

    if (root.resolve(solverLimitTime)) {
      val endTime = System.currentTimeMillis

      log.debug(s"Solution found in ${endTime - startTime} ms. Utility: " + root.instance.solutionUtility)
      log.debug(root.instance.describe)

    } else {
      val endTime = System.currentTimeMillis
      log.error(s"Error. No solution exists. Took ${endTime - startTime} ms to compute.")
    }

    val dataEntry = JsObject(
      "time" -> simulationState.time.toJson,
      "drones" -> simulationState.drones.toJson,
      "flocks" -> simulationState.flocks.toJson,
      "ensembles" -> scenario.root.instance.toJson
    )

    if (traceFileWriter != null) {
      traceFileWriter.println(dataEntry.compactPrint)
    }

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

