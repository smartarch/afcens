package afcens.afccase

import java.io.{File, FileOutputStream, PrintWriter}
import java.time.LocalDateTime
import java.util.zip.GZIPOutputStream

import afcens.MarshallersSupport
import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, Props}
import akka.event.Logging
import spray.json._

object Resolver {
  def props(traceFileBase: String, quantization: Int) = Props(new Resolver(traceFileBase, quantization))
}

class Resolver(val traceFileBase: String, val quantization: Int) extends Actor with MarshallersSupport {

  val traceFileWriter = if (traceFileBase != null) new PrintWriter(new GZIPOutputStream(new FileOutputStream(traceFileBase + ".jsonl.gz"))) else null

  private val log = Logging(context.system, this)

  private val solverLimitTime = 60000000000L

  override def postStop(): Unit = {
    if (traceFileWriter != null) {
      traceFileWriter.close()
    }
  }

  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): ResolutionResult = {
    log.debug("Resolver processStep")
    val scenario = new Scenario(simulationState, quantization, null)

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

    if (traceFileWriter != null) {
      val dataEntry = JsObject(
        "time" -> simulationState.time.toJson,
        "drones" -> simulationState.drones.toJson,
        "flocks" -> simulationState.flocks.toJson,
        "tasks" -> simulationState.tasks.toJson,
        "ensembles" -> scenario.root.instance.toJson,
        "eatTicks" -> simulationState.eatTicks.toJson
      )

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

