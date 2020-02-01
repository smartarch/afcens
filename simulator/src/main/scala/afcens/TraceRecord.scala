package afcens

import afcens.afccase.{ScenarioMap, Simulation, SimulationState}
import akka.actor.{ActorRef, ActorSystem}
import akka.event.{LogSource, Logging}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

object TraceRecord {
  def main(args: Array[String]) {
    ScenarioMap.init()
    implicit val system = ActorSystem("afcens-trace-record")
    implicit val executionContext = system.dispatcher

    implicit val timeout = Timeout(1 second)

    implicit val logSource = new LogSource[TraceRecord.type] {
      def genString(x: TraceRecord.type) = "Main"
    }

    val log = Logging(system, this)

    val concurrencyLevel = 32
    val iterationCount = 4096

    var iterationsToGo = (0 until iterationCount).toList
    var iterationsInProgress = List.empty[(Int, ActorRef)]

    while (!iterationsToGo.isEmpty || !iterationsInProgress.isEmpty) {
      var newIterationsInProgress = List.empty[(Int, ActorRef)]
      for ((iterIdx, sim) <- iterationsInProgress.reverse) {
        val state = Await.result((sim ? Simulation.Status).mapTo[SimulationState], timeout.duration)
        log.info(s"Iteration ${iterIdx}: " + state.time.toString)

        if (state.playState != Simulation.State.END) {
          newIterationsInProgress = ((iterIdx, sim)) :: newIterationsInProgress
        } else {
          system.stop(sim)
        }
      }

      iterationsInProgress = newIterationsInProgress

      while (iterationsInProgress.size < concurrencyLevel && !iterationsToGo.isEmpty) {
        val iterIdx = iterationsToGo.head
        iterationsToGo = iterationsToGo.tail

        val sim = system.actorOf(Simulation.props(true, iterIdx, "traces/trace-" + iterIdx))
        sim ! Simulation.Play(0)

        iterationsInProgress = ((iterIdx, sim)) :: iterationsInProgress
      }

      Thread.sleep(1000)
    }

    system.terminate()
  }
}
