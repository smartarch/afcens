package afcens

import java.io.File

import afcens.afccase.{ScenarioMap, Simulation, SimulationState}
import akka.actor.{ActorRef, ActorSystem}
import akka.event.{LogSource, Logging}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

object TraceRecordWithPredictor {
  def main(args: Array[String]) {
    ScenarioMap.init()
    implicit val system = ActorSystem("afcens-trace-record")
    implicit val executionContext = system.dispatcher

    implicit val timeout = Timeout(60 second)

    val quantization = 30

    implicit val logSource = new LogSource[TraceRecordWithPredictor.type] {
      def genString(x: TraceRecordWithPredictor.type) = "Main"
    }

    val log = Logging(system, this)

    val concurrencyLevel = 4
    val predictorId = args(0)
    val iterationStart = args(1).toInt
    val iterationUntil = args(2).toInt

    val simulationFactories = Map(
      "standalone" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(false, randSeed, quantization,traceFileBase, null))),
      "ensembles" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization,traceFileBase, null))),
      "dt_multiple_outputs_classifier" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_multiple_outputs_classifier"))),
      "dt_single_output_classifiers" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_single_output_classifiers"))),
      "dt_multiple_outputs_classifier_1m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_multiple_outputs_classifier_1m"))),
      "dt_single_output_classifiers_1m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_single_output_classifiers_1m"))),
      "dt_multiple_outputs_classifier_10m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_multiple_outputs_classifier_10m"))),
      "dt_single_output_classifiers_10m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_single_output_classifiers_10m"))),
      "dt_multiple_outputs_classifier_100m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_multiple_outputs_classifier_100m"))),
      "dt_single_output_classifiers_100m" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://ai-dev.smartarch.cz:5001/dt_single_output_classifiers_100m"))),
      "nn_166k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5166/nn_166k"))),
      "nn_166k2" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5135/nn_166k"))),
      "nn_WHand10" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5404/nn_WHand10"))),
      "nn_WFairHandIgnore" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5505/nn_WFairHandIgnore"))),
      "nn_v3" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5678/nn_v3"))),
      "nn_occam" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5555/nn_occam"))),
      "nn_w10_166k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5200/nn_w10_166k"))),
      "nn_w100_166k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5210/nn_w100_166k"))),
      "occams2" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5270/occams2"))),
      "nn_16k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5220/nn_16k"))),
      "nn_1k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5230/nn_1k"))),
      "nn_w100_16k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5240/nn_w100_16k"))),
      "nn_w10_16k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5250/nn_w10_16k"))),
      "nn_w10_1k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5260/nn_w10_1k"))),
      "nn_occam_16k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5565/nn_occam_16k"))),
      "nn_occam_1k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5575/nn_occam_1k"))),

      "nn_occams_v4_1k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5280/nn_occams_v4_1k"))),
      "nn_occams_v4_16k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5290/nn_occams_v4_16k"))),
      "nn_occams_v4_166k" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5300/nn_occams_v4_166k"))),
      "debug_zeros" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5100/debug_zeros"))),
      "debug_1234" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5101/debug_1234"))),
      "debug_random" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5102/debug_random"))),
      "debug_random_unique" -> ((randSeed: Int, traceFileBase: String) => system.actorOf(Simulation.props(true, randSeed, quantization, traceFileBase, "http://10.10.73.156:5103/debug_random_unique")))

      /*
sh run-predictor-test.sh nn_w10_166k
sh run-predictor-test.sh nn_w100_166k
sh run-predictor-test.sh occams2
sh run-predictor-test.sh nn_16k
sh run-predictor-test.sh nn_1k
sh run-predictor-test.sh nn_w100_16k
sh run-predictor-test.sh nn_w10_16k
sh run-predictor-test.sh nn_w10_1k
 */
    )


    var iterationsToGo = (iterationStart until iterationUntil).toList
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

        val dirPath = f"traces/v2/$predictorId/${iterIdx / 1000}%04d"

        (new File(dirPath)).mkdirs()

        val sim = simulationFactories(predictorId)(iterIdx, f"${dirPath}/${iterIdx % 1000}%03d")
        sim ! Simulation.Play(0)

        iterationsInProgress = ((iterIdx, sim)) :: iterationsInProgress
      }

      Thread.sleep(1000)
    }

    system.terminate()
  }
}
