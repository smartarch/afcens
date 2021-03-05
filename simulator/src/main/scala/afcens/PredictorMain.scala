package afcens

import afcens.afccase.{ScenarioMap, Simulation, SimulationState}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout

import scala.concurrent.duration._
import scala.io.StdIn
import scala.language.postfixOps

object PredictorMain extends MarshallersSupport {
  def main(args: Array[String]) {
    ScenarioMap.init()
    implicit val system = ActorSystem("afcens-demo")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    implicit val timeout = Timeout(1 second)

    val quantization = 30

    val simulations = Map(
      "standalone" -> system.actorOf(Simulation.props(false, 0, quantization,null, null)),
      "ensembles" -> system.actorOf(Simulation.props(true, 0, quantization,null, null)),
      "dt_10k_single" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_10k_single")),
      "dt_100k_single" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_100k_single")),
      "dt_500k_single" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_500k_single")),
      "dt_10k_multiple" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_10k_multiple")),
      "dt_100k_multiple" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_100k_multiple")),
      "dt_500k_multiple" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5001/dt_500k_multiple")),
      "nn_10k" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5000/nn_10k")),
      "nn_100k" -> system.actorOf(Simulation.props(true, 0, quantization, null, "http://10.10.73.156:5000/nn_100k"))
    )

    val route =
      pathPrefix(Segment) { predictorId =>
        path("play") {
          post {
            simulations(predictorId) ! Simulation.Play(100)
            complete(OK)
          }
        } ~
        path("pause") {
          post {
            simulations(predictorId) ! Simulation.Pause
            complete(OK)
          }
        } ~
        path("reset") {
          post {
            simulations(predictorId) ! Simulation.Reset
            complete(OK)
          }
        } ~
        path("status") {
          get {
            complete((simulations(predictorId) ? Simulation.Status).mapTo[SimulationState])
          }
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 3100)

    println("Listening on 0.0.0.0:3100.")

    println("Press ENTER to finish.")
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
