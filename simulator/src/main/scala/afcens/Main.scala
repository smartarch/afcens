package afcens

import scala.language.postfixOps
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import afcens.afccase.{ScenarioMap, Simulation, SimulationState}
import akka.pattern.ask

import scala.concurrent.duration._
import scala.io.StdIn

object Main extends MarshallersSupport {
  def main(args: Array[String]) {
    ScenarioMap.init()
    implicit val system = ActorSystem("afcens-demo")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    implicit val timeout = Timeout(1 second)

    val simulation = system.actorOf(Simulation.props(true, 0, 30, null, null))

    simulation ! Simulation.Play(100)

    val route =
      path("play") {
        post {
          simulation ! Simulation.Play(100)
          complete(OK)
        }
      } ~
      path("pause") {
        post {
          simulation ! Simulation.Pause
          complete(OK)
        }
      } ~
      path("reset") {
        post {
          simulation ! Simulation.Reset
          complete(OK)
        }
      } ~
      path("status") {
        get {
          complete((simulation ? Simulation.Status).mapTo[SimulationState])
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
