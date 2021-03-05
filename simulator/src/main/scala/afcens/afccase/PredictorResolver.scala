package afcens.afccase

import java.io.{FileOutputStream, PrintWriter}
import java.time.LocalDateTime
import java.util.zip.GZIPOutputStream

import afcens.MarshallersSupport
import afcens.afccase.Simulation.{SimReset, SimStep}
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, RequestEntity, StatusCodes, _}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.pipe
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


object PredictorResolver {
  def props(predictorUrl: String, traceFileBase: String, quantization: Int) = Props(new PredictorResolver(predictorUrl, traceFileBase, quantization))
}

class PredictorResolver(val predictorUrl: String, val traceFileBase: String, val quantization: Int) extends Actor with MarshallersSupport {

  val traceFileWriter = if (traceFileBase != null) new PrintWriter(new GZIPOutputStream(new FileOutputStream(traceFileBase + ".jsonl.gz"))) else null

  private val log = Logging(context.system, this)

  val http = Http(context.system)

  import context.dispatcher

  final implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(context.system))

  private val solverLimitTime = 60000000000L

  override def postStop(): Unit = {
    if (traceFileWriter != null) {
      traceFileWriter.close()
    }
  }

  private val inputKeys = List(
    "Charge-1-occupied",
    "Charge-2-occupied",
    "Charge-3-occupied",
    "Drone-1-energy",
    "Drone-1-mode",
    "Drone-1-x",
    "Drone-1-y",
    "Drone-2-energy",
    "Drone-2-mode",
    "Drone-2-x",
    "Drone-2-y",
    "Drone-3-energy",
    "Drone-3-mode",
    "Drone-3-x",
    "Drone-3-y",
    "Drone-4-energy",
    "Drone-4-mode",
    "Drone-4-x",
    "Drone-4-y",
    "Flock-1-x",
    "Flock-1-y",
    "Flock-2-x",
    "Flock-2-y",
    "Flock-3-x",
    "Flock-3-y",
    "Flock-4-x",
    "Flock-4-y",
    "Flock-5-x",
    "Flock-5-y"
  )

  private val outputKeys = List(
    "Drone-1-inApproachFieldsUnderThreat-0",
    "Drone-1-inApproachFieldsUnderThreat-1",
    "Drone-1-inApproachFieldsUnderThreat-2",
    "Drone-1-inApproachFieldsUnderThreat-3",
    "Drone-1-inApproachFieldsUnderThreat-4",
    "Drone-1-inPatrolUnknown-0",
    "Drone-1-inPatrolUnknown-1",
    "Drone-1-inPatrolUnknown-2",
    "Drone-1-inPatrolUnknown-3",
    "Drone-1-inPatrolUnknown-4",
    "Drone-1-inScareFormations-0",
    "Drone-1-inScareFormations-1",
    "Drone-1-inScareFormations-2",
    "Drone-1-inScareFormations-3",
    "Drone-1-inScareFormations-4",
    "Drone-1-inChargerAssignments-0",
    "Drone-1-inChargerAssignments-1",
    "Drone-1-inChargerAssignments-2",
    "Drone-2-inApproachFieldsUnderThreat-0",
    "Drone-2-inApproachFieldsUnderThreat-1",
    "Drone-2-inApproachFieldsUnderThreat-2",
    "Drone-2-inApproachFieldsUnderThreat-3",
    "Drone-2-inApproachFieldsUnderThreat-4",
    "Drone-2-inPatrolUnknown-0",
    "Drone-2-inPatrolUnknown-1",
    "Drone-2-inPatrolUnknown-2",
    "Drone-2-inPatrolUnknown-3",
    "Drone-2-inPatrolUnknown-4",
    "Drone-2-inScareFormations-0",
    "Drone-2-inScareFormations-1",
    "Drone-2-inScareFormations-2",
    "Drone-2-inScareFormations-3",
    "Drone-2-inScareFormations-4",
    "Drone-2-inChargerAssignments-0",
    "Drone-2-inChargerAssignments-1",
    "Drone-2-inChargerAssignments-2",
    "Drone-3-inApproachFieldsUnderThreat-0",
    "Drone-3-inApproachFieldsUnderThreat-1",
    "Drone-3-inApproachFieldsUnderThreat-2",
    "Drone-3-inApproachFieldsUnderThreat-3",
    "Drone-3-inApproachFieldsUnderThreat-4",
    "Drone-3-inPatrolUnknown-0",
    "Drone-3-inPatrolUnknown-1",
    "Drone-3-inPatrolUnknown-2",
    "Drone-3-inPatrolUnknown-3",
    "Drone-3-inPatrolUnknown-4",
    "Drone-3-inScareFormations-0",
    "Drone-3-inScareFormations-1",
    "Drone-3-inScareFormations-2",
    "Drone-3-inScareFormations-3",
    "Drone-3-inScareFormations-4",
    "Drone-3-inChargerAssignments-0",
    "Drone-3-inChargerAssignments-1",
    "Drone-3-inChargerAssignments-2",
    "Drone-4-inApproachFieldsUnderThreat-0",
    "Drone-4-inApproachFieldsUnderThreat-1",
    "Drone-4-inApproachFieldsUnderThreat-2",
    "Drone-4-inApproachFieldsUnderThreat-3",
    "Drone-4-inApproachFieldsUnderThreat-4",
    "Drone-4-inPatrolUnknown-0",
    "Drone-4-inPatrolUnknown-1",
    "Drone-4-inPatrolUnknown-2",
    "Drone-4-inPatrolUnknown-3",
    "Drone-4-inPatrolUnknown-4",
    "Drone-4-inScareFormations-0",
    "Drone-4-inScareFormations-1",
    "Drone-4-inScareFormations-2",
    "Drone-4-inScareFormations-3",
    "Drone-4-inScareFormations-4",
    "Drone-4-inChargerAssignments-0",
    "Drone-4-inChargerAssignments-1",
    "Drone-4-inChargerAssignments-2"
  )

  private class InputData {
    val data = new Array[Double](inputKeys.size)

    def update(key: String, value: Double): Unit = {
      val idx = inputKeys.indexOf(key)
      require(idx != -1)
      data(idx) = value
    }

    def toJson = JsArray(data.map(x => JsNumber(x)).toVector)
  }

  case class StepResult(sender: ActorRef, uuid: String, outputs: Array[Double])


  private def processStepAsync(currentTime: LocalDateTime, simulationState: SimulationState): Future[ResolutionResult] = {
    val inputData = new InputData()

    for (chargerIdx <- 0 until ScenarioMap.chargerCount) {
      inputData(s"Charge-${chargerIdx + 1}-occupied") = 0
    }

    for (i <- 1 to Simulation.droneCount) {
      val droneId = s"Drone-${i}"
      val droneState = simulationState.drones(droneId)

      inputData(s"${droneId}-energy") = droneState.energy
      inputData(s"${droneId}-x") = droneState.position.x
      inputData(s"${droneId}-y") = droneState.position.y
      inputData(s"${droneId}-mode") = droneState.mode.id

       droneState.chargingInChargerId match {
         case Some(chargerId) => inputData(s"${chargerId}-occupied") = 1
         case None =>
       }
    }

    for (i <- 1 to Simulation.flockCount) {
      val flockId = s"Flock-${i}"
      val flockState = simulationState.flocks(flockId)

      inputData(s"${flockId}-x") = flockState.position.x
      inputData(s"${flockId}-y") = flockState.position.y
    }

    def doRequest(reqEntity: RequestEntity, remainingAttempts: Int): Future[ResponseEntity] =
      http.singleRequest(HttpRequest(uri = predictorUrl, method = HttpMethods.POST, entity = reqEntity))
        .transformWith {
          case Success(HttpResponse(StatusCodes.OK, _, respEntity, _)) => Future.successful(respEntity)
          case Success(HttpResponse(StatusCodes.InternalServerError, _, _, _)) =>
            if (remainingAttempts > 0)
              doRequest(reqEntity, remainingAttempts - 1)
            else
              Future.failed(new Exception("Too many unsuccessful tries when calling the predictor"))
          case Failure(exc) =>
            if (remainingAttempts > 0)
              doRequest(reqEntity, remainingAttempts - 1)
            else
              Future.failed(exc)
        }

    Marshal(inputData.toJson).to[RequestEntity]
      .flatMap { entity =>
        doRequest(entity, 5)
      }.flatMap { entity =>
          Unmarshal(entity).to[JsArray]
      }.map { jsArray =>
        val output = jsArray.elements.map(x => x.asInstanceOf[JsNumber].value.toInt).toArray

        log.debug(s"output: ${jsArray}")
        var approachFieldUnderThreatAssignment = Map.empty[Int, List[String]]
        for (fieldIdx <- 0 until ScenarioMap.fieldCount) {
          var droneIds = List.empty[String]

          for (droneIdx <- 0 until Simulation.droneCount if output(droneIdx) == fieldIdx + 1) {
            val droneId = s"Drone-${droneIdx + 1}"
            droneIds = droneId :: droneIds
          }

          approachFieldUnderThreatAssignment = approachFieldUnderThreatAssignment + (fieldIdx -> droneIds)
        }
        log.debug(s"approachFieldUnderThreatAssignment: ${approachFieldUnderThreatAssignment}")

        val scenario = new Scenario(simulationState, quantization, approachFieldUnderThreatAssignment)

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

      }.transform {
        case Success(resolutionResult: ResolutionResult) => Success(resolutionResult)
        case Failure(t) =>
          t.printStackTrace()
          context.system.terminate()
          Failure(t)
      }
  }

  private def processReset(): ResolutionResult = {
    ResolutionResult(List())
  }

  def receive = {
    case msg@SimStep(currentTime, simulationState) =>
      processStepAsync(currentTime, simulationState).map(ResolverAck(msg.uuid, _)).pipeTo(sender())

    case msg@SimReset() =>
      sender() ! ResolverAck(msg.uuid, processReset())

    case StepResult(sender, uuid, output) =>
      println(output)
  }
}

