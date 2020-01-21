package afcens

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import afcens.afccase._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsNumber, JsString, JsValue, JsonFormat, deserializationError}

trait MarshallersSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit object LocalDateTimeJsonFormat extends JsonFormat[LocalDateTime] {
    def write(x: LocalDateTime) = JsString(x.atZone(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
    def read(value: JsValue) = value match {
      case JsString(x) => LocalDateTime.parse(x, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit object SimulationStateJsonFormat extends JsonFormat[Simulation.State.State] {
    def write(x: Simulation.State.State) = JsNumber(x.id)
    def read(value: JsValue) = value match {
      case JsNumber(x) => Simulation.State(x.toInt)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit object DroneModeJsonFormat extends JsonFormat[DroneMode.DroneMode] {
    def write(x: DroneMode.DroneMode) = JsNumber(x.id)
    def read(value: JsValue) = value match {
      case JsNumber(x) => DroneMode(x.toInt)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit object FlockModeJsonFormat extends JsonFormat[FlockMode.FlockMode] {
    def write(x: FlockMode.FlockMode) = JsNumber(x.id)
    def read(value: JsValue) = value match {
      case JsNumber(x) => FlockMode(x.toInt)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit object ObservedFieldStatusJsonFormat extends JsonFormat[ObservedFieldStatus.ObservedFieldStatus] {
    def write(x: ObservedFieldStatus.ObservedFieldStatus) = JsNumber(x.id)
    def read(value: JsValue) = value match {
      case JsNumber(x) => ObservedFieldStatus(x.toInt)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit val positionFormat = jsonFormat2(Position)
  implicit val chargerIdFormat = jsonFormat1(ChargerId)
  implicit val fieldIdFormat = jsonFormat2(FieldId)
  implicit val observedFieldIdFormat = jsonFormat3(ObservedFieldId)
  implicit val droneStateFormat = jsonFormat5(DroneState)
  implicit val flockStateFormat = jsonFormat3(FlockState)
  implicit val resolutionResultFormat = jsonFormat0(ResolutionResult)
  implicit val simulationStateFormat = jsonFormat5(SimulationState)
}
