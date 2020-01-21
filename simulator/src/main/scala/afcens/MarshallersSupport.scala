package afcens

import afcens.afccase._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsNumber, JsValue, JsonFormat, deserializationError}

trait MarshallersSupport extends SprayJsonSupport with DefaultJsonProtocol {
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

  implicit val positionFormat = jsonFormat2(Position)
  implicit val chargerIdFormat = jsonFormat1(ChargerId)
  implicit val droneStateFormat = jsonFormat4(DroneState)
  implicit val flockStateFormat = jsonFormat3(FlockState)
  implicit val resolutionResultFormat = jsonFormat0(ResolutionResult)
  implicit val simulationStateFormat = jsonFormat5(SimulationState)
}
