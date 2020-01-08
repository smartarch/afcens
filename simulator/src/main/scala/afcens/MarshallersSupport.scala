package afcens

import afcens.afccase._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsNumber, JsValue, JsonFormat, deserializationError}

trait MarshallersSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit object SimulationStateStateJsonFormat extends JsonFormat[Simulation.State.State] {
    def write(x: Simulation.State.State) = JsNumber(x.id)
    def read(value: JsValue) = value match {
      case JsNumber(x) => Simulation.State(x.toInt)
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit val positionFormat = jsonFormat2(Position)
  implicit val droneStateFormat = jsonFormat1(DroneState)
  implicit val flockStateFormat = jsonFormat1(FlockState)
  implicit val resolutionResultFormat = jsonFormat0(ResolutionResult)
  implicit val simulationStateFormat = jsonFormat5(SimulationState)
}
