package afcens.afccase
import scala.xml._

object Map {
  private var positions: Map[String, Position] = _

  def init(): Unit = {
    val xmlFile = XML.loadFile("fields-symbols.svg")

    positions = (
      for {
        positions <- xmlFile \ "g" if positions \@ "id" == "Positions"
        posCircle <- positions \ "circle"
        posId = posCircle \@ "id"
        posX = posCircle \@ "cx"
        posY = posCircle \@ "cy"
      } yield posId -> Position(posX.toDouble, posY.toDouble)
      ).toMap
  }

  def apply(id: String): Position = positions("Pos-" + id)

  def main(args: Array[String]) {
    Map.init()
    println(positions.keys)
  }
}