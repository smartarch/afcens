package scenario.model
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scenario.model.DroneState.DroneState
import scala.collection.mutable
import scala.runtime.Nothing$


//used by field, charging station and drones
class Position(val x:Int, val y: Int)

object Helper {
  def InverseDistance(distance : Double) : Double = {
    //constraint distance has to be >= 0

    return 1 / (distance + 0.1)
  }
}





//expecting unit size field with rectangular shape
abstract class Field(name: String, val location: Position /*, val size: Position */, val protectedPriority: Int) extends Component {
  name(s"Field:$name")
}

class NeighborField(name:String, location: Position) extends Field("Neighbor " + name, location, -1)
class ProtectedField(name:String, location: Position) extends Field("Protected " + name, location, 1) {
//  var underAttack = false //TODO ?? does the field know or the drone tells if that is the case?
  var patrolledBy : Drone = null
}





class Charger {
  var charging : Drone = null
  def isEmpty() = charging == null
}

class ChargingStation(name: String, capacity: Int) extends Component {
  name(s"Charging station:$name")
  def GetChargers()  = ChargingStation.chargers
}

object ChargingStation {
  val position: Position = new Position(0,0)
  val chargerCount = 5

  var chargers: Seq[Charger] = Seq[Charger]()
  (1 to chargerCount).map(x => chargers = chargers :+ new Charger())

  def freeChargerCount() :Int = {
    chargers.count(ch => ch.isEmpty())
  }
}




class Birds(name: String, val size: Int, val position: Position){
  name(s"Flock of birds:$name")
}



object DroneState extends Enumeration {
  type DroneState = Value
  val ReadyToFly, FlyingToField, MonitoringFields, ProtectingField, ReturningToBase, InQueueForCharging, Charging = Value
}

class Drone (name: String) extends Component {
  name(s"Drone:$name")
  var position = ChargingStation.position //position of the only charging station
  var state = DroneState.ReadyToFly
  var chargedPercentage = 100
  var assignedFields: Seq[Field] = Seq.empty
  var scaringFlock: Birds = null

  def estimateFlockSize(field: Field): Birds = {
    new Birds("??", new scala.util.Random().nextInt(500), field.location)
  }

  def isDoingWork(): Boolean = {
    if (state == DroneState.FlyingToField ||
        state == DroneState.MonitoringFields ||
        state == DroneState.ProtectingField){
      return true
    }
    return false
  }

  def distance(a : Position) : Double = {
    var x = (a.x  - x)
    var y = (a.y - y)

    x = x*x
    y = y*y

    Math.sqrt(x+y)
  }
}


class DroneFieldBirdsModel(val drones: Seq[Drone],
                           val fieldMap: Seq[Field], //total field state space including both protected and not
                           val birds: Seq[Birds]
) extends Model {
  val returnToBaseBatteryThreshold = 30
  val stopChargingThreshold = 85
  val MinimalBirdCountThresholdToProtectField = 5

  class DroneAssignment extends RootEnsemble {
    name("assign drones to fields and charging stations")

    val lowBatteryDrones = drones.filter(_.chargedPercentage <= returnToBaseBatteryThreshold)
    val someWhatChargedDrones = drones.filter(_.chargedPercentage > returnToBaseBatteryThreshold)
    val fullBatteryDrones = drones.filter(_.chargedPercentage >= stopChargingThreshold)


    class ChargingStationAssignment() extends Ensamble {
      class ChargerAssignment(charger: Charger) extends Ensemble {
        val drone = oneOf(lowBatteryDrones)

          //prefer nearby drones
        utility {
          val d : Drone = getInstanceFrom_oneOF_set(drone)

          Helper.InverseDistance(d.distance(ChargingStation.position))
        }
      }

      val chargerAssignment = rules(ChargingStation.chargers.map(new ChargerAssignment(_)))

      // ensure that a drone is not assigned to more than one charger
      constraints(chargerAssignment.map(_.charging).allDisjoint) //TODO bug? when is charger.charging set?

      utility {
//        ChargingStation.chargerCount - ChargingStation.freeChargerCount()
        var sum = 0
        for (charger <- chargerAssignment.... {
          sum += chargerAssignment.utility //TODO get utility of a charger
        }
        sum
      }
      rules(ChargingStation) //TODO ??
    }

    // SCARE the birds
    class BirdScaringAssignment(field : Field) extends Ensemble {
      def computeResponseSize(flockSize: Int):Int = {
        if (flockSize < MinimalBirdCountThresholdToProtectField)
          0
        else
          Math.floor(Math.sqrt(flockSize)).toInt
      }

      //drone estimates the number of birds on the given field
      val flock : Birds = drones.find(d => d.assignedFields.contains(field)).get.estimateFlockSize(field)

      situation {
        (field.isInstanceOf[ProtectedField]) &&
          (flock.size > MinimalBirdCountThresholdToProtectField)
      }

      val droneRequiredCount = computeResponseSize(flock.size)
      val respondingDrones = subsetOf(someWhatChargedDrones)

      constraints {
        //TODO how to get length -- may be lower due to not enough drones avaliable
        getInstanceFrom_subsetOF_(respondingDrones).length < droneRequiredCount
      }

      utility {
        //prefer flying drones that are nearby
        var sum = 0
        val drones :Seq[Drone] = getInstanceFrom_subsetOF_(respondingDrones)
        drones.map(d => sum += Helper.InverseDistance(d.distance(flock.position)))

        //prefer sending required number of drones
        sum += Math.abs(getInstanceFrom_subsetOF_(respondingDrones).length - droneRequiredCount) * 50
      }

      val birdScaringAssignment = rules(fieldMap.filter(_.isInstanceOf[ProtectedField]).map(new BirdScaringAssignment(_)))

    }


    class PatrolAssignment(field : Field) extends Ensemble {
      name("assign ready drones to fields")

      val selected = oneOf(someWhatChargedDrones)


      //prefer closest drones and those that are not being charged
      utility {
        val drone : Drone = getInstanceFrom_oneOF_set(selected)
        Helper.InverseDistance(drone.distance(field.location))
      }

      val dronesToFieldAssignments = rules(fieldMap.map(new PatrolAssignment(_)))


      // ensure that every field is assigned to one drone
      constraints(dronesToFieldAssignments.map(_.assignees).allDisjoint)
      //TODO how to do constraint all different with charger assignment
    }

  }

  val problem = root(new DroneToFieldAssignment)
}
