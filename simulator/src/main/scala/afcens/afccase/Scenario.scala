package afcens.afccase

import afcens.resolver.{Component, Ensemble, EnsembleSystem}

case class Position(x: Double, y: Double) {
  def distance(pos: Position) = Math.sqrt((pos.x - x) * (pos.x - x) + (pos.y - y) * (pos.y - y))
}

case class DroneComponent(
    id: String,
    mode: DroneMode.DroneMode,
    position: Position,
    energy: Double,
    chargingInChargerId: Option[ChargerId]
  ) extends Component {
  name(id)

  override def describe =
    s"""DroneComponent "${id}" mode:${mode} position:${position} energy:${energy} charginInChargerId:${chargingInChargerId}"""
}

case class FlockComponent(
    id: String,
    mode: FlockMode.FlockMode,
    position: Position
  ) extends Component {
  name(id)

  override def describe =
    s"""FlockComponent "${id}" mode:${mode} position:${position}"""
}

case class FieldComponent(idx: Int) extends Component {
  name(s"Field ${idx}")

  val flockAtFieldDistanceThreshold = 1

  def allPositionIds = for {
    otherSubIdx <- 0 until ScenarioMap.fieldSizes(idx)
  } yield FieldId(idx, otherSubIdx)

  def isInField(position: Position) = allPositionIds.exists(fldId => fldId.position.distance(position) <= flockAtFieldDistanceThreshold)
}

class Scenario(simulationState: SimulationState) {
  val allFields = for (idx <- 0 until ScenarioMap.fieldCount) yield FieldComponent(idx)
  val allDrones = for ((id, st) <- simulationState.drones) yield DroneComponent(id, st.mode, st.position, st.energy, st.chargingInChargerId)
  val allFlocks = for ((id, st) <- simulationState.flocks) yield FlockComponent(id, st.mode, st.position)

  val observedFlockPositions = for {
    flock <- allFlocks
    drone <- allDrones
    if flock.position.distance(drone.position) < Drone.visibilityRadius
  } yield flock.position

  class DroneProtectionSystem extends Ensemble {
    name(s"Root ensemble of the protection system")

    class ProtectionGroup(field: FieldComponent) extends Ensemble {
      name(s"Drones protecting ${field}")

      situation {
        observedFlockPositions.exists(flockPos => field.isInField(flockPos))
      }

      val drones = subsetOf(allDrones, card => card >= 1)

      utility {
        3 + drones.cardinality
      }
    }

    utility {
      protectionGroups.sum(_.utility)
    }

    val protectionGroups = rules(allFields.map(new ProtectionGroup(_))) // TODO try "ensembles"

    constraint(protectionGroups.map(_.drones).allDisjoint)
  }

  val root = EnsembleSystem(new DroneProtectionSystem)
}
