package afcens.afccase

import afcens.resolver.{Component, Ensemble, EnsembleSystem}

import scala.collection.mutable

case class Position(x: Double, y: Double) {
  def distance(pos: Position) = Math.sqrt((pos.x - x) * (pos.x - x) + (pos.y - y) * (pos.y - y))
}

case class DroneComponent(
    id: String,
    mode: DroneMode.DroneMode,
    position: Position,
    energy: Double,
    chargingInChargerId: Option[ChargerId],
    observedFields: Map[String, ObservedFieldId]
  ) extends Component {
  name(id)

  override def describe =
    s"""DroneComponent "${id}" mode:${mode} position:${position} energy:${energy} charginInChargerId:${chargingInChargerId}"""
}

case class FieldComponent(idx: Int) extends Component {
  name(s"Field ${idx}")

  val flockAtFieldDistanceThreshold = 1

  def allPositionIds = for {
    otherSubIdx <- 0 until ScenarioMap.fieldSizes(idx)
  } yield FieldId(idx, otherSubIdx)

  def isFlockInField(position: Position) = allPositionIds.exists(fldId => fldId.position.distance(position) <= flockAtFieldDistanceThreshold)
}

class Scenario(simulationState: SimulationState) {
  val allFields = for (idx <- 0 until ScenarioMap.fieldCount) yield FieldComponent(idx)
  val allDrones = for ((id, st) <- simulationState.drones) yield DroneComponent(id, st.mode, st.position, st.energy, st.chargingInChargerId, st.observedFieldIds)

  class DroneProtectionSystem extends Ensemble {
    name(s"Root ensemble of the protection system")

    val aggregatedFieldObservations = mutable.HashMap.empty[String, ObservedFieldId]
    for (drone <- allDrones) {
      for ((key, field) <- drone.observedFields) {
        if (!aggregatedFieldObservations.contains(key)) {
          aggregatedFieldObservations(key) = field

        } else {
          val existingField = aggregatedFieldObservations(key)

          if (existingField.status == ObservedFieldStatus.UNKNOWN ||
            (existingField.time.isBefore(field.time) && field.status != ObservedFieldStatus.UNKNOWN) ||
            (existingField.time.isEqual(field.time) && field.status == ObservedFieldStatus.UNDER_THREAT)
          ) {
            aggregatedFieldObservations(key) = field
          }
        }
      }
    }



    val operationalDrones = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy > Drone.chargingThreshold)

    val unknownFieldIdxs = for {
      fieldIdx <- 0 until ScenarioMap.fieldCount
      if aggregatedFieldObservations.values.exists(x => x.fieldId.idx == fieldIdx && x.status == ObservedFieldStatus.UNKNOWN)
    } yield fieldIdx


    class PatrolAssignment(fieldIdx: Int) extends Ensemble {
      val drone = oneOf(operationalDrones)
      val fieldCenter = FieldIdHelper.centerPosition(fieldIdx)

      utility {
        drone.sum(drone =>
          100 - (drone.position.distance(fieldCenter) / 20).round.toInt
        )
      }
    }


    val dronesInNeedOfCharging = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy < Drone.chargingThreshold)

    val freeChargerIds = for {
      chargerId <- ScenarioMap.allChargerIds
      if allDrones.forall(_.chargingInChargerId != Some(chargerId))
    } yield chargerId

    class ChargerAssignment(chargerId: ChargerId) extends Ensemble {
      val drone = oneOf(dronesInNeedOfCharging)

      utility {
        drone.sum(drone =>
          (50 - drone.position.distance(chargerId.position) / 30 + Drone.chargingThreshold - drone.energy).round.toInt
        )
      }
    }


    val patrolAssignments = ensembles(unknownFieldIdxs.map(new PatrolAssignment(_)))
    val chargerAssignments = ensembles(freeChargerIds.map(new ChargerAssignment(_)))

    utility {
      patrolAssignments.sum(assignment => assignment.utility) + chargerAssignments.sum(assignment => assignment.utility)
    }

    constraint(
      patrolAssignments.map(_.drone).allDisjoint &&
      chargerAssignments.map(_.drone).allDisjoint
    )
  }

  val root = EnsembleSystem(new DroneProtectionSystem)
}
