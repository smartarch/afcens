package afcens.afccase

import afcens.resolver.{Component, Ensemble, EnsembleSystem, MemberGroup}

import scala.collection.mutable

case class Position(x: Double, y: Double) {
  def distance(pos: Position) = Math.sqrt((pos.x - x) * (pos.x - x) + (pos.y - y) * (pos.y - y))
}

abstract class Task
case class MoveTask(droneId: String, targetPosition: Position) extends Task
case class ChargeTask(droneId: String, chargerId: ChargerId) extends Task

trait WithAFCEnsTasks {
  val emittedTasks = mutable.ListBuffer.empty[Task]

  private def selectOne[T <: Component](comp: MemberGroup[T]): T = {
    comp.selectedMembers.head
  }

  def moveTask(drone: MemberGroup[DroneComponent], targetPosition: Position): Unit = {
    emittedTasks += MoveTask(selectOne(drone).id, targetPosition)
  }

  def chargeTask(drone: MemberGroup[DroneComponent], chargerId: ChargerId): Unit = {
    emittedTasks += ChargeTask(selectOne(drone).id, chargerId)
  }
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


class Scenario(simulationState: SimulationState) extends WithAFCEnsTasks {
  val allFields = for (idx <- 0 until ScenarioMap.fieldCount) yield FieldComponent(idx)
  val allDrones = for ((id, st) <- simulationState.drones) yield DroneComponent(id, st.mode, st.position, st.energy, st.chargingInChargerId, st.observedFieldIds)

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

  val fieldsWithUnknownStatus = for {
    fieldIdx <- 0 until ScenarioMap.fieldCount
    if aggregatedFieldObservations.values.exists(x => x.fieldId.idx == fieldIdx && x.status == ObservedFieldStatus.UNKNOWN)
  } yield fieldIdx

  val freeChargers = for {
    chargerId <- ScenarioMap.allChargerIds
    if allDrones.forall(_.chargingInChargerId != Some(chargerId))
  } yield chargerId




  class DroneProtectionSystem extends Ensemble {
    name(s"Root ensemble of the protection system")


    val operationalDrones = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy > Drone.chargingThreshold)

    class PatrolAssignment(fieldIdx: Int) extends Ensemble {
      val drone = oneOf(operationalDrones)
      val fieldCenter = FieldIdHelper.centerPosition(fieldIdx)

      utility {
        drone.sum(drone =>
          100 - (drone.position.distance(fieldCenter) / 20).round.toInt
        )
      }

      tasks {
        moveTask(drone, fieldCenter)
      }
    }


    val dronesInNeedOfCharging = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy < Drone.chargingThreshold)

    class ChargerAssignment(chargerId: ChargerId) extends Ensemble {
      val drone = oneOf(dronesInNeedOfCharging)

      utility {
        drone.sum(drone =>
          (50 - drone.position.distance(chargerId.position) / 30 + Drone.chargingThreshold - drone.energy).round.toInt
        )
      }

      tasks {
        chargeTask(drone, chargerId)
      }
    }


    /* TODO: Handle protection */

    /* TODO: Handle non-assigned drones if any */

    val patrolAssignments = ensembles(fieldsWithUnknownStatus.map(new PatrolAssignment(_)))
    val chargerAssignments = ensembles(freeChargers.map(new ChargerAssignment(_)))

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
