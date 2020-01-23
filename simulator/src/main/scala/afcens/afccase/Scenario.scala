package afcens.afccase

import afcens.resolver.{Component, Ensemble, EnsembleSystem, MemberGroup}
import ObservedFieldStatus._
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
    observedFields: Map[String, FieldObservation]
  ) extends Component {
  name(id)

  override def describe =
    s"""DroneComponent "${id}" mode:${mode} position:${position} energy:${energy} charginInChargerId:${chargingInChargerId}"""
}


case class FieldComponent(idx: Int, fieldObservations: List[FieldObservation]) extends Component {
  name(s"Field ${idx}")

  val aggregatedFieldObservations = mutable.HashMap.empty[Int, FieldObservation]

  for (obs <- fieldObservations) {
    val key = obs.fieldId.subIdx
    if (!aggregatedFieldObservations.contains(key)) {
      aggregatedFieldObservations(key) = obs

    } else {
      val existingField = aggregatedFieldObservations(key)

      if (existingField.status == UNKNOWN ||
        (existingField.time.isBefore(obs.time) && obs.status != UNKNOWN) ||
        (existingField.time.isEqual(obs.time) && obs.status == UNDER_THREAT)
      ) {
        aggregatedFieldObservations(key) = obs
      }
    }
  }

  val isUnderThreat = aggregatedFieldObservations.values.exists(_.status == UNDER_THREAT)

  val isUnknown = aggregatedFieldObservations.values.exists(_.status == UNKNOWN)

  val center = FieldIdHelper.center(idx)

  val requiredDroneCountForProtection = FieldIdHelper.protectingDroneCountRequired(idx)
  val protectionCenters = FieldIdHelper.centers(idx, requiredDroneCountForProtection)
}

case class ChargerComponent(idx: Int, isFree: Boolean) extends Component {
  name(s"Charger ${idx}")

  val chargerId = ChargerId(idx)
  val position = chargerId.position
}


class Scenario(simulationState: SimulationState) extends WithAFCEnsTasks {
  def dist2Utility(pos1: Position, pos2: Position) = (50 - pos1.distance(pos2) / 6).round.toInt

  val allDrones = for ((id, st) <- simulationState.drones) yield DroneComponent(id, st.mode, st.position, st.energy, st.chargingInChargerId, st.observedFieldIds)
  val allFields = for (idx <- 0 until ScenarioMap.fieldCount) yield FieldComponent(idx, allDrones.flatMap(_.observedFields.values.filter(_.fieldId.idx == idx)).toList)
  val allChargers = for (idx <- 0 until ScenarioMap.chargerCount) yield ChargerComponent(idx, allDrones.forall(_.chargingInChargerId != Some(ChargerId(idx))))


  class DroneProtectionSystem extends Ensemble {
    name(s"Root ensemble of the protection system")

    val operationalDrones = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy > Drone.chargingThreshold)
    val dronesInNeedOfCharging = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy < Drone.chargingThreshold)

    val fieldsWithUnknownStatus = allFields.filter(_.isUnknown)
    val fieldsUnderThreat = allFields.filter(_.isUnderThreat)

    val freeChargers = allChargers.filter(_.isFree)


    class ProtectionAssignment(field: FieldComponent) extends Ensemble {
      name(s"ProtectionAssignment for field ${field.idx}")

      val droneCount = field.requiredDroneCountForProtection
      val segmentCenters = field.protectionCenters

      class SegmentProtectionAssignment(segmentCenter: Position) extends Ensemble {
        name(s"SegmentProtectionAssignment for field ${field.idx} @ ${segmentCenter.x},${segmentCenter.y}")
        val drone = oneOf(operationalDrones)

        utility {
          drone.sum(x => dist2Utility(x.position, segmentCenter))
        }

        tasks {
          moveTask(drone, segmentCenter)
        }
      }

      val protectionSegmentAssignments = rules(segmentCenters.map(new SegmentProtectionAssignment(_)))

      val protectingDrones = unionOf(protectionSegmentAssignments.map(_.drone))

      utility {
        protectionSegmentAssignments.sum(assignment => assignment.utility) / droneCount
      }

      constraint(
        protectionSegmentAssignments.map(_.drone).allDisjoint
      )
    }


    class PatrolAssignment(field: FieldComponent) extends Ensemble {
      name(s"PatrolAssignment for field ${field.idx}")

      val drone = oneOf(operationalDrones)
      val fieldCenter = field.center

      utility {
        drone.sum(x => dist2Utility(x.position, fieldCenter))
      }

      tasks {
        moveTask(drone, fieldCenter)
      }
    }


    class ChargerAssignment(charger: ChargerComponent) extends Ensemble {
      name(s"ChargerAssignment for charger ${charger.idx}")

      val drone = oneOf(dronesInNeedOfCharging)

      utility {
        drone.sum(drone =>
          dist2Utility(drone.position, charger.position) + (Drone.chargingThreshold - drone.energy).round.toInt
        )
      }

      tasks {
        chargeTask(drone, charger.chargerId)
      }
    }


    val patrolAssignments = ensembles(fieldsWithUnknownStatus.map(new PatrolAssignment(_)))
    val chargerAssignments = ensembles(freeChargers.map(new ChargerAssignment(_)))
    val protectionAssignments = ensembles(fieldsUnderThreat.map(new ProtectionAssignment(_)))

    utility {
      protectionAssignments.sum(assignment => assignment.utility) +
      patrolAssignments.sum(assignment => assignment.utility) / 3 + // The division by 3 expresses the relatively lower importance to the protection
      chargerAssignments.sum(assignment => assignment.utility)
    }

    constraint(
      (patrolAssignments.map(_.drone) ++ protectionAssignments.map(_.protectingDrones)).allDisjoint &&
      chargerAssignments.map(_.drone).allDisjoint
    )
  }

  val root = EnsembleSystem(new DroneProtectionSystem)
}
