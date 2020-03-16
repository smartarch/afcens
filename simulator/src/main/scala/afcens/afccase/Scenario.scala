package afcens.afccase

import afcens.resolver.{Component, Ensemble, EnsembleSystem, MemberGroup}
import ObservedFieldStatus._
import afcens.MarshallersSupport
import spray.json._

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

  def moveTask(drone: DroneComponent, targetPosition: Position): Unit = {
    emittedTasks += MoveTask(drone.id, targetPosition)
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


/*
case class FieldComponent(idx: Int, fieldObservations: List[FieldObservation]) extends Component {
  name(s"Field ${idx}")

  val aggregatedObservations = mutable.HashMap.empty[Int, FieldObservation]

  for (obs <- fieldObservations) {
    val key = obs.fieldId.subIdx
    if (!aggregatedObservations.contains(key)) {
      aggregatedObservations(key) = obs

    } else {
      val existing = aggregatedObservations(key)

      if (existing.status == UNKNOWN ||
        (existing.time.isBefore(obs.time) && obs.status != UNKNOWN) ||
        (existing.time.isEqual(obs.time) && obs.status == UNDER_THREAT)
      ) {
        aggregatedObservations(key) = obs
      }
    }
  }

  val isUnderThreat = aggregatedObservations.values.exists(_.status == UNDER_THREAT)

  val isUnknown = aggregatedObservations.values.exists(_.status == UNKNOWN)

  val center = FieldIdHelper.center(idx)

  val requiredDroneCountForProtection = FieldIdHelper.protectingDroneCountRequired(idx)
  val protectionCenters = FieldIdHelper.centers(idx, requiredDroneCountForProtection)

  val area = FieldIdHelper.area(idx)
}
*/

case class FieldComponent(idx: Int, flocks: Map[String, FlockState]) extends Component {
  name(s"Field ${idx}")

  val center = FieldIdHelper.center(idx)
  val area = FieldIdHelper.area(idx)

  val isUnknown = false

  val isUnderThreat = flocks.values.exists(flock => area.contains(flock.position))

  val requiredDroneCountForProtection = FieldIdHelper.protectingDroneCountRequired(idx)
  val protectionCenters = FieldIdHelper.centers(idx, requiredDroneCountForProtection)
}

case class ChargerComponent(idx: Int, isFree: Boolean) extends Component {
  name(s"Charger ${idx}")

  val chargerId = ChargerId(idx)
  val position = chargerId.position
}

case class FlockComponent(position: Position) extends Component {
  name(s"Flock @ ${position}")
}


class Scenario(simulationState: SimulationState) extends WithAFCEnsTasks with MarshallersSupport {
  def dist2Utility(pos1: Position, pos2: Position) = (10 - pos1.distance(pos2) / 30).round.toInt

  val allDrones = for ((id, st) <- simulationState.drones) yield DroneComponent(id, st.mode, st.position, st.energy, st.chargingInChargerId, st.observedFieldIds)
  val allFields = for (idx <- 0 until ScenarioMap.fieldCount) yield FieldComponent(idx, simulationState.flocks)
  val allChargers = for (idx <- 0 until ScenarioMap.chargerCount) yield ChargerComponent(idx, allDrones.forall(_.chargingInChargerId != Some(ChargerId(idx))))
  val allFlocks = simulationState.flocks.values.map(x => FlockComponent(x.position))


  class DroneProtectionSystem extends Ensemble {
    name(s"Root ensemble of the protection system")

    val operationalDrones = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy > Drone.chargingThreshold)
    val dronesInNeedOfCharging = allDrones.filter(drone => drone.mode != DroneMode.DEAD && drone.mode != DroneMode.CHARGING && drone.energy < Drone.chargingThreshold)

    val fieldsWithUnknownStatus = allFields.filter(_.isUnknown)
    val fieldsUnderThreat = allFields.filter(_.isUnderThreat)

    val freeChargers = allChargers.filter(_.isFree)


    object ApproachFieldUnderThreat {
      def isInSituation(field: FieldComponent) = {
        val dronesInField = operationalDrones.filter(x => field.area.contains(x.position))
        val droneCount = field.requiredDroneCountForProtection

        dronesInField.size < droneCount
      }
    }

    class ApproachFieldUnderThreat(val field: FieldComponent) extends Ensemble {
      name(s"ApproachFieldUnderThreat ensemble for field ${field.idx}")

      val flocksInField = allFlocks.filter(x => field.area.contains(x.position))
      val dronesInField = operationalDrones.filter(x => field.area.contains(x.position))

      val droneCount = field.requiredDroneCountForProtection
      val center = field.center

      /*
      situation {
        dronesInField.size < droneCount
      }
       */

      val drones = subsetOfComponents(operationalDrones, _ <= droneCount)

      utility {
        drones.sum(x => if (field.area.contains(x.position)) 10 else dist2Utility(x.position, center))
      }

      tasks {
        if (flocksInField.isEmpty) {
          for (drone <- drones.selectedMembers) moveTask(drone, center)
        } else {
          val selectedDronesInFieldCount = drones.selectedMembers.count(x => field.area.contains(x.position))

          val flockPos = flocksInField.head.position

          val step = Flock.disturbRadius * 2
          var x = flockPos.x - (selectedDronesInFieldCount - 1) * Flock.disturbRadius

          for (drone <- drones.selectedMembers) {
            if (field.area.contains(drone.position)) {
              moveTask(drone, Position(x, flockPos.y))
              x += step
            } else {
              moveTask(drone, center)
            }
          }
        }
      }

      def toJson: JsValue = JsObject(
        "id" -> field.idx.toJson,
        "drones" -> drones.selectedMembers.map(_.id).toJson
      )
    }


    object ScareFormation {
      def isInSituation(field: FieldComponent) = {
        val dronesInField = operationalDrones.filter(x => field.area.contains(x.position))
        val droneCount = field.requiredDroneCountForProtection

        dronesInField.size >= droneCount
      }
    }

    class ScareFormation(val field: FieldComponent) extends Ensemble {
      name(s"ScareFormation ensemble for field ${field.idx}")

      val dronesInField = operationalDrones.filter(x => field.area.contains(x.position))

      val droneCount = field.requiredDroneCountForProtection
      val segmentCenters = field.protectionCenters

      /*
      situation {
        dronesInField.size >= droneCount
      }
       */

      class SegmentAssignment(val segmentCenter: Position) extends Ensemble {
        name(s"SegmentProtectionAssignment for field ${field.idx} @ ${segmentCenter.x},${segmentCenter.y}")
        val drone = oneOf(operationalDrones)

        utility {
          drone.sum(x => dist2Utility(x.position, segmentCenter))
        }

        tasks {
          moveTask(drone, segmentCenter)
        }

        def toJson: JsValue = JsObject(
          "id" -> segmentCenter.toJson,
          "drone" -> drone.selectedMembers.head.id.toJson
        )
      }

      val protectionSegmentAssignments = rules(segmentCenters.map(new SegmentAssignment(_)))

      val drones = unionOf(protectionSegmentAssignments.map(_.drone))

      utility {
        protectionSegmentAssignments.sum(assignment => assignment.utility) / droneCount
      }

      constraint(
        protectionSegmentAssignments.map(_.drone).allDisjoint
      )

      def toJson: JsValue = JsObject(
        "id" -> field.idx.toJson,
        "protectionSegmentAssignments" -> protectionSegmentAssignments.selectedMembers.map(_.toJson).toJson,
        "drones" -> drones.selectedMembers.map(_.id).toJson
      )
    }


    class PatrolUnknown(val field: FieldComponent) extends Ensemble {
      name(s"PatrolUnknown ensemble for field ${field.idx}")

      val drone = oneOf(operationalDrones)
      val fieldCenter = field.center

      utility {
        drone.sum(x => dist2Utility(x.position, fieldCenter))
      }

      tasks {
        moveTask(drone, fieldCenter)
      }

      def toJson: JsValue = JsObject(
        "id" -> field.idx.toJson,
        "drone" -> drone.selectedMembers.head.id.toJson
      )
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

      def toJson: JsValue = JsObject(
        "id" -> charger.idx.toJson,
        "drone" -> drone.selectedMembers.head.id.toJson
      )
    }


    val patrolUnknown = ensembles(fieldsWithUnknownStatus.map(new PatrolUnknown(_)))
    val chargerAssignments = ensembles(freeChargers.map(new ChargerAssignment(_)))
    val approachFieldsUnderThreat = ensembles(fieldsUnderThreat.filter(ApproachFieldUnderThreat.isInSituation(_)).map(new ApproachFieldUnderThreat(_)))
    val scareFormations = ensembles(fieldsUnderThreat.filter(ScareFormation.isInSituation(_)).map(new ScareFormation(_)))

    utility {
      approachFieldsUnderThreat.sum(assignment => assignment.utility) +
      scareFormations.sum(assignment => assignment.utility) +
      patrolUnknown.sum(assignment => assignment.utility) / 4 + // The division by 4 expresses the relatively lower importance to the protection
      chargerAssignments.sum(assignment => assignment.utility)
    }

    constraint(
      (patrolUnknown.map(_.drone) ++ approachFieldsUnderThreat.map(_.drones) ++ scareFormations.map(_.drones)).allDisjoint &&
      chargerAssignments.map(_.drone).allDisjoint
    )

    def toJson: JsValue = JsObject(
      "patrolUnknown" -> patrolUnknown.selectedMembers.map(_.toJson).toJson,
      "chargerAssignments" -> chargerAssignments.selectedMembers.map(_.toJson).toJson,
      "approachFieldsUnderThreat" -> approachFieldsUnderThreat.selectedMembers.map(_.toJson).toJson,
      "scareFormations" -> scareFormations.selectedMembers.map(_.toJson).toJson,
    )
  }

  val root = EnsembleSystem(new DroneProtectionSystem)
}
