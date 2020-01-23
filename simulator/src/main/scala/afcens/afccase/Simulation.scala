package afcens.afccase

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime, ZoneOffset}

import akka.actor.{Actor, ActorRef, Props, Stash, Timers}
import akka.event.Logging

import scala.collection.mutable
import scala.concurrent.duration._

case class DroneState(mode: DroneMode.DroneMode, position: Position, energy: Double, chargingInChargerId: Option[ChargerId], observedFieldIds: Map[String, FieldObservation])
case class FlockState(mode: FlockMode.FlockMode, position: Position, observedDrones: List[Position], eatTicks: Int)
case class ResolutionResult(tasks: List[Task])
case class SimulationState(time: LocalDateTime, eatTicks: Int, playState: Simulation.State.State, drones: Map[String, DroneState], flocks: Map[String, FlockState], tasks: List[Task])


abstract class RSVPMessage {
  val uuid = java.util.UUID.randomUUID.toString
}

abstract class Ack
final case class DroneAck(origUUID: String, state: DroneState) extends Ack
final case class FlockAck(origUUID: String, state: FlockState) extends Ack
final case class ResolverAck(origUUID: String, result: ResolutionResult) extends Ack

object Simulation {
  def props(withEnsembles: Boolean) = Props(new Simulation(withEnsembles))

  final case class Play(tickIntervalMs: Int)
  case object Pause
  case object Reset
  case object Status

  final case class SimReset() extends RSVPMessage
  final case class SimStep(currentTime: LocalDateTime, simulationState: SimulationState) extends RSVPMessage

  private case object TickTimer
  private case object Tick
  private case object ResolverTick

  object State extends Enumeration {
    type State = Value

    // This has to be aligned with visualizer/client/src/FactoryMap.js
    val START = Value(0)
    val PLAYING = Value(1)
    val PAUSED = Value(2)
    val END = Value(3)
  }
}

class Simulation(val withEnsembles: Boolean) extends Actor with Timers with Stash {
  simulation =>

  import Simulation.State._
  import Simulation._

  private val log = Logging(context.system, this)

  private class Awaiter(val name: String) {
    val awaitedResponses = mutable.HashMap.empty[String, String] // uuid -> actor name

    def tellWithRSVP(actor: ActorRef, msg: RSVPMessage): Unit = {
      actor ! msg
      awaitedResponses += msg.uuid -> actor.path.name
    }

    def awaitResponses(): Unit = {
      if (!awaitedResponses.isEmpty) {
        context.become(receive)
        unstashAll()
      }
    }

    def removeAwait(uuid: String): Boolean = {
      if (awaitedResponses.contains(uuid) && awaitedResponses(uuid) == sender.path.name) {
        awaitedResponses -= uuid

        if (awaitedResponses.isEmpty) {
          context.become(simulation.receive)
          unstashAll()
        }

        true
      } else {
        log.warning("Ack message uuid not found. Dropping the ACK")
        false
      }
    }

    def receive: Receive = {
      case Status => processStatus()

      case Tick =>
        log.debug(s"Awaiter[${name}].Tick")
        log.warning("Dropping one tick")

      case msg =>
        log.debug(s"Awaiter[${name}]._ ... ${msg}")
        stash()
    }
  }

  private trait SimStepAwaiter extends Awaiter {
    override def receive: Receive = ({
      case DroneAck(origUUID, state) =>
        log.debug(s"SimStepAwaiter[${name}].DroneAck")
        if (removeAwait(origUUID)) {
          droneStates(sender.path.name) = state
        }

      case FlockAck(origUUID, state) =>
        log.debug(s"SimStepAwaiter[${name}].FlockAck")
        if (removeAwait(origUUID)) {
          flockStates(sender.path.name) = state
        }

    }: Receive) orElse(super.receive)
  }

  private trait ResolverAwaiter extends Awaiter {
    override def receive: Receive = ({
      case ResolverAck(origUUID, result) =>
        log.debug(s"ResolverAwaiter[${name}].ResolverAck")
        if (removeAwait(origUUID)) {
          resolutionResult = result
        }

    }: Receive) orElse(super.receive)
  }

  private object simStepAwaiter extends Awaiter("simStep") with SimStepAwaiter
  private object resolverAwaiter extends Awaiter("resolver") with ResolverAwaiter
  private object resetAwaiter extends Awaiter("reset") with SimStepAwaiter with ResolverAwaiter

  private var state = START
  private var currentTime: LocalDateTime = _
  private var ticksToResolution = 0

  private val startTime = LocalDateTime.parse("2020-01-01T08:00:00")
  private val endTime = startTime plus Duration.ofHours(10)

  private val resolver = context.actorOf(Resolver.props(), name = "resolver")
  private var drones = mutable.ListBuffer.empty[ActorRef]
  private var flocks = mutable.ListBuffer.empty[ActorRef]

  private val droneStates = mutable.HashMap.empty[String, DroneState]
  private val flockStates = mutable.HashMap.empty[String, FlockState]
  private var resolutionResult: ResolutionResult = null

  private var tickIntervalMs = 0

  flocks += context.actorOf(Flock.props(), "Flock-1")
  flocks += context.actorOf(Flock.props(), "Flock-2")
  flocks += context.actorOf(Flock.props(), "Flock-3")
  flocks += context.actorOf(Flock.props(), "Flock-4")
  flocks += context.actorOf(Flock.props(), "Flock-5")

  drones += context.actorOf(Drone.props(withEnsembles), "Drone-1")
  drones += context.actorOf(Drone.props(withEnsembles), "Drone-2")
  drones += context.actorOf(Drone.props(withEnsembles), "Drone-3")
  drones += context.actorOf(Drone.props(withEnsembles), "Drone-4")

  processReset()

  private def simulationState = SimulationState(
    currentTime,
    flockStates.values.map(_.eatTicks).sum,
    state,
    droneStates.toMap,
    flockStates.toMap,
    resolutionResult.tasks
  )

  private def processReset(): Unit = {
    state = START
    if (tickIntervalMs > 0) {
      timers.cancel(TickTimer)
    }

    currentTime = startTime
    ticksToResolution = 0
    droneStates.clear()
    flockStates.clear()
    resolutionResult = null

    resetAwaiter.tellWithRSVP(resolver, SimReset())

    for (drone <- drones) {
      resetAwaiter.tellWithRSVP(drone, SimReset())
    }

    for (flock <- flocks) {
      resetAwaiter.tellWithRSVP(flock, SimReset())
    }

    resetAwaiter.awaitResponses()
  }

  private def processPlay(tickIntervalMs: Int): Unit = {
    if (state == START || state == PAUSED) {
      state = PLAYING

      this.tickIntervalMs = tickIntervalMs
      if (tickIntervalMs > 0) {
        timers.startPeriodicTimer(TickTimer, Tick, tickIntervalMs millis)
      } else {
        self ! Tick
      }
    }
  }


  private def processPause(): Unit = {
    if (state == PLAYING) {
      state = PAUSED

      if (tickIntervalMs > 0) {
        timers.cancel(TickTimer)
      }
    }
  }

  private def processTick(): Unit = {
    assert(state == PLAYING)

    currentTime = currentTime plusSeconds(10)

    val simulationStateSnapshot = simulationState

    for (drone <- drones) {
      simStepAwaiter.tellWithRSVP(drone, SimStep(currentTime, simulationStateSnapshot))
    }

    for (flock <- flocks) {
      simStepAwaiter.tellWithRSVP(flock, SimStep(currentTime, simulationStateSnapshot))
    }

    if (ticksToResolution == 0) {
      self ! ResolverTick
      ticksToResolution = 5
    } else {
      ticksToResolution -= 1
    }

    if (!currentTime.isBefore(endTime)) {
      state = END
      if (tickIntervalMs > 0) {
        timers.cancel(TickTimer)
      }
    } else if (tickIntervalMs == 0) {
      self ! Tick
    }

    simStepAwaiter.awaitResponses()

    log.debug(simulationState.toString)
  }

  private def processResolverTick(): Unit = {
    if (withEnsembles) {
      resolverAwaiter.tellWithRSVP(resolver, SimStep(currentTime, simulationState))
      resolverAwaiter.awaitResponses()
    }
  }

  private def processStatus(): Unit = {
    sender() ! simulationState
  }

  def receive = {
    case Play(tickIntervalMs) => processPlay(tickIntervalMs)
    case Pause => processPause()
    case Reset => processReset()
    case Status => processStatus()

    case Tick =>
      log.debug(s"Simulation.Tick")
      processTick()
    case ResolverTick =>
      log.debug(s"Simulation.ResolverTick")
      processResolverTick()

    case msg: Ack =>
      log.debug(s"Simulation.Ack")
      stash()
  }


}

