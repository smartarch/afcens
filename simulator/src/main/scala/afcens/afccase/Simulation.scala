package afcens.afccase

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime, ZoneOffset}

import akka.actor.{Actor, ActorRef, Props, Timers, Stash}
import akka.event.Logging
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.duration._

case class DroneState(position: Position)
case class FlockState(position: Position)
case class ResolutionResult()
case class SimulationState(time: String, playState: Simulation.State.State, drones: Map[String, DroneState], flocks: Map[String, FlockState], ensembles: ResolutionResult)


abstract class RSVPMessage {
  val uuid = java.util.UUID.randomUUID.toString
}

final case class DroneAck(origUUID: String, state: DroneState)
final case class FlockAck(origUUID: String, state: FlockState)
final case class ResolverAck(origUUID: String, result: ResolutionResult)

object Simulation {
  def props() = Props(new Simulation())

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

class Simulation() extends Actor with Timers with Stash {
  import Simulation.State._
  import Simulation._

  private val log = Logging(context.system, this)

  private var state = START
  private var currentTime: LocalDateTime = _
  private var ticksToResolution = 0

  private val startTime = LocalDateTime.parse("2020-01-01T08:00:00")
  private val endTime = startTime plus Duration.ofHours(10)

  private val scenarioSpec = TestScenario.createScenarioSpec(droneCount = 3, flockCount = 2, startTime)

  private val resolver = context.actorOf(Resolver.props(scenarioSpec), name = "resolver")
  private var drones = mutable.ListBuffer.empty[ActorRef]
  private var flocks = mutable.ListBuffer.empty[ActorRef]

  private val droneStates = mutable.HashMap.empty[String, DroneState]
  private val flockStates = mutable.HashMap.empty[String, FlockState]
  private var resolutionResult: ResolutionResult = null

  private val awaitedResponses = mutable.HashMap.empty[String, String] // uuid -> actor name

  private var tickIntervalMs = 0

  processReset()


  private def tellWithRSVP(actor: ActorRef, msg: RSVPMessage): Unit = {
    actor ! msg
    awaitedResponses += msg.uuid -> actor.path.name
  }

  private def awaitResponses(): Unit = {
    if (!awaitedResponses.isEmpty) {
      context.become(awaitingResponses)
    }
  }

  private def removeAwait(uuid: String, sender: ActorRef): Unit = {
    assert(awaitedResponses(uuid) == sender.path.name)
    awaitedResponses -= uuid

    if (awaitedResponses.isEmpty) {
      context.become(receive)
      unstashAll()
    }
  }

  def awaitingResponses: Receive = {
    case Tick =>
      log.warning("Dropping one tick")

    case DroneAck(origUUID, state) =>
      droneStates(awaitedResponses(origUUID)) = state
      removeAwait(origUUID, sender())

    case FlockAck(origUUID, state) =>
      flockStates(awaitedResponses(origUUID)) = state
      removeAwait(origUUID, sender())

    case ResolverAck(origUUID, result) =>
      resolutionResult = result
      removeAwait(origUUID, sender())

    case _ => stash()
  }


  private def simulationState = SimulationState(
    currentTime.atZone(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
    state,
    droneStates.toMap,
    flockStates.toMap,
    resolutionResult
  )

  private def processReset(): Unit = {
    state = START
    if (tickIntervalMs > 0) {
      timers.cancel(TickTimer)
    }

    currentTime = null
    ticksToResolution = 0
    droneStates.clear()
    flockStates.clear()
    resolutionResult = null

    tellWithRSVP(resolver, SimReset())

    for (drone <- drones) {
      tellWithRSVP(drone, SimReset())
    }

    for (flock <- flocks) {
      tellWithRSVP(flock, SimReset())
    }

    awaitResponses()
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

    if (currentTime == null) {
      currentTime = startTime
    } else {
      currentTime = currentTime plusSeconds(10)
    }

    val simulationStateSnapshot = simulationState

    for (drone <- drones) {
      tellWithRSVP(drone, SimStep(currentTime, simulationStateSnapshot))
    }

    for (flock <- flocks) {
      tellWithRSVP(flock, SimStep(currentTime, simulationStateSnapshot))
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

    awaitResponses()
  }

  private def processResolverTick(): Unit = {
    // TODO - invoke resolver

    awaitResponses()
  }

  private def processStatus(): Unit = {
    sender() ! simulationState
  }

  def receive = {
    case Play(tickIntervalMs) => processPlay(tickIntervalMs)
    case Pause => processPause()
    case Reset => processReset()
    case Status => processStatus()

    case Tick => processTick()
    case ResolverTick => processResolverTick()
  }


}

