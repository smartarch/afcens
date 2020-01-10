package afcens.afccase

import java.time.LocalDateTime

import akka.actor.{Actor, Props}
import akka.event.Logging

import afcens.afccase.Simulation.{Reset, SimReset, SimStep}

import scala.collection.mutable

object Resolver {
  def props(scenarioSpec: ScenarioSpec) = Props(new Resolver(scenarioSpec))
}

class Resolver(val scenarioSpec: ScenarioSpec) extends Actor {
  import Resolver._

  private val log = Logging(context.system, this)

  private val solverLimitTime = 60000000000L

  private var scenario: Scenario = _

  private var currentEpoch: Int = _


  private def processStep(currentTime: LocalDateTime, simulationState: SimulationState): ResolutionResult = {
    // log.info("Resolver started")
    // log.info("Time: " + currentTime)
    // log.info("Events: " + events)


    /*
    scenario.now = currentTime

    val factoryTeam = scenario.factoryTeam
    factoryTeam.init()
    factoryTeam.solverLimitTime(solverLimitTime)
    factoryTeam.solve()
        if (factoryTeam.exists) {
      // log.info("Utility: " + shiftTeams.instance.solutionUtility)
      // log.info(shiftTeams.instance.toString)

      factoryTeam.commit()

    } else {
      log.error("Error. No solution exists.")
    }
    */

    ResolutionResult()
  }

  private def processReset(): ResolutionResult = {
    scenario = new Scenario(scenarioSpec)
    ResolutionResult()
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      sender() ! ResolverAck(msg.uuid, processStep(currentTime, simulationState))

    case msg @ SimReset() =>
      sender() ! ResolverAck(msg.uuid, processReset())
  }

}

