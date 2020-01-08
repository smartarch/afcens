package afcens.afccase

import java.time.LocalDateTime

import akka.actor.{Actor, Props}
import akka.event.Logging

import afcens.afccase.Simulation.{Reset, SimReset, SimStep}

import scala.collection.mutable

object Resolver {
  def props(scenarioSpec: TestScenarioSpec) = Props(new Resolver(scenarioSpec))
}

class Resolver(val scenarioSpec: TestScenarioSpec) extends Actor {
  import Resolver._

  private val log = Logging(context.system, this)

  private val solverLimitTime = 60000000000L

  private var scenario: TestScenario = _

  private var currentEpoch: Int = _


  private def processStep(origUUID: String, currentTime: LocalDateTime, simulationState: SimulationState): Unit = {
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

    sender() ! ResolverAck(origUUID, ResolutionResult())
  }

  private def processReset(): Unit = {
    scenario = new TestScenario(scenarioSpec)
  }

  def receive = {
    case msg @ SimStep(currentTime, simulationState) =>
      processStep(msg.uuid, currentTime, simulationState)

    case SimReset() =>
      processReset()
  }

}

