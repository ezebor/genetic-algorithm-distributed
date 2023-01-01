package app

import akka.actor.*
import com.typesafe.config.ConfigFactory
import domain.SolutionDescription.*
import domain.actor.*

/**
 * TODO: AGREGAR MAILBOX
 */

object Population extends App {
  val config = ConfigFactory.parseString(
    """
      |akka.remote.artery.canonical.port = 2551
      |""".stripMargin)
    .withFallback(ConfigFactory.load("resources/application.conf"))

  val system = ActorSystem("PopulationSystem", config)

  val populationManager = system.actorOf(PopulationManager.props(500), "populationManager")

  populationManager ! "HOLAA"
}
