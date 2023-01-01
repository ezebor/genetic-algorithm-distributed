package app

import akka.actor.*
import com.typesafe.config.ConfigFactory
import domain.actor._
import domain.SolutionDescription.*

object Population extends App {
  val config = ConfigFactory.parseString(
    """
      |akka.actor.provider = cluster
      |akka.remote.artery.canonical.hostname = localhost
      |akka.remote.artery.canonical.port = 2551
      |""".stripMargin)
    .withFallback(ConfigFactory.load("application.conf"))

  val system = ActorSystem("PopulationSystem", config)

  val populationManager = system.actorOf(PopulationManager.props(500), "populationManager")

  populationManager ! "HOLAA"
}
