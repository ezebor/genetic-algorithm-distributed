package app

import akka.actor.*
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import com.typesafe.config.ConfigFactory
import domain.SolutionDescription.*
import domain.actor.*

object PopulationNode extends App {
  val config = ConfigFactory.parseString(
    """
      |akka.remote.artery.canonical.port = 2551
      |""".stripMargin)
    .withFallback(ConfigFactory.load("resources/application.conf"))

  val system = ActorSystem("GeneticAlgorithmSystem", config)

  val populationActorProxy = system.actorOf(
    ClusterSingletonProxy.props(
      singletonManagerPath = "/user/populationActor",
      settings = ClusterSingletonProxySettings(system)
    ),
    "populationActorProxy"
  )

  populationActorProxy ! "HALAAAAA"
}
