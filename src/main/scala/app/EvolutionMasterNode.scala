package app

import akka.actor.*
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.routing.FromConfig
import com.typesafe.config.ConfigFactory
import domain.Execute
import domain.Operators.*
import domain.actors.*
import domain.individuals.*

class EvolutionMasterNode(quantityOfChildrenPerNode: Int) extends App {
  val configSource = ConfigFactory.load("resources/application.conf")
  val serializationConfig = configSource.getConfig("executeBasketSerializationConfig")
  val mainConfig = configSource.getConfig("mainConfig")
  val masterRouterConfig = configSource.getConfig("masterRouterConfig")

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = 2551
      |akka.actor.deployment./evolutionRouter.cluster.max-nr-of-instances-per-node = $quantityOfChildrenPerNode
      |""".stripMargin)
    .withFallback(serializationConfig)
    .withFallback(masterRouterConfig)
    .withFallback(mainConfig)

  val system = ActorSystem("GeneticAlgorithmSystem", config)

  val master = system.actorOf(EvolutionMaster.props(3 * quantityOfChildrenPerNode, system.actorOf(FromConfig.props(EvolutionWorker.props()), "evolutionRouter")))

  Thread.sleep(10000)

  master ! Execute(EVOLUTION, BasketGenerator.generateRandomPopulation(500))
}

object MasterNode extends EvolutionMasterNode(3)
