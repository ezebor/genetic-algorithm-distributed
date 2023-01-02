package app

import akka.actor.*
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.routing.FromConfig
import com.typesafe.config.ConfigFactory
import domain.SolutionDescription.*
import domain.actor.*

class EvolutionMasterNode(quantityOfChildrenPerNode: Int) extends App {
  val configSource = ConfigFactory.load("resources/application.conf")
  val mainConfig = configSource.getConfig("mainConfig")
  val masterRouterConfig = configSource.getConfig("masterRouterConfig")

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = 2551
      |akka.actor.deployment./evolutionRouter.cluster.max-nr-of-instances-per-node = $quantityOfChildrenPerNode
      |""".stripMargin)
    .withFallback(masterRouterConfig)
    .withFallback(mainConfig)

  val system = ActorSystem("GeneticAlgorithmSystem", config)

  val master = system.actorOf(EvolutionMaster.props(system.actorOf(FromConfig.props(EvolutionWorker.props()), "evolutionRouter")))

  Thread.sleep(10000)
  (1 to 10).map(i => master ! s"HALAAAAAAA $i")
}

object MasterNode extends EvolutionMasterNode(1)
