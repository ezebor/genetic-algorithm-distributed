package app

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.sharding.ShardRegion.Passivate
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings, ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.cluster.{Cluster, Member}
import com.typesafe.config.ConfigFactory
import domain.actors.EvolutionWorker

class EvolutionWorkersNode(port: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = $port
      |""".stripMargin)
    .withFallback(ConfigFactory.load("resources/application.conf").getConfig("mainConfig"))

  val system = ActorSystem("GeneticAlgorithmSystem", config)
}

object WorkersNode1 extends EvolutionWorkersNode(2561)
object WorkersNode2 extends EvolutionWorkersNode(2562)
