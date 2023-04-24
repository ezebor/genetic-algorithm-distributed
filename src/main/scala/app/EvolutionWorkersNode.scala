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
  val configSource = ConfigFactory.load("resources/application.conf")
  val serializationConfig = configSource.getConfig(ExecutionScript.SERIALIZATION_CONFIG)
  val mainConfig = configSource.getConfig("mainConfig")

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = $port
      |""".stripMargin)
    .withFallback(serializationConfig)
    .withFallback(mainConfig)

  val system = ActorSystem("GeneticAlgorithmSystem", config)
}
