package app

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.sharding.ShardRegion.Passivate
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings, ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.cluster.{Cluster, Member}
import app.ExecutionScript.QUANTITY_OF_WORKERS_PER_NODE
import com.typesafe.config.ConfigFactory
import domain.actors.{EvolutionWorker, WORKER_ROLE}
import domain.entities.ImagesManager

class EvolutionWorkersNode(port: Int) extends App {
  val configSource = ConfigFactory.load("resources/application.conf")
  val serializationConfig = configSource.getConfig(ExecutionScript.SERIALIZATION_CONFIG)
  val mainConfig = configSource.getConfig("mainConfig")
  val role = "worker"

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = $port
      |akka.cluster.roles = ["$WORKER_ROLE"]
      |""".stripMargin)
    .withFallback(serializationConfig)
    .withFallback(mainConfig)

  ActorSystem("GeneticAlgorithmSystem", config)

  ImagesManager.referencesPixels
  ImagesManager.referencesBlocks
  ImagesManager.referencesImmutableImages
  ImagesManager.referencesImages
}
