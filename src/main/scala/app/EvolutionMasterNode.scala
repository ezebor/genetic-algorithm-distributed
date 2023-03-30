package app

import akka.actor.*
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.routing.FromConfig
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import domain.Operators.*
import domain.actors.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.{BuildNewGeneration, Execute, ManagerOnline}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

class EvolutionMasterNode(quantityOfWorkersPerNode: Int) extends App with SprayJsonSupport with EvolutionRequestBodyJsonProtocol {
  val configSource = ConfigFactory.load("resources/application.conf")
  val serializationConfig = configSource.getConfig("executeBasketSerializationConfig")
  val mainConfig = configSource.getConfig("mainConfig")
  val masterRouterConfig = configSource.getConfig("masterRouterConfig")

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = 2551
      |akka.actor.deployment./evolutionRouter.cluster.max-nr-of-instances-per-node = $quantityOfWorkersPerNode
      |""".stripMargin)
    .withFallback(serializationConfig)
    .withFallback(masterRouterConfig)
    .withFallback(mainConfig)

  implicit val system: ActorSystem = ActorSystem("GeneticAlgorithmSystem", config)
  implicit val timeout: Timeout = Timeout(3 seconds)
  implicit val ec: ExecutionContext = system.dispatcher

  val routesTree: Route = pathPrefix("api" / "evolution") {
    (post & pathEndOrSingleSlash) {
      entity(as[EvolutionRequestBody]) { case EvolutionRequestBody(
      quantityOfNodes,
      populationSize,
      crossoverLikelihood,
      mutationLikelihood,
      maxQuantityOfGenerationsWithoutImprovements,
      solutionsPopulationsSize
      ) =>
        val generationsManager = system.actorOf(GenerationsManager.props(solutionsPopulationsSize, maxQuantityOfGenerationsWithoutImprovements), s"generationManager")

        val master = system.actorOf(EvolutionMaster.props(
          quantityOfNodes * quantityOfWorkersPerNode,
          system.actorOf(FromConfig.props(EvolutionWorker.props(
            populationSize / (quantityOfNodes * quantityOfWorkersPerNode),
            crossoverLikelihood,
            mutationLikelihood
          )), s"evolutionRouter"),
          generationsManager
        ), s"master")

        generationsManager ? ManagerOnline(master)
        generationsManager ? BuildNewGeneration(BasketsPopulationRandomGenerator.randomPopulation(populationSize))
        complete(StatusCodes.Created)
      }
    }
  }

  Http().newServerAt("localhost", 8080).bind(routesTree)
}

object MasterNode extends EvolutionMasterNode(3)
