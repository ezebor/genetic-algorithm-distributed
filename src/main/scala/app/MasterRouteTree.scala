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
import domain.*

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

object MasterRouteTree extends Parallel with SprayJsonSupport with EvolutionRequestBodyJsonProtocol {

  implicit val timeout: Timeout = Timeout(3 seconds)

  def apply(
             generationsManager: ActorRef,
             master: ActorRef,
             solutionsPrinter: ActorRef,
             individualTypeName: String
           ): Route = {
    pathPrefix("api" / "evolution") {
      (post & pathEndOrSingleSlash) {
        entity(as[EvolutionRequestBody]) { case EvolutionRequestBody(
        survivalLikelihood,
        crossoverLikelihood,
        mutationLikelihood,
        maxQuantityOfGenerationsWithoutImprovements,
        solutionsPopulationsSize
        ) =>
          solutionsPrinter ? PrinterOnline
          generationsManager ? ManagerOnline(solutionsPrinter, master, solutionsPopulationsSize, maxQuantityOfGenerationsWithoutImprovements)
          master ? MasterOnline(generationsManager, survivalLikelihood, crossoverLikelihood, mutationLikelihood)
          this.distributeWork(
            generationsManager,
            InitialPopulation(individualTypeName),
            1,
            1
          )
          complete(StatusCodes.Created, "Evolution has started")
        }
      }
    }
  }
}
