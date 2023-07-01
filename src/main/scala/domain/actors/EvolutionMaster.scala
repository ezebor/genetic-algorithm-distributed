package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.pattern.pipe
import akka.routing.FromConfig
import akka.util.Timeout
import app.ExecutionScript
import app.ExecutionScript.*
import com.typesafe.config.Config
import domain.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, InitialPopulation, Population}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.{Failure, Random, Success}

object EvolutionMaster {
  def props(): Props = Props(new EvolutionMaster())
}

class EvolutionMaster() extends BaseActor {
  override def receive: Receive = offline()

  private def offline(): Receive = {
    case MasterOnline(manager: ActorRef, router: ActorRef, quantityOfWorkers: Int, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double) =>
      val survivalPopulationSize = (survivalLikelihood * POPULATION_SIZE).toInt

      def initializeWorkers(): Unit = {
        (1 to quantityOfWorkers).foreach { _ =>
          router ! WorkerOnline(
            self,
            SurvivalPopulationSize(survivalPopulationSize / QUANTITY_OF_WORKERS),
            CrossoverLikelihood(crossoverLikelihood),
            MutationLikelihood(mutationLikelihood)
          )
        }
      }

      def returnGeneration: Operator = { population =>
        this.distributeWork(manager, population)
        log.info(s"Starting evolution over a population with size = ${population.individuals.size}")
        startEvolution(population)
      }

      def startEvolution: Operator = { population =>
        context.become(this.waitingPopulations(
          returnGeneration,
          population.empty(),
          quantityOfWorkers + 1
        ))

        val strongestPopulation = population.selectStrongerPopulation(survivalPopulationSize)
        this.distributeWork(router, strongestPopulation, 1, quantityOfWorkers)

        Future {
          val mutants = strongestPopulation.mutate(mutationLikelihood)
          log.info(s"Mutants generated: [${mutants.individuals.size}]")
          mutants
        }.onComplete {
          case Success(mutants: Population) => this.distributeWork(self, mutants)
          case Failure(exception) => 
            log.info(s"No mutant generated. Exception: ${exception.getMessage}")
            this.distributeWork(self, strongestPopulation.empty())
        }
      }

      initializeWorkers()
      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))
  }
}
