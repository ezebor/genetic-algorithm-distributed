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
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

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
        this.distributeWork(router, population, 1, quantityOfWorkers)

        val mutants = population.mutate(mutationLikelihood)
        log.info(s"[${mutants.individuals.size}] Mutants generated")

        context.become(this.waitingPopulations(
          returnGeneration,
          mutants,
          quantityOfWorkers
        ))
      }

      initializeWorkers()
      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))
  }
}
