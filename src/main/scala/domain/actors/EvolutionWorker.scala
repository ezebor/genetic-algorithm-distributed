package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import app.ExecutionScript
import app.ExecutionScript.{CHUNK_SIZE, POPULATION_SIZE, QUANTITY_OF_WORKERS, QUANTITY_OF_WORKERS_PER_NODE}
import domain.*
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker())
}

class EvolutionWorker() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case WorkerOnline(evolutionMaster, SurvivalPopulationSize(survivalPopulationSize), CrossoverLikelihood(crossoverLikelihood), MutationLikelihood(mutationLikelihood)) =>
      def startEvolution: Operator = { population =>
        log.info(s"Starting evolution over a population of ${population.individuals.size} members")

        val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)

        val futureMutants = Future {
          log.info("Starting mutation")
          val mutants = population.mutate(mutationLikelihood)
          log.info(s"Mutants generated: [${mutants.individuals.size}]")
          mutants
        }

        val futureChildren = Future {
          log.info("Starting crossover")
          populationLookingForReproduction.crossoverWith(population, crossoverLikelihood)
        }

        log.info("Generating new population")
        val futureNewPopulation = for {
          mutants <- futureMutants
          children <- futureChildren
        } yield children
          .fusionWith(mutants)
          .mutate(1)

        val newPopulation = Await.result(futureNewPopulation, Duration.Inf)

        log.info("Returning new population to master")
        this.distributeWork(
          evolutionMaster,
          newPopulation,
          CHUNK_SIZE
        )

        context.become(this.waitingPopulations(
          startEvolution,
          newPopulation,
          1
        ))
      }

      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))
  }
}
