package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import app.ExecutionScript
import app.ExecutionScript.{POPULATION_SIZE, QUANTITY_OF_WORKERS, QUANTITY_OF_WORKERS_PER_NODE, QUANTITY_OF_WORKER_NODES}
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.*

import scala.util.Random

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker())
}

class EvolutionWorker() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case WorkerOnline(
    evolutionMaster,
    SurvivalPopulationSize(survivalPopulationSize),
    CrossoverLikelihood(crossoverLikelihood),
    MutationLikelihood(mutationLikelihood)
    ) =>

      def startEvolution: Operator = { population =>
        log.info(s"Starting evolution over a population with size = ${population.individuals.size}")
        val strongestPopulation = population.selectStrongerPopulation(survivalPopulationSize)
        val populationLookingForReproduction = strongestPopulation.randomSubPopulation(strongestPopulation.individuals.size / 2)
        val children = populationLookingForReproduction.crossoverWith(strongestPopulation, crossoverLikelihood)
        val mutatedPopulation = strongestPopulation.mutate(mutationLikelihood)
        val finalPopulation = strongestPopulation
          .fusionWith(children)
          .fusionWith(mutatedPopulation)
        
        log.info(s"A new population was created with size = ${finalPopulation.individuals.size}")

        this.distributeWork(
          evolutionMaster,
          finalPopulation.copyWith(List(finalPopulation.bestIndividual))
        )

        startEvolution(finalPopulation)
      }

      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))
  }
}
