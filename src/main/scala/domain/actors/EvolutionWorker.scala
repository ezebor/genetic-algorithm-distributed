package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import app.ExecutionScript
import app.ExecutionScript.{POPULATION_SIZE, QUANTITY_OF_INDIVIDUALS_TO_DISTRIBUTE, QUANTITY_OF_WORKERS, QUANTITY_OF_WORKERS_PER_NODE}
import domain.*
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.util.Random

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker())
}

class EvolutionWorker() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case WorkerOnline(evolutionMaster, SurvivalPopulationSize(survivalPopulationSize), CrossoverLikelihood(crossoverLikelihood), MutationLikelihood(mutationLikelihood)) =>
      def startEvolution: Operator = { population =>
        val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
        val children = populationLookingForReproduction.crossoverWith(population, crossoverLikelihood)

        this.distributeWork(
          evolutionMaster,
          population
            .fusionWith(children)
            .selectStrongerPopulation(survivalPopulationSize),
          QUANTITY_OF_INDIVIDUALS_TO_DISTRIBUTE
        )

        context.become(this.waitingPopulations(
          startEvolution,
          EmptyPopulation,
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
