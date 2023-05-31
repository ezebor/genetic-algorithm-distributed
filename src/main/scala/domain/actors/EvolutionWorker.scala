package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import app.ExecutionScript
import app.ExecutionScript.{POPULATION_SIZE, QUANTITY_OF_WORKERS, QUANTITY_OF_WORKERS_PER_NODE, QUANTITY_OF_WORKER_NODES}
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.{Execute, WorkerOnline}

import scala.util.Random

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker())
}

class EvolutionWorker() extends BaseActor {
  override def receive: Receive = offline

  private val initialPopulation: Population = InitialPopulation(ExecutionScript.INDIVIDUAL_TYPE_NAME)

  private def offline: Receive = {
    case WorkerOnline(evolutionMaster, survivalPopulationSize, crossoverLikelihood, mutationLikelihood) =>

      def startEvolution: Operator = { population =>
        log.info(s"Starting evolution over a population with size = ${population.individuals.size}")
        val strongerPopulation = population.selectStrongerPopulation(survivalPopulationSize)
        val populationLookingForReproduction = strongerPopulation.randomSubPopulation(strongerPopulation.individuals.size / 2)
        val children = populationLookingForReproduction.crossoverWith(strongerPopulation, crossoverLikelihood)
        val parentsAndChildren = children.fusionWith(strongerPopulation)
        val mutatedPopulation = parentsAndChildren.mutate(mutationLikelihood)
        val finalPopulation = mutatedPopulation.fusionWith(parentsAndChildren)
        log.info(s"A new population was created with size = ${finalPopulation.individuals.size}")

        this.distributeWork(
          evolutionMaster,
          finalPopulation.selectStrongerPopulation(POPULATION_SIZE / QUANTITY_OF_WORKERS)
        )

        context.become(this.waitingPopulations(
          startEvolution,
          finalPopulation,
          1
        ))
      }

      context.become(this.waitingPopulations(
        startEvolution,
        initialPopulation,
        1
      ))
  }
}
