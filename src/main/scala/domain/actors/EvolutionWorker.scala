package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
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

  private def offline: Receive = {
    case WorkerOnline(evolutionMaster, survivalPopulationSize, crossoverLikelihood, mutationLikelihood) =>

      def startEvolution: Operator = { population =>
        val strongerPopulation = population.selectStrongerPopulation(survivalPopulationSize)
        log.info(s"Tamaño population: ${population.individuals.size}")
        log.info(s"fitness: ${population.individuals.map(_.fitness.get)}")
        log.info(s"Tamaño stronger: ${strongerPopulation.individuals.size}")
        val populationLookingForReproduction = strongerPopulation.randomSubPopulation(strongerPopulation.individuals.size / 2)
        val children = populationLookingForReproduction.crossoverWith(strongerPopulation, crossoverLikelihood)
        val mutatedPopulation = strongerPopulation.fusionWith(children).mutate(mutationLikelihood)
        val finalPopulation = strongerPopulation.fusionWith(children.fusionWith(mutatedPopulation))
        this.distributeWork(
          evolutionMaster,
          finalPopulation,
          1,
          1
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
