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
    case WorkerOnline(survivalPopulationSize: Int, crossoverLikelihood: Double, mutationLikelihood: Double) =>
      def online: Receive = {
        case Execute(NATURAL_SELECTION, population: Population) =>
          val strongerPopulation = population.selectStrongerPopulation(survivalPopulationSize)
          log.debug(s"Population got through natural selection. The new population has  ${strongerPopulation.individuals.size} members: ${strongerPopulation.individuals}")
          sender() ! Execute(LAST_INDIVIDUALS, strongerPopulation)
        case Execute(CROSSOVER, population: Population) =>
          val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
          val children = populationLookingForReproduction.crossoverWith(population, crossoverLikelihood)
          log.debug(s"Population got through crossover. The new population has  ${children.individuals.size} children: ${children.individuals}")
          sender() ! Execute(LAST_INDIVIDUALS, children)
        case Execute(MUTATION, population: Population) =>
          val mutatedPopulation = population.mutate(mutationLikelihood)
          log.debug(s"Population got through mutation. The new population has ${mutatedPopulation.individuals.size} members: ${mutatedPopulation.individuals}")
          sender() ! Execute(LAST_INDIVIDUALS, mutatedPopulation)
      }

      context.become(online)
  }
}
