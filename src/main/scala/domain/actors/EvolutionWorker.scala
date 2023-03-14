package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.util.Random

object EvolutionWorker {
  def props(survivalPopulationSize: Int, crossoverLikelihood: Double, mutationLikelihood: Double): Props = Props(new EvolutionWorker(
    survivalPopulationSize,
    crossoverLikelihood,
    mutationLikelihood
  ))
}

class EvolutionWorker(survivalPopulationSize: Int,
                      crossoverLikelihood: Double,
                      mutationLikelihood: Double) extends Actor with ActorLogging {

  override def receive: Receive = {
    case Execute(NATURAL_SELECTION, population: Population) =>
      val strongerPopulation = population.selectStrongerPopulation(survivalPopulationSize)
      log.debug(s"Population got through natural selection. The new population has  ${strongerPopulation.individuals.size} members: ${strongerPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, strongerPopulation)
    case Execute(CROSSOVER, population: Population) =>
      val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
      val children = populationLookingForReproduction.crossoverWith(population, crossoverLikelihood)
      log.debug(s"Population got through crossover. The new population has  ${children.individuals.size} children: ${children.individuals}")
      sender() ! Execute(ADD_POPULATION, children)
    case Execute(MUTATION, population: Population) =>
      val mutatedPopulation = population.mutate(mutationLikelihood)
      log.debug(s"Population got through mutation. The new population has ${mutatedPopulation.individuals.size} members: ${mutatedPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, mutatedPopulation)
  }

  def likelihood: Int = {
    val random = new Random()
    random.nextInt(100) + 1
  }
}
