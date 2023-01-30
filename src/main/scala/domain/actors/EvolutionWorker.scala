package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.entities.OperatorRatios.*

import scala.util.Random

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker(
    SURVIVAL_LIKELIHOOD,
    CROSSOVER_LIKELIHOOD,
    MUTATION_LIKELIHOOD
  ))
}

class EvolutionWorker(survivalLikelihood: Double,
                      crossoverLikelihood: Double,
                      mutationLikelihood: Double) extends Actor with ActorLogging {

  override def receive: Receive = {
    case Execute(NATURAL_SELECTION, population: Population) =>
      val strongerPopulation = population.randomSubPopulation((population.individuals.size * SURVIVAL_LIKELIHOOD).toInt)
      log.info(s"Population got through natural selection. The leftover population has ${strongerPopulation.individuals.size} members: ${strongerPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, strongerPopulation)
    case Execute(CROSSOVER, population: Population) =>
      val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
      val children = populationLookingForReproduction.crossoverWith(population)
      log.info(s"Crossover has generated ${children.individuals.size} children: ${children.individuals}")
      sender() ! Execute(ADD_POPULATION, children)
    case Execute(MUTATION, population: Population) =>
      val mutatedPopulation = population.mutate
      log.info(s"The new mutated population has ${mutatedPopulation.individuals.size} members: ${mutatedPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, mutatedPopulation)
  }

  def likelihood: Int = {
    val random = new Random()
    random.nextInt(100) + 1
  }
}
