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
      val strongerPopulation = population.selectStrongerPopulation(POPULATION_SIZE / (QUANTITY_OF_NODES * QUANTITY_OF_WORKERS_PER_NODE))
      log.info(s"Population got through natural selection. The new population has  ${strongerPopulation.individuals.size} members: ${strongerPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, strongerPopulation)
    case Execute(CROSSOVER, population: Population) =>
      val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
      val children = populationLookingForReproduction.crossoverWith(population)
      log.info(s"Population got through crossover. The new population has  ${children.individuals.size} children: ${children.individuals}")
      sender() ! Execute(ADD_POPULATION, children)
    case Execute(MUTATION, population: Population) =>
      val mutatedPopulation = population.mutate
      log.info(s"Population got through mutation. The new population has ${mutatedPopulation.individuals.size} members: ${mutatedPopulation.individuals}")
      sender() ! Execute(ADD_POPULATION, mutatedPopulation)
    case Execute(STOP, population: Population) =>
      val hasToStop: Boolean = population.hasToStop
      log.info(s"Has to stop: ${hasToStop}")
      sender() ! Execute({
        if(hasToStop) TAKE_BESTS_INDIVIDUALS
        else ADD_POPULATION
      }, population)
  }

  def likelihood: Int = {
    val random = new Random()
    random.nextInt(100) + 1
  }
}
