package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Execute
import domain.Operators.*
import domain.actors.EvolutionWorker.SURVIVAL_LIKELIHOOD
import domain.entities.*

import scala.util.Random

object EvolutionWorker {
  val SURVIVAL_LIKELIHOOD = 0.8
  val CROSSOVER_LIKELIHOOD = 0.5
  val MUTATION_LIKELIHOOD = 0.1
  
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
      log.info("LLEGO CROSSOVER AL WORKEEERR!!!!!!!!!!")
  }

  def likelihood: Int = {
    val random = new Random()
    random.nextInt(100) + 1
  }
}
