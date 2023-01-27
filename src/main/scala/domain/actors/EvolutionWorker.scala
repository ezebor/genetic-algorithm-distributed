package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Execute
import domain.Operators.*
import domain.entities.*

import scala.util.Random

object EvolutionWorker {
  def props(): Props = Props(new EvolutionWorker())
}

class EvolutionWorker() extends Actor with ActorLogging {
  val SURVIVAL_LIKELIHOOD = 0.8
  val CROSSOVER_LIKELIHOOD = 0.5
  val MUTATION_LIKELIHOOD = 0.1

  override def receive: Receive = {
    case Execute(NATURAL_SELECTION, population: Population) =>
      val strongerIndividuals = population.individuals.filter { _ => 
        likelihood <= (SURVIVAL_LIKELIHOOD * 100)
      }
      log.info(s"Population got through natural selection. The leftover population has ${strongerIndividuals.size} members: $strongerIndividuals")
      sender() ! Execute(ADD_POPULATION, Population(strongerIndividuals))
    case Execute(CROSSOVER, population: Population) =>
      log.info("LLEGO CROSS OVER")
  }

  def likelihood: Int = {
    val random = new Random()
    random.nextInt(100) + 1
  }
}
