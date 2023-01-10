package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Execute
import domain.Operators.*
import domain.individuals.*

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
      val random = new Random()
      val strongerPopulation = population.filter { _ =>
        val chromosomeLikelihood = random.nextInt(100) + 1
        chromosomeLikelihood <= (SURVIVAL_LIKELIHOOD * 100)
      }
      log.info(s"Population got through natural selection. The leftover population has ${strongerPopulation.size} members: $strongerPopulation")
      sender() ! Execute(ADD_POPULATION, strongerPopulation)
  }
}
