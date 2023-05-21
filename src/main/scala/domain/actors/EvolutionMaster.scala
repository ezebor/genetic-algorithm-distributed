package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.routing.FromConfig
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{Individual, Population}
import domain.{Execute, GenerationBuilt, MasterOnline, WorkerOnline}

import scala.util.Random

object EvolutionMaster {
  def props(): Props = Props(new EvolutionMaster())
}

class EvolutionMaster() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case MasterOnline(manager: ActorRef, router: ActorRef, quantityOfWorkers: Int, populationSize: Int, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double) =>
      (1 to quantityOfWorkers).foreach(_ => router ! WorkerOnline(
        ((populationSize / quantityOfWorkers) *  survivalLikelihood).toInt,
        crossoverLikelihood,
        mutationLikelihood
      ))

      def online: Receive = {
          case Execute(EVOLUTION, population: Population) =>
            self ! Execute(NATURAL_SELECTION, population)
          case Execute(STOP, population: Population) =>
            manager ! GenerationBuilt(population)
          case Execute(currentOperatorName: String, population: Population) =>
            val (basePopulation: Population, nextOperatorName: String) = currentOperatorName match
              case NATURAL_SELECTION => (population.copyWith(List()), CROSSOVER)
              case CROSSOVER => (population, MUTATION)
              case MUTATION => (population, STOP)

            log.debug(s"Executing $currentOperatorName for a population with size = ${population.individuals.size}. Next operator: $nextOperatorName")

            val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
            context.become(waitingPopulations(nextOperatorName, online, basePopulation, chunks.size))
            chunks.foreach(chunk => router ! Execute(currentOperatorName, chunk))
          case HEALTH => sender() ! OK
          case OFFLINE => context.become(offline)
      }
      
      context.become(online)
    case HEALTH => sender() ! OK
  }
}
