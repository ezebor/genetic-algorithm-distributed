package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.routing.FromConfig
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, Population}
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

      def returnGeneration: Operator = { population =>
        this.distributeWork(
          manager,
          population,
          1,
          1
        )

        context.become(this.waitingPopulations(
          startEvolution,
          EmptyPopulation,
          1
        ))
      }

      def startEvolution: Operator = { population =>
        this.distributeWork(
          router,
          population,
          1,
          quantityOfWorkers
        )

        context.become(this.waitingPopulations(
          returnGeneration,
          EmptyPopulation,
          quantityOfWorkers
        ))
      }

      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))
    case HEALTH => sender() ! OK
  }
}
