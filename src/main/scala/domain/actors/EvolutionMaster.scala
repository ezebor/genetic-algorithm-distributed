package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.pattern.pipe
import akka.routing.FromConfig
import akka.util.Timeout
import app.ExecutionScript
import app.ExecutionScript.*
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, InitialPopulation, Population}
import domain.{Execute, GenerationBuilt, MasterOnline, WorkerOnline}

import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

object EvolutionMaster {
  def props(): Props = Props(new EvolutionMaster())
}

class EvolutionMaster() extends BaseActor {
  import context.dispatcher
  implicit val timeout: Timeout = Timeout(3 seconds)

  val cluster: Cluster = Cluster(context.system)

  override def preStart(): Unit = {
    cluster.subscribe(
      self,
      initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent],
      classOf[UnreachableMember]
    )
  }

  override def postStop(): Unit = {
    cluster.unsubscribe(self)
  }

  override def receive: Receive = offline(Map())

  private def offline(indexedWorkers: Map[Address, Vector[ActorRef]]): Receive = {
    case MasterOnline(manager: ActorRef, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double) =>
      val workers: Vector[ActorRef] = indexedWorkers.values.flatten.toVector

      def initializeWorkers(): Unit = {
        workers.foreach { worker =>
          worker ! WorkerOnline(
            self,
            (survivalLikelihood * POPULATION_SIZE).toInt,
            crossoverLikelihood,
            mutationLikelihood
          )
        }
      }

      def returnGeneration: Operator = { population =>
        this.distributeWork(manager, population)
        startEvolution(population)
      }

      def startEvolution: Operator = { population =>
        this.distributeWork(workers(random.nextInt(workers.size)), population)

        context.become(this.waitingPopulations(
          returnGeneration,
          population.empty(),
          QUANTITY_OF_PENDING_WORKERS
        ))
      }

      initializeWorkers()
      context.become(this.waitingPopulations(
        startEvolution,
        EmptyPopulation,
        1
      ))

    case MemberUp(member) if member.hasRole(WORKER_ROLE) =>
      log.info(s"Member is up: ${member.address}")
      (0 until QUANTITY_OF_WORKERS_PER_NODE).foreach { index =>
        val workerSelection = context.actorSelection(s"${member.address}/user/evolutionWorker_$index")
        workerSelection
          .resolveOne()
          .map(ref => (member.address, ref))
          .pipeTo(self)
      }

    case UnreachableMember(member) if member.hasRole(WORKER_ROLE) =>
      log.info(s"Member detected as unreachable: ${member.address}")
      context.become(offline(indexedWorkers.removed(member.address)))

    case MemberRemoved(member, previousStatus) =>
      log.info(s"Member ${member.address} removed after $previousStatus")
      context.become(offline(indexedWorkers.removed(member.address)))

    case m: MemberEvent =>
      log.info(s"Unrecognized member Event: $m")

    case pair: (Address, ActorRef) =>
      log.info(s"Registering worker: $pair")
      val workersActorRefs = indexedWorkers.getOrElse(pair._1, Vector())
      context.become(offline(indexedWorkers.updated(pair._1, pair._2 +: workersActorRefs)))
  }
}
