package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.pattern.pipe
import akka.routing.FromConfig
import akka.util.Timeout
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, Population}
import domain.{Execute, GenerationBuilt, MasterOnline, WorkerOnline}
import app.ExecutionScript.QUANTITY_OF_WORKERS_PER_NODE
import app.ExecutionScript.QUANTITY_OF_WORKER_NODES
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
  val workers: Map[ActorRef, Vector[ActorRef]] = Map()

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

  private def offline(workers: Map[Address, Vector[ActorRef]]): Receive = {
    case MasterOnline(manager: ActorRef, populationSize: Int, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double) =>
      workers.values.flatten.foreach(worker => worker ! WorkerOnline(
        self,
        ((populationSize / (QUANTITY_OF_WORKERS_PER_NODE * QUANTITY_OF_WORKER_NODES)) *  survivalLikelihood).toInt,
        crossoverLikelihood,
        mutationLikelihood
      ))

      def returnGeneration: Operator = { population =>
        this.distributeWork(manager, population)

        context.become(this.waitingPopulations(
          startEvolution,
          EmptyPopulation,
          1
        ))
      }

      def startEvolution: Operator = { population =>
        val workerChunks = population.intoChunks(population.individuals.size / QUANTITY_OF_WORKERS_PER_NODE * QUANTITY_OF_WORKER_NODES)
        val workersActorRefs = workers.values.flatten.toVector
        
        workersActorRefs.indices.foreach { index =>
          this.distributeWork(workersActorRefs(index), workerChunks(index))
        }

        context.become(this.waitingPopulations(
          returnGeneration,
          EmptyPopulation,
          QUANTITY_OF_WORKERS_PER_NODE * QUANTITY_OF_WORKER_NODES
        ))
      }

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
      context.become(offline(workers.removed(member.address)))

    case MemberRemoved(member, previousStatus) =>
      log.info(s"Member ${member.address} removed after $previousStatus")
      context.become(offline(workers.removed(member.address)))

    case m: MemberEvent =>
      log.info(s"Unrecognized member Event: $m")

    case pair: (Address, ActorRef) =>
      log.info(s"Registering worker: $pair")
      val workersActorRefs = workers.getOrElse(pair._1, Vector())
      context.become(offline(workers.updated(pair._1, pair._2 +: workersActorRefs)))
  }
}
