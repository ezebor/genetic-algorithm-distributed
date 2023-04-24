package domain.actors
import akka.actor.*
import domain.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{Individual, Population}

object GenerationsManager {
  def props(): Props = Props(new GenerationsManager())
}

class GenerationsManager() extends Actor with ActorLogging {
  override def receive: Receive = offline

  private def offline: Receive = {
    case ManagerOnline(originalSender: ActorRef, evolutionMaster, solutionsPopulationSize, maxQuantityOfGenerationsWithoutImprovements) =>
      def online(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Receive =
        case BuildNewGeneration(population: Population) =>
          log.info(s"Starting generation [$generationId] over a population with ${population.individuals.size} individuals")
          evolutionMaster ! Execute(EVOLUTION, population)
        case GenerationBuilt(population: Population) =>
          if(solutions.individuals.isEmpty)
            context.become(online(generationId + 1, 0, Population(List(population.bestIndividual))))
            self ! BuildNewGeneration(population)
          else if (population.bestIndividual.fitness.getOrElse(0d) > solutions.individuals.head.fitness.getOrElse(0d))
            log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
            val newSolutions = Population(population.bestIndividual :: {
              if (solutions.individuals.size == solutionsPopulationSize) solutions.individuals.dropRight(1)
              else solutions.individuals
            })
            context.become(online(generationId + 1, 0, newSolutions))
            self ! BuildNewGeneration(population)
          else if (quantityOfGenerationsWithoutImprovements <= maxQuantityOfGenerationsWithoutImprovements)
            context.become(online(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions))
            self ! BuildNewGeneration(population)
          else
            originalSender ! solutions
            evolutionMaster ! OFFLINE
            context.become(offline)

      context.become(online(1, 0, Population(List())))
  }
}
