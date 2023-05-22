package domain.actors
import akka.actor.*
import domain.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, Population}

object GenerationsManager {
  def props(): Props = Props(new GenerationsManager())
}

class GenerationsManager() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case ManagerOnline(originalSender, evolutionMaster, solutionsPopulationSize, maxQuantityOfGenerationsWithoutImprovements) =>
      def buildNewGeneration(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Operator = { population =>
        log.info(s"Starting generation [$generationId] over a population with ${population.individuals.size} individuals")

        this.distributeWork(
          evolutionMaster,
          population,
          1,
          1
        )

        context.become(this.waitingPopulations(
          manageBuiltGeneration(generationId, quantityOfGenerationsWithoutImprovements, solutions),
          EmptyPopulation,
          1
        ))
      }

      def manageBuiltGeneration(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Operator = { population =>
          if (solutions.individuals.isEmpty)
            buildNewGeneration(generationId + 1, 0, population.copyWith(List(population.bestIndividual)))(population)
          else if (population.bestIndividual.fitness.getOrElse(0d) > solutions.individuals.head.fitness.getOrElse(0d))
            log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
            val newSolutions = population.copyWith(population.bestIndividual :: {
              if (solutions.individuals.size == solutionsPopulationSize) solutions.individuals.dropRight(1)
              else solutions.individuals
            })
            buildNewGeneration(generationId + 1, 0, newSolutions)(population)
          else if (quantityOfGenerationsWithoutImprovements <= maxQuantityOfGenerationsWithoutImprovements)
            buildNewGeneration(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions)(population)
          else
            context.become(this.waitingPopulations(
              printSolutions,
              solutions,
              1
            ))
      }

      def printSolutions: Operator = { solutions =>
        this.distributeWork(
          originalSender,
          solutions,
          1,
          1
        )
      }

      context.become(this.waitingPopulations(
        buildNewGeneration(1, 0, EmptyPopulation),
        EmptyPopulation,
        1
      ))
  }
}
