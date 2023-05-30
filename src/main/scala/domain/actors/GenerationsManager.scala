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
    case ManagerOnline(solutionsPrinter, evolutionMaster, solutionsPopulationSize, maxQuantityOfGenerationsWithoutImprovements) =>
      def manageBuiltGeneration(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Operator = { population =>
        log.info(s"Generation #[$generationId] built")
        val nextOperator: Operator = {
          if (solutions.individuals.isEmpty)
            manageBuiltGeneration(generationId + 1, 0, population.copyWith(List(population.bestIndividual)))
          else if (population.bestIndividual.fitness.getOrElse(0d) > solutions.individuals.head.fitness.getOrElse(0d)) {
            log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
            val newSolutions = population.copyWith(population.bestIndividual :: {
              if (solutions.individuals.size == solutionsPopulationSize) solutions.individuals.dropRight(1)
              else solutions.individuals
            })
            manageBuiltGeneration(generationId + 1, 0, newSolutions)
          }
          else if (quantityOfGenerationsWithoutImprovements <= maxQuantityOfGenerationsWithoutImprovements)
            manageBuiltGeneration(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions)
          else if (quantityOfGenerationsWithoutImprovements == maxQuantityOfGenerationsWithoutImprovements) {
            printSolutions(solutions)
            doNothing
          }
          else doNothing
        }
        
        context.become(this.waitingPopulations(
          nextOperator,
          EmptyPopulation,
          1
        ))
      }

      def printSolutions: Operator = { solutions =>
        this.distributeWork(
          solutionsPrinter,
          solutions
        )
      }

      def doNothing: Operator = { _ => }

      context.become(this.waitingPopulations(
        manageBuiltGeneration(1, 0, EmptyPopulation),
        EmptyPopulation,
        1
      ))
  }
}
