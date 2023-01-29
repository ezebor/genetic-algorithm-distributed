package domain.entities

import domain.Execute
import domain.Operators.*
import domain.entities.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

class GenericTypesSpec extends AnyWordSpecLike with should.Matchers {
  val POPULATION_SIZE = 200
  val CHUNKS_SIZE = 60
  val population: Population = Population((1 to POPULATION_SIZE).map { _ =>
    new Individual(new Chromosome {}) {
      override protected def calculateFitness: Double = 10
    }
  }.toList)

  "Population" should {
    "reckon accumulated fitness for each individual" in {
      val expectedSize = population.individuals.size
      val actualSize = population.accumulatedFitness.size

      actualSize should be(expectedSize)
      actualSize should be(POPULATION_SIZE)
    }

    "accumulate fitness for each individual" in {
      (1 until POPULATION_SIZE).foreach { index =>
        val expectedIndividual = population.individuals(index - 1)
        val expectedFitness = population.accumulatedFitness(index - 1)._2 + expectedIndividual.fitness

        population.accumulatedFitness(index - 1)._1 should be(expectedIndividual)
        population.accumulatedFitness(index)._2 should be(expectedFitness)
      }
    }

    "find individual whose accumulated fitness includes the given fitness" in {
      population
        .individuals
        .zipWithIndex
        .drop(1)
        .foreach { (individual, index) =>
          val bottomThresholdFitness = population.accumulatedFitness(index - 1)._2.toInt
          val topThresholdFitness = population.accumulatedFitness(index)._2.toInt

          (topThresholdFitness until bottomThresholdFitness).foreach { fitness =>
            population.findIndividualWhoseAccumulatedFitnessWindowIncludes(fitness) should be(individual)
          }
        }
    }

    "slice individuals list into chunks of populations whose individuals re-grouped are the original population" in {
      val chunks = population.intoChunks(CHUNKS_SIZE)
      chunks.size should be(POPULATION_SIZE / CHUNKS_SIZE + 1)

      val actualIndividuals = chunks.flatMap(aPopulation => aPopulation.individuals)
      actualIndividuals should be(population.individuals)
    }

    "generate sub populations given population size" in {
      population.randomSubPopulation(0).individuals.size should be(0)
      population.randomSubPopulation(1).individuals.size should be(1)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals.size should be(POPULATION_SIZE / 2)
      population.randomSubPopulation(POPULATION_SIZE).individuals.size should be(POPULATION_SIZE)
    }

    "generate the same population when the given size is equals to the population size" in {
      val subPopulation = population.randomSubPopulation(POPULATION_SIZE)

      subPopulation.individuals.size should be(POPULATION_SIZE)
      subPopulation.individuals.foreach { individual =>
        population.individuals.contains(individual) should be(true)
      }
      population.individuals.foreach { individual =>
        subPopulation.individuals.contains(individual) should be(true)
      }
    }
  }
}
