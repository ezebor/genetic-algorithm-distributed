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
    buildIndividual(buildChromosome(List(buildGene)))
  }.toList)

  def buildGene: Gene = new Gene {
    override def mutate: Gene = buildGene
  }

  def buildChromosome(genes: List[Gene]): Chromosome = new Chromosome(genes) {
    override def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
    override def copyWith(genes: List[Gene]): Chromosome = buildChromosome(genes)
  }

  def buildIndividual(chromosome: Chromosome): Individual = new Individual(chromosome) {
    override protected def calculateFitness: Double = 10
    override def copyWith(chromosome: Chromosome): Individual = buildIndividual(chromosome)
  }

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

    "generate a new population composed by original members plus the children created through crossover" in {
      val newPopulation = population.crossoverWith(population)
      newPopulation.individuals.size should be(population.individuals.size * 2)
    }
  }

  "Individual" should {
    "crossover with other individual blending their genes" in {
      val firstIndividual = buildIndividual(buildChromosome(List(buildGene, buildGene, buildGene)))
      val secondIndividual = firstIndividual.mutate

      val children = firstIndividual.crossoverWith(secondIndividual)
      children.size should be(2)
      val parentsGenes = firstIndividual.getChromosome.getGenes ::: secondIndividual.getChromosome.getGenes
      val childrenGenes = children.flatMap(individual => individual.getChromosome.getGenes)
      parentsGenes.foreach(gene => childrenGenes.contains(gene))
      childrenGenes.foreach(gene => parentsGenes.contains(gene))
    }
  }
}
