package domain.entities

import domain.Execute
import domain.Operators.*
import domain.entities.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random

class GenericTypesSpec extends AnyWordSpecLike with should.Matchers {
  val POPULATION_SIZE = 200
  val CHUNKS_SIZE = 60

  implicit val random: Random = new Random()

  // TODO: llevar los build a un factory

  def buildGene: Gene = new Gene {
    override def mutate: Gene = buildGene
  }

  def buildChromosome(genes: List[Gene]): Chromosome = new Chromosome(genes) {
    override def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
    override def copyWith(genes: List[Gene]): Chromosome = buildChromosome(genes)
  }

  def buildIndividual(chromosome: Chromosome, fitnessValue: Double = 10): Individual = new Individual(chromosome) {
    override protected def calculateFitness: Double = fitnessValue
    override def copyWith(chromosome: Chromosome): Individual = buildIndividual(chromosome)
    override def accomplishStopCriteria: Boolean = true
  }

  def buildPopulation(size: Int, fitnessValue: Double = 10): Population = Population((1 to size).map { _ =>
    buildIndividual(buildChromosome(List(buildGene)), fitnessValue)
  }.toList)

  "Population" should {
    "reckon accumulated fitness for each individual" in {
      val population = buildPopulation(POPULATION_SIZE)
      val expectedSize = population.individuals.size
      val actualSize = population.accumulatedFitness.size

      actualSize should be(expectedSize)
      actualSize should be(POPULATION_SIZE)
    }

    "build an accumulated fitness list when it has individuals with fitness greater than 0" in {
      val population = buildPopulation(POPULATION_SIZE)
      population.accumulatedFitness.last._2 should be(1)
      population.accumulatedFitness.size should be(population.individuals.size)
      population.accumulatedFitness.size should be(POPULATION_SIZE)

      (1 until POPULATION_SIZE).foreach { index =>
        assert(population.individuals.contains(population.accumulatedFitness(index)._1))
        assert(population.accumulatedFitness(index)._2 > population.accumulatedFitness(index - 1)._2)
      }
    }

    "build an empty accumulated fitness list when it is empty" in {
      val population = buildPopulation(0)
      assert(population.individuals.isEmpty)
      assert(population.accumulatedFitness.isEmpty)
    }

    "build an accumulated fitness list ignoring individuals with fitness equals to 0" in {
      val population = Population(buildPopulation(POPULATION_SIZE / 2).individuals ::: buildPopulation(POPULATION_SIZE / 2, 0).individuals)

      population.accumulatedFitness.last._2 should be(1)
      population.accumulatedFitness.size should be(population.individuals.size / 2)
      population.accumulatedFitness.size should be(POPULATION_SIZE / 2)

      (1 until POPULATION_SIZE / 2).foreach { index =>
        assert(population.individuals.contains(population.accumulatedFitness(index)._1))
        assert(population.accumulatedFitness(index)._2 > population.accumulatedFitness(index - 1)._2)
      }
    }

    "find an individual whose accumulated fitness includes a given fitness" in {
      val population = buildPopulation(POPULATION_SIZE)
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

    "throw an exception when it is empty and tries to find an individual" in {
      val population = buildPopulation(0)
      intercept[IllegalStateException] {
        population.findIndividualWhoseAccumulatedFitnessWindowIncludes(10)
      }
    }

    "throw an exception when all the individuals are unfit and tries to find an individual" in {
      val population = buildPopulation(POPULATION_SIZE, 0)

      population.individuals.size should be(POPULATION_SIZE)
      population.accumulatedFitness.size should be(0)
      intercept[IllegalStateException] {
        population.findIndividualWhoseAccumulatedFitnessWindowIncludes(10)
      }
    }

    "slice individuals list into chunks of populations whose individuals re-grouped are the original population" in {
      val population = buildPopulation(POPULATION_SIZE)
      val chunks = population.intoChunks(CHUNKS_SIZE)
      chunks.size should be(POPULATION_SIZE / CHUNKS_SIZE + 1)

      val actualIndividuals = chunks.flatMap(aPopulation => aPopulation.individuals)
      actualIndividuals should be(population.individuals)
    }

    // TODO: agregar intoChunks para population vacía
    // TODO: agregar intoChunks para population con individuos con fitness = 0 todos

    "generate sub populations given population size" in {
      val population = buildPopulation(POPULATION_SIZE)
      population.randomSubPopulation(0).individuals.size should be(0)
      population.randomSubPopulation(1).individuals.size should be(1)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals.size should be(POPULATION_SIZE / 2)
      population.randomSubPopulation(POPULATION_SIZE).individuals.size should be(POPULATION_SIZE)
    }

    // TODO: agregar randomSubPopulation con individuals size = 0
    // TODO: agregar randomSubPopulation con individuals size = 1
    // TODO: agregar randomSubPopulation controlando el random (random = 1 elige el último)
    // TODO: agregar randomSubPopulation controlando el random (random = 0 elige el primero)
    // TODO: agregar randomSubPopulation para population con individuos con fitness = 0 todos

    "generate the same population when the given size is equals to the population size" in {
      val population = buildPopulation(POPULATION_SIZE)
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
      val population = buildPopulation(POPULATION_SIZE)
      val newPopulation = population.crossoverWith(population)
      newPopulation.individuals.size should be(population.individuals.size * 2)
    }

    // TODO: agregar crossoverWith para probabilidad = 1 (se cruzan todos)
    // TODO: agregar crossoverWith para probabilidad = 0 (no se cruza nadie)

    // TODO: agregar mutate con probabilidad = 0 (no muta nadie)
    // TODO: agregar mutate con probabilidad = 1 (mutan todos)
  }

  "Individual" should {
    "crossover with other individual blending their genes" in {
      val firstIndividual = buildIndividual(buildChromosome(List(buildGene, buildGene, buildGene)))
      val secondIndividual = firstIndividual.mutate

      val children = firstIndividual.crossoverWith(secondIndividual)
      children.size should be(2)
      val parentsGenes = firstIndividual.getChromosome.getGenes ::: secondIndividual.getChromosome.getGenes
      val childrenGenes = children.flatMap(individual => individual.getChromosome.getGenes)
      assert(parentsGenes.forall(gene => childrenGenes.contains(gene)))
      assert(childrenGenes.forall(gene => parentsGenes.contains(gene)))
    }

    // TODO: agregar crossoverWith con probabilidad = 0 (los hijos no heredan ningún gen de los parents)
    // TODO: agregar crossoverWith con probabilidad = 1 (los hijos heredan todos los genes de los parents)

    "mutate its chromosome creating new genes" in {
      val individual = buildIndividual(buildChromosome(List(buildGene, buildGene, buildGene)))
      val mutatedIndividual = individual.mutate

      mutatedIndividual.getChromosome.getGenes.foreach { gene =>
        assert(!individual.getChromosome.getGenes.contains(gene))
      }
    }

    // TODO: agregar mutate con probabilidad = 0 (no muta nadie)
    // TODO: agregar mutate con probabilidad = 1 (mutan todos)
  }
}
