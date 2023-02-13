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

  implicit val standardRandom: Random = new Random()

  def buildPopulation(size: Int, fitnessValue: Double = 10)(implicit customRandom: Random = standardRandom): Population = Population((1 to size).map { _ =>
    buildIndividual(buildChromosome(List(buildGene)), fitnessValue)
  }.toList)(customRandom)

  "Population" should {
    "build an accumulated fitness list with the same size than the individuals list when all the individuals have fitness greater than 0" in {
      val population = buildPopulation(POPULATION_SIZE, 10)
      population.accumulatedFitness.last._2 should be(1)
      population.accumulatedFitness.size should be(population.individuals.size)
      population.accumulatedFitness.size should be(POPULATION_SIZE)

      (1 until POPULATION_SIZE).foreach { index =>
        assert(population.individuals.contains(population.accumulatedFitness(index)._1))
        assert(population.accumulatedFitness(index)._2 > population.accumulatedFitness(index - 1)._2)
      }
    }

    "build an empty accumulated fitness list with the individuals list is empty" in {
      val population = buildPopulation(0)
      assert(population.individuals.isEmpty)
      assert(population.accumulatedFitness.isEmpty)
    }

    "build an accumulated fitness list fewer than the individuals list when some individuals have fitness equals to 0" in {
      val population = Population(buildPopulation(POPULATION_SIZE / 2).individuals ::: buildPopulation(POPULATION_SIZE / 2, 0).individuals)

      population.accumulatedFitness.last._2 should be(1)
      population.accumulatedFitness.size should be(population.individuals.size / 2)
      population.accumulatedFitness.size should be(POPULATION_SIZE / 2)

      (1 until POPULATION_SIZE / 2).foreach { index =>
        assert(population.individuals.contains(population.accumulatedFitness(index)._1))
        assert(population.accumulatedFitness(index)._2 > population.accumulatedFitness(index - 1)._2)
      }

      population.accumulatedFitness.map(_._1) should be(population.individuals.filter(_.fitness > 0))
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

    "create an empty chunk when it is empty" in {
      val population = buildPopulation(0)
      val chunks = population.intoChunks(CHUNKS_SIZE)
      chunks.size should be(0)
    }

    "create a chunk equals to the population when the required size is 0" in {
      val population = buildPopulation(POPULATION_SIZE)
      intercept[IllegalArgumentException] {
        population.intoChunks(0)
      }
    }
    
    "throw an exception when trying to generate a random sub populations from an empty accumulated fitness list" in {
      intercept[IllegalStateException] {
        buildPopulation(0).randomSubPopulation(0)
      }

      intercept[IllegalStateException] {
        buildPopulation(0).randomSubPopulation(1)
      }

      intercept[IllegalStateException] {
        buildPopulation(POPULATION_SIZE, 0).randomSubPopulation(0)
      }

      intercept[IllegalStateException] {
        buildPopulation(POPULATION_SIZE, 0).randomSubPopulation(1)
      }
    }

    "build a random sub population from a population with fit individuals picking always the last individuals" in {
      case object CustomRandom extends Random {
        override def nextDouble(): Double = 1
      }
      val population = buildPopulation(POPULATION_SIZE)(CustomRandom)

      population.randomSubPopulation(0).individuals should be(List())
      population.randomSubPopulation(1).individuals.head should be(population.individuals.last)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals.reverse should be(population.individuals.takeRight(POPULATION_SIZE / 2))
      population.randomSubPopulation(POPULATION_SIZE).individuals.reverse should be(population.individuals.takeRight(POPULATION_SIZE))
    }

    "build a random sub population from a population with fit individuals picking always the first individuals" in {
      case object CustomRandom extends Random {
        override def nextDouble(): Double = 0
      }
      val population = buildPopulation(POPULATION_SIZE)(CustomRandom)

      population.randomSubPopulation(0).individuals should be(List())
      population.randomSubPopulation(1).individuals.head should be(population.individuals.head)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals should be(population.individuals.take(POPULATION_SIZE / 2))
      population.randomSubPopulation(POPULATION_SIZE).individuals should be(population.individuals.take(POPULATION_SIZE))
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
