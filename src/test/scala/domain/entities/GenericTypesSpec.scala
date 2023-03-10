package domain.entities

import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.OperatorRatios.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.{Random, Success, Try}

class GenericTypesSpec extends AnyWordSpecLike with should.Matchers {
  val POPULATION_SIZE = 200
  val CHUNKS_SIZE = 60
  val QUANTITY_OF_GENES = 3

  // TODO: llevar los build a un factory

  def buildGene: Gene = new Gene {
    override def mutate: Gene = buildGene
  }

  def buildDefaultListOfGenes: List[Gene] = (1 to QUANTITY_OF_GENES).map(_ => buildGene).toList

  def buildChromosome(genes: List[Gene], fitnessValue: Double = 10): Chromosome = new Chromosome(genes) {
    override protected def calculateFitness: Double = fitnessValue
    override def mutate: Chromosome = copyWith(genes.map(_.mutate))
    override def copyWith(genes: List[Gene]): Chromosome = buildChromosome(genes)
  }

  def buildIndividual(chromosome: Try[Chromosome])(implicit customRandom: Random = standardRandom): Individual = new Individual(chromosome) {
    override def copyWith(chromosome: Try[Chromosome]): Individual = buildIndividual(chromosome)
  }

  implicit val standardRandom: Random = new Random()

  def buildPopulation(size: Int, fitnessValue: Double = 10)(implicit customRandom: Random = standardRandom): Population = Population((1 to size).map { _ =>
    buildIndividual(Success(buildChromosome(buildDefaultListOfGenes, fitnessValue)))
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

      population.accumulatedFitness.map(_._1) should be(population.individuals.filter(_.fitness.getOrElse(0d) > 0))
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

    "build a random sub population from a population with fit individuals picking always the last individuals" in {
      implicit case object CustomRandom extends Random {
        override def nextDouble(): Double = 1
      }
      val population = buildPopulation(POPULATION_SIZE)

      population.randomSubPopulation(0).individuals should be(List())
      population.randomSubPopulation(1).individuals.head should be(population.individuals.last)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals.reverse should be(population.individuals.takeRight(POPULATION_SIZE / 2))
      population.randomSubPopulation(POPULATION_SIZE).individuals.reverse should be(population.individuals.takeRight(POPULATION_SIZE))
    }

    "build a random sub population from a population with fit individuals picking always the first individuals" in {
      implicit case object CustomRandom extends Random {
        override def nextDouble(): Double = 0
      }
      val population = buildPopulation(POPULATION_SIZE)

      population.randomSubPopulation(0).individuals should be(List())
      population.randomSubPopulation(1).individuals.head should be(population.individuals.head)
      population.randomSubPopulation(POPULATION_SIZE / 2).individuals should be(population.individuals.take(POPULATION_SIZE / 2))
      population.randomSubPopulation(POPULATION_SIZE).individuals should be(population.individuals.take(POPULATION_SIZE))
    }

    "build an empty sub population when the population is empty" in {
      buildPopulation(0).randomSubPopulation(0).individuals should be (List())
      buildPopulation(0).randomSubPopulation(1).individuals should be (List())
    }

    "build an empty sub population when the individuals of the population are unfit" in {
      buildPopulation(POPULATION_SIZE, 0).randomSubPopulation(0).individuals should be (List())
      buildPopulation(POPULATION_SIZE, 0).randomSubPopulation(1).individuals should be (List())
    }

    "build an empty sub population when an intermediate accumulated fitness list is empty" in {
      implicit case object CustomRandom extends Random {
        override def nextDouble(): Double = 0.5
      }
      val population = Population(List(
        buildIndividual(Success(buildChromosome(buildDefaultListOfGenes, 0))),
        buildIndividual(Success(buildChromosome(buildDefaultListOfGenes, 0))),
        buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))
      ))

      val subPopulation: Population = population.randomSubPopulation(2)

      subPopulation.individuals.size should be(1)
      subPopulation.individuals.head should be(population.individuals.last)
    }

    "build children population without parent's genes" in {
      implicit case object CustomRandom extends Random {
        override def nextDouble(): Double = 0
        override def nextInt(n: Int): Int = 100
      }
      val parentsPopulationA = buildPopulation(POPULATION_SIZE / 2)
      val parentsPopulationB = buildPopulation(POPULATION_SIZE / 2)

      val children = parentsPopulationA.crossoverWith(parentsPopulationB, CROSSOVER_LIKELIHOOD)

      children.individuals.size should be(POPULATION_SIZE)
      children
        .individuals
        .foreach { individual =>
          assert(
            individual
              .getTryGenes
              .getOrElse(List())
              .isEmpty
          )
        }
    }

    "build children population with all the genes of the parents" in {
      implicit case object CustomRandom extends Random {
        override def nextDouble(): Double = 0
        override def nextInt(n: Int): Int = 0
      }
      val parentsPopulationA = buildPopulation(POPULATION_SIZE / 2)
      val parentsPopulationB = buildPopulation(POPULATION_SIZE / 2)

      val children = parentsPopulationA.crossoverWith(parentsPopulationB, CROSSOVER_LIKELIHOOD)

      children.individuals.size should be(POPULATION_SIZE)
      children
        .individuals
        .foreach { individual =>
          individual
            .getTryGenes
            .getOrElse(List())
            .size should be(QUANTITY_OF_GENES * 2)
        }
    }

    "build an empty mutated population when no individual mutated" in {
      implicit case object CustomRandom extends Random {
        override def nextInt(n: Int): Int = 100
      }
      val population = buildPopulation(POPULATION_SIZE)

      population.mutate(MUTATION_LIKELIHOOD).individuals should be(List())
    }

    "build new population equals than the base population but with all the individuals mutated" in {
      implicit case object CustomRandom extends Random {
        override def nextInt(n: Int): Int = 0
      }
      val population = buildPopulation(POPULATION_SIZE)

      population.mutate(MUTATION_LIKELIHOOD).individuals.size should be(population.individuals.size)
    }
  }

  "Individual" should {
    "cross with other individual building new children without parent's genes" in {
      implicit case object CustomRandom extends Random {
        override def nextInt(n: Int): Int = 100
      }
      val individualA = buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))
      val individualB = buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))

      val children = individualA
        .crossoverWith(individualB, CROSSOVER_LIKELIHOOD)
        .getOrElse(List())

      children.size should be(2)
      children
        .foreach { child =>
          child
            .getTryGenes
            .getOrElse(List())
            .size should be(0)
        }
    }

    "cross with other individual building new children with all the parent's genes" in {
      implicit case object CustomRandom extends Random {
        override def nextInt(n: Int): Int = 0
      }
      val individualA = buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))
      val individualB = buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))

      val children = individualA
        .crossoverWith(individualB, CROSSOVER_LIKELIHOOD)
        .getOrElse(List())

      children.size should be(2)
      children
        .foreach { child =>
          val genes = child
            .getTryGenes
            .getOrElse(List())
          genes.size should be(QUANTITY_OF_GENES * 2)

          genes.foreach { gene =>
            assert(
              individualA.getTryGenes.getOrElse(List()).contains(gene)
                || individualB.getTryGenes.getOrElse(List()).contains(gene)
            )
          }
        }
    }

    "build a new individual with a new mutated chromosome" in {
      val individual = buildIndividual(Success(buildChromosome(buildDefaultListOfGenes)))

      individual.mutate
        .getTryGenes
        .getOrElse(List())
        .foreach { gene =>
        assert(
          !individual.mutate
            .getTryGenes
            .getOrElse(List()).contains(gene)
        )
      }
    }
  }
}
