package domain.entities

import domain.actors.EvolutionWorker
import domain.entities.AlgorithmConfig.*
import domain.entities.OperatorRatios.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Chromosome(genes: List[Gene]) {
  def getGenes: List[Gene] = genes
  def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
  def copyWith(genes: List[Gene]): Chromosome
}
trait Gene {
  def mutate: Gene
}

object OperatorRatios {
  val SURVIVAL_LIKELIHOOD = 0.8
  val CROSSOVER_LIKELIHOOD = 0.5
  val MUTATION_LIKELIHOOD = 0.03
}

object AlgorithmConfig {
  val POPULATION_SIZE = 500
  val QUANTITY_OF_WORKERS_PER_NODE = 3
  val QUANTITY_OF_NODES = 2
  val POPULATION_GROWTH_RATIO = 1.648
}

case class Population(individuals: List[Individual])(implicit random: Random = new Random()) {
  lazy val accumulatedFitness: List[(Individual, Double)] = {
    val totalFitness = individuals.foldLeft(0d)((total, individual) => total + individual.fitness)
    individuals
      .filter(_.fitness > 0)
      .zipWithIndex
      .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
        result :+ (
          individual,
          if(index == 0) individual.fitness / totalFitness
          else if (index == individuals.size - 1) 1.0
          else individual.fitness / totalFitness + result(index - 1)._2
        )
      }
  }

  def findIndividualWhoseAccumulatedFitnessWindowIncludes(aFitness: Double): Individual = {
    @tailrec
    def recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness: List[(Individual, Double)]): Individual = {
      if(anAccumulatedFitness.size == 1) anAccumulatedFitness.head._1
      else {
        val middleIndex = anAccumulatedFitness.size / 2
        val middleFitness = anAccumulatedFitness(middleIndex)._2
        aFitness match
          case _ if aFitness == middleFitness => anAccumulatedFitness(middleIndex)._1
          case _ if aFitness > middleFitness => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.slice(middleIndex + 1, anAccumulatedFitness.size))
          case _ if aFitness > anAccumulatedFitness(middleIndex - 1)._2 => anAccumulatedFitness(middleIndex)._1
          case _ => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.slice(0, middleIndex))
      }
    }

    recFindIndividualWhoseAccumulatedFitnessWindowIncludes(accumulatedFitness)
  }

  def intoChunks(chunkSize: Int): List[Population] = individuals
    .grouped(chunkSize)
    .map(anIndividuals => Population(anIndividuals))
    .toList

  def randomSubPopulation(size: Int): Population = {
    @tailrec
    def recRandomSubPopulation(sourcePopulation: Population, sinkPopulation: Population, aSize: Int): Population = {
      if(aSize == 0 || sourcePopulation.individuals.isEmpty) sinkPopulation
      else {
        val foundIndividual = sourcePopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())

        recRandomSubPopulation(
          Population(sourcePopulation.individuals.filter(_ != foundIndividual)),
          Population(foundIndividual :: sinkPopulation.individuals),
          aSize - 1
        )
      }
    }

    recRandomSubPopulation(
      this,
      Population(List()),
      size
    )
  }

  def selectStrongerPopulation(size: Int) = randomSubPopulation(size)

  def crossoverWith(otherPopulation: Population): Population = {
    Population(individuals.flatMap { individual =>
      val couple = otherPopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
      individual.crossoverWith(couple) 
    })
  }

  def mutate: Population = {
    val individualsToMutate = individuals.filter(_ => random.nextInt(100) + 1 <= MUTATION_LIKELIHOOD * 100)
    Population(
      individualsToMutate.map(individual => individual.mutate)
    )
  }
  
  def hasToStop: Boolean = individuals.exists(_.accomplishStopCriteria)
  
  def bestIndividuals: List[Individual] = individuals.filter(_.accomplishStopCriteria)
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  def getChromosome: Chromosome = chromosome
  protected def copyWith(chromosome: Chromosome): Individual
  lazy val fitness: Double = calculateFitness
  private val random = new Random()
  def accomplishStopCriteria: Boolean

  def crossoverWith(couple: Individual): List[Individual] = {
    def addGeneAccordingToLikelihood(nextGene: Gene, genes: List[Gene]): List[Gene] =
      if(random.nextInt(100) + 1 <= CROSSOVER_LIKELIHOOD * 100) nextGene :: genes
      else genes

    val crossedGenes: (List[Gene], List[Gene]) = (chromosome.getGenes ::: couple.getChromosome.getGenes).foldLeft((List[Gene](), List[Gene]())) { (result, nextGene) =>
      (
        addGeneAccordingToLikelihood(nextGene, result._1),
        addGeneAccordingToLikelihood(nextGene, result._2)
      )
    }
    
    List(
      copyWith(chromosome.copyWith(crossedGenes._1)),
      copyWith(chromosome.copyWith(crossedGenes._2))
    )
  }

  def mutate: Individual = copyWith(chromosome.mutate)
}
