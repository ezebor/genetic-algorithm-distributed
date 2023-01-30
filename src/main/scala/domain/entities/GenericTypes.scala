package domain.entities

import domain.actors.EvolutionWorker
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
  val MUTATION_LIKELIHOOD = 0.1
}

object AlgorithmConfig {
  val POPULATION_SIZE = 500
  val QUANTITY_OF_WORKERS_PER_NODE = 3
}

case class Population(individuals: List[Individual]) {
  val random = new Random()

  lazy val accumulatedFitness: List[(Individual, Double)] = individuals
    .zipWithIndex
    .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
      result :+ (
        individual,
        if(index == 0) individual.fitness else individual.fitness + result(index - 1)._2
      )
    }

  def findIndividualWhoseAccumulatedFitnessWindowIncludes(aFitness: Int): Individual = {
    @tailrec
    def recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness: List[(Individual, Double)]): Individual = {
      if(anAccumulatedFitness.size == 1) anAccumulatedFitness.head._1
      else {
        val middleIndex = anAccumulatedFitness.size / 2
        val middleFitness = anAccumulatedFitness(middleIndex)._2
        aFitness match
          case _ if aFitness >= middleFitness => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.takeRight(middleIndex))
          case _ if aFitness >= anAccumulatedFitness(middleIndex - 1)._2 => anAccumulatedFitness(middleIndex)._1
          case _ => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.take(middleIndex))
      }
    }
    recFindIndividualWhoseAccumulatedFitnessWindowIncludes(accumulatedFitness)
  }

  def randomFitness = random.nextInt(accumulatedFitness.last._2.toInt) + 1

  def intoChunks(chunkSize: Int): List[Population] = individuals
    .grouped(chunkSize)
    .map(anIndividuals => Population(anIndividuals))
    .toList

  def randomSubPopulation(size: Int): Population = {
    def recRandomSubPopulation(sourcePopulation: Population, sinkPopulation: Population, aSize: Int): Population = {
      if(aSize == 0) sinkPopulation
      else {
        val foundIndividual = sourcePopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(sourcePopulation.randomFitness)
        recRandomSubPopulation(
          Population(sourcePopulation.individuals.filter(individual => individual != foundIndividual)),
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

  def crossoverWith(otherPopulation: Population): Population = {
    Population(individuals.flatMap { individual =>
      val couple = otherPopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(randomFitness)
      individual.crossoverWith(couple) 
    })
  }

  def mutate: Population = {
    val individualsToMutate = individuals.filter(_ => random.nextInt(100) + 1 <= MUTATION_LIKELIHOOD * 100)
    Population(
      individualsToMutate.map(individual => individual.mutate)
    )
  }
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  def getChromosome: Chromosome = chromosome
  protected def copyWith(chromosome: Chromosome): Individual
  lazy val fitness: Double = calculateFitness
  private val random = new Random()

  def crossoverWith(couple: Individual): List[Individual] = {
    val crossedGenes = for {
      case (leftGene, rightGene) <- chromosome.getGenes.zip(couple.getChromosome.getGenes)
    } yield {
      if(random.nextInt(100) + 1 <= CROSSOVER_LIKELIHOOD * 100) (leftGene, rightGene)
      else (rightGene, leftGene)
    }
    List(
      copyWith(chromosome.copyWith(crossedGenes.map(_._1))),
      copyWith(chromosome.copyWith(crossedGenes.map(_._2)))
    )
  }

  def mutate: Individual = copyWith(chromosome.mutate)
}
