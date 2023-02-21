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
  val SURVIVAL_LIKELIHOOD: Int = POPULATION_SIZE / (QUANTITY_OF_NODES * QUANTITY_OF_WORKERS_PER_NODE)
  val CROSSOVER_LIKELIHOOD: Double = 0.5
  val MUTATION_LIKELIHOOD: Double = 0.03
}

object AlgorithmConfig {
  val POPULATION_SIZE = 500
  val QUANTITY_OF_WORKERS_PER_NODE = 3
  val QUANTITY_OF_NODES = 2
  val POPULATION_GROWTH_RATIO = 1.648
  implicit val random: Random = new Random()
}

case class Population(individuals: List[Individual])(implicit random: Random) {
  lazy val accumulatedFitness: List[(Individual, Double)] = {
    val totalFitness = individuals.foldLeft(0d)((total, individual) => total + individual.fitness)
    val fitIndividuals = individuals.filter(_.fitness > 0)
    fitIndividuals
      .zipWithIndex
      .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
        result :+ (
          individual,
          if(index == 0) individual.fitness / totalFitness
          else if (index == fitIndividuals.size - 1) 1.0
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

    if(accumulatedFitness.isEmpty) throw new IllegalStateException(s"Unable to find individual with fitness = $aFitness: accumulatedFitness list is empty")

    recFindIndividualWhoseAccumulatedFitnessWindowIncludes(accumulatedFitness)
  }

  def intoChunks(chunkSize: Int): List[Population] = 
    if(chunkSize == 0) throw new IllegalArgumentException(s"Unable to slice into chunks of size $chunkSize")
    
    individuals
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
          Population(sinkPopulation.individuals ::: List(foundIndividual)),
          aSize - 1
        )
      }
    }

    if(this.accumulatedFitness.isEmpty) throw new IllegalStateException(s"Unable to generate a random subpopulation with with size = $size: accumulatedFitness list is empty")

    recRandomSubPopulation(
      this,
      Population(List()),
      size
    )
  }

  def selectStrongerPopulation(size: Int) = randomSubPopulation(size)

  def crossoverWith(otherPopulation: Population, crossoverLikelihood: Double): Population = {
    Population(individuals.flatMap { individual =>
      val couple = otherPopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
      individual.crossoverWith(couple, crossoverLikelihood: Double)
    })
  }

  def mutate(mutationLikelihood: Double): Population = {
    val individualsToMutate = individuals.filter(_ => random.nextInt(100) + 1 <= mutationLikelihood * 100)
    Population(
      individualsToMutate.map(individual => individual.mutate)
    )
  }
  
  def hasToStop: Boolean = individuals.exists(_.accomplishStopCriteria)
  
  def bestIndividuals: List[Individual] = individuals.filter(_.accomplishStopCriteria)
}

trait Individual(chromosome: Chromosome)(implicit random: Random) {
  protected def calculateFitness: Double
  def getChromosome: Chromosome = chromosome
  protected def copyWith(chromosome: Chromosome): Individual
  lazy val fitness: Double = calculateFitness
  def accomplishStopCriteria: Boolean

  def crossoverWith(couple: Individual, crossoverLikelihood: Double): List[Individual] = {
    def addGeneAccordingToLikelihood(nextGene: Gene, genes: List[Gene]): List[Gene] =
      if(random.nextInt(100) + 1 <= crossoverLikelihood * 100) nextGene :: genes
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
