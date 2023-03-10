package domain.entities

import domain.actors.EvolutionWorker
import domain.entities.AlgorithmConfig.*
import domain.entities.OperatorRatios.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}

trait Chromosome(genes: List[Gene])(implicit random: Random) {
  def getGenes: List[Gene] = genes
  def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
  def copyWith(genes: List[Gene]): Chromosome

  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness

  def crossoverWith(coupleGenes: List[Gene], crossoverLikelihood: Double)(implicit random: Random): (List[Gene], List[Gene]) = {
    def addGeneAccordingToLikelihood(nextGene: Gene, genes: List[Gene]): List[Gene] =
      if(random.nextInt(100) + 1 <= crossoverLikelihood * 100) nextGene :: genes
      else genes

    (genes ::: coupleGenes).foldLeft((List[Gene](), List[Gene]())) { (result, nextGene) =>
      (
        addGeneAccordingToLikelihood(nextGene, result._1),
        addGeneAccordingToLikelihood(nextGene, result._2)
      )
    }
  }
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
  implicit val random: Random = new Random()
  val MAX_QUANTITY_OF_GENERATIONS_WITHOUT_IMPROVEMENTS = 50
  val SOLUTIONS_POPULATION_SIZE = 10
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

    if(accumulatedFitness.isEmpty) throw new IllegalStateException(s"Unable to find individual with fitness = $aFitness: accumulatedFitness list is empty. Individuals size = ${individuals.size}")

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
      if(aSize == 0 || sourcePopulation.accumulatedFitness.isEmpty) sinkPopulation
      else {
        val foundIndividual = sourcePopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())

        recRandomSubPopulation(
          Population(sourcePopulation.individuals.filter(_ != foundIndividual)),
          Population(sinkPopulation.individuals ::: List(foundIndividual)),
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

  def selectStrongerPopulation(size: Int): Population = randomSubPopulation(size)

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

  override def toString: String = {
    s"""
      |***** Population summary *****
      |* Size of the population: ${individuals.size} individuals
      |* Best individual (fitness = ${bestIndividual.fitness}): $bestIndividual
      |* Individuals: $individuals
      |******************************
      |""".stripMargin
  }

  lazy val bestIndividual: Individual = individuals.reduceLeft((firstIndividual: Individual, secondIndividual: Individual) => {
    if(firstIndividual.fitness >= secondIndividual.fitness) firstIndividual
    else secondIndividual
  })
}

trait Individual(chromosome: Try[Chromosome])(implicit random: Random) {
  protected def copyWith(chromosome: Try[Chromosome]): Individual

  def getGenes: List[Gene] = chromosome
    .map(_.getGenes)
    .getOrElse(List())

  def fitness: Double = chromosome
    .map(_.fitness)
    .getOrElse(0)

  def crossoverWith(couple: Individual, crossoverLikelihood: Double): List[Individual] = {
    (for {
      crossedGenes <- chromosome.map(_.crossoverWith(couple.getGenes, crossoverLikelihood)(random))
    } yield {
      List(
        copyWith(chromosome.map(_.copyWith(crossedGenes._1))),
        copyWith(chromosome.map(_.copyWith(crossedGenes._2)))
      )
    }).getOrElse(List())
  }

  def mutate: Individual = copyWith(chromosome.map(_.mutate))
}
