package domain.entities

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Chromosome(genes: List[Gene]) {
  def getGenes: List[Gene] = genes
}
trait Gene

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

  def crossoverWith(otherPopulation: Population): Population = Population(individuals.flatMap(individual => individual.crossoverWith(otherPopulation)))
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
  def crossoverWith(population: Population): List[Individual] = {
    val couple = population.findIndividualWhoseAccumulatedFitnessWindowIncludes(population.randomFitness)
    List()
  }
}
