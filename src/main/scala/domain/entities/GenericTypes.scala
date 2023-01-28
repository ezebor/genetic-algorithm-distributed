package domain.entities

import scala.annotation.tailrec
import scala.util.Random

trait Chromosome
trait Gene

case class Population(individuals: List[Individual]) {
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
  
  def randomFitness = {
    val random = new Random()
    random.nextInt(accumulatedFitness.last._2.toInt) + 1
  }
  
  def intoChunks(chunkSize: Int): List[Population] = individuals
    .grouped(chunkSize)
    .map(anIndividuals => Population(anIndividuals))
    .toList
  
  def randomSubPopulation(size: Int): Population = {
    Population(
      LazyList // This is to avoid doing more than 1 loop
        .range(1, size + 1)
        .map(_ => randomFitness)
        .map(findIndividualWhoseAccumulatedFitnessWindowIncludes)
        .toList
    )
  }
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}
