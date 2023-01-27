package domain.entities

import scala.annotation.tailrec

trait Chromosome
trait Gene

case class Population(individuals: List[Individual]) {
  lazy val accumulatedFitness: List[(Individual, Double)] = individuals
    .zipWithIndex
    .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
      result :+ (individual, {
        if(index == 0) individual.fitness
        else individual.fitness + result(index - 1)._2
      })
    }

  def findIndividualWithFitnessCloserTo(aFitness: Int): Individual = {
    @tailrec
    def recursiveFindIndividualWithFitnessCloserTo(anAccumulatedFitness: List[(Individual, Double)]): Individual = {
      if(anAccumulatedFitness.size == 1) anAccumulatedFitness.head._1
      else {
        val middleIndex = anAccumulatedFitness.size / 2
        val middleFitness = anAccumulatedFitness(middleIndex)._2
        if(aFitness >= middleFitness) recursiveFindIndividualWithFitnessCloserTo(anAccumulatedFitness.takeRight(middleIndex))
        else {
          if(aFitness >= anAccumulatedFitness(middleIndex - 1)._2) anAccumulatedFitness(middleIndex)._1
          else recursiveFindIndividualWithFitnessCloserTo(anAccumulatedFitness.take(middleIndex))
        }
      }
    }
    recursiveFindIndividualWithFitnessCloserTo(accumulatedFitness)
  }
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}
