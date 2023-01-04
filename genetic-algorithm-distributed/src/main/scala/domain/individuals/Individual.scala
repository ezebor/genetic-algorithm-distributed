package domain.individuals

import spray.json.*

type Population = List[Individual]
trait Chromosome
trait Gen

trait Individual(chromosome: Chromosome) extends DefaultJsonProtocol {
  def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}

trait IndividualGenerator {
  def generateRandomPopulation(populationSize: Int): Population
}
