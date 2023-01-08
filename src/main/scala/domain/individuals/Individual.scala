package domain.individuals

type Population = List[Individual]
trait Chromosome
trait Gene

trait Individual(chromosome: Chromosome) {
  def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}

trait IndividualGenerator {
  def generateRandomPopulation(populationSize: Int): Population
}
