package domain.individuals

type Population = List[Individual]
trait Chromosome
trait Gen

trait Individual {
  def calculateFitness(chromosome: Chromosome): Double
  lazy val fitness: (chromosome: Chromosome) => Double = (chromosome: Chromosome) => calculateFitness(chromosome)
}

trait IndividualGenerator {
  def generateRandomPopulation(populationSize: Int): Population
}
