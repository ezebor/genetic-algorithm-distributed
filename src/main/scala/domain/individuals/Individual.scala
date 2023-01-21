package domain.individuals

type Population = List[Individual]
trait Chromosome
trait Gene

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}
type LeveledIndividual = (Individual, Double)
type LeveledPopulation = List[LeveledIndividual]

trait IndividualGenerator {
  def generateRandomPopulation(populationSize: Int): Population
}

trait FitnessTree
case class Node(leveledIndividual: LeveledIndividual, leftTree: FitnessTree, rightTree: FitnessTree) extends FitnessTree
case class Leaf(leveledIndividual: LeveledIndividual) extends FitnessTree
