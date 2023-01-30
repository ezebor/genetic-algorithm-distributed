package domain.entities

import akka.remote.DaemonMsgCreate
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.entities.OperatorRatios.*

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double) extends Gene {
  override def mutate: Gene =
    val random = new Random()
    if(random.nextInt(100) + 1 > MUTATION_LIKELIHOOD * 100) this
    else Item(s"Item ${name}", random.nextInt(POPULATION_SIZE) + 1, random.nextInt(POPULATION_SIZE) + 1)
}

case class ItemsList(items: List[Item]) extends Chromosome(items) {
  override def mutate: Chromosome = copyWith(items.map(item => item.mutate))
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case items: List[Item] => ItemsList(items)
}

case class Basket(itemsList: ItemsList) extends Individual(itemsList) {
  protected override def calculateFitness: Double = itemsList match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => Math.max(price, satisfaction) - Math.min(price, satisfaction)}.sum
  }

  override protected def copyWith(chromosome: Chromosome): Individual = chromosome match
    case itemsList: ItemsList => Basket(itemsList)

  override def mutate: Individual = copyWith(itemsList.mutate)
}

object BasketsPopulationRandomGenerator {
  def randomPopulation(populationSize: Int): Population = {
    val random = new Random()
    Population(
      (1 to populationSize).map(i => Basket(
        ItemsList(List(
          Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1),
          Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1)
        )))).toList
    )
  }
}


