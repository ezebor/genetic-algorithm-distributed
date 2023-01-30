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
    Item(s"Item ${name}", random.nextInt(POPULATION_SIZE) + 1, random.nextInt(POPULATION_SIZE) + 1)
}

case class ItemsList(items: List[Item]) extends Chromosome(items) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case items: List[Item] => ItemsList(items)
}

case class Basket(itemsList: ItemsList) extends Individual(itemsList) {
  protected override def calculateFitness: Double = itemsList match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => Math.max(price, satisfaction) - Math.min(price, satisfaction)}.sum
  }

  override protected def copyWith(chromosome: Chromosome): Individual = chromosome match
    case itemsList: ItemsList => Basket(itemsList)
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


