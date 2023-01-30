package domain.entities

import akka.remote.DaemonMsgCreate
import domain.Execute
import domain.Operators.*
import domain.entities.*
import AlgorithmConfig.*
import OperatorRatios.*

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double) extends Gene

case class ItemsList(items: List[Item]) extends Chromosome(items)

case class Basket(itemsList: ItemsList) extends Individual(itemsList) {
  protected override def calculateFitness: Double = itemsList match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => Math.max(price, satisfaction) - Math.min(price, satisfaction)}.sum
  }

  override protected def copyWith(genes: List[Gene]): Individual = genes match
    case items: List[Item] => Basket(ItemsList(items))

  override def mutate: Individual = {
    val random = new Random()
    copyWith(
      itemsList.items.map(item =>
        if(random.nextInt(100) + 1 > MUTATION_LIKELIHOOD * 100) item
        else Item(s"Item ${item.name}", random.nextInt(POPULATION_SIZE) + 1, random.nextInt(POPULATION_SIZE) + 1)
      )
    )
  }
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


