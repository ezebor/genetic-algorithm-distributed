package domain.individuals

import akka.remote.DaemonMsgCreate
import domain.Execute
import domain.Operators.*
import domain.individuals.*

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double) extends Gene

case class ItemsList(items: List[Item]) extends Chromosome

case class Basket(itemsList: ItemsList) extends Individual(itemsList) {
  protected override def calculateFitness: Double = itemsList match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => Math.max(price, satisfaction) - Math.min(price, satisfaction)}.sum
  }
}

case class BasketsPopulation(baskets: List[Basket]) extends Population(baskets)

object BasketGenerator extends IndividualGenerator {
  def apply(chromosome: ItemsList): Individual = Basket(chromosome)

  override def generateRandomPopulation(populationSize: Int): BasketsPopulation = {
    val random = new Random()
    BasketsPopulation(
      (1 to populationSize).map(i => Basket(
      ItemsList(List(
        Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1),
        Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1)
      )))).toList
    )
  }
}


