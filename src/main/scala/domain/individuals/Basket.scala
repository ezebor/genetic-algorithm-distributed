package domain.individuals

import domain.Operators.*
import domain.Execute
import domain.individuals.{Chromosome, IndividualGenerator}

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double)

case class ItemsList(items: List[Item]) extends Chromosome

object BasketGenerator extends IndividualGenerator {
  def apply(chromosome: ItemsList): Individual = Basket(chromosome)

  override def generateRandomPopulation(populationSize: Int): List[Basket] = {
    val random = new Random()
    (1 to populationSize).map(i => Basket(
      ItemsList(List(Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1))))
    ).toList
  }
}

case class Basket(itemsList: ItemsList) extends Individual(itemsList) {
  override def calculateFitness: Double = itemsList match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => satisfaction - price}.sum
  }
}


