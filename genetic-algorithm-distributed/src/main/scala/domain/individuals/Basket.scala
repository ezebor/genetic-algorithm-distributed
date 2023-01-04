package domain.individuals

import domain.individuals.{Chromosome, Gen, IndividualGenerator}

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double) extends Gen
case class ItemsList(items: List[Item]) extends Chromosome

object BasketGenerator extends IndividualGenerator {
  def apply(chromosome: Chromosome): Individual = Basket(chromosome)

  override def generateRandomPopulation(populationSize: Int): Population = {
    val random = new Random()
    (1 to populationSize).map(i => Basket(
      ItemsList(List(Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1))))
    ).toList
  }
}

case class Basket(chromosome: Chromosome) extends Individual {
  override def calculateFitness(chromosome: Chromosome): Double = chromosome match {
    case ItemsList(items) => items.map{ case Item(_, price, satisfaction) => satisfaction - price}.sum
  }
}


