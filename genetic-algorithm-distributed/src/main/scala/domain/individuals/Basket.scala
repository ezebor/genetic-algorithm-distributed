package domain.individuals

import domain.Operators.Execute
import domain.individuals.{Chromosome, IndividualGenerator}
import spray.json.*

import scala.util.Random

trait BasketJsonProtocol extends DefaultJsonProtocol {
  implicit val itemFormatter: JsonFormat[Item] = jsonFormat3(Item)
  implicit val itemsListFormatter: JsonFormat[ItemsList] = jsonFormat1(ItemsList)
  implicit val individualFormatter: JsonFormat[Basket] = jsonFormat1(Basket)
}

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


