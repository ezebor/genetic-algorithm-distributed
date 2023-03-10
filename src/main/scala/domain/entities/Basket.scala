package domain.entities

import akka.remote.DaemonMsgCreate
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.entities.OperatorRatios.*

import scala.util.{Random, Success, Try}

case class Item(name: String, price: Double, satisfaction: Double) extends Gene {
  override def mutate: Gene =
    val random = new Random()
    Item(s"Item ${name}", random.nextInt(10) + 1, random.nextInt(10) + 1)
}

case class ItemsList(items: List[Item]) extends Chromosome(items) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case items: List[Item] => ItemsList(items)

  protected override def calculateFitness: Double = {
    if(items.size >= 5 || items.size <= 2) 0.1
    else items.map{ case Item(_, price, satisfaction) =>
      if(price > satisfaction) 0
      else satisfaction - price
    }.sum
  }
}

case class Basket(itemsList: Try[ItemsList]) extends Individual(itemsList) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case itemsList: Try[ItemsList] => Basket(itemsList)
}

object BasketsPopulationRandomGenerator {
  def randomPopulation(populationSize: Int): Population = {
    Population(
      (1 to populationSize).map(i => Basket(
        Success(ItemsList(
          (1 to (random.nextInt(5) + 1)).map(_ => Item(s"Item $i", random.nextInt(10), random.nextInt(10))).toList
        ))
      )).toList)
  }
}


