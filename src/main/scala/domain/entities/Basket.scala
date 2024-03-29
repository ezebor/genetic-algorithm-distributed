package domain.entities

import akka.remote.DaemonMsgCreate
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Random, Success, Try}

case class Item(name: String, price: Double, satisfaction: Double)(implicit customRandom: Random = random) extends Gene(1) {
  override def mutate: Gene =
    Item(s"Item ${name}", customRandom.nextInt(10) + 1, customRandom.nextInt(10) + 1)

  override def isHealthy: Boolean = false
}

case class ItemsList(items: List[Item])(implicit customRandom: Random = random) extends Chromosome(items)(customRandom) {
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

case class Basket(itemsList: Try[ItemsList])(implicit customRandom: Random = random) extends Individual(itemsList)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case itemsList: Try[ItemsList] => Basket(itemsList)

  override def isHealthy: Boolean = false
}

case class BasketsPopulation(baskets: List[Basket]) extends Population(baskets) {
  override def copyWith(newIndividuals: List[Individual]): Population = newIndividuals match
    case baskets: List[Basket] => BasketsPopulation(baskets)

  override def empty(): Population = BasketsPopulation(List[Basket]())
}


