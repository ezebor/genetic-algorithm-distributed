package domain.individuals

import domain.SolutionDescription.*

import scala.util.Random

case class Item(name: String, price: Double, satisfaction: Double)

object Basket extends GenericIndividual[Item] {
  protected override def fitness(items: List[Item]): Double = items.map{ case Item(_, price, satisfaction) => satisfaction - price}.sum

  override def generateRandomPopulation(populationSize: Int): Population[Item] = {
    val random = new Random()
    (1 to populationSize).map(i => Basket(List(Item(s"Item $i", random.nextInt(populationSize) + 1, random.nextInt(populationSize) + 1)))).toList
  }
}


