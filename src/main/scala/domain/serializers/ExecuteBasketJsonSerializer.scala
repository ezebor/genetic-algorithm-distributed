package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.entities.*
import spray.json.*

import scala.util.Success

class ExecuteBasketJsonSerializer extends ExecuteJsonSerializer {
  protected override def chromosomeOf = (individual: Individual) => individual match {
    case Basket(Success(itemsList)) => itemsList.items
  }

  override def createPopulation(individuals: List[Individual]): Population = individuals match
    case baskets: List[Basket] => BasketsPopulation(baskets)

  protected override def serializeGene = (gene: Gene) => gene match {
    case Item(name, price, satisfaction) => JsObject(
      "name" -> JsString(name),
      "price" -> JsNumber(price),
      "satisfaction" -> JsNumber(satisfaction)
    )
  }

  private def buildItem = (gene: JsObject) => gene.getFields("name", "price", "satisfaction") match {
    case Seq(JsString(name), JsNumber(price), JsNumber(satisfaction)) =>
      Item(name, price.doubleValue, satisfaction.doubleValue)
  }

  protected override def deserializeIndividual = (genes: List[JsValue]) => {
    Basket(Success(ItemsList(
      for case gene: JsValue <- genes
        yield buildItem(gene.asJsObject)
    )))
  }
}
