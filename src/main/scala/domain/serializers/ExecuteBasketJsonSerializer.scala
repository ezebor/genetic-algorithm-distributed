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

  override def createPopulation(genes: Vector[JsValue]): Population = ??? /*individuals match
    case baskets: List[Basket] => BasketsPopulation(baskets)*/

  protected override def serializeGenes(genes: Vector[Gene]): JsValue = ??? /* gene match {
    case Item(name, price, satisfaction) => JsObject(
      "name" -> JsString(name),
      "price" -> JsNumber(price),
      "satisfaction" -> JsNumber(satisfaction)
    )
  }*/
}
