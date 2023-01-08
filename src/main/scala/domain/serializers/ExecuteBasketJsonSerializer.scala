package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.individuals.*
import spray.json.{RootJsonFormat, *}

class ExecuteBasketJsonSerializer extends ExecuteJsonSerializer {
  override def chromosomeOf = (individual: Individual) => individual match {
    case Basket(itemsList) => itemsList.items
  }

  override def serializeGene = (gene: Gene) => gene match {
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

  override def deserializeIndividual = (genes: List[JsValue]) => {
    Basket(ItemsList(
      for case gene: JsValue <- genes
        yield buildItem(gene.asJsObject)
    ))
  }
}
