package domain.serializers

import akka.serialization.Serializer
import domain.Operators.Execute
import domain.individuals.{Basket, Item, ItemsList}
import spray.json.*

class BasketJsonSerializer extends Serializer with DefaultJsonProtocol {
  implicit def itemFormatter: JsonFormat[Item] = jsonFormat3(Item)
  implicit def itemListFormatter: JsonFormat[ItemsList] = jsonFormat1(ItemsList)
  implicit def basketFormatter: JsonFormat[Basket] = jsonFormat1(Basket)

  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case basket: Basket =>
      println(s"Serializing json basket $basket")
      basket.toJson.prettyPrint.getBytes()
    case _ => throw new Exception("Only Basket is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val string = new String(bytes)
    val basket = string.parseJson.convertTo[Basket]
    println(s"Deserializing json basket $basket")
    basket
  }

  override def includeManifest: Boolean = false
}
