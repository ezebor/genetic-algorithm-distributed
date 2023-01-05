package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.individuals.*
import spray.json.{RootJsonFormat, *}

//TODO: llevar código a IndividualJsonSerializer, y overridear acá
class BasketJsonSerializer extends Serializer with DefaultJsonProtocol {
  implicit object ColorJsonFormat extends RootJsonFormat[Execute] {
    // TODO: pasar a FOR comprehension
    def write(message: Execute) = message match
      case Execute(operatorName, population: List[Basket]) =>
        JsObject(
          "operatorName" -> JsString(operatorName),
          "population" -> JsArray(
            population.map{ case Basket(itemsList: ItemsList) => JsObject(
              "individual" -> JsArray(
                itemsList.items.map { case Item(name, price, satisfaction) => JsObject(
                  "chromosome" -> JsObject(
                    "gen" -> JsString(name),
                    "gen" -> JsNumber(price),
                    "gen" -> JsNumber(satisfaction),
                  )
                )
                }.toVector
              )
            )}.toVector
          )
        )
    def read(value: JsValue) = {
      println(value)
      /*value.asJsObject.getFields("name", "red", "green", "blue") match {
        case Seq(JsString(name), JsNumber(red), JsNumber(green), JsNumber(blue)) =>
          new Color(name, red.toInt, green.toInt, blue.toInt)
        case _ => throw new DeserializationException("Color expected")
      }*/
      Execute("hola", List(Basket(ItemsList(List()))))
    }
  }

  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case execute: Execute =>
      println(s"Serializing json basket $execute")
      execute.toJson.prettyPrint.getBytes()
    case _ => throw new Exception("Only Basket is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val string = new String(bytes)
    val execute = string.parseJson.convertTo[Execute]
    println(s"Deserializing json basket $execute")
    execute
  }

  override def includeManifest: Boolean = false
}
