package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.individuals.*
import spray.json.{RootJsonFormat, *}

//TODO: llevar código a IndividualJsonSerializer, y overridear acá
class BasketJsonSerializer extends Serializer with DefaultJsonProtocol {
  implicit object BasketJsonFormat extends RootJsonFormat[Execute] {
    // TODO: pasar a FOR comprehension
    def write(message: Execute) = message match
      case Execute(operatorName, population: List[Basket]) =>

        val formattedPopulation = JsArray(
          for {
            case Basket(itemsList) <- population.toVector
            items = itemsList.items.toVector
          } yield {
            JsObject(
              "chromosome" -> JsArray(
                for {
                  case Item(name, price, satisfaction) <- items
                } yield {
                  JsObject(
                    "name" -> JsString(name),
                    "price" -> JsNumber(price),
                    "satisfaction" -> JsNumber(satisfaction)
                  )
                }
              )
            )
          }
        )

        JsObject(
          "operatorName" -> JsString(operatorName),
          "population" -> formattedPopulation
        )
    def read(value: JsValue) = {
      value.asJsObject.getFields("operatorName", "population") match {
        case Seq(JsString(operatorName), JsArray(population)) =>
          val deserializedPopulation: Population = population.toList.map { case chromosome: JsValue =>
            chromosome.asJsObject.getFields("chromosome") match {
              case Seq(JsArray(genes)) =>
                Basket(
                  ItemsList(
                    genes.toList.map { case gene: JsValue =>
                      gene.asJsObject.getFields("name", "price", "satisfaction") match {
                        case Seq(JsString(name), JsNumber(price), JsNumber(satisfaction)) =>
                          Item(name, price.doubleValue, satisfaction.doubleValue)
                      }
                    }
                  )
                )
            }}
          Execute(operatorName, deserializedPopulation)
        case _ => throw new DeserializationException("Color expected")
      }
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
