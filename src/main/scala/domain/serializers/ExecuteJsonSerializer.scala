package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.individuals.*
import spray.json.{RootJsonFormat, *}

trait ExecuteJsonSerializer extends Serializer with DefaultJsonProtocol with RootJsonFormat[Execute] {
  def chromosomeOf: Individual => List[Gene]
  def serializeGene: Gene => JsValue
  def deserializeIndividual: List[JsValue] => Individual

  def write(message: Execute) = message match {
    case Execute(operatorName: String, population: Population) =>
      val formattedPopulation = JsArray(
        for {
          case individual: Individual <- population.toVector
          chromosome = chromosomeOf(individual).toVector
        } yield {
          JsObject(
            "chromosome" -> JsArray(
              for gene: Gene <- chromosome
                yield serializeGene(gene)
            )
          )
        }
      )

      JsObject(
        "operatorName" -> JsString(operatorName),
        "population" -> formattedPopulation
      )
  }

  def read(value: JsValue) = {
    value.asJsObject.getFields("operatorName", "population") match {
      case Seq(JsString(operatorName), JsArray(population)) =>
        Execute(operatorName,
          for {
            case chromosome: JsValue <- population.toList
            case JsArray(genes) <- chromosome.asJsObject.getFields("chromosome")
          } yield deserializeIndividual(genes.toList)
        )
      case _ => throw new DeserializationException("Execute message expected")
    }
  }

  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case executeMessage: Execute =>
      println(s"Serializing Execute message to json: $executeMessage")
      executeMessage.toJson(write).prettyPrint.getBytes()
    case _ => throw new Exception("Only Basket is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val executeMessage = new String(bytes).parseJson.convertTo[Execute](read)
    println(s"Deserializing Execute message from json: $executeMessage")
    executeMessage
  }

  override def includeManifest: Boolean = false
}
