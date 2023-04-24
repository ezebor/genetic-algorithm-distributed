package app

object ExecutionScript {
  private val BASKET_INDIVIDUAL_TYPE_NAME = "Basket"
  private val BASKET_SERIALIZATION_CONFIG = "executeBasketSerializationConfig"

  private val IMAGES_SIMILARITIES_TYPE_NAME = "ImagesSimilarities"
  private val IMAGES_SIMILARITIES_SERIALIZATION_CONFIG = "executeImagesSimilaritiesSerializationConfig"
  
  val INDIVIDUAL_TYPE_NAME: String = BASKET_INDIVIDUAL_TYPE_NAME
  val SERIALIZATION_CONFIG: String = BASKET_SERIALIZATION_CONFIG
}

object MasterNode extends EvolutionMasterNode(10, ExecutionScript.INDIVIDUAL_TYPE_NAME)
object WorkersNode1 extends EvolutionWorkersNode(2561)
object WorkersNode2 extends EvolutionWorkersNode(2562)
