package app

object ExecutionScript {
  val BASKET_INDIVIDUAL_TYPE_NAME = "Basket"
  val BASKET_SERIALIZATION_CONFIG = "executeBasketSerializationConfig"

  val IMAGES_SIMILARITIES_TYPE_NAME = "ImagesSimilarities"
  val IMAGES_SIMILARITIES_SERIALIZATION_CONFIG = "executeImagesSimilaritiesSerializationConfig"
  
  val INDIVIDUAL_TYPE_NAME: String = IMAGES_SIMILARITIES_TYPE_NAME
  val SERIALIZATION_CONFIG: String = IMAGES_SIMILARITIES_SERIALIZATION_CONFIG
}

object MasterNode extends EvolutionMasterNode(20, ExecutionScript.INDIVIDUAL_TYPE_NAME)
object WorkersNode1 extends EvolutionWorkersNode(2561)
object WorkersNode2 extends EvolutionWorkersNode(2562)
