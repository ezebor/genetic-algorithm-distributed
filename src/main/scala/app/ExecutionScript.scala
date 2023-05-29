package app

object ExecutionScript {
  val BASKET_INDIVIDUAL_TYPE_NAME = "Basket"
  val BASKET_SERIALIZATION_CONFIG = "executeBasketSerializationConfig"

  val IMAGES_SIMILARITIES_TYPE_NAME = "ImagesSimilarities"
  val IMAGES_SIMILARITIES_SERIALIZATION_CONFIG = "executeImagesSimilaritiesSerializationConfig"
  
  val INDIVIDUAL_TYPE_NAME: String = IMAGES_SIMILARITIES_TYPE_NAME
  val SERIALIZATION_CONFIG: String = IMAGES_SIMILARITIES_SERIALIZATION_CONFIG
  val DIMENSION_IMAGE_SIZE: Int = 110
  val DIMENSION_BLOCK_SIZE: Int = 11
  
  val QUANTITY_OF_WORKERS_PER_NODE: Int = 50
  val QUANTITY_OF_WORKER_NODES: Int = 1
  val POPULATION_SIZE: Int = 200
}

object MasterNode extends EvolutionMasterNode(ExecutionScript.INDIVIDUAL_TYPE_NAME)
object WorkersNode1 extends EvolutionWorkersNode(2561)
object WorkersNode2 extends EvolutionWorkersNode(2562)
