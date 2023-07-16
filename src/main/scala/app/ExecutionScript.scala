package app

object ExecutionScript {
  val BASKET_INDIVIDUAL_TYPE_NAME = "Basket"
  val BASKET_SERIALIZATION_CONFIG = "executeBasketSerializationConfig"

  val IMAGES_SIMILARITIES_TYPE_NAME = "ImagesSimilarities"
  val IMAGES_SIMILARITIES_SERIALIZATION_CONFIG = "executeImagesSimilaritiesSerializationConfig"
  
  val INDIVIDUAL_TYPE_NAME: String = IMAGES_SIMILARITIES_TYPE_NAME
  val SERIALIZATION_CONFIG: String = IMAGES_SIMILARITIES_SERIALIZATION_CONFIG
  val DIMENSION_IMAGE_SIZE: Int = 550
  val DIMENSION_BLOCK_SIZE: Int = 11

  val QUANTITY_OF_NODES: Int = 2
  val QUANTITY_OF_WORKERS_PER_NODE: Int = 1
  val POPULATION_SIZE: Int = 5
  val CHUNK_SIZE = 1
  val QUANTITY_OF_WORKERS: Int = QUANTITY_OF_WORKERS_PER_NODE * QUANTITY_OF_NODES
}

object MasterNode extends EvolutionMasterNode(ExecutionScript.QUANTITY_OF_WORKERS_PER_NODE)
object WorkersNode1 extends EvolutionWorkersNode(2561)
object WorkersNode2 extends EvolutionWorkersNode(2562)
