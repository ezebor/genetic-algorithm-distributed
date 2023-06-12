package domain.entities.ssim

import app.ExecutionScript.*
import com.sksamuel.scrimage.*
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.transform.BackgroundGradient
import domain.Execute
import domain.entities.*
import domain.serializers.ExecuteImagesSimilaritiesJsonSerializer

import java.awt.Color
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Success}

object CustomSsim extends App {
  val reference = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
  val comp = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/fusionfire.png")

  val crossoverLikelihood = 0.5
  val mutationLikelihood = 0.1
  val survivalPopulationSize = 180
  val population = ImagesManager.initialPopulation()
  val populationLookingForReproduction = population.randomSubPopulation(population.individuals.size / 2)
  val children = populationLookingForReproduction.crossoverWith(population, crossoverLikelihood)
  val parentsAndChildren = children.fusionWith(population)
  val mutatedPopulation = parentsAndChildren.mutate(mutationLikelihood)
  val finalPopulation = mutatedPopulation
    .fusionWith(parentsAndChildren)
    .selectStrongerPopulation(survivalPopulationSize)
  println(finalPopulation.individuals.map(i => i.fitness.get))

  //val population2 = population.crossoverWith(population, 0.5)
  //.mutate(0.5)

  /*import scala.concurrent.ExecutionContext.Implicits.global
  val a: Population = Await.result(Future {
    population.selectStrongerPopulation(75)
  }, Duration.Inf)*/



//  val serializer = new ExecuteImagesSimilaritiesJsonSerializer()
//  println(serializer.read(serializer.write(Execute("example", population))).population.individuals.map(_.fitness.get))

  //println(population.selectStrongerPopulation(8).accumulatedFitness.map(_._2))

  /*println(population.images.flatMap{ case Image(Success(Frame(blocks))) =>
    blocks.map(_.pixels.map(p => (p.x, p.y)))
  })*/

  /*population.mutate(0.5).individuals
    .zipWithIndex
    .foreach { case (Image(Success(Frame(blocks))), index) =>
    val newImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    blocks.foreach { aBlock =>
      aBlock.pixels.foreach { pixel =>
        newImage.setPixel(aBlock.pixelWithFixedLocation(pixel))
      }
    }
    newImage.output(PngWriter.NoCompression, s"src/main/scala/resources/ssim/result_$index.png")
  }*/

  /*
  // TODO: crear nueva imagen a partir de píxeles
  // TODO: IMPORTANTE ---> cuando haga crossover, si el pixel que quiero usar está ocupado buscar el primero libre
  // TODO: argb del pixel libre = -16777216
  //println(List.from(other.blank().pixels()))
  val newImage = reference.blank()

  println(intoBlocks(reference).head.pixels.size)
  val a = intoBlocks(reference).head.pixels.flatMap{pixel =>
    val s = s"${pixel.x + pixel.y}"
    s.getBytes
  }
  println(a.size)

  for {
    case BlockCoordinates(pixels) <- intoBlocks(reference).take(400)
    pixel <- pixels
  } yield {
    newImage.setPixel(reference.pixel(pixel.x, pixel.y))
  }

  def intoBlocks(immutableImage: ImmutableImage, blockSize: Int = 11): List[BlockCoordinates] = {
    def dimensionOrderedIndexes(dimension: Pixel => Int): List[List[Int]] = Set
      .from(immutableImage.pixels().map(dimension))
      .toList
      .sortWith((a, b) => a <= b)
      .grouped(blockSize).toList

    val rows: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.x)
    val columns: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.y)

    for {
      blockX <- rows
      blockY <- columns
      positionsBlock = blockX.flatMap(index => (1 to blockSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      Block(positionsBlock.map((x, y) => immutableImage.pixel(x, y)))
    }
  }

  newImage.output(PngWriter.MinCompression, "src/main/scala/resources/ssim/cyndaquil2.png")

  println(List((1, 2), (1, 5), (3, 4)).toMap.values)

  // TODO: FORMULA
  val block1 = intoBlocks(reference).head
  val block2 = intoBlocks(comp).head
  println(block1.ssim(block2))

  // TODO: gaussian formula distribution: https://en.wikipedia.org/wiki/Normal_distribution

  // TODO: hacerlo mediante ventanas
  def mu(image: ImmutableImage): Double = List.from(image.pixels().map(_.average())).sum / image.count()

  // TODO: hacerlo mediante ventanas
  def sigma(image: ImmutableImage): Double = {
    val imageMu = mu(image)
    Math.sqrt(List.from(image.pixels().map(pixel => Math.pow(pixel.average() - imageMu, 2))).sum / (image.count() - 1))
  }

  def sigma(ref: ImmutableImage, comp: ImmutableImage): Double = {
    val refMu = mu(ref)
    val compMu = mu(comp)

    val refAverages = ref.pixels().map(_.average())
    val compAverages = comp.pixels().map(_.average())

    val averagesSum = refAverages.indices.map(index => (refAverages(index) - refMu) * (compAverages(index) - compMu)).sum

    averagesSum / (ref.count() - 1)
  }

  val k1 = 0.01
  val k2 = 0.03f
  val L = 255
  val c1 = Math.pow(k1 * L, 2)
  val c2 = Math.pow(k2 * L, 2)
  val c3 = c2 / 2

  def luminance(ref: ImmutableImage, comp: ImmutableImage) = (2 * mu(ref) * mu(comp) + c1) / (Math.pow(mu(ref), 2) + Math.pow(mu(comp), 2) + c1)
  def contrast(ref: ImmutableImage, comp: ImmutableImage) = (2 * sigma(ref) * sigma(comp) + c2) / (Math.pow(sigma(ref), 2) + Math.pow(sigma(comp), 2) + c2)
  def structure(ref: ImmutableImage, comp: ImmutableImage) = (sigma(ref, comp) + c3) / (sigma(ref) * sigma(comp) + c3)

  def ssim(ref: ImmutableImage, comp: ImmutableImage) = luminance(ref, comp) * contrast(ref, comp) * structure(ref, comp)
  */
}
