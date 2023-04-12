package domain.entities.ssim

import com.sksamuel.scrimage.*
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.transform.BackgroundGradient

import java.awt.Color
import scala.util.Random

object CustomSsim extends App {
  val reference = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/bulbasaur.png")

  // TODO: crear nueva imagen a partir de píxeles
  // TODO: IMPORTANTE ---> cuando haga crossover, si el pixel que quiero usar está ocupado buscar el primero libre
  // TODO: argb del pixel libre = -16777216
  //println(List.from(other.blank().pixels()))
  val newImage = reference.copy()
  val pixels = reference.pixels().map(pixel => Pixel(pixel.x, pixel.y, pixel.argb))
  pixels.foreach(pixel => newImage.setPixel(pixel))
  //newImage.output(PngWriter.MinCompression, "src/main/scala/resources/ssim/cyndaquil2.png")

  // TODO: FORMULA
  println(ssim(reference, ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/fusiongrass.png")))
  println(ssim(reference, ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/fusionfire.png")))
  println(ssim(reference, newImage))

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
}
