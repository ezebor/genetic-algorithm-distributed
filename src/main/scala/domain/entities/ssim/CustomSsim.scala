package domain.entities.ssim

import com.sksamuel.scrimage.*
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.transform.BackgroundGradient

import java.awt.Color

object CustomSsim extends App {
  val reference = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png")
  val other = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/fusiongrass.png")

  // TODO: crear nueva imagen a partir de píxeles
  // TODO: IMPORTANTE ---> cuando haga crossover, si el pixel que quiero usar está ocupado buscar el primero libre
  // TODO: argb del pixel libre = -16777216
  println(List.from(other.blank().pixels()))
  val newImage = reference.copy()
  val pixels = reference.pixels().map(pixel => Pixel(pixel.x, pixel.y, pixel.argb))
  pixels.foreach(pixel => newImage.setPixel(pixel))
  newImage.output(PngWriter.MinCompression, "src/main/scala/resources/ssim/cyndaquil2.png")

  // TODO: FORMULA
  println(mu(reference))
  println(mu(newImage))
  println(mu(other))

  println(sigma(reference))
  println(sigma(newImage))
  println(sigma(other))

  // TODO: gaussian formula distribution: https://en.wikipedia.org/wiki/Normal_distribution


  // TODO: hacerlo mediante ventanas
  def mu(image: ImmutableImage): Double = List.from(image.pixels().map(_.average())).sum / image.count()

  // TODO: hacerlo mediante ventanas
  def sigma(image: ImmutableImage): Double = {
    val imageMu = mu(image)
    Math.sqrt(List.from(image.pixels().map(pixel => Math.pow(pixel.average() - imageMu, 2))).sum / (image.count() - 1))
  }

}
