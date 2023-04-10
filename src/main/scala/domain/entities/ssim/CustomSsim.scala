package domain.entities.ssim

import com.sksamuel.scrimage.*
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.transform.BackgroundGradient

import java.awt.Color

object CustomSsim extends App {
  val reference = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png")
  val other = ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/fusiongrass.png")

  val transformed = reference.fill(Color.BLUE)

  transformed.output(PngWriter.NoCompression, "src/main/scala/resources/ssim/cyndaquil2.png")
}
