package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.fscape.graph
import de.sciss.fscape.Graph
import de.sciss.fscape.graph.ImageFile
import de.sciss.anemone.entangled.Render.runGraphOnConsole

object Aquila {
  case class Config(startInIdx: Int = 1, endInIdx0: Int = -1,
                    gain: Double = 1.5, gamma: Double = 3.0)

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Neural") {
      opt[Int] ("start")
        .text (s"Image input start index (default ${default.startInIdx})")
        .action { (v, c) => c.copy(startInIdx = v) }

      opt[Int] ("end")
        .text ("Image input end index (inclusive)")
        .action { (v, c) => c.copy(endInIdx0 = v) }

      opt[Double] ("gain")
        .text (s"Gain factor (default ${default.gain})")
        .action { (v, c) => c.copy(gain = v) }

      opt[Double] ("gamma")
        .text (s"Gamma value for right-hand side (default ${default.gamma})")
        .action { (v, c) => c.copy(gamma = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def any2stringadd: Nothing = sys.error("")

  def run(config: Config): Unit = {
    import config._
//    val startInIdx  = 1

    val imgInTemp   = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/mix/mix-%d.png")
    val imgOutTemp  = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/aquila/aquila-%d.png")

    def formatImgIn (frame: Int): File = Neural.formatTemplate(imgInTemp, frame)

    // def formatImgOut(frame: Int): File = Neural.formatTemplate(imgOutTemp , frame)

    val specIn    = ImageFile.readSpec(formatImgIn(startInIdx))
    val endInIdx  = if (endInIdx0 >= 0) endInIdx0 else {
      val numImages = Iterator.from(startInIdx).indexWhere { idx =>
        val f = formatImgIn(idx)
        !f.isFile
      }
      startInIdx + numImages - 1
    }
    val numFrames = endInIdx - startInIdx + 1

    val widthIn         = specIn.width
    val heightIn        = specIn.height
    val widthOut        = 1024
    val heightOut       = 512 // 768
    val widthOutH       = widthOut  / 2
    // val heightOutH      = heightOut / 2
    val zeroCrossings   = 0 // 3
    val scale           = {
      val sx = widthOut .toDouble / specIn.width
      val sy = heightOut.toDouble / specIn.height
      val res = math.min(sx, sy)
      // assert(res == 0.5, res.toString)
      // 0.5
      res
    }

    val g = Graph {
      import graph._
      def frameInOutIndices() = ArithmSeq(start = startInIdx, length = endInIdx - startInIdx + 1)

      val frameInIndices  = frameInOutIndices()
      val frameOutIndices = frameInOutIndices()

      val in      = ImageFileSeqIn(imgInTemp, numChannels = 1, indices = frameInIndices)
      val scaled  = AffineTransform2D.scale(in,
        widthIn   = widthIn , heightIn  = heightIn,
        widthOut  = widthOut, heightOut = heightOut,
        sx        = scale, // widthIn .toDouble / specIn.width,
        sy        = scale, // heightIn.toDouble / specIn.height,
        zeroCrossings = zeroCrossings /* , wrap = 0 */)

      val boosted0  = scaled * gain
      val pulse     = RepeatWindow(Metro(2), size = 1, num = widthOutH)
      val pow       = pulse.linLin(1, 0, 1.0, 1.0 / gamma)
//      val boosted   = boosted0 * pulse
      val boosted   = boosted0.pow(pow)
      val sigOut    = boosted.clip(0, 1)
      val frameSize = widthOut * heightOut

      Progress(Frames(sigOut) / (frameSize.toLong * numFrames), Metro(widthOut))

      val tpeOut  = if (imgOutTemp.extL == "png") ImageFile.Type.PNG else ImageFile.Type.JPG
      val specOut = ImageFile.Spec(fileType = tpeOut, sampleFormat = ImageFile.SampleFormat.Int8, quality = 95,
        width = widthOut, height = heightOut, numChannels = 1)
      ImageFileSeqOut(imgOutTemp, specOut, indices = frameOutIndices, in = sigOut)
    }
    runGraphOnConsole(g, blockSize = widthOut, seed = 23478)
  }
}
