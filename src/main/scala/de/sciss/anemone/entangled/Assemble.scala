package de.sciss.anemone.entangled

import de.sciss.anemone.entangled.Render.runGraphOnConsole
import de.sciss.file._
import de.sciss.fscape.graph.ImageFile
import de.sciss.fscape.{Graph, graph}

object Assemble {
  case class Config(
                    imgIn1Temp    : File  = file("in1-%d.png"),
                    imgIn2Temp    : File  = file("in2-%d.png"),
                    imgOutTemp    : File  = file("out-%d.png"),
                    startInIdx    : Int   = 1,
                    endInIdx      : Int   = -1,
                    widthIn       : Int   = 1024,
                    heightIn      : Int   = 1024,
                    widthOut      : Int   = 2160,
                    heightOut     : Int   = 1080,
                   ) {

    def formatImgIn1(frame: Int): File = Neural.formatTemplate(imgIn1Temp , frame)
    def formatImgIn2(frame: Int): File = Neural.formatTemplate(imgIn2Temp , frame)
    def formatImgOut(frame: Int): File = Neural.formatTemplate(imgOutTemp , frame)
  }

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Neural") {
      opt[File]("input1")
        .required()
        .text ("Image 1 input file template, where %d would represent frame number")
        .action { (f, c) => c.copy(imgIn1Temp = f) }

      opt[File]("input2")
        .required()
        .text ("Image 2 input file template, where %d would represent frame number")
        .action { (f, c) => c.copy(imgIn2Temp = f) }

      opt[File]('o', "output")
        .required()
        .text ("Image output file template, should end in '.png' or '.jpg', and %d would represent frame number.")
        .action { (f, c) => c.copy(imgOutTemp = f) }

      opt[Int] ("start")
        .text (s"Image input start index (default ${default.startInIdx})")
        .action { (v, c) => c.copy(startInIdx = v) }

      opt[Int] ("end")
        .text ("Image input end index (inclusive)")
        .action { (v, c) => c.copy(endInIdx = v) }

      opt[Int] ('w', "width-out")
        .text (s"Width out (default ${default.widthOut})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(widthOut = v) }

      opt[Int] ('h', "height-out")
        .text (s"Height out (default ${default.heightOut})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(heightOut = v) }

      opt[Int] ("width-in")
        .text (s"Inner width (default ${default.widthIn})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(widthIn = v) }


      opt[Int] ("height-in")
        .text (s"Inner width (default ${default.heightIn})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(heightIn = v) }

    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def any2stringadd: Nothing = sys.error("")

  def run(config: Config): Unit = {
    import config._
    val specIn1   = ImageFile.readSpec(formatImgIn1(startInIdx))
    val specIn2   = ImageFile.readSpec(formatImgIn2(startInIdx))
    val endInIdx1 = if (endInIdx >= 0) endInIdx else {
      val numImages = Iterator.from(startInIdx).indexWhere { idx =>
        val f1 = formatImgIn1(idx)
        val f2 = formatImgIn2(idx)
        !f1.isFile || !f2.isFile
      }
      startInIdx + numImages - 1
    }
    val numFrames     = endInIdx1 - startInIdx + 1

    val g = Graph {
      import graph._
      def frameInOutIndices() = ArithmSeq(start = startInIdx, length = endInIdx1 - startInIdx + 1)

      val frameIn1Indices = frameInOutIndices()
      val frameIn2Indices = frameInOutIndices()
      val frameOutIndices = frameInOutIndices()

      val in1 = ImageFileSeqIn(imgIn1Temp, numChannels = 1, indices = frameIn1Indices)
      val in2 = ImageFileSeqIn(imgIn2Temp, numChannels = 1, indices = frameIn2Indices)
      val r1  = AffineTransform2D.scale(in1, widthIn = specIn1.width, heightIn = specIn1.height,
        widthOut = widthIn, heightOut = heightIn,
        sx = widthIn.toDouble / specIn1.width, sy = heightIn.toDouble / specIn1.height,
        zeroCrossings = 0, wrap = 0)
      val r2  = AffineTransform2D.scale(in2, widthIn = specIn2.width, heightIn = specIn2.height,
        widthOut = widthIn, heightOut = heightIn,
        sx = widthIn.toDouble / specIn2.width, sy = heightIn.toDouble / specIn2.height,
        zeroCrossings = 0, wrap = 0)
      val tx1 = (widthOut - 2*widthIn)/4
      val tx2 = widthOut - widthIn - (widthOut - 2*widthIn)/4
      val ty  = (heightOut - heightIn)/2
      val p1 = AffineTransform2D.translate(r1, widthIn = widthIn, heightIn = heightIn,
        widthOut = widthOut, heightOut = heightOut, tx = tx1, ty = ty, zeroCrossings = 0, wrap = 0)
      val p2 = AffineTransform2D.translate(r2, widthIn = widthIn, heightIn = heightIn,
        widthOut = widthOut, heightOut = heightOut, tx = tx2, ty = ty, zeroCrossings = 0, wrap = 0)
      val sigOut = p1 + p2
      val frameSize = widthOut * heightOut

      Progress(Frames(sigOut) / (frameSize.toLong * numFrames), Metro(widthOut))

      val tpeOut  = if (imgOutTemp.extL == "png") ImageFile.Type.PNG else ImageFile.Type.JPG
//      val specOut = specIn1.copy()
      val specOut = ImageFile.Spec(fileType = tpeOut, sampleFormat = ImageFile.SampleFormat.Int8, quality = 95,
        width = widthOut, height = heightOut, numChannels = 1)
      ImageFileSeqOut(imgOutTemp, specOut, indices = frameOutIndices, in = sigOut)
    }
    runGraphOnConsole(g, blockSize = widthOut, seed = 23478)
  }
}
