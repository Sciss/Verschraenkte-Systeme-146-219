/*
 *  Render.scala
 *  (Verschränkte Systeme 146:219)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.fscape.graph.ImageFile
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.kollflitz

import scala.concurrent.Future
import scala.swing.Swing

object Render {
  case class Config(imgInTemp     : File  = file("in-%d.png"),
                    srcInTemp     : Option[File] = None, // file("src-%d.png"),
                    imgOutTemp    : File  = file("out-%d.png"),
                    startInIdx    : Int   = 1,
                    endInIdx      : Int   = -1,
//                    skipFrames    : Int   = 0,
                    frameStep     : Int   = 10,
                    holdFirst     : Int   = 0,
                    rngSeed       : Int   = 0xDEAD,
                    calm          : Boolean = false
                   ) {

    def formatImgIn (frame: Int): File = Neural.formatTemplate(imgInTemp  , frame)
//    def formatSrcIn (frame: Int): File = Neural.formatTemplate(srcInTemp  , frame)
    def formatImgOut(frame: Int): File = Neural.formatTemplate(imgOutTemp , frame)
  }

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Neural") {
      opt[File]('i', "input")
        .required()
        .text ("Image input file template, where %d would represent frame number")
        .action { (f, c) => c.copy(imgInTemp = f) }

      opt[File]('s', "source")
//        .required()
        .text ("Video image source file template, should end in '.png' or '.jpg', and %d would represent frame number.")
        .action { (f, c) => c.copy(srcInTemp = Some(f)) }

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

      // not yet implemented
//      opt[Int] ("skip")
//        .text (s"Number of output frames to skip (default: ${default.skipFrames}).")
//        .action { (v, c) => c.copy(skipFrames = v) }

      opt[Int] ('f', "frame-step")
        .text (s"Frame step or output frames per input frame (default ${default.frameStep})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(frameStep = v) }

      opt[Int] ('l', "hold-first")
        .text (s"Number of input frames to 'hold' while building up the GNG (default ${default.holdFirst})")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(holdFirst = v) }

      opt[Int] ("seed")
        .text (s"Random number generator seed (default ${default.rngSeed})")
        .action { (v, c) => c.copy(rngSeed = v) }

      opt[Unit] ("calm")
        .text (s"Apply calmness filter")
        .action { (_, c) => c.copy(calm = true) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }


  def run(config: Config): Unit = {
    import config._
    val specIn    = ImageFile.readSpec(formatImgIn(startInIdx))
    val endInIdx1 = if (endInIdx >= 0) endInIdx else {
      val numImages = Iterator.from(startInIdx).indexWhere { idx =>
        val f = formatImgIn(idx)
        !f.isFile
      }
      startInIdx + numImages - 1
    }
    val startSrcIdx   = startInIdx  // XXX TODO
    val numFrames     = endInIdx1 - startSrcIdx + 1
    val numFramesSrc0 = (numFrames + frameStep - 1) / frameStep - holdFirst
    val numFramesSrc  = srcInTemp.fold(numFramesSrc0 - 1) { srcInTemp0 =>
      if (Neural.formatTemplate(srcInTemp0, numFramesSrc0).exists()) numFramesSrc0 else numFramesSrc0 - 1
    }
    val endSrcIdx     = numFramesSrc

    import specIn.{width, height}
    val g = Graph {
      import graph._
      def frameInOutIndices() = ArithmSeq(start = startInIdx, length = endInIdx1 - startInIdx + 1)
      val frameInIndices    = frameInOutIndices()
      val frameOutIndices   = frameInOutIndices()
      val frameSrcIndices   = ValueIntSeq(
        Vector.fill(holdFirst)(startSrcIdx) ++ (startSrcIdx to endSrcIdx) :+ endSrcIdx: _*
      )

      val imgIn = ImageFileSeqIn(imgInTemp, numChannels = specIn.numChannels, indices = frameInIndices )

      val scale: Option[GE] = srcInTemp.map { srcInTemp0 =>
        val specSrc = ImageFile.readSpec(Neural.formatTemplate(srcInTemp0, startInIdx))

        val srcIn = ImageFileSeqIn(srcInTemp0, numChannels = specIn.numChannels, indices = frameSrcIndices)

        val resample = if (frameStep == 1) srcIn else {
          ResampleWindow(srcIn, size = specSrc.width * specSrc.height, factor = frameStep).clip(0.0, 1.0)
        }

        if (specSrc.width == width && specSrc.height == height) resample else {
          val i2 = resample
          AffineTransform2D.scale(i2,
            widthIn = specSrc.width, heightIn = specSrc.height, widthOut = width, heightOut = height,
            sx = width.toDouble / specSrc.width, sy = height.toDouble / specSrc.height, zeroCrossings = 0)
        }
      }

      val erode = {
        val kernel    = 3
        val kernelS   = kernel * kernel
        val m1        = MatrixInMatrix(imgIn, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
        val m3        = RunningMin(m1, Metro(kernelS))
        ResizeWindow(m3, size = kernelS, start = kernelS - 1)
      }
      val slur = {
        val narrow        = 0.8
        val randomization = 0.2
        val repeat        = 20
        val pTL           = (1.0 - narrow) * 0.5 * randomization
        val pT            = narrow * randomization
        val pTR           = pTL
        val pC            = 1.0 - randomization
        val wv            = Vector(pTL, pT, pTR, 0.0, pC, 0.0, 0.0, 0.0, 0.0)
        import kollflitz.Ops._
        val wvi     = wv.integrate
        assert(wvi.last == 1.0 && wvi.size == 9)
        val kernel  = ValueDoubleSeq(wvi: _*).take(wvi.size)
        GimpSlur(erode, width = width, height = height, kernel = kernel, kernelWidth = 3, kernelHeight = 3,
          repeat = repeat)
      }

      val burn = scale.fold[GE](slur) { scale0 =>
        val i1 = slur
        val i2 = scale0
        (-((-i1 * 255.0 + (255.0: GE)) * 256.0 / (i2 * 255.0 + (1.0: GE))) + (255.0: GE)) / 255.0 // .clip(0.0, 1.0)
      }

      val sigOut0   = burn.clip(0, 1)
      val frameSize = width * height
      val sigOut    = if (calm) Calm.mkCalm(sigOut0, frameSize = frameSize) else sigOut0

      Progress(Frames(sigOut) / (frameSize.toLong * numFrames), Metro(width))

      val tpeOut  = if (imgOutTemp.extL == "png") ImageFile.Type.PNG else ImageFile.Type.JPG
      val specOut = specIn.copy(fileType = tpeOut, sampleFormat = ImageFile.SampleFormat.Int8, quality = 95)
      ImageFileSeqOut(imgOutTemp, specOut, indices = frameOutIndices, in = sigOut)
    }
    runGraphOnConsole(g, blockSize = width, seed = rngSeed)
  }

  def any2stringadd: Nothing = sys.error("")

  def runGraphOnSwing(g: Graph): Unit = {
    var gui: SimpleGUI = null
    val cfg       = Control.Config()
    cfg.useAsync  = false
    cfg.blockSize = 2160
    cfg.progressReporter = p => Swing.onEDT(gui.progress = p.total)
    val ctl = Control(cfg)
    Swing.onEDT {
      gui = SimpleGUI(ctl)
    }
    ctl.run(g)
  }

  def runGraphOnConsole(g: Graph, blockSize: Int, seed: Long): Future[Unit] = {
    val cfg       = Control.Config()
    cfg.useAsync  = false
    cfg.blockSize = blockSize
    cfg.seed      = seed
    println("_" * 100)
    var lastProg = 0
    cfg.progressReporter = { p =>
      val prog = (p.total * 100).toInt
      while (lastProg < prog) {
        print('#')
        lastProg += 1
      }
    }
    val ctl = Control(cfg)
    ctl.run(g)
    ctl.status
  }

  def testBurn(): Unit = {
    val g = Graph {
      import graph._
      val frameIn   = 9475
      val frameOrig = frameIn / 25
      val fIn1      = file(s"/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-$frameIn-erode-slur.png")
      val fIn2      = file(s"/data/projects/Anemone/Verschraenkte-Systeme-146-219/prothese_7a510609_image_out/collat_7a510609-$frameOrig.png")
      val fOut      = file(s"/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-$frameIn-erode-slur-burn.png")
      val i1        = ImageFileIn(fIn1, numChannels = 1)
      val i2s       = ImageFileIn(fIn2, numChannels = 1)
      val width     = 2160
      val height    = 2160
      val frameSize = width * height

      val i2        = AffineTransform2D.scale(i2s,
        widthIn = width/2, heightIn = height/2, widthOut = width, heightOut = height, sx = 2, sy = 2, zeroCrossings = 0)

//      val flt = -((-i1 + (1.0: GE)) / i2).min(1.0) + (1.0: GE)
//      val flt = (-((-i1 + (1.0: GE)) * (256.0/255.0) / (i2 + (1.0: GE))) + (1.0: GE)).clip(0.0, 1.0)
      val flt = ((-((-i1 * 255.0 + (255.0: GE)) * 256.0 / (i2 * 255.0 + (1.0: GE))) + (255.0: GE)) / 255.0).clip(0.0, 1.0)

      Progress(Frames(flt) / frameSize, Metro(width))

      val sig     = flt
      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

    runGraphOnSwing(g)
  }

  def testSlur(): Unit = {
    val g = Graph {
      import graph._
      val fIn1      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode.png")
      val fOut      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode-slur.png")
      val i1        = ImageFileIn(fIn1, numChannels = 1)
      val width     = 2160
      val height    = 2160
      val frameSize = width * height

      /*

        0.1 * 0.2 : -(width + 1)
        0.8 * 0.2 : -(width)
        0.1 * 0.2 : -(width - 1)
        0.8       : 0

       */

      val upP     = 0.8
      val sideP   = (1.0 - upP)/2
      val rand    = 0.2
      val repeat  = 20

      def slur(in: GE): GE = {
        import kollflitz.Ops._
        val wv      = Vector(sideP * rand, upP * rand, sideP * rand, 1.0 - rand).integrate
        require(wv.last == 1.0)
        val dv      = Vector(width + 1, width, width - 1, 0)
        val maxLen  = dv.max
        val weights = ValueDoubleSeq(wv: _*)  // integrated and normalized
//        RepeatWindow(weights).take(100).poll(Metro(2), "weights")
        val delays  = ValueIntSeq   (dv: _*)
//        val noise   = RepeatWindow((WhiteNoise() + (1.0: GE)) * 0.5, num = wv.size)  // zero to one
        val noise   = Latch((WhiteNoise() + (1.0: GE)) * 0.5, Metro(wv.size))  // zero to one
        // noise.poll(Metro(2), "noise")
        val p       = noise < weights
//        RepeatWindow(p).take(100).poll(Metro(2), "p")
        val index   = WindowIndexWhere(p = p, size = wv.size)
//        index.poll(Metro(2), "index")
        val delay   = WindowApply(delays, size = wv.size, index = index)
//        delay.poll(Metro(2), "delay")
        DelayN(in, maxLen, delay)
      }

      val flt = (0 until repeat).foldLeft(i1: GE)((in, _) => slur(in))

      Progress(Frames(flt) / frameSize, Metro(width))

      val sig     = flt
      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

    runGraphOnSwing(g)
  }

  def testSlur_OLD(): Unit = {
    val g = Graph {
      import graph._
      val fIn1      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode.png")
      val fOut      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode-slur.png")
      val i1        = ImageFileIn(fIn1, numChannels = 1)
      val width     = 2160
      val height    = 2160
      val frameSize = width * height
      val kernel    = 3
      val kernelS   = kernel * kernel

      def slur(in: GE): GE = {
        val m1      = MatrixInMatrix(in, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
        import kollflitz.Ops._
        val wv      = Vector(0.1 * 0.2, 0.8 * 0.2, 0.1 * 0.2, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0).integrate
        val weights = ValueDoubleSeq(wv: _*)  // integrated and normalized
        val noise   = RepeatWindow((WhiteNoise() + (1.0: GE)) * 0.5, num = wv.size)  // zero to one
        val p       = noise < weights
        val index   = WindowIndexWhere(p = p, size = kernelS)
        WindowApply(m1, size = kernelS, index = index, wrap = 1)
      }

      val flt = (0 until 20).foldLeft(i1: GE)((in, _) => slur(in))

      Progress(Frames(flt) / frameSize, Metro(width))

      val sig     = flt
      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

    runGraphOnSwing(g)
  }

  def testErode(): Unit = {

    val g = Graph {
      import graph._
      val fIn1      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/out_test/collat_7a510609-out-9475.png")
      val fOut      = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode.png")
      val i1        = ImageFileIn(fIn1, numChannels = 1)
      val width     = 2160
      val height    = 2160
      val frameSize = width * height
      val kernel    = 3
      val kernelS   = kernel * kernel
      val m1        = MatrixInMatrix(i1, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
      val m3        = RunningMin(m1, Metro(kernelS))
      val flt       = ResizeWindow(m3, size = kernelS, start = kernelS - 1)

//      Progress(Frames(flt) / (2 * frameSize), Metro(width))
//      Length(flt).poll(0, "flt-len")

      Progress(Frames(flt) / frameSize, Metro(width))

      val sig     = flt
      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

    runGraphOnSwing(g)
  }
}