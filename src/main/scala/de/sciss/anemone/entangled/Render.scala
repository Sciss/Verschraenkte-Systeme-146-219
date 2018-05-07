package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.kollflitz

import scala.swing.Swing

object Render {
  def main(args: Array[String]): Unit = {
    testSlur()
  }

  def any2stringadd: Nothing = sys.error("")

  def runSwingSwing(g: Graph): Unit = {
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
        val noise   = RepeatWindow((WhiteNoise() + (1.0: GE)) * 0.5, num = wv.size)  // zero to one
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

    runSwingSwing(g)
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

    runSwingSwing(g)
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

    runSwingSwing(g)
  }
}