package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}

import scala.swing.Swing

object Render {
  def main(args: Array[String]): Unit = {
    testErode()
  }

  def runSwingSwing(g: Graph): Unit = {
    var gui: SimpleGUI = null
    val cfg       = Control.Config()
    cfg.useAsync  = false
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
      val fIn1    = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode.png")
      val fOut    = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode-slur.png")
      val i1      = ImageFileIn(fIn1, numChannels = 1)
      val width   = 2160
      val height  = 2160
      val frameSize = width * height
      val kernel  = 3
      val kernelS = kernel * kernel
      val m1      = MatrixInMatrix(i1, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
      val weights = ??? : GE  // integrated and normalized
      val noise   = ??? : GE  // zero to one
      val p       = noise > weights
      val index0  = WindowIndexWhere(p = p, size = kernelS)
      val index   = ??? : GE // index0.wrap(0, kernelS)
      val flt     = ??? : GE // WindowApply(m1, size = kernelS, index = index)

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
      val fIn1    = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/out_test/collat_7a510609-out-9475.png")
      val fOut    = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/collat_7a510609-out-9475-erode.png")
      val i1      = ImageFileIn(fIn1, numChannels = 1)
      val width   = 2160
      val height  = 2160
      val frameSize = width * height
      val kernel  = 3
      val kernelS = kernel * kernel
      val m1      = MatrixInMatrix(i1, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
      val m3      = RunningMin(m1, Metro(kernelS))
      val flt     = ResizeWindow(m3, size = kernelS, start = kernelS - 1)

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