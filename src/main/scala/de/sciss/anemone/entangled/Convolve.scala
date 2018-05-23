package de.sciss.anemone.entangled

import de.sciss.anemone.entangled.Render.runGraphOnConsole
import de.sciss.file._
import de.sciss.numbers
import de.sciss.fscape.{Graph, graph}

object Convolve {
  def main(args: Array[String]): Unit = {
    run()
  }

  def any2stringadd: Nothing = sys.error("")

  def run(): Unit = {
//    val cfg       = Control.Config()
//    cfg.useAsync  = false
//    cfg.blockSize = 1080

    val baseDir = {
      val tmp = file("/data")
      if (tmp.isDirectory) tmp else userHome / "Documents"
    }

    val g = Graph {
      import graph._
      import numbers.Implicits._
      val kernel        = 32 // 64 // 8
      val stepSize      = 1
      val fIn1          = baseDir / "projects" / "Anemone" / "Verschraenkte-Systeme-146-219" / "render_test" / "collat_7a510609-outR-8444.png"
      val fIn2          = baseDir / "projects" / "Anemone" / "Verschraenkte-Systeme-146-219" / "render_david" / "david_ngn_32_3-R-292.png"
      val fOut          = baseDir / "temp" / s"test-conv-k$kernel-X.png"
      val i1l           = ImageFileIn(fIn1, numChannels = 1)
      val i2l           = ImageFileIn(fIn2, numChannels = 1)
      val widthIn       = 2160
      val heightIn      = 2160
      val widthOut      = 1080
      val heightOut     = 1080
//      val frameSizeIn   = widthIn * heightIn
      val frameSizeOut  = widthOut * heightOut
      val i1s           = AffineTransform2D.scale(i1l,
        widthIn  = widthIn , heightIn  = heightIn,
        widthOut = widthOut, heightOut = heightOut,
        sx = 0.5, sy = 0.5, zeroCrossings = 0)
      val i1 = BufferMemory(i1s, frameSizeOut)
      val i2            = AffineTransform2D.scale(i2l,
        widthIn  = widthIn , heightIn  = heightIn,
        widthOut = widthOut, heightOut = heightOut,
        sx = 0.5, sy = 0.5, zeroCrossings = 0)
      val kernelS       = kernel * kernel
      val m1      = MatrixInMatrix(i1,
        rowsOuter = heightOut , columnsOuter  = widthOut,
        rowsInner = kernel    , columnsInner  = kernel,
        rowStep   = stepSize  , columnStep    = stepSize
      )
      val m2      = MatrixInMatrix(i2,
        rowsOuter = heightOut , columnsOuter  = widthOut,
        rowsInner = kernel    , columnsInner  = kernel,
        rowStep   = stepSize  , columnStep    = stepSize
      )

      val m1f       = Real2FFT(m1, rows = kernel, columns = kernel)
      val m2f       = Real2FFT(m2, rows = kernel, columns = kernel)
      val m3f       = m1f.complex * m2f
      val m3        = Real2IFFT(m3f, rows = kernel, columns = kernel)

      val resize    = ResizeWindow(m3, size = kernelS, start = kernelS - stepSize.squared)

//      val flt = ResizeWindow(
//        ResizeWindow(m3, size = kernelS, start = kernel * (kernel - stepSize)),
//          size = kernel, start = (kernel - stepSize))

//      val flt = {
//        val widthS  = kernel * widthOut
//        val heightS = kernel
//        val winSizeS= widthOut * stepSize
//        val next    = Metro(winSizeS) // .tail
//        val xB      = for {
//          y <- 0 until stepSize
//          x <- 0 until (widthOut / stepSize)
//          z <- 0 until stepSize
//        } yield x * kernelS + y * kernel + z
//
//        assert(xB.size == winSizeS)
//
//        val yB      = for {
//          y <- 0 until stepSize
//          x <- 0 until widthOut
//        } yield y
//
//        assert(yB.size == winSizeS)
//
//        val xV      = ValueIntSeq(xB: _*)
//        val yV      = ValueIntSeq(yB: _*)
//        val xS      = RepeatWindow(xV, size = winSizeS, num = Int.MaxValue)
//        val yS      = RepeatWindow(yV, size = winSizeS, num = Int.MaxValue)
//        RepeatWindow(xV).poll(Metro(2), "xV")
//        RepeatWindow(yV).poll(Metro(2), "yV")
//        ScanImage(m3, width = widthS, height = heightS, x = xS, y = yS, next = next, zeroCrossings = 0)
//      }


      val fft1  = Real1FFT(i1    , size = widthOut)
      val fft2  = Real1FFT(resize, size = widthOut)
      val amt1  = 1076 // widthOut/3
      val amt2  = widthOut - amt1
      val fft1r = ResizeWindow(fft1 , size = widthOut       , start = +amt1)
      val fft1s = ResizeWindow(fft1r, size = widthOut - amt1, start = -amt1)
      val fft2r = ResizeWindow(fft2 , size = widthOut       , stop  = -amt2)
      val fft2s = ResizeWindow(fft2r, size = widthOut - amt2, stop  = +amt2)
      val zip   = fft1s + fft2s
      val flt   = Real1IFFT(zip, size = widthOut)

      Progress(Frames(flt) / (2 * frameSizeOut), Metro(widthOut))
//      Length(flt).poll(0, "flt-len")

      val i3        = flt
      val frameTr1  = Metro(frameSizeOut)
      val frameTr2  = Metro(frameSizeOut)
      val maxR      = RunningMax(i3, trig = frameTr1).drop(frameSizeOut - 1)
//      val minR      = RunningMin(i3, trig = frameTr1).drop(frameSizeOut - 1)
      val max       = Gate(maxR, gate = frameTr2)
//      val min       = Gate(minR, gate = frameTr2)
      val mul       = (max /* - min */).reciprocal
//      val add       = -min
      val i3e       = i3.elastic(frameSizeOut / 1080 /* cfg.blockSize */ + 1)

//      val noise     = WhiteNoise(0.1)
      val i4        = (i3e /* + add */) * mul // + noise

      Progress(Frames(i4) / (2 * frameSizeOut), Metro(widthOut))

      val sig     = i4.clip(0.0, 1.0)
      val specOut = ImageFile.Spec(width = widthOut, height = heightOut, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

//    val ctl = Control(cfg)
//    ctl.run(g)
    runGraphOnConsole(g, blockSize = 1080, seed = 12345)
  }
}
