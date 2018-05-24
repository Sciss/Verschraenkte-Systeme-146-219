package de.sciss.anemone.entangled

import de.sciss.anemone.entangled.Render.runGraphOnConsole
import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.numbers

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
      val kernel        = 64 // 32 // 64 // 8
      val stepSize      = 2 // 16 // 2
      val frameIdx      = 20000
      val fIn1          = baseDir / "projects" / "Anemone" / "Verschraenkte-Systeme-146-219" / "calm_test" / s"collat_7a510609-outRC-$frameIdx.png"
      val fIn2          = baseDir / "projects" / "Anemone" / "Verschraenkte-Systeme-146-219" / "render_david" / s"david_ngn_32_3-R-$frameIdx.png"
      val fOut          = baseDir / "temp" / s"test-conv-k$kernel-X.png"
      val i1l           = ImageFileIn(fIn1, numChannels = 1)
      val i2l           = ImageFileIn(fIn2, numChannels = 1)
      val widthIn       = 2160
      val heightIn      = 2160
      val widthOut      = 1024 // 1080
      val heightOut     = 1024 // 1080
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

      val i1t = i1.excess(0.2)
      val i2t = i2.excess(0.2)

      def mkMatrix(in: GE): GE = {
        val a = MatrixInMatrix(in,
          rowsOuter = heightOut , columnsOuter  = widthOut,
          rowsInner = kernel    , columnsInner  = kernel,
          rowStep   = stepSize  , columnStep    = stepSize
        )
//        val b = ResizeWindow(a, kernelS, stop = kernelS)
//        val c = ResizeWindow(b, kernel , stop = kernel )
//        c
        a
      }

      val m1 = mkMatrix(i1t)
      val m2 = mkMatrix(i2t)

      val kernelOut = kernel // * 2

      val win = {
        val win1     = GenWindow(size = kernel , shape = GenWindow.Hann)
        val win2    = GenWindow(size = kernelS, shape = GenWindow.Hann)
        RepeatWindow(win2 * win1, size = kernelS, num = Int.MaxValue)
      }
      val m1w       = m1 * win
      val m2w       = m2 * win
      val m1f       = Real2FFT(m1w, rows = kernelOut, columns = kernelOut)
      val m2f       = Real2FFT(m2w, rows = kernelOut, columns = kernelOut)
      val m3f       = m1f.complex * m2f
      val m3        = Real2IFFT(m3f, rows = kernelOut, columns = kernelOut)

//      val resize    = ResizeWindow(m3, size = kernelOut.squared, start = kernelOut.squared - stepSize.squared)

      val resize: GE = if (stepSize == 1) {
//        ResizeWindow(m3, size = kernelOut.squared, start = kernelOut.squared - 1)
        ResizeWindow(m3, size = kernelOut.squared, stop = -(kernelOut.squared - 1))
      } else {
        val r0 = m3
//        println(s"size = $kernelOut, start = ${kernelOut- stepSize}")
////        val r1 = ResizeWindow(r0, size = kernelOut, start = kernelOut - stepSize)
//        val bar = kernelOut - stepSize
//        val r1  = ResizeWindow(r0, size = kernelOut, start = bar/2, stop = -(bar - bar/2))
////        AudioFileOut(file("/data/temp/fuckyou.aif"), AudioFileSpec(numChannels = 1, sampleRate = 44100), r1)
//        val foo = (kernelOut - stepSize) * widthOut
//        val r2 = ResizeWindow(r1, size = kernelOut * widthOut, start = foo/2, stop = -(foo - foo/2))
//        Length(r2).poll(0, "r2.length")
//        RunningMax(r0).last.poll(0, "r0.max")
//        RunningMax(r1).last.poll(0, "r1.max")
//        RunningMax(r2).last.poll(0, "r2.max")
//        r2

        ???
//        val r2 = MatrixOutMatrix(r0, rowsInner = kernel, columnsInner = kernel, columnsOuter = widthOut,
//          rowOff = (kernel - stepSize)/2, columnOff = (kernel - stepSize)/2, rowNum = stepSize, columnNum = stepSize)
////        Length(r2).poll(0, "r2.length")
////        RunningMax(r2).last.poll(0, "r2.max")
//        r2
      }

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


//      val fft1  = Real1FFT(i1    , size = widthOut)
//      val fft2  = Real1FFT(resize, size = widthOut)
//      val amt1  = 1078 // widthOut/3
//      val amt2  = widthOut - amt1
//      val fft1r = ResizeWindow(fft1 , size = widthOut       , start = +amt1)
//      val fft1s = ResizeWindow(fft1r, size = widthOut - amt1, start = -amt1)
//      val fft2r = ResizeWindow(fft2 , size = widthOut       , stop  = -amt2)
//      val fft2s = ResizeWindow(fft2r, size = widthOut - amt2, stop  = +amt2)
//      val zip   = fft1s + fft2s
//      val flt   = Real1IFFT(zip, size = widthOut)
      val flt   = resize

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

      val sig     = i4.clip(0.0, 1.0).pow(0.5)
      val specOut = ImageFile.Spec(width = widthOut, height = heightOut, numChannels = 1)
      ImageFileOut(fOut, specOut, in = sig)
    }

//    val ctl = Control(cfg)
//    ctl.run(g)
    runGraphOnConsole(g, blockSize = 1024 /* 1080 */, seed = 12345)
  }
}
