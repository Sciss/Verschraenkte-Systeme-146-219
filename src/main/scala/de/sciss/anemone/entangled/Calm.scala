/*
 *  Calm.scala
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
import de.sciss.fscape.{GE, Graph, graph}

object Calm {
  final case class Config(
                           imgInTemp     : File      = file("in-%d.png"),
                           imgOutTemp    : File      = file("out-%d.png"),
                           startInIdx    : Int       = 1,
                           endInIdx      : Int       = -1,
                           rngSeed       : Int       = 0x1234
                         ) {

    def formatImgIn (frame: Int): File = Neural.formatTemplate(imgInTemp  , frame)
    def formatImgOut(frame: Int): File = Neural.formatTemplate(imgOutTemp , frame)
  }

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Neural") {
      opt[File]('i', "input")
        .required()
        .text ("Image input file template, where %d would represent frame number")
        .action { (f, c) => c.copy(imgInTemp = f) }

      opt[File]('o', "output")
        .required()
        .text ("Image output file template, should end in '.png' or '.jpg', and %d would represent frame number.")
        .action { (f, c) =>
          c.copy(imgOutTemp = f)
        }

      opt[Int] ("seed")
        .text (s"Random number generator seed (default ${default.rngSeed})")
        .action { (v, c) => c.copy(rngSeed = v) }
    }
    p.parse(args, default)
  }

  def mkCalm(in: GE, frameSize: Int): GE = {
    import graph._
    val slid      = Sliding(in, size = frameSize * 3, step = frameSize)
    val wMax      = RunningWindowMax(slid, size = frameSize, trig = Metro(frameSize * 3))
    val sigOut    = ResizeWindow(wMax, size = frameSize * 3, start = frameSize * 2)
    sigOut
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
//    val numFrames     = endInIdx1 - startSrcIdx + 1

    val indicesV  = startSrcIdx to endInIdx1
    val width     = specIn.width    // 2160
    val height    = specIn.height   // 2160
    val frameSize = width * height
    val g = Graph {
      import graph._
      def frameIndices  = ValueIntSeq(indicesV: _*) // .take(indicesV.size)
      val imgIn     = ImageFileSeqIn(imgInTemp, numChannels = 1, indices = frameIndices)
      val sigOut    = mkCalm(imgIn, frameSize = frameSize)
      ImageFileSeqOut(imgOutTemp, ImageFile.Spec(ImageFile.Type.PNG, width = width, height = height, numChannels = 1),
        indices = frameIndices, in = sigOut)
      Progress(Frames(sigOut) / (frameSize.toLong * indicesV.size), Metro(width))
    }

    Render.runGraphOnConsole(g, blockSize = width, seed = rngSeed)
  }
}
