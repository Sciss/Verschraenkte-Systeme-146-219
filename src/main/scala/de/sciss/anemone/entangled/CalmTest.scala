/*
 *  CalmTest.scala
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
import de.sciss.fscape.{Graph, graph}

object CalmTest {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val tempIn    = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/render_test/collat_7a510609-outR-%d.png")
    val tempOut   = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/calm_test/collat_7a510609-outRC-%d.png")
    val indicesV  = 1 to 8440
    val width     = 2160
    val height    = 2160
    val frameSize = width * height
    val g = Graph {
      import graph._
      def frameIndices  = ValueIntSeq(indicesV: _*) // .take(indicesV.size)
      val imgIn     = ImageFileSeqIn(tempIn, numChannels = 1, indices = frameIndices)
      val slid      = Sliding(imgIn, size = frameSize * 3, step = frameSize)
      val wMax      = RunningWindowMax(slid, size = frameSize, trig = Metro(frameSize * 3))
      val sigOut    = ResizeWindow(wMax, size = frameSize * 3, start = frameSize * 2)
      ImageFileSeqOut(tempOut, ImageFile.Spec(ImageFile.Type.PNG, width = width, height = height, numChannels = 1),
        indices = frameIndices, in = sigOut)
      Progress(Frames(sigOut) / (frameSize.toLong * indicesV.size), Metro(width))
    }

    Render.runGraphOnConsole(g, blockSize = width, seed = 1234)
  }
}
