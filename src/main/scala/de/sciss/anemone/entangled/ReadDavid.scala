/*
 *  ReadDavid.scala
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
import de.sciss.neuralgas.ComputeGNG

object ReadDavid {
  def main(args: Array[String]): Unit = {
    Neural.parseConfig(args).fold(sys.exit(1))(run)
  }

  def run(config: Neural.Config): Unit = {
//    val baseDir   = {
//      val tmp = file("/data") / "projects"
//      val d   = if (tmp.isDirectory) tmp else userHome / "Documents" / "projects"
//      d / "Anemone" / "Verschraenkte-Systeme-146-219"
//    }
    val fIn       = config.minGNGIn.get //  baseDir / "out_ngn_32_3.bin"
    val tempOut   = config.imgOutTemp   //  baseDir / "out_david" / "david_ngn_32_3-%d.png"
    tempOut.parent.mkdirs()
    val c         = new ComputeGNG
    c.panelWidth  = config.widthOut
    c.panelHeight = config.heightOut
    var lastFrame = -1
    try {
      Neural.readMinGNG(fIn, c, cookie = 0x00010002, bipolar = true)(numFrames => println(s"numFrames = $numFrames")) {
        frameIdx =>

        lastFrame = frameIdx
        val fImgOut = Neural.formatTemplate(tempOut, frameIdx + 1)
        if (!fImgOut.exists()) {
          Neural.renderImage(config, c, fImgOut, quiet = true)
        }
//        val cc = c
//        if (cc.nNodes > 0) {
//          println(s"frameIdx = $frameIdx, nNodes = ${cc.nNodes}, nEdges = ${cc.nEdges}")
//        }
      }
    } finally {
      println(s"Received ${lastFrame + 1} frames.")
    }
  }
}
