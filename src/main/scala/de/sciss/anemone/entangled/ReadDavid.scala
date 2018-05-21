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
    run()
  }

  def run(): Unit = {
    val fIn       = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/out_ngn_32_cov.bin")
    val c         = new ComputeGNG
    c.panelWidth  = 2160
    c.panelHeight = 2160
    var lastFrame = -1
    try {
      Neural.readMinGNG(fIn, c, cookie = false, bipolar = true)(numFrames => println(s"numFrames = $numFrames")) {
        frameIdx =>

        val cc = c
        lastFrame = frameIdx
        if (cc.nNodes > 0) {
          println(s"frameIdx = $frameIdx, nNodes = ${cc.nNodes}, nEdges = ${cc.nEdges}")
        }
      }
    } finally {
      println(s"Received ${lastFrame + 1} frames.")
    }
  }
}
