package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.neuralgas.ComputeGNG

object TestData {
  def main(args: Array[String]): Unit = {
    writeTestTextData()
  }

  def writeTestTextData(): Unit = {
    val frameIdx = 0
    val fIn   = file(s"/data/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-${frameIdx + 1}.gng")
    val c     = new ComputeGNG
    val fOut  = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/test-min-first.txt")
    if (fOut.exists()) {
      println(s"File '$fOut' already exists. Not overwriting.")
      return
    }
    Neural.readGNG(c, fIn)
    Neural.writeMinGNGText(c, fOut)
  }

  def writeTestAllData(): Unit = {
    val numFrames = 20025
    val fOut = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/test-min-20k.bin")
    if (fOut.exists()) {
      println(s"File '$fOut' already exists. Not overwriting.")
      return
    }
    Neural.writeMinGNG(numFrames = numFrames, fOut = fOut) {
      (c, frameIdx) =>
        val fIn = file(s"/data/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-${frameIdx + 1}.gng")
        Neural.readGNG(c, fIn)
    }
  }

  def readNumEdges(): Unit = {
    val c = new ComputeGNG
    val fIn = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-25337.gng")
    Neural.readGNG(c, fIn)
    println(s"nEdges = ${c.nEdges}")
  }
}
