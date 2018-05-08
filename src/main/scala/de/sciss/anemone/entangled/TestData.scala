package de.sciss.anemone.entangled

import de.sciss.file._
import de.sciss.neuralgas.ComputeGNG

object TestData {
  def main(args: Array[String]): Unit = {
    testEnvelope()
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

  def baseDir: File = {
    val baseDir0 = file("/data/projects")
    val baseDir1 = if (baseDir0.isDirectory) baseDir0 else userHome / "Documents" / "projects"
    baseDir1 / "Anemone" / "Verschraenkte-Systeme-146-219"
  }

  def testEnvelope(): Unit = {
    val s = "8640;25000,8640,step;40050,2,lin"
    val e = Neural.Envelope.Read.reads(s)
    val range = 25000 to 40050 by 100
    for (frameIdx <- range) {
      val v = e.levelAt(frameIdx)
      println(s"[$frameIdx]: level = $v")
    }
  }

  def nodeDevelopment(): Unit = {
    val c = new ComputeGNG
    val tempIn = baseDir / "gng_test" / "collat_7a510609-out-%d.gng"
    val range = 25000 to 40050
    for (frameIdx <- range) {
      val fIn = Neural.formatTemplate(tempIn, frameIdx)
      Neural.readGNG(c, fIn)
      println(s"[$frameIdx]: nNodes = ${c.nNodes}")
    }
  }

  def readNumEdges(): Unit = {
    val c = new ComputeGNG
    val fIn = file("/data/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-25337.gng")
    Neural.readGNG(c, fIn)
    println(s"nEdges = ${c.nEdges}")
  }
}
