/*
 *  Neural.scala
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

import java.awt.geom.Line2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints}
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.neuralgas.{ComputeGNG, ImagePD, PD}
import de.sciss.{kollflitz, neuralgas, numbers}
import javax.imageio.ImageIO

object Neural {
  def formatTemplate(temp: File, n: Int): File =
    temp.replaceName(temp.name.format(n))

  case class Config(imgInTemp     : File    = file("in-%d.png"),
                    imgOutTemp    : File    = file("out-%d.png"),
                    gngOutTemp    : File    = null,
                    startInIdx    : Int     = 1,
                    endInIdx      : Int     = -1,
                    frameStep     : Int     = 10,
                    holdFirst     : Int     = 0,
                    invertPD      : Boolean = false,
                    invertImgOut  : Boolean = false,
                    strokeWidth   : Double  = 2.0,
                    rngSeed       : Int     = 0xBEE,
                    maxNodes      : Int     = 4000,
                    gngStepSize   : Int     = 27,
                    gngLambda     : Int     = 27,
                    gngEdgeAge    : Int     = 108,
                    gngEpsilon    : Double  = 0.05,
                    gngEpsilon2   : Double  = 1.0e-4,
                    gngAlpha      : Double  = 0.2,
                    gngBeta       : Double  = 5.0e-6,
                    gngUtility    : Double  = 18.0,
                    widthOut      : Int     = 0,
                    heightOut     : Int     = 0
                   ) {

    def formatImgIn (frame: Int): File = formatTemplate(imgInTemp , frame)
    def formatImgOut(frame: Int): File = formatTemplate(imgOutTemp, frame)
    def formatGNGOut(frame: Int): File = formatTemplate(gngOutTemp, frame)
  }

  final val GNG_COOKIE = 0x474E4701   // "GNG\1"

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
          val c1 = c.copy(imgOutTemp = f)
          if (c.gngOutTemp == null) c1.copy(gngOutTemp = f.replaceExt("gng")) else c1
        }

      opt[File]('g', "gng-output")
        .text ("GNG data output file template, should end in '.gng', and %d would represent frame number.")
        .action { (f, c) => c.copy(gngOutTemp = f) }

      opt[Int] ("start")
        .text (s"Image input start index (default ${default.startInIdx})")
        .action { (v, c) => c.copy(startInIdx = v) }

      opt[Int] ("end")
        .text ("Image input end index (inclusive)")
        .action { (v, c) => c.copy(endInIdx = v) }

      opt[Int] ('f', "frame-step")
        .text (s"Frame step or output frames per input frame (default ${default.frameStep})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(frameStep = v) }

      opt[Int] ('l', "hold-first")
        .text (s"Number of input frames to 'hold' while building up the GNG (default ${default.holdFirst})")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(holdFirst = v) }

      opt[Unit] ("invert-pd")
        .text ("Invert gray scale probabilities.")
        .action { (_, c) => c.copy(invertPD = true) }

      opt[Unit] ("invert-output")
        .text ("Invert image output.")
        .action { (_, c) => c.copy(invertImgOut = true) }

      opt[Double] ("stroke")
        .text (s"Stroke width in pixels (default ${default.strokeWidth})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(strokeWidth = v) }

      opt[Int] ("seed")
        .text (s"Random number generator seed (default ${default.rngSeed})")
        .action { (v, c) => c.copy(rngSeed = v) }

      opt[Int] ('n', "max-nodes")
        .text (s"Maximum number of nodes (default: ${default.maxNodes})")
        .validate(i => if (i >= 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(maxNodes = v) }

//      opt[Int] ("decim")
//        .text (s"Pixel decimation to determine maximum number of nodes (default ${default.maxNodesDecim})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(maxNodesDecim = v) }

      opt[Int] ("gng-step")
        .text (s"GNG step size (default ${default.gngStepSize})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngStepSize = v) }

      opt[Int] ("lambda")
        .text (s"GNG lambda parameter (default ${default.gngLambda})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngLambda = v) }

      opt[Int] ("edge-age")
        .text (s"GNG maximum edge age (default ${default.gngEdgeAge})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEdgeAge = v) }

      opt[Double] ("eps")
        .text (s"GNG epsilon parameter (default ${default.gngEpsilon})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEpsilon = v) }

      opt[Double] ("eps2")
        .text (s"GNG epsilon 2 parameter (default ${default.gngEpsilon2})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEpsilon2 = v) }

      opt[Double] ("alpha")
        .text (s"GNG alpha parameter (default ${default.gngAlpha})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngAlpha = v) }

      opt[Double] ("beta")
        .text (s"GNG beta parameter (default ${default.gngBeta})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngBeta = v) }

      opt[Double] ("utility")
        .text (s"GNG-U utility parameter (default ${default.gngUtility})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngUtility = v) }

//      opt[Int] ('t', "interim")
//        .text (s"Interim file output, every n steps, or zero to turn off (default ${default.interim})")
//        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
//        .action { (v, c) => c.copy(interim = v) }
//
//      opt[Unit] ("interim-images")
//        .text ("Generate images for interim files.")
//        .action { (_, c) => c.copy(interimImages = true) }

      opt[Int] ('w', "width")
        .text (s"Rendering output width in pixels, or zero to match input.")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(widthOut = v) }

      opt[Int] ('h', "height")
        .text (s"Rendering output height in pixels, or zero to match input.")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(heightOut = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      run(config)
    }
  }

  case class Edge(from: Int, to: Int)

  def renderImage(config: Config, c: ComputeGNG, fImgOut: File, quiet: Boolean = false): Unit = {
    val wIn     = c.panelWidth
    val hIn     = c.panelHeight
    val wOut    = if (config.widthOut  == 0) wIn else config.widthOut
    val hOut    = if (config.heightOut == 0) hIn else config.heightOut
    val sx      = wOut.toDouble / wIn
    val sy      = hOut.toDouble / hIn
    val img     = new BufferedImage(wOut, hOut, BufferedImage.TYPE_BYTE_GRAY)
    val g       = img.createGraphics()
    val colrBg  = if (config.invertImgOut) Color.black else Color.white
    val colrFg  = if (config.invertImgOut) Color.white else Color.black
    g.setColor(colrBg)
    g.fillRect(0, 0, wOut, hOut)
    if (sx != 1.0 || sy != 1.0) g.scale(sx, sy)
    g.setColor(colrFg)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON   )
    g.setRenderingHint(RenderingHints.KEY_RENDERING     , RenderingHints.VALUE_RENDER_QUALITY )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE    )
    g.setStroke(new BasicStroke(config.strokeWidth.toFloat))
    val ln = new Line2D.Double()
    var i = 0
    while (i < c.nEdges) {
      val e = c.edges(i)
      val nFrom = c.nodes(e.from)
      val nTo   = c.nodes(e.to  )
      ln.setLine(nFrom.x, nFrom.y, nTo.x, nTo.y)
      g.draw(ln)
      i += 1
    }
    g.dispose()
    val fmt   = if (fImgOut.extL == "png") "png" else "jpg"
    ImageIO.write(img, fmt, fImgOut)
    if (!quiet) println(s"Wrote ${fImgOut.name}")
  }

  case class Point2D(x: Float, y: Float)

  case class ResGNG(surfaceWidthPx: Int, surfaceHeightPx: Int, nodes: Vec[Point2D],
                    edges: Vec[Edge])

  def readGNG(fGNG: File): ResGNG = {
    val fis = new FileInputStream(fGNG)
    try {
      val dis = new DataInputStream(fis)
      require (dis.readInt() == GNG_COOKIE)
      val surfaceWidthPx  = dis.readShort()
      val surfaceHeightPx = dis.readShort()
      val nNodes = dis.readInt()
      val nodes = Vector.fill(nNodes) {
        val x = dis.readFloat()
        val y = dis.readFloat()
        Point2D(x, y)
      }
      val nEdges = dis.readInt()
      val edges = Vector.fill(nEdges) {
        val from = dis.readInt()
        val to   = dis.readInt()
        Edge(from, to)
      }
      ResGNG(surfaceWidthPx, surfaceHeightPx, nodes, edges)

    } finally {
      fis.close()
    }
  }

  def run(config: Config): Unit = {
    import config._

    val c         = new ComputeGNG
    c.maxNodes    = maxNodes
    c.stepSize    = gngStepSize
    c.algorithm   = neuralgas.Algorithm.GNGU
    c.lambdaGNG   = gngLambda
    c.maxEdgeAge  = gngEdgeAge
    c.epsilonGNG  = gngEpsilon  .toFloat
    c.epsilonGNG2 = gngEpsilon2 .toFloat
    c.alphaGNG    = gngAlpha    .toFloat
    c.setBetaGNG(gngBeta.toFloat)
    c.noNewNodesGNGB = false
    c.GNG_U_B     = true
    c.utilityGNG  = gngUtility  .toFloat
    c.autoStopB   = false
    c.reset()
    val rnd       = c.getRNG
    rnd.setSeed(rngSeed)
    c.addNode(null)
    c.addNode(null)

    var imgFloorIdx             = -1
    var imgCeilIdx              = -1
    var imgFloor: BufferedImage = null
    var imgCeil : BufferedImage = null
    var pdFloor : PD            = null
    var pdCeil  : PD            = null

    val res           = new ComputeGNG.Result

    require (formatImgIn(startInIdx).isFile)
    require (formatImgIn (0) != formatImgIn (1), "Input  image template does not specify frame index")
    require (formatImgOut(0) != formatImgOut(1), "Output image template does not specify frame index")
    require (formatGNGOut(0) != formatGNGOut(1), "Output GNG   template does not specify frame index")
    val endInIdx1       = if (endInIdx >= 0) endInIdx else {
      val numImages = Iterator.from(startInIdx).indexWhere { idx =>
        val f = formatImgIn(idx)
        !f.isFile
      }
      startInIdx + numImages - 1
    }
    val frameInIndices0 = Vector.fill(holdFirst)(startInIdx) ++ (startInIdx to endInIdx1) :+ endInIdx1
    val frameInIndices  = if (holdFirst == 0) frameInIndices0 else Vector.fill(holdFirst)(startInIdx) ++ frameInIndices0
    var frameOutIdx     = 1
    val numOutFrames    = (frameInIndices.size - 1) * frameStep
    println(s"numOutFrames = $numOutFrames")

    var lastProgress  = 0
    println("_" * 100)

    import kollflitz.Ops._
    frameInIndices.foreachPair { (frameInFloor, frameInCeil) =>
      if (imgFloorIdx != frameInFloor) {
        imgFloorIdx = frameInFloor
        if (imgFloorIdx == imgCeilIdx ) {
          imgFloor    = imgCeil
          pdFloor     = pdCeil
        } else {
          val imgInF  = formatImgIn(imgFloorIdx)
          imgFloor    = ImageIO.read(imgInF)
          pdFloor     = new ImagePD(imgFloor, invertPD)
        }
      }
      if (imgCeilIdx != frameInCeil) {
        imgCeilIdx  = frameInCeil
        if (imgCeilIdx == imgFloorIdx) {
          imgCeil     = imgFloor
          pdCeil      = pdFloor
        } else {
          val imgInF  = formatImgIn(imgCeilIdx)
          imgCeil     = ImageIO.read(imgInF)
          pdCeil      = new ImagePD(imgCeil, invertPD)
        }
      }

      for (fStep <- 0 until frameStep) {
        import numbers.Implicits._
        val weight    = fStep.linlin(0, frameStep, 0, 1)
        val isFloor   = rnd.nextDouble() >= weight
        c.pd          = if (isFloor) pdFloor  else pdCeil
        val img       = if (isFloor) imgFloor else imgCeil
        val w         = img.getWidth
        val h         = img.getHeight
        c.panelWidth  = w
        c.panelHeight = h

        c.learn(res)
        val fGNG      = formatGNGOut(frameOutIdx)
        val fImgOut   = formatImgOut(frameOutIdx)
        writeGNG(c, fGNG)
        renderImage(config, c = c, fImgOut = fImgOut, quiet = true)

        val progress: Int = (frameOutIdx * 100) / numOutFrames
        if (lastProgress < progress) {
          while (lastProgress < progress) {
            print('#')
            lastProgress += 1
          }
        }

        frameOutIdx += 1
      }
    }
  }

  def writeGNG(c: ComputeGNG, fOut: File): Unit = {
    val fos = new FileOutputStream(fOut)
    try {
      val dos = new DataOutputStream(fos)
      dos writeInt      GNG_COOKIE
      dos writeInt      c.algorithm.ordinal()
      dos writeFloat    c.alphaGNG
      dos writeBoolean  c.autoStopB
      dos writeFloat    c.getBetaGNG
      dos writeFloat    c.delEdge_f
      dos writeFloat    c.delEdge_i
      dos writeFloat    c.e_f
      dos writeFloat    c.e_i
      dos writeFloat    c.epsilon
      dos writeFloat    c.epsilonGNG
      dos writeFloat    c.epsilonGNG2
      dos writeFloat    c.errorBestLBG_U
      dos writeBoolean  c.fineTuningB
//      c.fineTuningS
      dos writeBoolean  c.GNG_U_B
      dos writeInt      c.gridWidth
      dos writeInt      c.gridHeight
      dos writeFloat    c.l_f
      dos writeFloat    c.l_i
      dos writeInt      c.lambdaGNG
      dos writeBoolean  c.LBG_U_B
      dos writeInt      c.maxEdgeAge
      dos writeInt      c.maxNodes
      dos writeInt      c.maxYGG
      dos writeBoolean  c.nNodesChangedB
      dos writeBoolean  c.noNewNodesGGB
      dos writeBoolean  c.noNewNodesGNGB
      dos writeInt      c.numDiscreteSignals
      dos writeInt      c.numSignals
      dos writeInt      c.panelWidth
      dos writeInt      c.panelHeight
      dos writeBoolean  c.readyLBG_B
      dos writeBoolean  c.rndInitB
      dos writeFloat    c.sigma
      dos writeFloat    c.sigma_f
      dos writeFloat    c.sigma_i
      dos writeFloat    c.SignalX
      dos writeFloat    c.SignalY
      dos writeInt      c.stepSize
      dos writeFloat    c.t_max
      dos writeBoolean  c.torusGGB
      dos writeBoolean  c.torusSOMB
      dos writeFloat    c.utilityGNG
      dos writeFloat    c.valueGraph
      dos writeBoolean  c.variableB

      dos.writeInt(c.nNodes)
      var i = 0
      while (i < c.nNodes) {
        val n = c.nodes(i)
        dos.writeFloat(n.x)
        dos.writeFloat(n.y)
        i += 1
      }
      dos.writeInt(c.nEdges)
      i = 0
      while (i < c.nEdges) {
        val e = c.edges(i)
        dos.writeInt(e.from)
        dos.writeInt(e.to  )
        i += 1
      }

    } finally {
      fos.close()
    }
  }
}
