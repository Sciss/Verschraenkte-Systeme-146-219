package de.sciss.anemone.verschraenkte

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, EventQueue, Font, Frame, Graphics, GraphicsEnvironment, Point}

import de.sciss.file._

object PlayVid {
  case class Config(videoFile: File = userHome / "Videos" / "verschraenkte-146-219.mp4",
                    disableEnergySaving: Boolean = true,
                    titleMessage: String = "Verschränkte Systeme 146:219"
                   )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Verschraenkte-PlayVid") {
      opt[File]('v', "video")
//        .required()
        .text("Video file to play")
        .action { (v, c) => c.copy(videoFile = v) }

      opt[String]('t', "title")
        .text("Video file to play")
        .action { (v, c) => c.copy(titleMessage = v) }

      opt[Unit] ("keep-energy")
        .text ("Do not turn off energy saving")
        .action   { (_, c) => c.copy(disableEnergySaving = false) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    if (config.disableEnergySaving) {
      import sys.process._
      Seq("xset", "s", "off").!
      Seq("xset", "-dpms").!
    }

    sys.addShutdownHook(killPlayer())

    EventQueue.invokeLater(() => openBlackWindow(config))
  }

  private var debugMessage = ""

  private var window = Option.empty[Frame]

//  private var process = Option.empty[sys.process.Process]

  def killPlayer(): Unit = {
    import sys.process._
    Seq("killall", "omxplayer.bin").!
  }

  def playVideo(config: Config): Unit = {
    import sys.process._
//    process.foreach(_.destroy())
    val p = Seq("omxplayer", /* "--no-osd", */ config.videoFile.path).! // run()
//    process = Some(p)
  }

  def openBlackWindow(config: Config): Unit = {
    val screen      = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    val screenConf  = screen.getDefaultConfiguration
    val fnt         = new Font(Font.SANS_SERIF, Font.BOLD, 36)
    val w = new Frame(null, screenConf) {
      override def paint(g: Graphics): Unit = {
        super.paint(g)
        val m = debugMessage
        if (!m.isEmpty) {
          g.setFont(fnt)
          val fm = g.getFontMetrics
          val tw = fm.stringWidth(debugMessage)
          g.setColor(Color.white)
          g.drawString(m, (getWidth - tw)/2, getHeight/2 - fm.getAscent)
        }
      }
    }
    w.setUndecorated  (true)
    //    w.setIgnoreRepaint(true)
    w.setBackground(Color.black) // new Color(config.background))
    w.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match {
//          case KeyEvent.VK_ESCAPE => control.foreach(_.shutdown())
          case KeyEvent.VK_Q  =>
            killPlayer()
            sys.exit()  // 'tschack!
          case KeyEvent.VK_T  =>
            debugMessage = if (debugMessage == config.titleMessage) "" else config.titleMessage
            w.repaint()
          case KeyEvent.VK_ENTER =>
            if (!debugMessage.isEmpty) {
              debugMessage = ""
              w.repaint()
            }
            playVideo(config)

          case _ =>
        }
      }
    })
    w.setSize(screenConf.getBounds.getSize)
    screen.setFullScreenWindow(w)
    w.requestFocus()

    // "hide" cursor
    val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val cursor = w.getToolkit.createCustomCursor(cursorImg, new Point(0, 0), "blank")
    w.setCursor(cursor)
    window = Some(w)
  }
}