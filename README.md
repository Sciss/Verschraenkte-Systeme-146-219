# Verschränkte Systeme 146:219

This project contains my set-up for a particular _Anemone Actiniaria_ performance / piece.

All code here
is (C)opyright 2018 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](http://github.com/Sciss/AnemoneActiniaria/blob/master/LICENSE) v3+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.12.

## video rendering

    ffmpeg -i out_test/collat_7a510609-out-%d.png -r 25 -vf scale=1080:1080 out.mp4
