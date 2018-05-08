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

Old neural settings:

    -i
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/prothese_7a510609_image_out/collat_7a510609-%d.png
    -o
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/out_test/collat_7a510609-out-%d.png
    -g
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-%d.gng
    --hold-first
    25
    --frame-step
    25
    --invert-pd
    --invert-output
    -w
    2160
    -h
    2160
    --max-nodes
    8640;25000,8640,step;40050,2,lin
    --beta
    5.0e-6;20025,5.0e-6,step;40050,5.0e-4,exp
    --utility
    18;20800,18,step;40050,9,lin
    --alpha
    0.2;22000,0.2,step;40050,0.4,lin
    --edge-age
    108;24600,108,step;40050,54,lin
    --eps
    0.05;25200,0.05,step;40050,0.075,exp
    --eps2
    1.0e-4;25400,0.05,step;40050,0.01,exp

New neural settings:

    sbt 'runMain de.sciss.anemone.entangled.Neural \
     -i /home/hhrutz/Documents/devel/Miniaturen15/collateral_vid/image_out/collat_7a510609-%d.png \
     -o /home/hhrutz/Documents/projects/Anemone/Verschraenkte-Systeme-146-219/out_test/collat_7a510609-out-%d.png \
     -g /home/hhrutz/Documents/projects/Anemone/Verschraenkte-Systeme-146-219/gng_test/collat_7a510609-out-%d.gng \
     --hold-first 25 --frame-step 25 --invert-pd --invert-output -w 2160 -h 2160 \
     --max-nodes 8640;22050,8640,step;40050,2,lin --beta 5.0e-6;20025,5.0e-6,step;40050,5.0e-4,exp \
     --utility 18;20800,18,step;40050,9,lin --alpha 0.2;22000,0.2,step;40050,0.4,lin \
     --edge-age 108;24600,108,step;40050,54,lin --eps 0.05;25200,0.05,step;40050,0.075,exp \
     --eps2 1.0e-4;25400,0.05,step;40050,0.01,exp --skip 20020'

Render settings:

    -s
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/prothese_7a510609_image_out/collat_7a510609-%d.png
    -i
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/out_test/collat_7a510609-out-%d.png
    -o
    /data/projects/Anemone/Verschraenkte-Systeme-146-219/render_test/collat_7a510609-outR-%d.png
    --hold-first
    25
    --frame-step
    25
