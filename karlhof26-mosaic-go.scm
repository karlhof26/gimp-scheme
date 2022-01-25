; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;   
;   Windows 10
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   C:\Users\YOUR-NAME\.gimp-2.10\scripts
;   C:\Users\YOUR-NAME\AppData\Roaming\GIMP\2.10\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;   Linux
;   /home/yourname/.gimp-2.8/scripts   
;   or
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
;   
;==============================================================


(define (script-fu-mosaic-go image group 
                option-sel
                cell-size Xsize
                Hsize
                CSpacing
                CNeatness
                CAllowSplit CLightDirection CColorVariation CAliasing CAveraging CShape CSurface CColoring NoExamples)
  (gimp-image-undo-group-start image)
  (gimp-context-push)
  (let* (
            (x-offset (car (gimp-drawable-offsets group)))
            (y-offset (cadr (gimp-drawable-offsets group))) 
            (mosaic-layer (car (gimp-layer-new image 
                                     (car (gimp-drawable-width group))
                                     (car (gimp-drawable-height group))
                                     (car (gimp-drawable-type group))
                                     "Black Mosaic layer"
                                     100
                                     LAYER-MODE-NORMAL
                                     )))
            (try2-layer (car (gimp-layer-copy group TRUE)))
            (try3-layer (car (gimp-layer-copy group TRUE)))
            (try4-layer (car (gimp-layer-copy group TRUE)))
            (your-layer (car (gimp-layer-copy group TRUE)))
            
            (splitVar 1)
            (aliasVar 1)
            (averagingVar 1)
          )
      (gimp-context-set-default-colors) 
      (if (= NoExamples FALSE)
        (begin
            (gimp-image-insert-layer image mosaic-layer 0 -1)
            (gimp-image-insert-layer image try2-layer 0 -1)
            (gimp-image-insert-layer image try3-layer 0 -1)
            (gimp-image-insert-layer image try4-layer 0 -1)
            
            (gimp-item-set-name try4-layer "Example4 layer")
            (gimp-item-set-name try3-layer "Example3 layer")
            (gimp-item-set-name try2-layer "Example2 layer")
        )
      )
      (gimp-image-insert-layer image your-layer 0 -1)
      
      (gimp-item-set-name your-layer "Your layer")
      
      (if (= CAllowSplit TRUE)
            (set! splitVar 1)
            (set! splitVar 0)
      )
      (if (= CAliasing TRUE)
            (set! aliasVar 1)
            (set! aliasVar 0)
      )
      (if (= CAveraging TRUE)
            (set! averagingVar 1)
            (set! averagingVar 0)
      )
      
      (if (= NoExamples FALSE)
        (begin
            (plug-in-mosaic 1 image mosaic-layer 60 60 1 0 TRUE 175.2  0.3 1 1 1 0 1)
            (plug-in-mosaic 1 image try4-layer 40 40 2 0.5 TRUE 175.2  0.8 0 0 1 0 0)
            
            (plug-in-mosaic 1 image try2-layer 40 40 1 0 TRUE 175.2 0.3 1 0 3 0 1)
            (plug-in-mosaic 1 image try3-layer 40 40 2 0.25 TRUE 175.2 0.8 1 1 3 0 0)
        )
      )
      
      (cond
            ((= option-sel 0)
                (plug-in-mosaic 1 image your-layer Xsize Hsize CSpacing CNeatness splitVar CLightDirection CColorVariation aliasVar averagingVar CShape CSurface CColoring)
            )
            ((= option-sel 1)
                (plug-in-mosaic 1 image your-layer 40 40 1 0 TRUE 175.2 0.7 1 0 3 0 1)
            )
            ((= option-sel 2)
                (plug-in-mosaic 1 image your-layer 30 40 1 0.25 TRUE 175.2 0.15 1 1 0 1 0)
            )
            ((= option-sel 3)
                (plug-in-mosaic 1 image your-layer Xsize Hsize CSpacing CNeatness splitVar 175.2 0.7 1 0 3 0 1)
            )
      )
  )
  (gimp-context-pop)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-mosaic-go"
  "Mosaic go"
  "Draws a mosaic over the image \nfile:karlhof26-mosaic-go.scm"
  "Karl Hofmeyr"
  "karlhof26"
  "February 2021"
  "*"
  SF-IMAGE    "Image"           0
  SF-DRAWABLE "Drawable"        0
  SF-OPTION   "Select mode"     '("Customise" "Preselect 1" "Preselect 2" "Customised Triangle")
  SF-ADJUSTMENT "Cell size (for both X and Y sizes)"    '(30 0 1000 1 10 0 0)
  SF-ADJUSTMENT "Tile size"     '(30 0 1000 1 10 0 0)
  SF-ADJUSTMENT "Height size"   '(30 0 1000 1 10 0 0)
  SF-ADJUSTMENT "Spacing"       '(1.0 0.1 1000.0 1 10 1 0)
  SF-ADJUSTMENT "Neatness"      '(0.5 0.0 1.0 0.01 0.1 2 0)
  SF-TOGGLE "Allow split"       TRUE
  SF-ADJUSTMENT "Light Direction"    '(175.2 0.0 359.9 1 10 1 0)
  SF-ADJUSTMENT "Color Variation"    '(0.5 0.0 1.0 0.01 0.1 2 0)
  SF-TOGGLE "Antialiasing Tick->Yes"        TRUE
  SF-TOGGLE "Color Averaging Tick->Yes"     TRUE
  SF-OPTION   "Shape"                       '("Squares" "Hexagons" "Octagons" "Triangles")
  SF-OPTION "Tile Surface"                  '("Smooth" "Rough")
  SF-OPTION "Tile colouring"                '("White-Black (left to right)" "Foreground-Background (FG-BG) colors")
  SF-TOGGLE "No Examples"                   FALSE
)

(script-fu-menu-register "script-fu-mosaic-go"
  "<Toolbox>/Script-Fu/Effects"
)

;end of script