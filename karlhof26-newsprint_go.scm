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


(define (script-fu-newsprint-go image 
                    group
                    option-sel
                    cell-size
                    color-space
                    black-pullout
                    grey-ang
                    grey-spotfn
                    red-ang
                    red-spotfn
                    green-ang
                    green-spotfn
                    blue-ang
                    blue-spotfn
                    oversample-times
                    
                    NoExamples
                    
                    )
  (gimp-image-undo-group-start image)
  (gimp-context-push)
  (let* (
           
            (notworking-pressprint-layer (car (gimp-layer-new image 
                                     (car (gimp-drawable-width group))
                                     (car (gimp-drawable-height group))
                                     (car (gimp-drawable-type group))
                                     "Pressprint layer"
                                     100
                                     LAYER-MODE-NORMAL
                                     )))
            (pressprint-layer (car (gimp-layer-copy group TRUE)))
            (try2-layer (car (gimp-layer-copy group TRUE)))
            (try3-layer (car (gimp-layer-copy group TRUE)))
            (try4-layer (car (gimp-layer-copy group TRUE)))
            (your-layer (car (gimp-layer-copy group TRUE)))
            
            ;(x-offset (car (gimp-drawable-offsets group)))
            ;(y-offset (cadr (gimp-drawable-offsets group))) 
            
            ;
            ;(splitVar 1)
            ;(aliasVar 1)
            ;(averagingVar 1)
          )
          
      
      (gimp-context-set-default-colors)
      
      (if (= NoExamples FALSE)
        (begin
            (gimp-image-insert-layer image pressprint-layer 0 -1)
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
      
      
      
      (if (= NoExamples FALSE)
        (begin
            (plug-in-newsprint 1 image pressprint-layer 4 2 5 120.0 0 45.0 1 45.1 1 45.2 1 15)
            (plug-in-newsprint 1 image try2-layer 12 1 10 120.0 0 45.0 1 45.1 1 45.2 1 15)
            (plug-in-newsprint 1 image try3-layer 15 3 10 310.0 0 45.0 1 60.1 1 45.2 1 15)
            (plug-in-newsprint 1 image try4-layer 24 0 10 45.0 0 45.0 1 120.1 2 45.2 3 15)
        )
      )
      
      ;(plug-in-newsprint 1 image your-layer cell-size color-space black-pullout grey-ang grey-spotfn red-ang red-spotfn green-ang green-spotfn blue-ang blue-spotfn oversample-times)
      
      (cond
            ((= option-sel 0)
                 (plug-in-newsprint 1 image your-layer cell-size color-space black-pullout grey-ang grey-spotfn red-ang red-spotfn green-ang green-spotfn blue-ang blue-spotfn oversample-times)
            )
            ((= option-sel 1)
                (plug-in-newsprint 1 image your-layer 4 1 5 90.0 3 180.0 1 45.0 2 45.0 1 15)
                
            )
            ((= option-sel 2)
                (plug-in-newsprint 1 image your-layer 8 1 0 45.0 0 45.0 1 45.1 1 45.2 1 15)
                
            )
            ((= option-sel 3)
                (plug-in-newsprint 1 image your-layer 5 3 10 310.0 0 45.0 11 60.1 1 45.2 1 15)
                
            )
      )
      
      
  )
  (gimp-context-pop)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-newsprint-go"
  "Pressprint Newsprint"
  "Creates a newspaper print of the image. Pressprint. \nfile:karlhof26-pressprint-go.scm"
  "Karl Hofmeyr"
  "karlhof26"
  "February 2021"
  "*"
  SF-IMAGE    "Image"           0
  SF-DRAWABLE "Drawable"        0
  SF-OPTION   "Select mode"                 '("Customise" "Preselect 1" "Preselect 2" "Preselect 3")
  SF-ADJUSTMENT "Cell size (for both X and Y sizes)"    '(12 0 1500 1 10 0 0)
  SF-OPTION "Color Space"                   '("Greyscale" "RGB" "CMYK" "Luminance")
  SF-ADJUSTMENT "Black pullout %"           '(30 0 1000 1 10 1 0)
  SF-ADJUSTMENT "Grey - Angle 1"            '(25 0 360 1 10 1 0)
  SF-OPTION "Grey Dither type"              '("Dots" "Lines" "Diamonds" "Euclidean-Dot" "PS-Diamonds")
  SF-ADJUSTMENT "Red - Angle 2"             '(45 0 360 1 10 1 0)
  SF-OPTION "Red Dither type"               '("Dots" "Lines" "Diamonds" "Euclidean-Dot" "PS-Diamonds")
  SF-ADJUSTMENT "Green - Angle 3"           '(135 0 360 1 10 1 0)
  SF-OPTION "Green Dither type"             '("Dots" "Lines" "Diamonds" "Euclidean-Dot" "PS-Diamonds")
  SF-ADJUSTMENT "Blue - Angle 4"            '(225 0 360 1 10 1 0)
  SF-OPTION "Blue Dither type"              '("Dots" "Lines" "Diamonds" "Euclidean-Dot" "PS-Diamonds")
  SF-ADJUSTMENT "Times to oversample"       '(15 1 127 1 10 1 0)
  
  SF-TOGGLE "No Examples"                   FALSE
  
)

(script-fu-menu-register "script-fu-newsprint-go"
  "<Toolbox>/Script-Fu/Photo/Graphic")

;end of script