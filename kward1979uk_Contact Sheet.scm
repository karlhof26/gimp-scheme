; Created by kward1979uk
;
;----------------------------------------------------------------------------------------------------------
; License: GPLv3
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;----------------------------------------------------------------------------------------------------------
; Modification History:
; Updated for Gimp-2.10.20
; ------------------------------------------------------------------------------------------------------------------

(define (contact load opt maxh maxw imagew imageh)
 (let* (
        (a 0)
        (height 0)
        (width 0)
        (scale 0)
        (indraw 0)
        (float 0)
        (filelist 0)
        (newimage 0)
        (newlayer 0)
        (framelayer 0)
        (image 0)
    )
    ;(createnew maxh maxw imagew imageh) ; expanded below
    
    (set! newimage (gimp-image-new (* imagew (+ maxw 2)) (* imageh (+ maxh 2)) 0))
    (set! newlayer (gimp-layer-new (car newimage) (* imagew (+ maxw 2)) (* imageh (+ maxh 2)) 1 "newlayer" 100 0))
    (gimp-image-add-layer (car newimage) (car newlayer) 0)
    (gimp-drawable-fill (car newlayer) 3)   
    
    (set! framelayer (gimp-layer-new (car newimage) (* imagew (+ maxw 2)) (* imageh (+ maxh 2)) 1 "newlayer" 100 0))
    (gimp-image-insert-layer (car newimage) (car framelayer) 0 0)
    (gimp-drawable-fill (car framelayer) 3)
    (gimp-display-new (car newimage))
    
    
    (set! a
        (cond 
            (( equal? opt 0 ) ".jpg" )
            (( equal? opt 1 ) ".bmp" )
            (( equal? opt 2 ) ".xcf" )
            (( equal? opt 3 ) ".png" )
            (( equal? opt 4 ) ".gif" )
            (( equal? opt 5 ) ".gbr" )
        )
    )
    
    
    (let* (
            (num-files (car (file-glob (string-append load "\\*" a)  1)))
            (filelist (cadr (file-glob (string-append load "\\*" a)  1)))
            (x 1)
            (y 1)
            (xy 1)
          )
        ;(while filelist
        (while (> num-files 0)
            (let* (
                    (loadfile (car filelist))
                    (image (car (gimp-file-load RUN-NONINTERACTIVE loadfile loadfile)))
                  )
                (set! height (car (gimp-image-height image)))
                (set! width (car (gimp-image-width image)))
                (set! scale (/ maxw width))
                (if ( < maxh (* height scale))
                    (set! scale (/ maxh height))
                )
                ;(gimp-message "line82")
                (gimp-image-scale image (* width scale) (* height scale) )
                (set! indraw (car (gimp-image-get-active-drawable image)))
                (gimp-edit-copy indraw)
                
                ;(gimp-rect-select (car newimage) (* (- x 1)(+ maxw 2)) (* (- y 1)(+ maxh 2)) (+ maxw 2) (+ maxh 2) 2 0 0)
                (gimp-context-set-feather FALSE)
                (gimp-image-select-rectangle (car newimage) CHANNEL-OP-REPLACE (* (- x 1)(+ maxw 2)) (* (- y 1)(+ maxh 2)) (+ maxw 2) (+ maxh 2))
                
                (set! float (car (gimp-edit-paste (car newlayer) 0)))
                (gimp-floating-sel-anchor float)
                ;(gimp-rect-select (car newimage) (* (- x 1)(+ maxw 2)) (* (- y 1)(+ maxh 2)) (+ maxw 2) (+ maxh 2) 2 0 0)
                (gimp-image-select-rectangle (car newimage) CHANNEL-OP-REPLACE (* (- x 1)(+ maxw 2)) (* (- y 1)(+ maxh 2)) (+ maxw 2) (+ maxh 2))
                
                (gimp-edit-bucket-fill (car framelayer) 0 0 100 0 0 0 0)
                (gimp-selection-shrink (car newimage) 1)
                (gimp-edit-cut (car framelayer))
                (set! x (+ x 1))
                (set! xy (+ xy 1))
                
                (if (> x imagew)(set! y (+ y 1)))
                (if (> x imagew)(set! x 1))
                
                (if (> xy (* imagew imageh)) (createnew maxh maxw imagew imageh))
                (if (> xy (* imagew imageh)) (set! y 1))
                (if (> xy (* imagew imageh)) (set! xy 1))
                (gimp-image-delete image)
                (set! filelist (cdr filelist))
                (set! num-files (- num-files 1))
                
                (gimp-displays-flush)
                ;(gimp-message "line108")
            )
        )
    )
    (gimp-message "Good finish OK")
  )
)

(script-fu-register  "contact"
        "<Image>/Script-Fu2/Misc/Contact sheet..."
        "Creates a contact sheet of images in a folder. \nfile:kward1979uk_ContactSheet.scm"
        "Karl Ward"
        "Karl Ward"
        "JAN 2007"
        ""
        SF-DIRNAME      "Load from"     ""
        SF-OPTION       "File Type"     '("jpg" "bmp""xcf""png""gif""gbr")
        SF-ADJUSTMENT   "Max Height"    '(100 1 400 1 2 0 1)
        SF-ADJUSTMENT   "Max Width"     '(100 1 400 1 2 0 1)
        SF-ADJUSTMENT   "Images Wide"   '(5 1 100 1 2 0 1)
        SF-ADJUSTMENT   "Images high"   '(5 1 100 1 2 0 1)
        
)

;end of script 