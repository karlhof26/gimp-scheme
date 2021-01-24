; Rasterize rel 0.02
; Created by Graechan
; Many thanks to Saul Goode for his assistance with this script
; Comments directed to http://gimpchat.com or http://gimpscripts.com 
;
; License: GPLv3
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
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
; Rel 0.02 - Changed script to call 'gimp-drawable-mask-intersect' 
;            so as to correctly determine if a selection is affecting the drawable
; Rel 0.03 - Updated to Gimp 2.10.22

(define (script-fu-rasterize-selection image
                                  drawable
                                  blocksize
                                  blurfactor
                                  keep-selection)

   (gimp-image-undo-group-start image)
 (let* (
            (original-selection 0)
            (original-sel FALSE)
      )   
    (if (and (= (car (gimp-drawable-mask-intersect drawable)) FALSE) (car (gimp-selection-is-empty image)) FALSE)
        (begin
            (gimp-selection-save image)
            (set! original-selection (car (gimp-image-get-active-drawable image)))	
            (gimp-channel-set-opacity original-selection 100)	
            (gimp-drawable-set-name original-selection "Original-selection")
            (gimp-image-set-active-layer image drawable)
            (set! original-sel TRUE)
            (gimp-selection-none image)
        )
    )
    
  (let* (
      (has-selection (car (gimp-drawable-mask-intersect drawable)))
      (select-bounds (cdr (gimp-drawable-mask-intersect drawable)))         
      (x1 (car select-bounds))
      (y1 (cadr select-bounds))
      (x2 (+ (caddr select-bounds) x1))
      (y2 (+ (cadddr select-bounds) y1))
      (xrun (- x1 (modulo x1 blocksize)))
      (yrun y1)
      (width (- x2 x1))
      (height (- y2 y1))
      (blur (* (/ blocksize 2) blurfactor))
      (saved-selection (car (gimp-selection-save image)))
      (xoff (car (gimp-drawable-offsets drawable)))
      (yoff (cadr (gimp-drawable-offsets drawable)))
      (draw-width (car (gimp-drawable-width drawable)))
      (draw-height (car (gimp-drawable-height drawable)))
      (sel (car (gimp-selection-is-empty image)))
      )
      
      ;;;;begin script
      (set! blocksize (max blocksize 1))
      (set! blur (max blur 1))
      (while (<= xrun x2 )
         (set! yrun (- y1 (modulo y1 blocksize)))
         (while (<= yrun y2)
                (if (= has-selection TRUE) (gimp-selection-load saved-selection))
                (if (= (car (gimp-selection-is-empty image)) TRUE) (gimp-rect-select image xoff yoff width height 2 0 0))
                (gimp-rect-select image (+ xrun xoff) (+ yrun yoff) blocksize blocksize 3 0 0)
                (if (= (car (gimp-selection-is-empty image)) FALSE) (plug-in-gauss-rle2 1 image drawable blur blur))
                (set! yrun (+ yrun blocksize))
        )
        (set! xrun (+ xrun blocksize))
        (gimp-displays-flush)
      )
      ;;;;end script
      (if (= original-sel TRUE) (gimp-selection-load original-selection) (gimp-selection-load saved-selection))
      (gimp-image-remove-channel image saved-selection)
      (if (= original-sel TRUE) (gimp-image-remove-channel image original-selection))
      (if (= keep-selection FALSE) (gimp-selection-none image))
      (gimp-displays-flush)
  )
 )
   (gimp-image-undo-group-end image)
)

(script-fu-register "script-fu-rasterize-selection" 
                    "Rasterize..."
                    "Can make the active Selection or Image look \"blocky\".  \nfile:Rasterize.scm" 
                    "Graechan"
                    "Graechan - http://gimpchat.com"
                    "Dec 2012"
                    "RGB* GRAY*"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
                    SF-ADJUSTMENT "Size of blocks" '(20 5 100 1 5 0 0)
                    SF-ADJUSTMENT "Blurfactor" '(2 1 10 1 5 0 0)
                    SF-TOGGLE     "Keep selection"    FALSE
                )

(script-fu-menu-register "script-fu-rasterize-selection" "<Image>/Script-Fu/Distorts")

;end of script