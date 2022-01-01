; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2008 elsamuko <elsamuko@web.de>
;
; Version 0.1 - Simulating the 2 color technicolor
; Version 0.2 - Some decomposing options 
;


(define (elsamuko-technicolor-2-color aimg adraw redpart greenpart cyanfill redfill yellowfill sharpen)
  (let* ((img          (car (gimp-item-get-image adraw)))
         (owidth       (car (gimp-image-width aimg)))
         (oheight      (car (gimp-image-height aimg)))
         (sharpenlayer (car (gimp-layer-copy adraw FALSE)))
         (redlayer     (car (gimp-layer-copy adraw FALSE)))
         (cyanlayer    (car (gimp-layer-copy adraw FALSE)))
         (yellowlayer  (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            1
                                            "Yellow" 
                                            30 ;opacity
                                            LAYER-MODE-OVERLAY-LEGACY)))
         ;decomposing filter colors, you may change these
         (red-R redpart)
         (red-G (/ (- 1 redpart) 2) )
         (red-B (/ (- 1 redpart) 2) )
         (cyan-R 0)
         (cyan-G greenpart)
         (cyan-B (/ (- 1 greenpart) 2))
         )
    
    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    (gimp-item-set-visible adraw FALSE)
    
    ;red and cyan filter
    (gimp-item-set-name cyanlayer "Cyan")
    (gimp-item-set-name redlayer "Red")
    
    (gimp-image-insert-layer img redlayer 0 -1)
    (gimp-image-insert-layer img cyanlayer 0 -1)
    
    (plug-in-colors-channel-mixer 1 img redlayer TRUE
                                  red-R red-G red-B ;R
                                  0 0 0 ;G
                                  0 0 0 ;B
                                  )
    (plug-in-colors-channel-mixer 1 img cyanlayer TRUE
                                  cyan-R cyan-G cyan-B ;R
                                  0 0 0 ;G
                                  0 0 0 ;B
                                  )
    
    ;colorize filter layers back
    (gimp-context-set-foreground cyanfill)
    (gimp-context-set-background redfill)
    
    (gimp-selection-all img)
    (gimp-edit-bucket-fill redlayer BUCKET-FILL-FG LAYER-MODE-SCREEN-LEGACY 100 0 FALSE 0 0)
    (gimp-edit-bucket-fill cyanlayer BUCKET-FILL-BG LAYER-MODE-SCREEN-LEGACY 100 0 FALSE 0 0)
    
    (gimp-layer-set-mode cyanlayer LAYER-MODE-MULTIPLY-LEGACY)
    
    ;add yellow layer
    (gimp-image-insert-layer img yellowlayer 0 -1)
    (gimp-context-set-foreground yellowfill)
    (gimp-edit-bucket-fill yellowlayer BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
    
    ;sharpness + contrast layer
    (if(= sharpen TRUE)
       (begin
         (gimp-image-insert-layer img sharpenlayer 0 -1)
         (gimp-drawable-desaturate sharpenlayer DESATURATE-LIGHTNESS)
         (plug-in-unsharp-mask 1 aimg sharpenlayer 5.0 1.5 0)
         (gimp-layer-set-mode sharpenlayer LAYER-MODE-OVERLAY-LEGACY)
         (gimp-layer-set-opacity sharpenlayer 40)
         )
       )
    
    ; tidy up
    (gimp-selection-none img)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gc) ; memory cleanup
    )
  )

(script-fu-register "elsamuko-technicolor-2-color"
                    "Technicolor 2 Color"
                    "Simulating Technicolor Film. \n
Requires a layer to be selected. \nfile:elsamuko-technicolor-2-color.scm"
                    "elsamuko <elsamuko@web.de> "
                    "elsamuko"
                    "09/09/08"
                    "*"
                    SF-IMAGE       "Input image"      0
                    SF-DRAWABLE    "Input drawable"   0
                    SF-ADJUSTMENT _"Red Part of Red Filter"    '(0.5 -2.0 2.0 0.1 0.2 1 0)
                    SF-ADJUSTMENT _"Green Part of Cyan Filter" '(0.5 -2.0 2.0 0.1 0.2 1 0)
                    SF-COLOR      _"Recomposing Cyan" '(0 255 255)
                    SF-COLOR      _"Recomposing Red"  '(255 0 0)
                    SF-COLOR      _"Additive Yellow"  '(255 255 0)
                    SF-TOGGLE     _"Sharpen"  TRUE
                    )

(script-fu-menu-register "elsamuko-technicolor-2-color" "<Image>/Script-Fu2/Color/")

;end of script