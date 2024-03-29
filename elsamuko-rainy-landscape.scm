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
; Copyright (C) 2010 elsamuko <elsamuko@web.de>
;

(define (elsamuko-rainy-landscape aimg adraw
                                  cast preserve
                                  yellow-hues red-hues)
  (let* (
         (img (car (gimp-item-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         
         (hue-layer     (car (gimp-layer-copy adraw FALSE)))
         (bw-layer      (car (gimp-layer-copy adraw FALSE)))
         (extract-layer (car (gimp-layer-copy adraw FALSE)))
         
         (cast-layer (car (gimp-layer-new img
                                          owidth 
                                          oheight
                                          1
                                          "Blue Cast" 
                                          40 
                                          LAYER-MODE-MULTIPLY)))
         (cast-layer-mask (car (gimp-layer-create-mask cast-layer ADD-MASK-WHITE)))
         (preserve-layer 0)
         (preserve-layer-mask 0)
         )
    
        ; init
        (gimp-context-push)
        (gimp-image-undo-group-start img)
        (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
            (gimp-image-convert-rgb img)
        )
        
        ;add hue layer
        (gimp-image-insert-layer img hue-layer 0 -1)
        (gimp-item-set-name hue-layer "Hue: Y->G, R->Y")
        
        ;create layer mask for the cast layer
        (gimp-image-insert-layer img bw-layer 0 -1)
        (gimp-item-set-name bw-layer "B/W Filter")
        (plug-in-colors-channel-mixer 1 img bw-layer TRUE
                                  -0.466 0.133 1.333  ;R
                                  0      0     0      ;G
                                  0      0     0 )    ;B)
        (gimp-invert bw-layer)
        (gimp-item-set-visible bw-layer FALSE)
        
        ;preserve yellow
        (gimp-image-insert-layer img extract-layer 0 -1)
        (gimp-item-set-name extract-layer "Extract")
        (gimp-layer-set-mode extract-layer LAYER-MODE-GRAIN-EXTRACT-LEGACY)
        (plug-in-colortoalpha 1 img extract-layer preserve)
        (gimp-edit-copy-visible img)
        (set! preserve-layer (car (gimp-layer-new-from-visible img img "Preserve Y")))
        (gimp-image-insert-layer img preserve-layer 0 0)
        (gimp-drawable-brightness-contrast preserve-layer -0.352 0.352) ; was -90 90 
        
        (set! preserve-layer-mask (car (gimp-layer-create-mask preserve-layer ADD-MASK-COPY)))
        (gimp-layer-add-mask preserve-layer preserve-layer-mask)
        (gimp-item-set-visible extract-layer FALSE)
        
        ;add cast layer
        (gimp-image-insert-layer img cast-layer 0 -1)
        (gimp-image-raise-item-to-top img preserve-layer)
        (gimp-context-set-foreground cast)
        (gimp-selection-all img)
        (gimp-edit-bucket-fill cast-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        (gimp-edit-copy bw-layer)
        (gimp-layer-add-mask cast-layer cast-layer-mask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste cast-layer-mask TRUE)))
        (gimp-selection-none img)
        
        ;colorize hue layer
        (gimp-drawable-hue-saturation hue-layer HUE-RANGE-YELLOW yellow-hues 0 0 0)
        (gimp-drawable-hue-saturation hue-layer HUE-RANGE-RED red-hues 0 0 0) ; was 0 0 0 at end
        
        ;remove unneccessary layers
        (gimp-image-remove-layer img extract-layer)
        (gimp-image-remove-layer img bw-layer)
        
        ; tidy up
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gimp-context-pop)
  )
)

(script-fu-register "elsamuko-rainy-landscape"
                    "Rainy Landscape"
                    "After rain effect. \nfile:elsamuko-rainy-landscape.scm"
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "12/03/10"
                    "RGB*"
                    SF-IMAGE        "Input image"          0
                    SF-DRAWABLE     "Input drawable"       0
                    SF-COLOR        "Blue Cast"            '( 0   0  255)
                    SF-COLOR        "Preserve Color"       '(255 255  0 )
                    SF-ADJUSTMENT   "Yellow Hues"          '(20 0 50 1 5 0 0)               
                    SF-ADJUSTMENT   "Red Hues"             '(30 0 50 1 5 0 0)                    
)

(script-fu-menu-register "elsamuko-rainy-landscape" "<Toolbox>/Script-Fu/Light and Shadow")

;end of script