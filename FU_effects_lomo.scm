; FU_effects_lomo.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 04/09/2020 on GIMP-2.10.20
;
; remove deprecated procedures Oct 1, 2008
; Tweaked again by Paul Sherman on 11/30/2008
; (hvignette define and menu location) 
;
; 02/14/2014 - convert to RGB if needed, option to flatten image upon completion
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================
; Original information 
; 
; Copyright (C) 2005 Francois Le Lay <mfworx@gmail.com>
;
; Version 0.3 - Changed terminology, settings made more user-friendly
; Version 0.2 - Now using radial blending all-way
; Version 0.1 - Rectangular Selection Feathering doesn't look too good
; 
; Usage: 
;
; - Vignetting softness: The vignette layer is scaled with a 
;   default size equal to 1.5 the image size. Setting it to 2
;   will make the vignetting softer and going to 1 will make
;   the vignette layer the same size as the image, providing
;   for darker blending in the corners.
;
; - Saturation and contrast have default values set to 20 and act 
;   on the base layer.
;
; - Double vignetting: when checked this will duplicate the Vignette 
;   layer providing for a stronger vignetting effect.
;
; October 23, 2007
; Script made GIMP 2.4 compatible by Donncha O Caoimh, donncha@inphotos.org
; Download at http://inphotos.org/gimp-lomo-plugin/
;==============================================================


(define (fu-lomo-effect 
        aimg 
        adraw 
        avig 
        asat 
        acon 
        adv
        inFlatten
    )
    
    (gimp-image-undo-group-start aimg)
    (if (not (= RGB (car (gimp-image-base-type aimg))))
            (gimp-image-convert-rgb aimg))
            
  (let* (   (img (car (gimp-item-get-image adraw)))
            (owidth (car (gimp-image-width img)))
            (oheight (car (gimp-image-height img)))
            (halfwidth (/ owidth 2))
            (halfheight (/ oheight 2))
            (vignette (car (gimp-layer-new img
                                        owidth 
                                        oheight
                                        1
                                        "Vignette" 
                                        100 
                                        LAYER-MODE-OVERLAY)))
            (overexpo (car (gimp-layer-new img
                                      owidth 
                                      oheight
                                      1
                                      "Over Exposure" 
                                      80 
                                      LAYER-MODE-OVERLAY)))
        )
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))    
        
        ; adjust contrast and saturation
        (gimp-drawable-brightness-contrast adraw 0 (/ acon 255))
        (gimp-drawable-hue-saturation adraw HUE-RANGE-ALL 0 0 asat 1)
        
        ; add two layers
        ; prepare them for blending
        (gimp-image-insert-layer img overexpo 0 -1)
        (gimp-image-insert-layer img vignette 0 -1)
        (gimp-drawable-fill vignette FILL-TRANSPARENT)
        (gimp-layer-set-lock-alpha vignette 0)    
        (gimp-drawable-fill overexpo FILL-TRANSPARENT)
        (gimp-layer-set-lock-alpha overexpo 0)
        
        ; compute blend ending point depending on image orientation
        (define endingx)
        (define endingy)
        (if (> owidth oheight)
            (begin
                (set! endingx owidth )
                (set! endingy halfheight )
            )
            (begin
                (set! endingx halfwidth )
                (set! endingy oheight )
            )
        )   
        
        ; let's do the vignetting effect
        ; apply a reverse radial blend on layer
        ; then scale layer by "avig" factor with a local origin
        ; if double vignetting is needed, duplicate layer and set duplicate opacity to 80%
        (gimp-edit-blend vignette 2 0 2 100 0 REPEAT-NONE TRUE FALSE 0 0 TRUE halfwidth halfheight endingx endingy)
        (gimp-layer-scale vignette (* owidth avig) (* oheight avig) 1 )
        (if (= adv TRUE) 
            (begin 
                (define hvignette (car (gimp-layer-copy vignette 0)))
                (gimp-layer-set-opacity hvignette 80)
                (gimp-image-insert-layer img hvignette 0 -1)
            )
        )
        
        ; let's do the over-exposure effect
        ; swap foreground and background colors then
        ; apply a radial blend from center to farthest side of layer 
        
        (gimp-context-swap-colors)
        (gimp-edit-blend overexpo 2 0 2 100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE halfwidth halfheight endingx endingy)
        
        ; tidy up
        (if (= inFlatten TRUE)(gimp-image-flatten img))
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
  )
)

(script-fu-register "fu-lomo-effect"
    "<Image>/Script-Fu/Effects/Lomo (center highlight)"
    "Do a lomo effect on image. \nfile:FU_effects_lomo.scm"
    "Francois Le Lay <mfworx@gmail.com>"
    "Francois Le Lay"
    "15/02/05"
    "*"
    SF-IMAGE        "Input image"                   0
    SF-DRAWABLE     "Input drawable"                0
    SF-ADJUSTMENT   "Vignetting softness"           '(1.5 1 2 0.1 0.5 1 0)
    SF-ADJUSTMENT   "Saturation"                    '(20 0 40 1 5 1 0)
    SF-ADJUSTMENT   "Contrast"                      '(20 0 40 1 5 1 0)
    SF-TOGGLE       "Double vignetting"             FALSE
    SF-TOGGLE       "Flatten image when complete?"  FALSE
)

; end of script