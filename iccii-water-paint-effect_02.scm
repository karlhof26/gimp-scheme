; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Water paint effect script  for GIMP 1.2
;   <Image>/Script-Fu/Alchemy/Water Paint Effect...
;
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  2001/04/15 iccii <iccii@hotmail.com>
;     - Initial relased
; version 0.1a 2001/07/20 iccii <iccii@hotmail.com>
;     - more simple
;
; --------------------------------------------------------------------
; 
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;; Version de abcdugimp.free.fr

(define (script-fu-water-paint-effect
            inImage
            inDrawable
            inEffect
            inBrightBoost
    )
    
  (let* 
        (
            (text-layer 0)
            (theNewlayer (car (gimp-layer-copy inDrawable TRUE)))
            (text-layer (car (gimp-layer-new inImage 800 600 RGBA-IMAGE "chiseled-texte" 100 LAYER-MODE-NORMAL)))
         )
    (gimp-image-undo-group-start inImage)
    (plug-in-gauss-iir2 1 inImage inDrawable inEffect inEffect)
    (let* (
            (theNewlayer (car (gimp-layer-copy inDrawable TRUE)))
          )     
    )
    (gimp-drawable-set-name theNewlayer "New layer")
    (gimp-image-insert-layer inImage theNewlayer 0 -1)
    (if (= inBrightBoost TRUE)
        (gimp-drawable-brightness-contrast theNewlayer 0.85 0.10)
    )
    ;; not working (plug-in-laplace 1 inImage theNewlayer) 
    (plug-in-edge 1 inImage theNewlayer 5 2 0)
    (gimp-layer-set-mode theNewlayer LAYER-MODE-SUBTRACT)
    (gimp-drawable-posterize theNewlayer 8)
    (gimp-image-merge-down inImage theNewlayer EXPAND-AS-NECESSARY)
    
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-water-paint-effect"
    "<Toolbox>/Script-Fu/Effects/Water Paint Effect Iccii"
    "Draw with water paint effect. Version de abcdugimp.free.fr. \nfile:iccii-water-paint-effect_02.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "Jul, 2001"
    "RGB*, GRAY*"
    SF-IMAGE        "Image"	        0
    SF-DRAWABLE     "Drawable"      0
    SF-ADJUSTMENT   "Effect Size (pixels)"      '(45 0 150 1 10 0 0)
    SF-TOGGLE       "Brightness boost"          TRUE
)

;end of script