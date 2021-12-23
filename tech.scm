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
; file tech.scm
;Created by Wyatt Arent
;Thanks to Adrian Likins for pieces of the 'Predator' script
; GPLv3

(define (script-fu-tech-khb image drawable color edge pixamm brightness)
    (let* ( (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (normcolor (car (gimp-context-get-background)))
            (layer (car (gimp-layer-new image width height RGB-IMAGE "techrender" 100 LAYER-MODE-OVERLAY-LEGACY)))
          )
        (gimp-image-undo-group-start image)
        (gimp-image-insert-layer image layer 0 -1)
        (gimp-context-set-background color)
        (gimp-drawable-fill layer FILL-BACKGROUND)
        (gimp-layer-add-alpha layer)
        (gimp-context-set-background normcolor)
        
        ;;(plug-in-scatter-hsv 1 image layer 2 0 30 60)
        
        (plug-in-hsv-noise 1 image layer 2 0 30 60)
        (plug-in-gauss 1 image layer 5 5 0)
        (plug-in-sharpen 1 image layer 70)
        (plug-in-bump-map 1 image layer layer 150 40.70 40 0 0 0 0 1 0 1)
       ; (plug-in-pixelize 1 image layer pixamm)
       ; (plug-in-max-rgb 1 image layer 0)
       ; (plug-in-edge 1 image layer edge 1 0)
       ; (plug-in-sharpen 1 image layer 65)
       ; (gimp-brightness-contrast layer 50 75)
       ; (plug-in-colorify 1 image layer color)
       ; (plug-in-sharpen 1 image layer 70)
        ;(plug-in-neon 1 image layer 1 brightness)
        (plug-in-mblur 1 image drawable 0 4 0 50 50)
        (plug-in-pixelize2 1 image drawable pixamm pixamm)
        (plug-in-max-rgb 1 image drawable 1)
        (plug-in-edge 1 image drawable edge 1 0)
        (gimp-displays-flush)
        (gimp-selection-none image)
        (gimp-image-set-active-layer image layer)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-tech-khb"
    "<Image>/Script-Fu2/Render/Tech original"
    "Create a cool technological-like image \nfile:tech.scm"
    "Wyatt Arent"
    "Wyatt Arent"
    "November 2009"
    "*"
    SF-IMAGE      "SF-IMAGE"        0
    SF-DRAWABLE   "SF-DRAWABLE"     0
    SF-COLOR      "Color"           '(0 255 20)
    SF-ADJUSTMENT "Edge"            '(8 1 10 1 1 0 0)
    SF-ADJUSTMENT "Pixel Ammount"   '(3 1 10 1 1 0 0)
    SF-ADJUSTMENT "Brightness"      '(.9 .1 1 .01 .01 1 0)
)

;end of script 