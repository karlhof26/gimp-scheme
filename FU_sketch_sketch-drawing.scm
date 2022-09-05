; FU_sketch_sketch-drawing.scm
; version 2.9 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
; 23/07/2020 on GIMP-2.10.20
;
; 02/15/2014  - accommodated indexed images, option tyo flatten upon completion
; (and made flatten default)
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
; Drawing script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
;==============================================================


(define (FU-drawing
        img
        drawable
        thickness
        inFlatten
    )
    
    (gimp-image-undo-group-start img)
    (define indexed (car (gimp-drawable-is-indexed drawable)))
    (if (= indexed TRUE)(gimp-image-convert-rgb img))
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (thf (* height  0.005 thickness ))
            (image-type (car (gimp-image-base-type img)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL-LEGACY)))
            (layer-temp2 (car (gimp-layer-new img width height layer-type "temp2"  100 LAYER-MODE-NORMAL-LEGACY)))
        ) 
        
        (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
            (gimp-drawable-fill old-selection FILL-WHITE) 
        ) ; so Empty and All are the same.
        (gimp-selection-none img)
        (gimp-image-insert-layer img layer-temp1 0 -1)
        (gimp-image-insert-layer img layer-temp2 0 -1)
        (gimp-edit-copy drawable)
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
        
        (if (eqv? (car (gimp-drawable-is-gray drawable)) FALSE)#
            (begin
                (gimp-drawable-desaturate layer-temp1 DESATURATE-LUMINANCE)
            )
        )
        (gimp-edit-copy layer-temp1)
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
        (gimp-invert layer-temp2)
        (plug-in-gauss 1 img layer-temp2 thf thf 0)
        (gimp-layer-set-mode layer-temp2 16)
        (gimp-image-merge-down img layer-temp2 0)
        (set! layer-temp1 (car (gimp-image-get-active-layer img)))
        ;(gimp-levels layer-temp1 0 215 235 1.0 0 255) 
        (gimp-drawable-levels layer-temp1 HISTOGRAM-VALUE 0.843 0.917 TRUE 1.0 0.0 1.0 TRUE)
        
        (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
        (gimp-selection-invert img)
        (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
            (begin
                (gimp-edit-clear layer-temp1)
            )
        )
        
        (gimp-item-set-name layer-temp1 "Drawing")
        (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
        (gimp-image-remove-channel img old-selection)
        
        (if (= inFlatten TRUE)
            (gimp-image-flatten img)
        )
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
  )
)

(script-fu-register
    "FU-drawing"
    "<Image>/Script-Fu/Artist/Sketch Drawing"
    "Creates a drawing.\n\nThis version lets you adjust the line thickness.\nfile:FU_sketch_sketch-drawing.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "*"
    SF-IMAGE      "Image"                           0
    SF-DRAWABLE   "Drawable"                        0
    SF-ADJUSTMENT "thickness"                       '(2 1 10 1 1 0 0)
    SF-TOGGLE     "Flatten image when complete?"    TRUE
)

; end of script