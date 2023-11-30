; FU_artist_cutout.scm
; version 2.9 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
; 01/09/2020 on Gimp 2.10.20
;
; 02/15/2014 - work with non-rgb, merge option and install info added
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows 10
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.10\scripts
;	  
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	
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
; Cutout image script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
;==============================================================

(define (FU-cutout
        img
        drawable
        colors
        smoothness
        inMerge
    )
    
    (gimp-image-undo-group-start img)
    (define indexed (car (gimp-drawable-is-indexed drawable)))
    (if (= indexed TRUE)
        (gimp-image-convert-rgb img)
    )
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (blur (* (* width  smoothness) 0.001 ))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL)))
            (img2 (car (gimp-image-new width height image-type)))
            (layer-temp2 (car (gimp-layer-new img2 width height layer-type "temp2"  100 LAYER-MODE-NORMAL)))
        ) 
        
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-drawable-fill layer-temp1 FILL-TRANSPARENT)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (gimp-message (number->string blur))
    
    (plug-in-gauss 1 img layer-temp1 blur blur 0)
    (gimp-edit-copy layer-temp1)
    
    (gimp-image-insert-layer img2 layer-temp2 0 -1)
    (gimp-drawable-fill layer-temp2 FILL-TRANSPARENT)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-image-convert-indexed img2 0 0 colors 0 0 "0")
    (gimp-edit-copy layer-temp2)
    (gimp-image-delete img2)
    
    (gimp-layer-add-alpha layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    
    (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-item-set-name layer-temp1 "Cutout")
    (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
    (gimp-image-remove-channel img old-selection)
    
    (if (= inMerge TRUE)
        (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)
    )
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc) ; garbage cleanup; memory cleanup
  )
)

(script-fu-register
    "FU-cutout"
    "<Toolbox>/Script-Fu/Artist/Cutout"
    "Creates a drawing effect of soft-edged, simplified colors. \nfile:FU_artist_cutout.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com> Karlhof26 <karlhofmeyr@gmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "*"
    SF-IMAGE      "Image"                           0
    SF-DRAWABLE   "Drawable"                        0
    SF-ADJUSTMENT "Colors"                          '(10 4 32 1 10 0 0)
    SF-ADJUSTMENT "Smoothness"                      '(8 1 20 1 1 0 0)
    SF-TOGGLE     "Merge layers when complete?"     FALSE
)

;end of script