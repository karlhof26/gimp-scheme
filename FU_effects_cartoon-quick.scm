; FU_effects_cartoon-quick.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 03/09/2020 on GIMP-2.10.20
;
; first edit for gimp-2.4 by paul on 1/27/2008
; "peeled" from photoeffects.scm - an scm containing several scripts
; separated to more easily update and to place more easily in menus.
;
; 02/14/2014 - convert to RGB if needed, option to merge layer upon complegtion
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
; Cartoon script  for GIMP 2.2
; Copyright (C) 2007 Eddy Verlinden <eddy_verlinden@hotmail.com>
;==============================================================


(define (FU-cartoon-quick
        img
        drawable
        inMerge
    )
    
    (gimp-image-undo-group-start img)
    (if (not (= RGB (car (gimp-image-base-type img))))
        (gimp-image-convert-rgb img))
    (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL)))
            (layer-temp2 (car (gimp-layer-new img width height layer-type "temp2"  100 LAYER-MODE-NORMAL)))
        )
        
        (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
            (gimp-drawable-fill old-selection FILL-WHITE)
        ) ; so Empty and All are the same.
        (gimp-selection-none img)
        (gimp-image-insert-layer img layer-temp1 0 -1)
        (gimp-image-insert-layer img layer-temp2 0 -1)
        (gimp-edit-copy drawable)
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
        (gimp-edit-copy layer-temp1)
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
        
        (plug-in-photocopy 1 img layer-temp2 10.0 0.8 0.1 0.6) ; was 8.0 1.0 0.0 0.8
        (gimp-levels layer-temp2 0 215 235 0.95 0 255) ; 0 215 235 1.0 0 255 
        (gimp-layer-set-mode layer-temp2 3)
        (gimp-levels layer-temp1 0 25 225 2.25 0 255) 
        (gimp-image-merge-down img layer-temp2 0)
        (set! layer-temp1 (car (gimp-image-get-active-layer img)))
        
        (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
        (gimp-selection-invert img)
        (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
            (begin
                (gimp-edit-clear layer-temp1)
            )
        )
        
        (gimp-item-set-name layer-temp1 "Cartoon")
        (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
        (gimp-image-remove-channel img old-selection)
        
        (if (= inMerge TRUE)
            (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)
        )
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register "FU-cartoon-quick"
    "Cartoon Quick"
    "Creates a light cartoon. Uses Photocopy script. Works best on images near to 800x600. \nfile:FU_effects_cartoon-quick.scm"
    "Eddy Verlinden <eddy_verlinden@hotmail.com>"
    "Eddy Verlinden"
    "2007, juli"
    "*"
    SF-IMAGE      "Image"                           0
    SF-DRAWABLE   "Drawable"                        0
    SF-TOGGLE     "Merge layers when complete?"     FALSE
)

(script-fu-menu-register "FU-cartoon-quick" "<Toolbox>/Script-Fu/Effects/Cartoon Quick")

(script-fu-menu-register "FU-cartoon-quick" "<Toolbox>/Script-Fu3/Comics-o-matic")
; end of script