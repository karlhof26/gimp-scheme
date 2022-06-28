; FU_contrast_auto-contrast.scm
; version 2.9 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
; 01/09/2020 on Gimp 2.10.20
;
; 02/15/2014 - convert to RGB if needed 
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
; Automatically adjusts contrast of the current
; drawable by duplicating the layer, setting the
; new layer to Value blend mode, then running
; Auto Levels on the Value layer.
;==============================================================

(define (FU-auto-contrast 
        img 
        drawable 
        merge-flag
    )
    (let* (
            (dummy 0)
            (value-layer 0)
        )
        (gimp-image-undo-group-start img)
        (if (not (= RGB (car (gimp-image-base-type img))))
            (gimp-image-convert-rgb img)
        )   
        ; Create a new layer
        (set! value-layer (car (gimp-layer-copy drawable TRUE)))
        ; Give it a name
        (gimp-item-set-name value-layer "Contrast Adjustment Layer")
        ; Add the new layer to the image
        (gimp-image-insert-layer img value-layer 0 0)
        ; Set opacity to 100%
        (gimp-layer-set-opacity value-layer 100)
        ; Set the layer mode to Value 
        (gimp-layer-set-mode value-layer 14)
        ; Adjust contrast
        (gimp-drawable-levels-stretch value-layer)
        ; Merge down, if required 
        (if (equal? merge-flag TRUE)
            (gimp-image-merge-down img value-layer 1 )
            ()
        )
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register "FU-auto-contrast"
    "<Image>/Script-Fu/Photo/Contrast/Auto Contrast"
    "Automatically adjust contrast of drawable. \nfile:FU_contrast_auto-contrast.scm"
    "Mark Lowry"
    "Mark Lowry"
    "2006"
    "*"
    SF-IMAGE    "Image"         0
    SF-DRAWABLE "Current Layer" 0
    SF-TOGGLE   "Merge Layers?" FALSE
)

; end of file