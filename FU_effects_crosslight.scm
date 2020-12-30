; FU_effects_crosslight.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 03/09/2020 on GIMP-2.10.20
;
; 02/14/2014 - convert to RGB if needed, added option to merge layers upon completion
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
; Cross light script  for GIMP 2
; based on Cross light script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
; version 0.2  by Iccii 2001/08/09
;     - Add the Start Angle and the Number of Lighting options
; version 0.2a adapted for GIMP2  by EV
;==============================================================


(define (FU-cross-light
        img
        drawable
        length
        angle
        number
        threshold
        inMerge
    )
    (gimp-image-undo-group-start img)
    (if (not (= RGB (car (gimp-image-base-type img))))
        (gimp-image-convert-rgb img)
    )
    (let* (
            (modulo fmod);; in R4RS way
            (count 1)
            (tmp-layer (car (gimp-layer-copy drawable TRUE)))
            (target-layer (car (gimp-layer-copy drawable TRUE)))
            (layer-mask (car (gimp-layer-create-mask target-layer ADD-MASK-WHITE)))
            (marged-layer (car (gimp-layer-copy drawable TRUE)))
            (currentselection (car(gimp-selection-save img)))
        )
        ;  (set! currentselection (car(gimp-selection-save img)))
        (gimp-selection-none img)
        (gimp-image-insert-layer img target-layer 0 -1)
        (gimp-layer-add-mask target-layer layer-mask)
        (gimp-image-insert-layer img tmp-layer 0 -1)
        (gimp-drawable-desaturate tmp-layer DESATURATE-LIGHTNESS)
        (gimp-threshold tmp-layer threshold 255)
        (gimp-edit-copy tmp-layer)
        
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
        (gimp-layer-remove-mask target-layer MASK-APPLY)
        
        (gimp-drawable-fill tmp-layer FILL-TRANSPARENT)
        
        (gimp-image-set-active-layer img target-layer)
        (while (<= count number)
            (let* (
                    (layer-copy (car (gimp-layer-copy target-layer TRUE)))
                    (degree (modulo (+ (* count (/ 360 number)) angle) 360))
                )
                (gimp-image-insert-layer img layer-copy 0 -1)  
                (if (= count 1)
                    (gimp-image-raise-item img layer-copy)
                ) 
                (plug-in-mblur 1 img layer-copy 0 length degree 0 0); two argyuments added for GIMP2  by EV       
                
                (set! marged-layer (car (gimp-image-merge-down img layer-copy 0 )))
                (gimp-item-set-name marged-layer "cross-light") ; this line was added by EV
                (set! count (+ count 1))
            ) ; end of let*
        ) ; end of while
        
        (gimp-image-remove-layer img target-layer)
        
        (gimp-layer-set-opacity marged-layer 80)
        (gimp-layer-set-mode marged-layer LAYER-MODE-SCREEN)
        
        (gimp-image-select-item img CHANNEL-OP-REPLACE currentselection) ; these five lines are new in version 0.6a
        (if (equal? (car (gimp-selection-is-empty img)) FALSE) 
            (begin
                (gimp-selection-invert img)
                (if (equal? (car (gimp-selection-is-empty img)) FALSE)
                    (gimp-edit-fill marged-layer 3)
                )
                (gimp-selection-invert img)
            )
        )
        (gimp-image-remove-channel img currentselection)
        
        (if (= inMerge TRUE)
            (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)
        )
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register
    "FU-cross-light"
    "<Image>/Script-Fu/Effects/Cross Light"
    "Cross light effect - sort of a random criss-cross of speculars. \nfile:FU_effects_crosslight.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Aug"
    "*"
    SF-IMAGE        "Image"                                 0
    SF-DRAWABLE     "Drawable"                              0
    SF-ADJUSTMENT   "Light Length"                          '(40 1 255 1 10 0 0)
    SF-ADJUSTMENT   "Start Angle"                           '(30 0 360 1 10 0 0)
    SF-ADJUSTMENT   "Number of Lights"                      '(3 1 16 1 2 0 1)
    SF-ADJUSTMENT   "Threshold (Bigger 1<-->255 Smaller)"   '(223 1 255 1 10 0 0)
    SF-TOGGLE       "Merge layers when complete?"           FALSE
)

; end of script