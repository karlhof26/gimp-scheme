; FU_artist_notepaper.scm 
; version 2.9 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
;
; last modified/tested by karlhof26
; 08/04/2020 on GIMP-2.10.22
; 08/04/2020 - work on Gimp 2.10.18
; 02/13/2014 - work with non-rgb, merge option and install info added
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder. 
;
;   Windows Vista/7/8)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts   
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
; Note paper image script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
; version 0.1a by Iccii 2001/07/26
;     - Add Background color selector
; version 0.1b by Iccii 2001/09/25
;     - Add Cloud option in Background Texture
; version 0.2  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added some code (if selection exists...)
; version 0.2a by Iccii 2001/10/02 <iccii@hotmail.com>
;     - Fixed bug in keeping transparent area
; version 0.2b by Iccii 2001/10/02 <iccii@hotmail.com>
;     - Fixed bug (get error when drawable doesn't have alpha channel)
;==============================================================

(define (FU-notepaper
        img
        drawable
        threshold1
        threshold2
        base-color
        bg-color
        bg-type
        inMerge
    )
    
    (gimp-image-undo-group-start img)
    (if (not (= RGB (car (gimp-image-base-type img))))
            (gimp-image-convert-rgb img))
            
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-selection (car (gimp-selection-save img)))
            (layer-copy1 (car (gimp-layer-copy drawable TRUE)))
            (layer-copy2 (car (gimp-layer-copy drawable TRUE)))
            (layer-color1 (car (gimp-layer-new img width height RGBA-IMAGE "Color Upper" 100 LAYER-MODE-MULTIPLY-LEGACY)))
            (color-mask1 (car (gimp-layer-create-mask layer-color1 ADD-MASK-WHITE)))
            (layer-color2 (car (gimp-layer-new img width height RGBA-IMAGE "Color Under" 100 LAYER-MODE-MULTIPLY-LEGACY)))
            (color-mask2 (car (gimp-layer-create-mask layer-color2 ADD-MASK-WHITE)))
            (final-layer (car (gimp-layer-copy drawable TRUE)))
            (tmp 0)
            (invert? FALSE)
        )
        
    (gimp-selection-none img)
    (gimp-image-insert-layer img layer-copy1 0 -1)
    (gimp-image-insert-layer img layer-copy2 0 -1)
    (gimp-drawable-desaturate layer-copy2 0)
    (gimp-drawable-desaturate layer-copy1 0)
    (cond
      ((eqv? bg-type 0)
            (gimp-edit-fill layer-copy1 FILL-WHITE)
            ;(gimp-brightness-contrast layer-copy1 0 63)
            (gimp-drawable-brightness-contrast layer-copy1 0.0 0.327) ) ; was 0.0 0.246
      ((eqv? bg-type 1)
            (gimp-edit-fill layer-copy1 FILL-WHITE)
            (plug-in-noisify 1 img layer-copy1 FALSE 1.0 1.0 1.0 0)
            (gimp-drawable-brightness-contrast layer-copy1 0.0 0.247))
      ((eqv? bg-type 2)
            (plug-in-solid-noise 1 img layer-copy1 FALSE FALSE (rand 64235) 15 16 16)
            (plug-in-edge 1 img layer-copy1 5.0 1 3)  ; ev: needed too add the type (new plug-in) ; was 4 1 4
            ;(gimp-brightness-contrast layer-copy1 0 -63)
            (gimp-drawable-brightness-contrast layer-copy1 0.0 -0.247)
            )
      ((eqv? bg-type 3)
            (plug-in-plasma 1 img layer-copy1 (rand 65535) 4.0)
            (gimp-drawable-desaturate layer-copy1 DESATURATE-LUMINOSITY)
            (plug-in-gauss-iir2 1 img layer-copy1 1 1)
            (gimp-drawable-brightness-contrast layer-copy1 0.0 0.247) ; 0 63
            )
    ) ; end of cond
    (if (> threshold1 threshold2)
        (begin              ;; always (threshold1 < threshold2)
            (set! tmp threshold2)
            (set! threshold2 threshold1)
            (set! threshold1 tmp)
            (set! invert? TRUE)
        )
        (set! invert? FALSE)
    )
    (if (= threshold1 threshold2)
        (gimp-message "Execution error: Threshold1 equals to threshold2!")
        (begin 
            ;(gimp-threshold layer-copy2 threshold1 threshold2)
            (gimp-drawable-threshold layer-copy2 HISTOGRAM-VALUE (/ threshold1 255) (/ threshold2 255))
        )
    )
    (gimp-edit-copy layer-copy2)
    (plug-in-bump-map 1 img layer-copy1 layer-copy1 135 35 3 0 0 0 0 TRUE FALSE 0) ; was LINEAR at end (bug)
    (plug-in-bump-map 1 img layer-copy1 layer-copy2 135 35 3 0 0 0 0 FALSE invert? 0) ; was LINEAR at end (bug)
    ;(gimp-brightness-contrast layer-copy2 127 0)
    (gimp-drawable-brightness-contrast layer-copy2 0.5 0.0)
    
    (gimp-image-insert-layer img layer-color1 0 -1)
    (gimp-layer-add-mask layer-color1 color-mask1)
    (gimp-context-set-foreground base-color)
    (gimp-drawable-fill layer-color1 FILL-FOREGROUND)
    (gimp-floating-sel-anchor (car (gimp-edit-paste color-mask1 0)))
    (gimp-image-insert-layer img layer-color2 0 -1)
    (gimp-layer-add-mask layer-color2 color-mask2)
    (gimp-context-set-foreground bg-color)
    (gimp-drawable-fill layer-color2 FILL-FOREGROUND)
    (gimp-floating-sel-anchor (car (gimp-edit-paste color-mask2 0)))
    (gimp-drawable-invert color-mask2 FALSE)
    
    (gimp-layer-set-mode layer-copy2 LAYER-MODE-SCREEN-LEGACY)
    (gimp-layer-set-opacity layer-copy2 75)
    (gimp-image-merge-down img layer-copy2 EXPAND-AS-NECESSARY)
    (gimp-image-merge-down img layer-color1 EXPAND-AS-NECESSARY)
    (set! final-layer (car (gimp-image-merge-down img layer-color2 EXPAND-AS-NECESSARY)))
    (plug-in-bump-map 1 img final-layer final-layer 135 45 3 0 0 0 0 TRUE FALSE 0) ; added ev ; was LINEAR at end (bug)
    (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
        (gimp-image-select-item img CHANNEL-OP-REPLACE drawable))
        
    (if (= inMerge TRUE)(gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register "FU-notepaper"
    "<Toolbox>/Script-Fu/Artist/Note Paper"
    "Makes image look like a drawing on note paper \n file:FU_artist_notepaper.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Oct"
    "*"
    SF-IMAGE      "Image"               0
    SF-DRAWABLE   "Drawable"            0
    SF-ADJUSTMENT "Threshold (Bigger 1<-->255 Smaller)"  '(127 0 255 1 10 0 0)
    SF-ADJUSTMENT "Threshold (Bigger 1<-->255 Smaller)"  '(255 0 255 1 10 0 0)
    SF-COLOR      "Base Color"                          '(255 255 255)
    SF-COLOR      "Background Color"                    '(223 223 223)
    SF-OPTION     "Background Texture"                  '("Plain" "Sand" "Paper" "Cloud")
    SF-TOGGLE     "Merge layers when complete?"         FALSE
)

; end of script