; The GIMP -- an image manipulation program  
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
;--------------------------------------------------------------------------------------
; Photo effects bundle 1
; Made by Iccii for GIMP version 1
;
; Bundled and adapted for GIMP version 2.x by Eddy Verlinden 
; Most changes are documented
; 
;
; Find the Original scripts at http://wingimp.hp.infoseek.co.jp/files/script/script.html
; Copyright (C) 2001-2002 Iccii <iccii@hotmail.com>
;---------------------------------------------------------------------------------------
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

;***************************************************************************
; 
; Chrome image script  for GIMP 1.2
; Copyright (C) 2001-2002 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
; version 0.1a by Iccii 2001/07/23
;     - Fixed some bugs in curve caluclation
; version 0.1a by Iccii 2001/10/08
;     - Added Color option (which enable only RGB image type)
; version 0.1b 2002/02/25 Iccii <iccii@hotmail.com>
;     - Added Contrast option
; version 0.1c 2020/03/01 karlhof26
;     - optimised and fixed for Gimp 2.10.18
; --------------------------------------------------------------------
; 
(define (script-fu-chrome-image-ga img drawable color contrast deform randomx emboss? stopearly)
   
  (let* (   
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-context-get-foreground)))
            (image-type (if (eqv? (car (gimp-drawable-is-gray drawable)) TRUE)
                GRAYA-IMAGE
                RGBA-IMAGE))
            (layer-color (car (gimp-layer-new img width height image-type
                    "Color Layer" 100 LAYER-MODE-OVERLAY)))
            (point-num (+ 2 (* randomx 2)))
            (step (/ 255 (+ (* randomx 2) 1)))
            (control_pts (cons-array (* point-num 2) 'double))
            (count 0)
        )
        ;  ; blank line
        (gimp-image-undo-group-start img)
        (if (eqv? (car (gimp-drawable-is-gray drawable)) FALSE)
            (gimp-drawable-desaturate drawable DESATURATE-LIGHTNESS)
        )
    (plug-in-gauss-iir2 1 img drawable deform deform)
    (if (eqv? emboss? TRUE)
        (plug-in-emboss 1 img drawable 30 45.0 20 1)
    )
    
    (gimp-displays-flush)
    
    (if (= stopearly TRUE)
        (begin
            (gimp-message "Stop early OK")
        )
        (begin
            
            ; blank line
            ;; values now to be between 0 and 1 
            (aset control_pts 0                      0.0)
            (aset control_pts 1                      0.0)
            (while (< count randomx)
                (begin
                    (aset control_pts  (+ (* count 4) 2) (/ (* step (+ (* count 2) 1)) 255))
                    (aset control_pts  (+ (* count 4) 3) (/ (+ 128 contrast) 255))
                    (aset control_pts  (+ (* count 4) 4) (/ (* step (+ (* count 2) 2)) 255))
                    (aset control_pts  (+ (* count 4) 5) (/ (- 128 contrast) 255))
                    (set! count (+ count 1))
                )
            )
            
            (aset control_pts (- (* point-num 2) 2)  1.0) ; was 255 now 1
            (aset control_pts (- (* point-num 2) 2)  1.0) ; was 255 now 1
            (gimp-drawable-curves-spline drawable HISTOGRAM-VALUE (* point-num 2) control_pts)
            ; blank line   
            ;;
            (if (eqv? image-type RGBA-IMAGE)
                (begin
                    (gimp-context-set-foreground color)
                    (gimp-image-insert-layer img layer-color 0 -1)
                    (gimp-edit-fill layer-color FILL-FOREGROUND) ; was  FG-IMAGE-FILL
                )
            )
            
        )
    )
    ; blank line
    ;;
    (gimp-context-set-foreground old-fg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc); garbage cleanup
  )
)

(script-fu-register "script-fu-chrome-image-ga"
    "Chrome Image Art version"
    "Create chrome image.  Useful when you want to create metallic surfaces. Works best on a layer with transparency and non-black colors. Try very low values for all settings if a normal non-transparent image is used. \nfile:ev_iccii_photoeffects_246_02_part1.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2002, Feb"
    "RGB* GRAY*"
    SF-IMAGE      "Image"           0
    SF-DRAWABLE   "Drawable"        0
    SF-COLOR      "Color"           '(203 199 203) ;(203 127 0)
    SF-ADJUSTMENT "Contrast"        '(107 0 127 1 1 0 SF-SPINNER)
    SF-ADJUSTMENT "Deformation"     '(15 1 50 1 10 0 SF-SPINNER)
    SF-ADJUSTMENT "Ramdomness"      '(3 1 7 1 10 0 SF-SPINNER)
    SF-TOGGLE     "Enable Emboss"    TRUE
    SF-TOGGLE     "Stop at Emboss"   FALSE
)

(script-fu-menu-register "script-fu-chrome-image-ga" "<Toolbox>/Script-Fu/Effects/")


;*************************************************************************************** 
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
; version 0.3 adapted for GIMP2.10.18  by karlhof26
; --------------------------------------------------------------------
; 


(define (script-fu-cross-light-gab img drawable llength langle number threshold)
    (let* ( 
            (modulo fmod)           ;; in R4RS way
            (count 1)
            (tmp-layer (car (gimp-layer-copy drawable TRUE)))
            (target-layer (car (gimp-layer-copy drawable TRUE)))
            (layer-mask (car (gimp-layer-create-mask target-layer ADD-MASK-WHITE)))
            (marged-layer (car (gimp-layer-copy drawable TRUE)))
            (currentselection (car (gimp-selection-save img)))
          )
        
        (gimp-image-undo-group-start img)
;       
;        ; dont use set! currentselection (car(gimp-selection-save img}}}
        (gimp-selection-none img)
;       
;        ; these tree line were moved up by EV
        (gimp-image-insert-layer img target-layer 0 -1)
        (gimp-image-add-layer-mask img target-layer layer-mask)
        (gimp-image-insert-layer img tmp-layer 0 -1)
    
        (gimp-drawable-desaturate tmp-layer DESATURATE-LIGHTNESS) 
        (gimp-drawable-threshold tmp-layer HISTOGRAM-VALUE (/ threshold 255) 1)
        (gimp-edit-copy tmp-layer)
        ;;(gimp-message "@here166")
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
        (gimp-layer-remove-mask target-layer MASK-APPLY)
        
        (gimp-drawable-fill tmp-layer FILL-TRANSPARENT)
        
        (gimp-image-set-active-layer img target-layer)
        (while (<= count number)
            (let*   (   (layer-copy (car (gimp-layer-copy target-layer TRUE)))
                        (degree (modulo (+ (* count (/ 360 number)) langle) 360))
                    )
                (gimp-image-insert-layer img layer-copy 0 -1)  
                (if (= count 1) (gimp-image-raise-layer img layer-copy)) 
                (plug-in-mblur 1 img layer-copy 0 llength degree 0 0); two argyuments added for GIMP2  by EV       
               ; blank line
                (set! marged-layer (car (gimp-image-merge-down img layer-copy 0 )))
                (gimp-drawable-set-name marged-layer "cross-light") ; this line was added by EV
                (set! count (+ count 1))
            ) ; end of let*
        ) ; end of while
       ;;(gimp-message "@here186") 
        
        (gimp-image-remove-layer img target-layer)
        
        (gimp-layer-set-opacity marged-layer 80)
        (gimp-layer-set-mode marged-layer LAYER-MODE-SCREEN)
        
        (gimp-selection-load currentselection) ; these five lines are new in version 0.6a
        (if (= (car (gimp-selection-is-empty img)) FALSE) 
            (begin
                (gimp-selection-invert img)
                (if (= (car (gimp-selection-is-empty img)) FALSE)
                    (gimp-edit-fill marged-layer 3)
                )
                (gimp-selection-invert img)
            )
        )
        (gimp-image-remove-channel img currentselection)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-cross-light-gab"
    "Cross Light..."
    "Cross light effect. X shaped bokeh on the brightest spots in the image.\nfile:ev_iccii_photoeffects_246_02_part1.scm"
    "karlhof26"
    "Iccii & karlhof26"
    "March 2020"
    "RGB*"
    SF-IMAGE      "Image"       0
    SF-DRAWABLE   "Drawable"    0
    SF-ADJUSTMENT "Light Length"      '(40 1 255 1 10 0 0)
    SF-ADJUSTMENT "Start Angle"       '(30 0 360 1 10 0 0)
    SF-ADJUSTMENT "Number of Lights"  '(4 1 16 1 2 0 1)
    SF-ADJUSTMENT "Threshold -- Bigger 1 to 255 Smaller"    '(223 1 255 1 10 0 0)
)

(script-fu-menu-register "script-fu-cross-light-gab" "<Toolbox>/Script-Fu/Photo/Style")

;end of script;