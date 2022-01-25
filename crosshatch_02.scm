; Copyright © 2011 Bart Massey 
    ; Originally a transliteration of a Perl script Copyright © 2001 Sam Jones
    ; Please see the end of this file for licensing information.
    ;
    ; Makes image appear to have been drawn in and crosshatched.
    ; Pencil-like and etching-like effects are possible by manipulation
    ; of the various layers.
    ;
    ; If you are Sam Jones and are reading this, please get in touch
    ; with me---I would love to talk with you about this code.
    ;---x---
    ;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Crosshatch script  for GIMP 2.10.18
;
; Tags: crosshatch
;
; Author statement:
;
; 
;   - Changelog -
;
; Updated to work with Gimp2.10.18 (05-2020)
;
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(define (script-fu-crosshatch-make-layer image drawable name mode)
    (let* (
            (imgheight (car (gimp-image-height image)))
            (layer (car (gimp-layer-new image
                        (car (gimp-image-width image))
                        (car (gimp-image-height image))
                        (car (gimp-drawable-type drawable))
                        name
                        100
                        mode)
                    )
            )
          )
    ;; (gimp-image-undo-group-start image)
    ;(gimp-message "making layer started")
    (set! layer (car (gimp-layer-copy drawable 0)))
    (gimp-image-insert-layer image layer 0 -1)
    (gimp-context-set-background '(250 250 250))
    (gimp-drawable-edit-fill layer FILL-BACKGROUND)
    ;(gimp-drawable-edit-clear layer)
    
    layer  ; return the layer
    )
)
    
(define (script-fu-crosshatch-make-hatch
                        image
                        drawable
                        name
                        noise
                        hatch-length
                        angleA
                        darkness)
    (let* (
            (hatch (script-fu-crosshatch-make-layer
                         image drawable name LAYER-MODE-NORMAL-LEGACY))
            (widtha 0)
            (heighta 0)
            (hatchblurmult 0)
          )
            (set! widtha  (car (gimp-image-width  image)))
            (set! heighta (car (gimp-image-height image)))
            (set! hatchblurmult (* hatch-length 2.5)) 
        
        ;(gimp-message "hatch make layer")
        (plug-in-noisify RUN-NONINTERACTIVE image hatch 0 noise noise noise 0)
        (plug-in-mblur RUN-NONINTERACTIVE image hatch 0 hatchblurmult angleA (/ widtha 2) (/ heighta 2))
        
        
        
        (gimp-drawable-levels hatch HISTOGRAM-VALUE (/ (+ darkness 191) 255) 0.1 TRUE 2.2 0.0 1.0 TRUE)
        ;;(gimp-levels hatch HISTOGRAM-VALUE (+ darkness 40) 255 1.2 0 255) ; was darkeness+192
        
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-layer-set-opacity hatch 50)
        (gimp-item-set-name hatch name)
        
        hatch ; return the hatch;;
    )
)
    
(define (script-fu-massey-crosshatch
                        image
                        drawable
                        noise
                        hatch-length
                        h-angle
                        ch-angle
                        darkness
                        trace-threshold
                        trace-brush
                        trace-brush-size
                        merge-layers)

    (let*
            ; Trace penciling
          (
            (o-layer 0)
            ; The crosshatching is two layers normally 90 degrees apart
            (h-layer 0)
            (ch-layer 0)
            (b-layer)
            
            (brush1 (car trace-brush ))
            (opacity1 (cadr trace-brush))
            (brushsizerel 0)
            (finalangle 0)
            (finalangleCh 0)
            (width 10)
            (height 10)
          )
            
        (gimp-image-undo-group-start image)
        
        (set! width  (car (gimp-image-width  image)))
        (set! height (car (gimp-image-height image)))
        
        ;(gimp-message "making layers check") 
        
        ; Grab traceable area for later use
        (gimp-context-set-sample-threshold-int trace-threshold)
        (gimp-image-select-color image CHANNEL-OP-ADD drawable '(0 0 0)) ; was replace 
        ;(gimp-message "black select one done") 
        ;;(gimp-image-select-color image CHANNEL-OP-ADD trace-threshold '(0 0 0))
        ;;(gimp-by-color-select drawable '(0 0 0) trace-threshold CHANNEL-OP-REPLACE 1 1 0.2 1) ; was 0 0 0.0 0 at end
        
        ;(gimp-message "second color select")
        (gimp-selection-invert image)
        (plug-in-sel2path RUN-NONINTERACTIVE image drawable)
        
        ;(gimp-selection-none image)
        
        ;(gimp-message "making 0-layer")
        (if (< (- h-angle 90) 0)
            (begin
                (set! finalangle (+ (- h-angle 90) 91))
                ;(gimp-message (number->string finalangle))
            )
            (begin
                (set! finalangle h-angle)
            )
        )
        (if (> (+ ch-angle 180) 360)
            (begin
                (set! finalangleCh (+ (- ch-angle 10) 180))
                ;(gimp-message (number->string finalangleCh))
            )
            (begin
                (set! finalangleCh (+ ch-angle 180))
            )
        )
        ;(gimp-message "line 161")
        (set! o-layer (script-fu-crosshatch-make-layer image drawable "Traced Outline" LAYER-MODE-NORMAL))
         
         ;(set! b-layer (car (gimp-layer-new image width height RGB-IMAGE "Traced Outline layer" 100 LAYER-MODE-NORMAL)))
         ;(gimp-image-insert-layer image b-layer 0 -1)
         ;(gimp-item-set-name b-layer "Traced Outline layer")
         (gimp-item-set-name o-layer "Traced Outline")
         (gimp-context-set-foreground '(255 255 255))
         ;(gimp-drawable-fill b-layer FILL-FOREGROUND)         
         
         ;(set! o-layer (car (gimp-layer-copy drawable 0)))
         ;(gimp-layer-set-mode o-layer LAYER-MODE-MULTIPLY-LEGACY) 
         ;(gimp-image-insert-layer image o-layer 0 -1)
         ;(gimp-item-set-name o-layer "Multiply layer")         
        
        ;(gimp-message "line 192")
        
        (set! h-layer (script-fu-crosshatch-make-hatch image drawable "Hatch"
                                    noise hatch-length finalangle darkness))
        (gimp-layer-set-mode h-layer LAYER-MODE-GRAIN-MERGE) ; was SOFTLIGHT-LEGACY
        ;(gimp-message "h layer made")
        
        (set! ch-layer (script-fu-crosshatch-make-hatch
                                image drawable "Cross-Hatch"
                                noise hatch-length finalangleCh darkness))
        (gimp-layer-set-mode ch-layer LAYER-MODE-GRAIN-MERGE-LEGACY)
        
        ;(gimp-message "setting the brush")
        (gimp-context-set-brush brush1) ;; car (trace-brush)
        (set! brushsizerel trace-brush-size)
        (gimp-context-set-dynamics "Dynamics Off")
        (gimp-context-set-brush-size brushsizerel)
        (gimp-context-set-opacity 95); (cadr trace-brush))
        
        ;(gimp-message "opacity set")
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL-LEGACY)  ;;(cadddr trace-brush) ; was normal mode legacy
        (gimp-context-set-stroke-method STROKE-PAINT-METHOD)
        (gimp-context-set-brush-spacing 40.0)
        ;(gimp-message "mode set")
        (gimp-context-set-foreground '(1 1 1))
        ;(gimp-edit-stroke-vectors o-layer
        ;    (car (gimp-image-get-active-vectors image))
        ;)
        (gimp-drawable-edit-stroke-item o-layer
            (car (gimp-image-get-active-vectors image))
        )
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-brush-size (round (* trace-brush-size 0.6)))
        (gimp-context-set-brush-spacing 10.0)
        (gimp-drawable-edit-stroke-selection o-layer)
        
        ;(gimp-message "edit stroke vectors done")
        (gimp-layer-set-opacity o-layer 67)
        (gimp-layer-set-mode o-layer LAYER-MODE-MULTIPLY)
        ;(gimp-layer-set-mode b-layer LAYER-MODE-MULTIPLY)
        
        (gimp-selection-none image)
        
        (if (eq? merge-layers TRUE)
            (begin
                (gimp-image-merge-down image o-layer CLIP-TO-IMAGE)
                (gimp-image-merge-down image h-layer CLIP-TO-IMAGE)
                (gimp-image-merge-down image ch-layer CLIP-TO-IMAGE)
            )
        )
        
        (gimp-message "Good finish OK")
        ; Clean up
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        
    )
)

(script-fu-register "script-fu-massey-crosshatch"
    "Cross_hatch..."
    "Makes image appear to have been drawn in and crosshatched. Optimised for 1024x768. \nfile:crosshatch_02.scm"
    "Bart Massey"
    "Copyright 2011 Bart Massey (inspired by material Copyright 2001 Sam Jones)"
    "July 21, 2011"
    "RGB* GRAY*"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-ADJUSTMENT "Noise" '(0.25 0.0 1.0 0.05 0.15 2 0) ; Sf-slider
    SF-ADJUSTMENT "Hatch length" '(10.0 0.0 200.0 1.0 5.0 1 0)
    SF-ADJUSTMENT "Hatch angle" '(45.0 0.0 180.0 5.0 45.0 1 0)
    SF-ADJUSTMENT "Cross-Hatch angle" '(135.0 0.0 180.0 5.0 25.0 1 0)
    SF-ADJUSTMENT "Sketch darkness" '(58 0 63 1 8 0 0)
    SF-ADJUSTMENT "Trace threshold" '(40 0 255 1 16 0 0)
    SF-BRUSH "Trace brush" '("Diagonal Star (11)" 1.0 100 0) ; paint mode = 0 being layer-mode-normal-legacy
    SF-ADJUSTMENT "Trace brush size" '(30 0 100 1 16 0 0)
    SF-TOGGLE "Merge layers" FALSE
)

(script-fu-menu-register "script-fu-massey-crosshatch" "<Image>/Script-Fu2/Artistic")

;end of file

    ; This program is free software; you can redistribute it and/or modify
    ; it under the terms of the GNU General Public License as published by
    ; the Free Software Foundation; either version 2 of the License, or
    ; (at your option) any later version.
    ;
    ; This program is distributed in the hope that it will be useful,
    ; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    ; GNU General Public License for more details.
    ;
    ; You should have received a copy of the GNU General Public License along
    ; with this program; if not, write to the Free Software Foundation, Inc.,
    ; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. 