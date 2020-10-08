;----------------------------------------------------------------------------------------------------------
; JT-replicate-in-rings.scm
;
; Replicate a layer/selection as a motif in concentric (or offset) rings
;
; Created by Jon Tait  (jontait2 at gimpchat.com)
;
;----------------------------------------------------------------------------------------------------------
; License: GPLv3
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;----------------------------------------------------------------------------------------------------------
; Modification History:
; V1.0 - First release  JT 9-4-15
; V1.1 - introduced definition of pi, seems to be lacking on some systems (still)
; V1.2 - Updated for Gimp-2.10.20
;----------------------------------------------------------------------------------------------------------



;*********** Procedure definition ***********

(define (script-fu-JT-replicate-in-rings
    inImage inDrawable 
    inRepRadix inNrings inRingSpacing
    inOffsetAngle 
    inProgOffX inProgOffY
    inZoomFrom inZoomTo inZoomAdjustSpacing
    inCentralMotif inRotateMotif
    inOuterUppermost
    inCreateNewImage inLayersMode
  )

  ;*********** Subroutines **********

  ;For debugging, use:
  ;(debug-message (string-append "x=" (number->string x)))
  (define (debug-message msg)
    (let
      ((handler (car (gimp-message-get-handler))))
      (gimp-message-set-handler ERROR-CONSOLE)		;{ MESSAGE-BOX (0), CONSOLE (1), ERROR-CONSOLE (2) }
      (gimp-message (string-append "JTRIR: " msg))
      (gimp-message-set-handler handler)
    )
  )

  (define (name-layer-with-params layer)
    (gimp-drawable-set-name layer (string-append "replicate-in-rings (" 
        (number->string inRepRadix)     "," (number->string inNrings)      "," (number->string inRingSpacing)       ","
        (number->string inOffsetAngle)  "," (number->string inProgOffX)    "," (number->string inProgOffY)          ","
        (number->string inZoomFrom)     "," (number->string inZoomTo)      "," (number->string inZoomAdjustSpacing) ","
        (number->string inCentralMotif) "," (number->string inRotateMotif) "," (number->string inOuterUppermost)    ")"
    ))
  )
    
  ;*********** Main routine **********
    
  (let* (
            (outImage inImage)                  ;Output image (defaults to input image)
            (baseLayer (car (gimp-image-get-active-layer inImage)))     ;Active layer is basis of replication
            (selnLayer 0)                       ;temporary layer to hold selection (if there is one)
            (outLayer 0)                        ;
            (ring (if (= inCentralMotif TRUE) 0 1))                     ;current ring number (NB ring 0 = central motif, if required) 
            (repCnt)                            ;replication count (number of motifs in current ring)
            (repIdx)                            ;replication index
            (cx) (cy)                           ;centre coords of base layer in its image 
            (zoom)                              ;zoom factor for current ring
            (radius 0)                          ;radius of current ring
            (theta)                             ;angle of motif from centre
            (upperRingLayer 0)                  ;uppermost ring layer (required for final merge-down in "All on one layer" mode)
        )
    (define pi (acos -1))           ;@@V1.1@@ some systems don't seem to recognize "pi" or "*pi*" so use this instead
    
    (gimp-image-undo-group-start inImage)
    
    ; If there is a selection, copy-n-paste to new layer - this will be our base layer
    (when (= (car (gimp-selection-is-empty inImage)) FALSE)
        (gimp-edit-copy baseLayer)
        (set! selnLayer (car (gimp-edit-paste baseLayer TRUE)))
        (gimp-floating-sel-to-layer selnLayer)
        (set! baseLayer selnLayer)
        (gimp-selection-none inImage)
    )
    
    ; Create new image, if requested 
    (when (= inCreateNewImage TRUE)
        (set! outImage (car (gimp-image-new (car (gimp-drawable-width baseLayer))
                        (car (gimp-drawable-height baseLayer))
                        (car (gimp-image-base-type inImage))
            ))
        )
        (gimp-image-undo-disable outImage)
        (set! baseLayer (car (gimp-layer-new-from-drawable baseLayer outImage)))
        (gimp-image-insert-layer outImage baseLayer 0 -1)
    )
    
    ; Determine centre of base layer (this will be reference point for transformations)
    (set! cx (+ (car  (gimp-drawable-offsets baseLayer)) (/ (car (gimp-drawable-width  baseLayer)) 2)))	;centre coords of input layer
    (set! cy (+ (cadr (gimp-drawable-offsets baseLayer)) (/ (car (gimp-drawable-height baseLayer)) 2)))

    ; Per-ring work loop
    (while (<= ring inNrings)
        
        (set! repCnt (if (= ring 0) 1 (* inRepRadix ring)))	; number of motifs in this ring (NB ring 0 has 1 motif!)
        (set! repIdx 0)
        
        ;determine zoom factor and radius for this ring
        (set! zoom (+ inZoomFrom (* (/ ring inNrings) (- inZoomTo inZoomFrom))))
        (if (> ring 0)
            (if (= inZoomAdjustSpacing TRUE)
                (if (<= inZoomFrom inZoomTo)
                    (set! radius (* zoom (* ring inRingSpacing)))		;growing: radius scales with motif size
                    (set! radius (+ radius (* zoom inRingSpacing)))	;shrinking; radius increments with motif size
                )
                (set! radius (* ring inRingSpacing))		;no zoom-adjust: radius is proportional to ring number
            )
        )
        
      ; Per-motif work loop
      (while (< repIdx repCnt)
        
        ; Create new layer from base layer
        (set! outLayer (car (gimp-layer-new-from-drawable baseLayer outImage)))
        ; If new ring and outer-uppermost disabled, position immediately above baselayer, otherwise above last layer
        (gimp-image-insert-layer outImage outLayer 0 (if (and (= repIdx 0) (= inOuterUppermost FALSE))
            (car (gimp-image-get-layer-position outImage baseLayer)) -1))
        ; Name layer - record param settings in ring0 layer (or, if no ring0, first motif of ring1)
        (if (or (= ring 0) (and (= inCentralMotif FALSE) (= ring 1) (= repIdx 0)))
            (name-layer-with-params outLayer)
            (gimp-drawable-set-name outLayer (string-append "ring" (number->string ring)))
        )
        
        ; Apply offsets, rotation and zoom (if any)
        (set! theta (+ (/ (* 2 (* pi repIdx)) repCnt) (* pi (/ inOffsetAngle 180))))	;calc angle to motif replication posn
        
        (gimp-item-transform-2d 
            outLayer
            cx cy                       ;source-x, source-y
            zoom zoom                   ;scale-x, scale-y
            (if (= inRotateMotif TRUE) theta 0)                     ;rotation angle
                (+ cx (+ (* radius (cos theta)) (* ring inProgOffX)))   ;dest-x
                (+ cy (+ (* radius (sin theta)) (* ring inProgOffY)))   ;dest-y
        )
        
        ; Merge-down, if apt
        (if (and (<> repIdx 0) (<> inLayersMode 2))
            (set! outLayer (car (gimp-image-merge-down outImage outLayer EXPAND-AS-NECESSARY)))
        )
        
        (set! repIdx (+ repIdx 1))
      );end while (per-motif work loop)
        
        ;remember uppermost ring layer for final merge-down (if reqd)
        (if (or (= inOuterUppermost TRUE) (= upperRingLayer 0)) (set! upperRingLayer outLayer))
        
        (set! ring (+ ring 1))
    );end while (per-ring work loop)
    
    ; Final merge-down (for "All on one layer" mode) 
    (when (= inLayersMode 0)
        (set! repCnt (if (= inCentralMotif TRUE) inNrings (- inNrings 1)))
        (while (> repCnt 0)
            (set! upperRingLayer (car (gimp-image-merge-down outImage upperRingLayer EXPAND-AS-NECESSARY)))
            (set! repCnt (- repCnt 1))
        )
        (name-layer-with-params upperRingLayer)
    )
    
    ;Resize image (fit canvas to layers)
    (gimp-image-resize-to-layers outImage)
    
    ; Display new image if apt
    (if (= inCreateNewImage TRUE)
        (begin
            (gimp-image-remove-layer outImage baseLayer)
            (gimp-display-new outImage)
            (gimp-image-undo-enable outImage)
        )
      ;else
    )
    
    ; Tidy-up
    (gimp-image-clean-all outImage)
    (when (<> selnLayer 0)                                  ;if there was a selection in force..
        (gimp-image-select-item inImage CHANNEL-OP-REPLACE selnLayer)       ;..restore the user's selection
        (gimp-image-remove-layer inImage selnLayer)         ;..and remove temporary selection layer
    )
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
    (gc) ; garbage cleaner
    
  );end let*
);end define

;*********** Procedure registration ***********

(script-fu-register
    "script-fu-JT-replicate-in-rings"
    "JT Replicate in Rings"
    "Replicate a layer/selection as a motif in concentric (or offset) rings. \nfile:JT-replicate-in-rings-v1.1.scm"
    "Jon Tait"
    "Jon Tait (jontait2 at http://gimpchat.com)"
    "9th April 2015"
    "RGB* GRAY*"
    SF-IMAGE      "Image"             0
    SF-DRAWABLE   "Drawable"          0
    SF-ADJUSTMENT "Replication radix"     '(6 1 20 1 1 0 1)
    SF-ADJUSTMENT "Number of rings"       '(3 1 30 1 10 0 1)
    SF-ADJUSTMENT "Ring spacing"          '(50 10 1000 1 10 0 1)
    SF-ADJUSTMENT "Offset angle"          '(0 -360 360 1 15 0 1)
    SF-ADJUSTMENT "Progressive offset X"      '(0 -1000 1000 1 10 0 1)
    SF-ADJUSTMENT "Progressive offset Y"      '(0 -1000 1000 1 10 0 1)
    SF-ADJUSTMENT "Zoom at centre"            '(1 0 10 0.01 1 2 1)
    SF-ADJUSTMENT "Zoom at outer ring"        '(1 0 10 0.01 1 2 1)
    SF-TOGGLE "Zoom-adjust ring-spacing"  TRUE
    SF-TOGGLE "Central motif"             TRUE
    SF-TOGGLE "Rotate motifs"             FALSE
    SF-TOGGLE "Outer rings uppermost"     FALSE
    SF-TOGGLE "Create new image"          TRUE
    SF-OPTION "Layers mode"               '("All on one layer" "Rings on separate layers" "Motifs on separate layers")
)

(script-fu-menu-register "script-fu-JT-replicate-in-rings" "<Image>/Script-Fu2/Misc/")

;end-of-file