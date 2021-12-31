; transbg.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.1 (20090825) 

; Changes:
; v1.1 - added option to select all bg colour (i.e. holes)

; Description
; Turns the background colour transparent while maintaining shadows around the
; object

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html


(define (script-fu-trans-bg img inLayer inThresh inSize inHoles inMerge)
  
  (let* (
            (BGColour (car (gimp-image-pick-color img inLayer 0 0 FALSE FALSE 0)))
        )
    
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    
    ; add alpha to layer if not exists
    (if (equal? (car (gimp-drawable-has-alpha inLayer)) FALSE)
       (gimp-layer-add-alpha inLayer))
    
    (let* ( 
           (dupLayer (car (gimp-layer-copy inLayer TRUE)))
           (origSel (car (gimp-selection-save img)))
          )
        
        (gimp-selection-none img)
        
        (gimp-image-insert-layer img dupLayer 0 -1)
        
        ; colour to alpha on dupLayer
        (plug-in-colortoalpha RUN-NONINTERACTIVE img dupLayer BGColour)
        
        (if (equal? inHoles TRUE)
            (begin
                ;(gimp-by-color-select inLayer BGColour inThresh CHANNEL-OP-REPLACE TRUE FALSE 0 FALSE)
                (gimp-context-set-sample-threshold-int inThresh)
                (gimp-image-select-color img CHANNEL-OP-REPLACE inLayer BGColour)
            )
            (begin
                ;(gimp-fuzzy-select inLayer 0 0 inThresh CHANNEL-OP-REPLACE TRUE FALSE 0 FALSE)
                (gimp-context-set-sample-threshold-int inThresh)
                (gimp-image-select-contiguous-color img CHANNEL-OP-REPLACE inLayer 0 0)
            )
        )
        
        ; grow selection by inSize
        (gimp-selection-grow img inSize)
        (gimp-edit-clear inLayer)
        (gimp-displays-flush)
        ;(quit)
        
        ;invert selection
        ;(gimp-selection-invert img)
        
        ;fill
        ;(gimp-context-set-foreground '(0 0 0))
        ;(gimp-edit-fill inLayer FILL-FOREGROUND)
        
        
        ;merge down dupLayer to inLayer...  might have to play with layer names
        (if (= inMerge TRUE)
            (gimp-image-set-active-layer img (car (gimp-image-merge-down img dupLayer CLIP-TO-IMAGE)))
        )
        
        ;(gimp-selection-load origSel)
        ;(gimp-image-remove-channel img origSel)
    )
    
    ; Done
    
    (gimp-image-undo-group-end img)
    (gimp-context-pop)
    
    (gimp-displays-flush)
    (gc) ; memory cleanup
  )
)


(script-fu-register "script-fu-trans-bg"
                "Transparent Background..."
                "Make the background transparent. \nfile:transbg.scm"
                "Rob Antonishen"
                "Rob Antonishen"
                "Mar 2009"
                "RGB* GRAY*"
                SF-IMAGE       "Image"      0
                SF-DRAWABLE    "Drawable"   0
                SF-ADJUSTMENT  "Colour Threshold"  (list 25 0 255 1 10 0 SF-SLIDER)
                SF-ADJUSTMENT  "Shrink Size"       (list 5 1 100 1 10 0 SF-SLIDER)
                SF-TOGGLE      "Make Holes Transparent" FALSE
                SF-TOGGLE      "Merge layers" FALSE
)

(script-fu-menu-register "script-fu-trans-bg"
                    "<Toolbox>/Script-Fu/Layer/Transparency/")

;end of script