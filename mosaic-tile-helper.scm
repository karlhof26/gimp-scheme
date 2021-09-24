; mosaic-tile-helper.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.0 (20080620)

; Description 
;
; set up for one of four tile stroke paths, optionally stroking the path with the current brush
; Note!  It is better to keep the path and stroke manually, as the current programatic stroke path does not respect the tool settings.
;

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

(define (script-fu-mosaic-tile-helper img inLayer inSpacing inMethod inKeepPath inStroke inColour)
  (let*
        (
            (width (car (gimp-image-width img)))
            (height (car (gimp-image-height img)))
            (scratch-layer 0)
            (initial-selection 0)
            (colourtoggle 1)          
            (selectsize (/ inSpacing 2))  
            (newpath 0)
        )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    
    ;save selection or select all if no selection
    (if (= (car (gimp-selection-is-empty img)) FALSE)
        (set! initial-selection (car (gimp-selection-save img)))
        (gimp-selection-all img)  ;else
    )
    (set! scratch-layer (car (gimp-layer-new-from-drawable inLayer img)))
    
    (gimp-image-insert-layer img scratch-layer 0 -1)
    ;fill the scratch layer with white
    (gimp-drawable-fill scratch-layer FILL-WHITE)
    
    ; shrink by half the spacing so the outer path will be inside the selection.
    (gimp-selection-shrink img (/ inSpacing 2))
    
    ; work in scratch layer
    (if (= inMethod 0)  ;inward shaped
        (begin
            (while (= (car (gimp-selection-is-empty img)) FALSE)
                (begin
                    (if (= colourtoggle 0)
                        (begin
                            (gimp-context-set-foreground '(255 255 255))
                            (set! colourtoggle 1)
                        )
                        (begin
                            (gimp-context-set-foreground '(0 0 0))
                            (set! colourtoggle 0)
                        )
                    )
                    (gimp-edit-fill scratch-layer FOREGROUND-FILL) 
                    (gimp-selection-shrink img inSpacing)
                )
            )
        )
    )
    
    (if (= inMethod 1)  ; circular
        (begin
            (gimp-context-set-foreground '(255 255 255))
            (while (<= selectsize (sqrt (+ (/ (* width width) 4) (/ (* height height) 4))))
                (begin
                    (gimp-ellipse-select img (- (/ width 2) selectsize) (- (/ height 2) selectsize) (* selectsize 2) (* selectsize 2) CHANNEL-OP-REPLACE TRUE FALSE 0)
                    (gimp-edit-bucket-fill scratch-layer BUCKET-FILL-FG LAYER-MODE-DIFFERENCE 100 0 FALSE 0 0)
                    (set! selectsize (+ selectsize inSpacing))
                )
            )
            (gimp-selection-none img)
        )
    )
    
    (if (= inMethod 2)  ; horizontal
        (begin
            (gimp-context-set-foreground '(0 0 0))
            (while (<= selectsize height)
                (begin
                    (gimp-rect-select img 0 selectsize width inSpacing CHANNEL-OP-REPLACE FALSE 0)
                    (gimp-edit-fill scratch-layer FILL-FOREGROUND)
                    (set! selectsize (+ selectsize (* inSpacing 2)))
                )
            )
            (gimp-selection-none img)
        )
    )
    
    (if (= inMethod 3)  ; vertical
        (begin
            (gimp-context-set-foreground '(0 0 0))
            (while (<= selectsize width)
                (begin
                    (gimp-rect-select img selectsize 0 inSpacing height CHANNEL-OP-REPLACE FALSE 0)
                    (gimp-edit-fill scratch-layer FILL-FOREGROUND)
                    (set! selectsize (+ selectsize (* inSpacing 2)))
                )
            )
            (gimp-selection-none img)
        )
    )
    
    
    ;select black
    (gimp-by-color-select scratch-layer '(0 0 0) 128 CHANNEL-OP-REPLACE TRUE FALSE 0 FALSE)
    
    ;turn it into a path
    (plug-in-sel2path 1 img scratch-layer)
    (set! newpath (vector-ref (cadr (gimp-image-get-vectors img)) 0))
    
    ;make it visible and active
    (gimp-vectors-set-visible newpath 1)
    (gimp-image-set-active-vectors img newpath)
    
    ;delete scratch layer
    (gimp-image-remove-layer img scratch-layer)
    
    ;load initial selection back up and delete the channel
    (if (not (= initial-selection 0))
        (begin
            (gimp-selection-load initial-selection)
            (gimp-image-remove-channel img initial-selection)
        )
        (gimp-selection-none img) ;else clear the selection
    )
    
    (if (= inStroke TRUE)
        (begin
            (gimp-context-set-foreground inColour)
            (gimp-edit-stroke-vectors inLayer newpath)
        )
    )
    
    (if (= inKeepPath FALSE)
        (gimp-image-remove-vectors img newpath)
    )
    
    ;done
    (gimp-progress-end)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

(script-fu-register "script-fu-mosaic-tile-helper"
        "<Toolbox>/Script-Fu/Artistic/Mosaic Tile Helper"
        "Set up paths for stroking tile shapes. \nfile:mosaic-tile-helper.scm"
        "Rob Antonishen"
        "Rob Antonishen"
        "June 2008"
        "RGB* GRAY*"
        SF-IMAGE        "image"      0
        SF-DRAWABLE     "drawable"   0
        SF-ADJUSTMENT   "Tile Row Spacing"  '(10 2 100 1 5 0 0)
        SF-OPTION       "Method"            '("Shaped Inward" "Circular" "Horizontal" "Vertical")
        SF-TOGGLE       "Keep path?"        TRUE
        SF-TOGGLE       "Stroke with current brush using selected colour?" FALSE
        SF-COLOR        "Stroke Colour"     '(0 0 0)
)

;end of script