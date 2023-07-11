; Artist Copies Rel 1    
; Created by Tin Tran
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; To view a copy of the GNU General Public License
; visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------
; Rel 1: Initial release based on http://gimpchat.com/viewtopic.php?f=9&t=14116#p193028
;

(define (script-fu-mura-meister-inspired-spiral
            image 
            layer 
            radius
            arclength
            shrink-radius
            ;scale-percentages
            num-of-loops
            placement
            merge
            drop-shadow
            drop-shadow-offset-x
            drop-shadow-offset-y
            drop-shadow-blur-radius
            drop-shadow-color
            drop-shadow-opacity
            drop-shadow-allow-resize
        )
    (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (copy-count 0)
            (layer-copy 0)
            (opacity 100)
            (copies ())
            (position 0)
            (rotation 0)
            (inc_rotation 0)
            ;(scale-percentage scale-percentages)
            (original-radius radius)
            (original-arclength arclength)
            (current-layer 0)
          )
            ;(gimp-image-undo-disable image); DN = NO UNDO
            (gimp-context-push)
            (gimp-image-undo-group-start image)                   ;undo-group in one step
            (gimp-selection-none image)
            (set! current-layer layer)
            (while (< rotation (* num-of-loops 360))
                (set! arclength (/ (* original-arclength radius) original-radius))
                (set! inc_rotation (* (/ arclength (* (* radius 2) 3.14159265359)) 360))
                
                (set! rotation (+ rotation inc_rotation))
                (set! radius (- radius (* (/ shrink-radius 360) inc_rotation)))
                (set! layer-copy (car(gimp-layer-copy layer TRUE)))
                (if (= placement 0) ;infront selected
                    (begin
                        (set! position 0) ;always add ontop
                    )
                    (begin          ;behind selected
                        (set! position (+ (car(gimp-image-get-item-position image current-layer)) 1)) ;always add under current layer.
                    )
                )
                (gimp-image-insert-layer image layer-copy 0 position)
                
                ;move it
                (gimp-layer-translate layer-copy (- 0 radius) 0)
                
                
                (gimp-layer-scale layer-copy
                    (max 1 (/ (* (car (gimp-drawable-width layer)) radius) original-radius))
                    (max 1 (/ (* (car (gimp-drawable-height layer)) radius) original-radius))
                    TRUE ;use local origin
                )
                
                (set! layer-copy (car (gimp-item-transform-rotate 
                                            layer-copy 
                                            (/ (* (* rotation 2) 3.14159265359) 360)
                                            FALSE
                                            (+ (list-ref (gimp-drawable-offsets layer) 0) (/ (car (gimp-drawable-width layer)) 2))
                                            (+ (list-ref (gimp-drawable-offsets layer) 1) (/ (car (gimp-drawable-height layer)) 2))
                                      )
                                 )
                )
                
                
                ;(set! scale-percentage (* scale-percentage (/ scale-percentages 100)))
                ;(set! shift-x (/ (* shift-x scale-percentage) 100))
                ;(set! shift-y (/ (* shift-y scale-percentage) 100))
                ;(set! layer layer-copy)  ; now work on the next copy
                (set! current-layer layer-copy)
                (set! copies (append copies (list layer-copy))) ; puts it in a list
                (set! copy-count (+ copy-count 1))
            )
            
            ;resize canvas to layers just in case our rotations creates layers that are outside of canvas 
            (gimp-image-resize-to-layers image)
            
            
            (if (= drop-shadow TRUE)
                (begin
                    (while (> copy-count 0)
                        (set! layer-copy (car copies))
                        (gimp-image-set-active-layer image layer-copy)
                        (script-fu-drop-shadow image layer-copy 
                            drop-shadow-offset-x
                            drop-shadow-offset-y
                            drop-shadow-blur-radius
                            drop-shadow-color
                            drop-shadow-opacity
                            drop-shadow-allow-resize
                        )
                        (set! copies (cdr copies))
                        (set! copy-count (- copy-count 1))
                    )
                )
                (begin
                    )
            )
            (if (= merge TRUE)
                (begin
                    (define background-layer (car (gimp-image-merge-visible-layers image 0)))
                    (plug-in-autocrop 1 image background-layer)
                )
            )
            
            
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            (gimp-image-undo-group-end image)                     ;undo group in one step
            (gimp-context-pop)
            (gimp-displays-flush)                            
            (gc) ; memory garbage cleanup
    )
) ;end of define

(script-fu-register  "script-fu-mura-meister-inspired-spiral"         ;function name of the function defined above
    "<Toolbox>/Script-Fu2/Mura Meister/Mura Meister Inspired Spiral..."    ;menu register (where can this script be access from inside GIMP)
    "Mura Meister Inspired Spiral. \nfile:Mura_Meister_Inspired_Spiral.scm"          ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2016"                                  ;date
    "RGB* GRAY*"                            ;mode
    SF-IMAGE        "Image"         0                   
    SF-DRAWABLE     "Layer"         0
    SF-ADJUSTMENT   "Radius"        '(400 0 2000 1 50 0 0)
    SF-ADJUSTMENT   "Arc Length per copy"       '(100 0 2000 1 50 0 0)
    SF-ADJUSTMENT   "Shrink Radius by per loop" '(100 0 2000 1 50 0 0)
    ;SF-ADJUSTMENT   "Scale Percentage"         '(90 0 200 1 10 0 0)
    SF-ADJUSTMENT   "Number of Loops"           '(3 0 100 1 10 0 0)
    SF-OPTION       "Placement"                 '("Infront" "Behind")
    SF-TOGGLE       "Merge Layers"                  TRUE
    SF-TOGGLE       "Drop Shadow (Settings below)"  FALSE
    SF-ADJUSTMENT   "Drop Shadow Offset X"      '(4 1 2000 1 50 0 0)
    SF-ADJUSTMENT   "Drop Shadow Offset Y"      '(4 1 2000 1 50 0 0)
    SF-ADJUSTMENT   "Drop Shadow Blur Radius"   '(15 1 2000 1 50 0 0)
    SF-COLOR        "Drop Shadow Color"         '(0 0 0)
    SF-ADJUSTMENT   "Drop Shadow Opacity"       '(60 0 100 1 5 0 0)
    SF-TOGGLE       "Drop Shadow Allow Resizing"    TRUE
)

; end of script
