; Mura Meister Copies Rel 1   
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
(define (script-fu-mura-meister-perspective-tiling
            image 
            layer 
            x-tiles
            y-tiles
            scale-down-percentage
            perspective
            height-percentage
            width-percentage
         )
    (let* 
            (
                (width (car (gimp-image-width image)))
                (height (car (gimp-image-height image)))
                (x-count 0)
                (y-count 0)
                (layer-copy 0)
                
            )
            ;(gimp-image-undo-disable image); DN = NO UNDO
            (gimp-context-push)
            (gimp-image-undo-group-start image)                   ;undo-group in one step
            
            (while (< x-count x-tiles)
                    ;copy original layer
                    (set! layer-copy (car(gimp-layer-copy layer TRUE)))
                    (gimp-image-insert-layer image layer-copy 0 0)
                    
                    ;move it to the right place
                    (gimp-layer-translate layer-copy 
                                            (* (car (gimp-drawable-width layer-copy)) x-count)
                                            0
                    )
                    
                    (set! x-count (+ x-count 1))
            )
            (gimp-image-resize-to-layers image)
            ;merge to do y-copies of it
            (define background-layer (car (gimp-image-merge-visible-layers image 0)))
            (plug-in-autocrop 1 image background-layer)
            
            ;set layer to be whatever the result background-layer is to do more copies in the y direction
            (set! layer background-layer)			
            (while (< y-count y-tiles)
                ;copy original layer
                    (set! layer-copy (car(gimp-layer-copy layer TRUE)))
                    (gimp-image-insert-layer image layer-copy 0 0)
                    
                    ;move it to the right place
                    (gimp-layer-translate layer-copy 
                                            0
                                            (* (car (gimp-drawable-height layer-copy)) y-count)
                    )
                (set! y-count (+ y-count 1))
            )
            ;resize canvas to layers just in case our rotations creates layers that are outside of canvas
            (gimp-image-resize-to-layers image)
            
            
            ;merge to do perspective on it
            (define background-layer (car (gimp-image-merge-visible-layers image 0)))
            (plug-in-autocrop 1 image background-layer)
            
            ;do this scaling so it's not so slow to call perspective
            (gimp-image-scale image (/ (* (car (gimp-image-width image)) scale-down-percentage) 100)
                                    (/ (* (car (gimp-image-height image)) scale-down-percentage) 100)
            )
            
            (if (= perspective TRUE)
                (begin
                    (define x0 0)
                    (define x1 (car (gimp-drawable-width background-layer)))
                    
                    (define y0 (- (car (gimp-drawable-height background-layer)) 
                                    (/ (* (car (gimp-drawable-height background-layer)) 
                                        height-percentage) 
                                        100
                                    )))
                    (define y1 y0)
                    
                    (define y2 (car (gimp-drawable-height background-layer)))
                    (define y3 y2)
                    
                    (define x2 (- (/ (car (gimp-drawable-width background-layer)) 2)
                                 (/ (/ (* (car (gimp-drawable-width background-layer)) 
                                      width-percentage) 
                                      100
                                    ) 2
                                 )))
                    (define x3 (+ (/ (car (gimp-drawable-width background-layer)) 2)
                                (/ (/ (* (car (gimp-drawable-width background-layer)) 
                                      width-percentage) 
                                      100
                                   ) 2
                                )))
                    (set! background-layer (car (gimp-item-transform-perspective background-layer
                                  x0 y0
                                  x1 y1
                                  x2 y2
                                  x3 y3)))
                    
                )
            )
            
            (gimp-displays-flush)
            ;(gimp-image-undo-enable image) ;DN = NO UNDO
            (gimp-image-undo-group-end image)                     ;undo group in one step
            (gimp-context-pop)
            
    )
    
    
    
) ;end of define
(script-fu-register
    "script-fu-mura-meister-perspective-tiling"         ;function name of the function defined above
    "<Image>/Script-Fu2/Mura Meister/Mura Meister Perspective Tiling..."    ;menu register (where can this script be access from inside GIMP)
    "Imitates Mura Meister Perspective Tiling. \n file:Mura_Meister_Perspective_Tiling.scm"       ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2016"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image"           0                   
    SF-DRAWABLE   "Layer"           0
    SF-ADJUSTMENT   "# of X tiles"          '(15 0 1000 1 50 0 0)
    SF-ADJUSTMENT   "# of Y tiles"          '(15 0 1000 1 50 0 0)
    SF-ADJUSTMENT   "Scale down to percentage before perspective"   '(20 0 100 1 10 0 0)
    SF-TOGGLE       "Apply Final Perspective"                       FALSE
    SF-ADJUSTMENT   "Perspective Horizon Height Percentage"         '(50 0 100 1 10 0 0)
    SF-ADJUSTMENT   "Perspective Near Width Percentage"             '(200 0 1000 1 10 0 0)
)

;end of script
