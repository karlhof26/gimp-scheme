;
; distress selection bb
;
;
; Chris Gutteridge (cjg@ecs.soton.ac.uk) 
; At ECS Dept, University of Southampton, England.

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Updated by Karl Hofmeyr(karlhofmeyr@gmail.com) on 02/01/2022.

; Define the function:

(define (script-fu-distress-selection-bb  inImage
                                      inDrawable
                                      inThreshold
                                      inSpread
                                      inGranu
                                      inSmooth
                                      inSmoothH
                                      inSmoothV
        )
    
  (let* (
            (firstdummy 0)
            (theImage (car (gimp-image-duplicate inImage)))
            (theWidth (car (gimp-image-width inImage)))
            (theHeight (car (gimp-image-height inImage)))
            (theLayer 0)
            (theMode (car (gimp-image-base-type inImage)))
            (prevLayer (car (gimp-image-get-active-layer inImage)))
            (floattest 0)
            (difference 0)
            (ratio 0)
            (channel1)
       )
    
        (gimp-context-push)
        (gimp-context-set-defaults)
        (gimp-image-undo-group-start inImage) ; was theImage
        
        (if (= theMode GRAY)
            (set! theMode GRAYA-IMAGE)
            (set! theMode RGBA-IMAGE)
        )
        
        ;(gimp-message (number->string (rand 20)))
        ;(gimp-message (number->string (rand 50)))
        
        (if (> inSpread 200)
            (begin
                ;(gimp-message (number->string inSpread))
                (set! difference (- inSpread 200))
                (set! ratio (/ inSpread 1000))
                ;(gimp-message (number->string ratio))
                (set! inSpread (* 200 ratio))
                ;(gimp-message (number->string inSpread))
                (if (> inSpread 200)
                    (begin
                        (gimp-message "still gr than 200")
                        (set! inSpread (* 0.80 inSpread))
                    )
                )
            )
            (begin
                ;(gimp-message "less than 200") 
            )
        )
        (while (> inGranu 25)
            ;(gimp-message "inGranu more than 25")
            (set! inGranu (* inGranu 0.85))
            ;(gimp-message (number->string inGranu))
        )
        (set! theLayer (car (gimp-layer-new inImage
                                        theWidth
                                        theHeight
                                        theMode
                                        "Distress Scratch Layer"
                                        100
                                        LAYER-MODE-NORMAL)))
        
        (gimp-image-insert-layer inImage theLayer 0 0)
        
        (if (= (car (gimp-selection-is-empty inImage)) FALSE)
            (gimp-edit-fill theLayer FILL-BACKGROUND)
        )
        
        (gimp-selection-invert inImage)
        
        (if (= FALSE (car (gimp-selection-is-empty inImage)))
            (gimp-edit-clear theLayer)
        )
        
        ;;  (gimp-message "test running")
        (gimp-selection-invert inImage)
        (gimp-selection-none inImage)
        
        ;(gimp-message "theHeight is")
        ;(gimp-message (number->string theHeight))
        (if (> inGranu theHeight)
            (begin
                (set! inGranu (* theHeight 0.8))
                ;(gimp-message "inGranu change")
                ;(gimp-message (number->string inGranu))
            )
        )
        (if (> inGranu theWidth)
            (begin
                (set! inGranu (* theWidth 0.8))
                ;(gimp-message "inGranu change")
                ;(gimp-message (number->string inGranu))
            )
        )
        ;(gimp-message "inGranu before layer scale")
        ;(gimp-message (number->string inGranu))
        
        (gimp-layer-scale theLayer
                      (/ theWidth inGranu)
                      (/ theHeight inGranu)
                      TRUE) 
        
        (plug-in-spread RUN-NONINTERACTIVE
                    inImage
                    theLayer
                    inSpread
                    inSpread)
    
        (plug-in-gauss-iir RUN-NONINTERACTIVE
            inImage theLayer inSmooth inSmoothH inSmoothV)
        (gimp-layer-scale theLayer theWidth theHeight TRUE)
        (plug-in-threshold-alpha RUN-NONINTERACTIVE inImage theLayer inThreshold)
        (plug-in-gauss-iir RUN-NONINTERACTIVE inImage theLayer 1 TRUE TRUE)
        
        ;(gimp-message "test got here")
        (gimp-image-select-item inImage CHANNEL-OP-REPLACE theLayer)
        
        ;(gimp-message "test for floating sel")
        (set! floattest (car (gimp-layer-is-floating-sel theLayer)))
        ;(gimp-message (number->string floattest))
        ;(gimp-message (number->string theLayer))
        (if (= 1 floattest)
            (begin
                ;(gimp-message "anchoring the floating sel")
                (gimp-floating-sel-anchor theLayer)
            )
        )
        ;;(gimp-floating-sel-anchor theLayer)
        
        ;(gimp-image-remove-layer theImage theLayer)
        (if (and (= (car (gimp-item-is-channel inDrawable)) TRUE)
             (= (car (gimp-item-is-layer-mask inDrawable)) FALSE))
            (begin
                ;(gimp-message "about to set active channel")
                ;(gimp-image-set-active-channel inImage inDrawable)
                (if (= (car(gimp-image-get-active-channel inDrawable)) -1)
                    (begin
                        (gimp-message "no channel")
                    )
                    (begin
                        (set! channel1 (car (gimp-image-get-active-channel inImage)))
                        (gimp-image-set-active-channel inImage channel1)
                    )
                )
            )
            (begin
                ;(gimp-message "other side of if")
            )
        )
        ;(gimp-message "near end")
        
        (if (= prevLayer -1)
            (begin
                (gimp-message "no active layer")
            )
            (begin
                ;(gimp-message "there was an active layer")
                ;(gimp-message (number->string prevLayer))
                (gimp-image-set-active-layer inImage prevLayer)
                (gimp-item-set-visible theLayer FALSE)
            )
        )
        ;(gimp-image-set-active-layer theImage prevLayer)
        
        ;    (gimp-image-undo-group-end theImage)     
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush)
        (gimp-context-pop)
        (gimp-message "good finish of Distress selection")
  )
)


(script-fu-register "script-fu-distress-selection-bb"
    "Distress selection BB"
    "Distress the selection. \nfile:distress-selectionbb_02.scm"
    "Chris Gutteridge"
    "Chris Gutteridge, ECS dept, University of Southampton, England"
    "23rd April 1998"
    "*"     ; RGB*,GRAY*
    SF-IMAGE       "The image"                  0
    SF-DRAWABLE    "The layer"                  0
    SF-ADJUSTMENT  "Threshold bigger 1 to 254 smaller"  '(127 1 254 1 10 0 0)
    SF-ADJUSTMENT  "Spread"                     '(8 0 1000 1 10 0 1)
    SF-ADJUSTMENT  "Granularity 1 is low"       '(3 1 25 1 10 0 1)  ; was 25 as a max
    SF-ADJUSTMENT  "Smooth"                     '(2 1 150 1 10 0 1)
    SF-TOGGLE      "Smooth horizontally"        TRUE
    SF-TOGGLE      "Smooth vertically"          TRUE
)

(script-fu-menu-register "script-fu-distress-selection-bb" "<Image>/Select/ModifySel/")

; end of script 
