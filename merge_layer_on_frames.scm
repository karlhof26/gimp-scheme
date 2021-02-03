;
; Merge Layer on Frames
;
; Takes the selected layer and merge it with all the other frames, in the front
; or in the back, optionally only merge it with visible layers.
;
; Argel Arias (levhita@gmail.com)
; http://blog.levhita.net

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

; Version 0.01
; Updated for GIMP-2.10.22 by karlhof26 (Nov 2020)

;TODO: make sure the duplicated layer is visible so the merge doesn't fail
; Define the function:

(define (script-fu-merge-layer-on-frames inImage
    inLayer
    inOnlyVisible
    inBackground
    inCopy
    )
    (let* (
            (theImage 0)
            (layerId 0)
            (layerList 0)
            (currentLayer 0)
            (number_of_layers 0)
            (dupLayer 0)
            (resultLayer 0)
            (layerName "")
          )
        
        (set! theImage
            (if (= inCopy TRUE)
                (car (gimp-image-duplicate inImage))
                inImage
            )
        )
        
        ;Starts the undo queque
        (if (= inCopy FALSE)
            (gimp-image-undo-group-start theImage)
            ()
        )
        
        (set! layerList (cadr (gimp-image-get-layers theImage)))
        (set! number_of_layers (vector-length layerList))
        (set! layerId 0)
        (gimp-progress-init "merging" -1)
        
        (while (< layerId number_of_layers) ; scan through all the layers 
            (begin
                (set! currentLayer (aref layerList layerId))
                (set! layerName (car (gimp-drawable-get-name currentLayer)))
                (if (= currentLayer  inLayer)
                    ();Only when currentLayer != inLayer
                    (begin
                        (if (and (= inOnlyVisible TRUE) (= TRUE (car (gimp-drawable-get-visible currentLayer))))
                            (begin
                                ;(gimp-message "line72")
                                (set! dupLayer (levhita-duplicate-layer-on theImage inLayer inBackground currentLayer inCopy))
                                (if (= TRUE inBackground)
                                    (set! resultLayer (gimp-image-merge-down theImage currentLayer 0))
                                    (set! resultLayer (gimp-image-merge-down theImage dupLayer 0))
                                )
                                (gimp-drawable-set-name (car resultLayer) layerName)
                            )
                            (if (= inOnlyVisible FALSE)
                                (begin
                                    (set! dupLayer (levhita-duplicate-layer-on theImage inLayer inBackground currentLayer inCopy))
                                    (if (= TRUE inBackground)
                                        (set! resultLayer (gimp-image-merge-down theImage currentLayer 0))
                                        (set! resultLayer (gimp-image-merge-down theImage dupLayer 0))
                                    )
                                    (gimp-drawable-set-name (car resultLayer) layerName)
                                )
                            )
                        )
                    )
                )
                (set! layerId (+ layerId 1))
                (gimp-progress-update (/ layerId number_of_layers))
            )
        )
        
        ;Final Cleanup
        (if (= inCopy TRUE)
            (begin
                (gimp-image-clean-all theImage)
                (gimp-display-new theImage)
            )
            ()
        )
        
        ;Ends the undo queue
        (if (= inCopy FALSE)
            (gimp-image-undo-group-end theImage)
            ()
        )
        (gimp-displays-flush)
    )
)
;Function that duplicates a layer
(define (levhita-duplicate-layer-on image layer onBackground referenceLayer inCopyFlag)
    (let* (
            (dup-layer (car (gimp-layer-copy layer TRUE)))
            (position 0 )
            (return 0)
            (owidth (car (gimp-image-width image)))
            (oheight (car (gimp-image-height image)))
            (returnlayer (car (gimp-layer-new image
                                         owidth
                                         oheight
                                         RGBA-IMAGE
                                         "returnLayer"
                                         100
                                         LAYER-MODE-NORMAL)))
            (return2layer)
          )
        (if (= onBackground FALSE)
            (set! position (car (gimp-image-get-item-position image referenceLayer)))
            (set! position (+ (car (gimp-image-get-item-position image referenceLayer)) 1) )
        )
        ;(gimp-message "insert")
        ;(gimp-message (number->string position))
        
        (if (= inCopyFlag TRUE)
            (begin
                ;(gimp-message "line140")
                (set! return (car (gimp-edit-copy layer))) 
                ;(gimp-message "line142")
                (gimp-image-insert-layer image returnlayer 0 position)
                ;(gimp-message "line144")
                (set! return2layer (gimp-edit-paste returnlayer FALSE))
                ;(gimp-message "line146")
                ;;;;(gimp-image-insert-layer image dup-layer 0 position)
                ;;;;(gimp-image-insert-layer image return2layer 0 position)
            )
            (begin 
                ;(gimp-message "line151")
                (gimp-image-insert-layer image dup-layer 0 position)
            )
        )
        ;(gimp-message "line155")
        return2layer
        ;dup-layer
    )
)

;Register function on Gimp
(script-fu-register "script-fu-merge-layer-on-frames"
    "Merge Layer on Frames..."
    "Takes the selected layer and merges it with all the other frames, in the front or in the back, optionally only merges it with visible layers. \nfile:merge_layer_on_frames.scm"
    "Argel Arias"
    "2012 at HackerGarage"
    "Oct 5 2012"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE       "The Image"          0
    SF-DRAWABLE    "The Layer"          0
    SF-TOGGLE     _"Merge only with visible layers"	TRUE
    SF-TOGGLE     _"Merge layer as Background"	TRUE
    SF-TOGGLE     _"Work on copy"	FALSE
    
)

;Register function on menu structure
(script-fu-menu-register "script-fu-merge-layer-on-frames"
    "<Image>/Script-Fu2/Animation")

;end of script