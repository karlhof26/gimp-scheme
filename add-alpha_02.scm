;
;   Add Alpha Channel to All Frames ;
;   
;    Takes all the frames of the image and add Alpha Channel on them
;   
;    Argel Arias (levhita@gmail.com)
;    http://blog.levhita.net
;    
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;   
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;   GNU General Public License for more details.
;   
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to the Free Software
;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;    
;   Version 0.01
;   
;   
;   Define the function:
    
(define (script-fu-add-alpha-anim inImage
                            inLayer
                            inOnlyVisible
                            inCopy
        )
  (let* (
            (theImage 0)
            (layerId 0)
            (layerList 0)
            (currentLayer 0)
            (number_of_layers 0)
        )
        
    ;(gimp-message "started1")
    (set! theImage (if (= inCopy TRUE)
                      (car (gimp-image-duplicate inImage))
                      0
                    ) ; was extra inImage here
    )
    
    ;Starts the undo queque
    
    (gimp-image-undo-group-start theImage)
    
    ;(gimp-message "started2")
    (set! layerList (cadr (gimp-image-get-layers theImage)))
    ;(gimp-message "line 50")
    (set! number_of_layers (vector-length layerList))
    ;(gimp-message "line 52")
    (if (= number_of_layers 0)
        (begin
            (gimp-message "layers is nil")
            (quit)
        )
    )
    ;(gimp-message "line 59 starting loop")
    (while (< layerId number_of_layers); scan through all the layers
        (begin
            (set! currentLayer (aref layerList layerId))
            (if (and (= TRUE inOnlyVisible) (= FALSE (car (gimp-drawable-get-visible currentLayer))))
                ()
                (begin
                    (gimp-layer-add-alpha currentLayer)
                )
            )
            (set! layerId (+ layerId 1))
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
    ;(gimp-message "line 84")
    ;Ends the undo queque
    
    (gimp-image-undo-group-end theImage)
    
    (gimp-displays-flush)
    (gimp-message "Good finish OK")
  )
)
    
    ;Register function on Gimp
(script-fu-register "script-fu-add-alpha-anim"
    "Add Alpha Channel to All Layers..."
    "Takes all the frames of the image and add Alpha Channel on them \n file: add-alpha_02.scm"
    "Argel Arias"
    "2012 at HackerGarage"
    "Oct 5 2012"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE "The image"                    0
    SF-DRAWABLE "The layer"                 0
    SF-TOGGLE   "Add only in visible layers" TRUE
    SF-TOGGLE   "Work on copy (must be T)"               TRUE
)

;Register function on menu structure
(script-fu-menu-register "script-fu-add-alpha"
    "<Image>/Script-Fu2/Animation/")
  
;end of script