; GPLv3 
; This script was tested with Gimp 2.10.32
;
; New versions will be distributed from http://registry.gimp.org/ only
;
; This program is free software; you can redistribute it and/or modify
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
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function

(define (plastic-wrap inimage indraw radius nocopy)

    (let* (
            (flush-copy (car(gimp-layer-copy indraw 1)))
            (second-copy (car(gimp-layer-copy indraw 1)))
            (merge)
            (copy)
            (merge2)
            (layerCopy 1)
          )
        (gimp-image-undo-group-start inimage)

        (gimp-image-insert-layer inimage flush-copy 0 -1)
        (gimp-image-insert-layer inimage second-copy 0 -1)
        (gimp-layer-set-name flush-copy "flush copy")
        (gimp-layer-set-name second-copy "second copy")
        (gimp-drawable-desaturate flush-copy DESATURATE-LIGHTNESS)
        (gimp-drawable-desaturate second-copy DESATURATE-LIGHTNESS)
        (gimp-drawable-invert second-copy FALSE)
        ;(gimp-message "debug1")
        (gimp-progress-update 0.1)
        ;(gimp-displays-flush)
        (plug-in-neon 1 inimage  flush-copy radius 20) ; was radius 0 0=amount
        (plug-in-neon 1 inimage  second-copy radius 50) ; was radius 0 0=amount
        (gimp-layer-set-mode second-copy LAYER-MODE-OVERLAY-LEGACY) ; was 4=screen
        
        
        (set! merge (car (gimp-image-merge-down inimage second-copy 0))) ; 2=CLIP to bottom layer
        
        ;(gimp-message "debug2")   
        (gimp-progress-update 0.2)
        
        ;(set! copy (car (gimp-layer-new-from-drawable merge FALSE))) ; was layer-copy and 1=FALSE
        (set! copy (car (gimp-layer-copy merge FALSE)))
        
        (gimp-image-insert-layer inimage copy 0 -1)
        ;(gimp-message "debug3a")
        (gimp-progress-update 0.3)
        
        (gimp-layer-set-mode copy LAYER-MODE-DIVIDE) ; was 15(divide)  ; grain-merge
        (gimp-layer-set-name copy "copy")
        (gimp-layer-set-mode merge LAYER-MODE-GRAIN-MERGE)
        (gimp-layer-set-visible merge FALSE)
        (gimp-layer-set-name merge "merge")
        ;(gimp-message "debug3b")
        (gimp-progress-update 0.4)
        (gimp-displays-flush)
        
        
        
        (set! merge2 (gimp-image-merge-down inimage copy EXPAND-AS-NECESSARY))
        (set! layerCopy 1)
        (gimp-item-set-name merge "merge2")
        (gimp-displays-flush)
        (gimp-progress-update 0.5)
        
        ;(gimp-message "debug4")
        (gimp-progress-update 0.6)
        ; karlhofadded
        (gimp-layer-set-visible merge TRUE)
        
        (gimp-drawable-invert merge FALSE)
        ;(gimp-displays-flush)
        
        
        (gimp-layer-set-mode merge LAYER-MODE-SCREEN)
        ;(gimp-message "debug5")
        (gimp-progress-update 0.7)
        (gimp-displays-flush)
        
        
        (while (< layerCopy nocopy)
            
            (let* (
                    (newcopy (car (gimp-layer-copy merge FALSE)))
                  )
                ;(gimp-message "loop triggered")
                (gimp-progress-update 0.8)
                (gimp-image-insert-layer inimage newcopy 0 -1)
                
                (gimp-layer-set-mode newcopy LAYER-MODE-GRAIN-MERGE)
                (gimp-layer-set-opacity newcopy 35)
                (set! layerCopy (+ layerCopy 1))
            )
        )
        
        (gimp-progress-update 0.95)
        (gimp-displays-flush)
        (gimp-image-undo-group-end inimage)
        ;(gimp-message "debug - good end")
            
            
            
            
            
        
        
    )
    
)

(script-fu-register "plastic-wrap"
            "<Toolbox>/Script-Fu/Light and Shadow/Plastic-wrap..."
            "This Filter attempts to do the same as the pastic wrap filter.\n file:plastiwrap_02.scm"
            "Karl Ward"
            "Karl Ward"
            "SEPT 2007"
            "Licence GPL -http://www.gnu.org/licenses/gpl-3.0.txt"
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0
            SF-ADJUSTMENT "Neon Radius (10 recommended)"   '(10 1 100 1 2 0 1)
            SF-ADJUSTMENT "No. of final layer multiplier copies (darker)"    '(1 1 10 1 2 0 1)
)

;end of script