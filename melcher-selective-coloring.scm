; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; selective-colouring script  for GIMP 2.10.22
; Original author: Alexander Melcher (a.melchers@planet.nl)
; At xMedia, The Netherlands
;
; Tags: photo, selection, color
;
; Author statement:
;
; Allows you to change the overall color or the color balance of a selection
; and all the area around it at the same time, using two different adjustments.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
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

; Define the function

(define (script-fu-selective-coloring inImage
                                  inLayer
                                  inIntCR
                                  inIntMG
                                  inIntYB
                                  inIntApply
                                  inIntPreserveLum
                                  inIntColor
                                  inIntColorify
                                  inExtCR
                                  inExtMG
                                  inExtYB
                                  inExtApply
                                  inExtPreserveLum
                                  inExtColor
                                  inExtColorify
                                  inCopy
                                  inFlatten
    )
    (let* (
            (theImage (car (gimp-image-duplicate inImage)))
            (theLayer (car (gimp-layer-copy inLayer TRUE)))
          )
        
        (if (= (car (gimp-selection-is-empty inImage)) TRUE)
            (begin
                (gimp-message "This script needs a selection to work on.")
                (list -1 -1)
                (quit)
            )
        )
        
                ; Select the image; make a copy if needed
                ;(set! theImage (if (= inCopy TRUE)
                ;           (car (gimp-image-duplicate inImage))
                ;           inImage)
                ;           )
                ;)
                
                ; Group undo information
                (gimp-image-undo-group-start theImage)
                (gimp-image-undo-group-start inImage)
                
                ; If requested to flatten image, do it now
                (if (= inFlatten TRUE)
                    (gimp-image-flatten theImage)
                )
                (gimp-message "flatten OK")
                (if (= inFlatten TRUE)
                    (gimp-message "flatten TRUE")
                    (gimp-message "flatten FALSE")
                )
                (if (= inCopy TRUE)
                    (gimp-message "Copy TRUE")
                    (gimp-message "Copy FALSE")
                )
                ; Select the layer to which to apply the HSV changes
                (set! theLayer (if (= inFlatten TRUE)
                                    (aref (cadr (gimp-image-get-layers theImage)) 0)
                                    (if (= inCopy TRUE)
                                        (begin
                                            (car (gimp-image-get-active-layer theImage))
                                        )
                                        (begin 
                                            inLayer
                                        )
                            )
                           )
                )
                ;(gimp-display-new theImage)
                
                (gimp-message "invert")
                ; Invert the selection 
                (if (= inCopy TRUE)
                    (gimp-selection-invert theImage)
                    (gimp-selection-invert inImage)
                )
                
                (gimp-message "Colorify one")
                ; Colorify
                (if (= inExtColorify TRUE)
                    (plug-in-colorify TRUE theImage theLayer inExtColor)
                    (gimp-color-balance theLayer inExtApply
                                        inExtPreserveLum inExtCR
                                        inExtMG inExtYB)
                )
                (if (= inCopy FALSE)
                    (begin
                        (gimp-message "Copy FALSE- Colorify 1")
                        (if (= inExtColorify TRUE)
                            (plug-in-colorify TRUE inImage inLayer inExtColor)
                            (gimp-color-balance inLayer inExtApply
                                        inExtPreserveLum inExtCR
                                        inExtMG inExtYB)
                        )
                    )
                )
                
                
                ; Re-invert the selection to regain the original
                (gimp-selection-invert theImage)
                (if (= inCopy FALSE)
                    (begin
                        (gimp-message "Copy False - Invert")
                        (gimp-selection-invert inImage)
                    )
                )
                
                (gimp-message "Colorify two")
                ; Colorify
                (if (= inIntColorify TRUE)
                    (plug-in-colorify TRUE theImage theLayer inIntColor)
                    (gimp-color-balance theLayer inIntApply
                                    inIntPreserveLum inIntCR
                                    inIntMG inIntYB)
                )
                (if (= inCopy FALSE)
                    (begin
                        (gimp-message "Copy FALSE- Colorify 2")
                        (if (= inIntColorify TRUE)
                            (plug-in-colorify TRUE inImage inLayer inIntColor)
                            (gimp-color-balance inLayer inIntApply
                                    inIntPreserveLum inIntCR
                                    inIntMG inIntYB)
                        )
                    )
                )
                ; Group undo information
                (gimp-image-undo-group-end theImage)
                (gimp-image-undo-group-end inImage)
                
                ; If a copy was made, show the new image
                (if (= inCopy TRUE)
                    (begin
                        (gimp-image-clean-all theImage)
                        (gimp-display-new theImage)
                    )
                    (begin
                        (gimp-displays-flush)
                    )
                )
                
                (gimp-message "finish OK")
                ; Force updates
                (gimp-displays-flush)
                
                ; Return the results
                ;(list theImage inLayer)
            
        
    )
)

; Register script-fu-selective-coloring

(script-fu-register  "script-fu-selective-coloring"
    "<Toolbox>/Script-Fu2/Selection Effects/xMedia Selective Coloring"
    "Allows you to change the overall color or the color balance of a selection and all the area around it
    at the same time, using two different adjustments. \nColorify overrides balancing. \nFirst settings for
    inside selection, second set affect outside selection.\n If all zeros then no effect. \nfile:melcher-selective-coloring.scm"
    "Alexander Melchers"
    "2002, Alexander Melchers, xMedia"
    "8th November 2002"
    "RGB*"
    SF-IMAGE      "The Image"           0
    SF-DRAWABLE   "The Layer"           0
    SF-ADJUSTMENT "Internal Cyan/Red"   '(30 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Magenta/Green"       '(-30 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Yellow/Blue"         '(0 -100 100 1 1 0 0 0)
    SF-OPTION     "Apply To"            '("Shadows" "Midtones" "Highlights")
    SF-TOGGLE     "Preserve Luminosity" TRUE
    SF-COLOR      "Color"               '(255 255 0)
    SF-TOGGLE     "Colorify - selected area"            FALSE
    SF-ADJUSTMENT "External Cyan/Red"   '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Magenta/Green"       '(0 -100 100 1 1 0 0 0)
    SF-ADJUSTMENT "Yellow/Blue"         '(0 -100 100 1 1 0 0 0)
    SF-OPTION     "Apply To"            '("Shadows" "Midtones" "Highlights")
    SF-TOGGLE     "Preserve Luminosity" TRUE
    SF-COLOR      "Color"               '(255 255 0)
    SF-TOGGLE     "Colorify - unselected with settings above."            FALSE
    SF-TOGGLE     "Work on Copy"        TRUE
    SF-TOGGLE     "Flatten Image"       FALSE
)

;end of script