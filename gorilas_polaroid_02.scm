;
; Gorilas Polaroid 
;
; Do a polaroid style image like the one seen in "el hombre que comia diccionarios"
; http://www.elhombrequecomiadiccionarios.com/una-de-mis-semanas-favoritas-del-ano/
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
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;Version 1.01
;Made compatible with Gimp 2.4

; Define the function:

(define (script-fu-gorilas-polaroid inImage
            inLayer
            inBorderSize
            inBottomBorderSize
            inPolaroidColor
            inOffsetX
            inOffsetY
            inDropShadowRadius
            inShadowColor
            inShadowOpacity
            inCopy
            inFlatten
            inBackgroundColor
                )
    (let* (
            (theImage 0)
            (thePicture 0)
            (thePolaroidLayer 0)
            (theShadowLayer 0)
            (theBackgroundLayer 0)
            (polaroidWidth 0)
            (polaroidHeight 0)
            (theWidth 0)
            (theHeight 0)
            (thePictureBorder 0)
            (fullWidth 0)
            (fullHeight 0)
            (realXOffset 0)
            (realYOffset 0)
          )
        
        (gimp-selection-all inImage)
        (set! theImage (if (= inCopy TRUE)
            (car (gimp-image-duplicate inImage))
            inImage)
        )
        
        ;Starts the undo queque
        (if (= inCopy FALSE)
            (gimp-image-undo-group-start theImage)
            ()
        )
        (gimp-selection-none theImage)
        ;Convert to RGB
        (if (> (car (gimp-drawable-type inLayer)) 1)
            (gimp-image-convert-rgb theImage)
        )
        
        ;Picture Layer
        (set! thePicture
            (levhita-duplicate-layer theImage (car (gimp-image-get-active-drawable theImage)))
        )
        (gimp-item-set-name thePicture "picture")
        
        (set! theWidth (car (gimp-image-width inImage)))
        (set! theHeight (car (gimp-image-height inImage)))
        (set! polaroidWidth (+ theWidth inBorderSize))
        (set! polaroidHeight (+ theHeight (+ inBorderSize inBottomBorderSize)))
        
        ;Polaroid Layer
        (gimp-image-resize theImage
            polaroidWidth polaroidHeight
            (/ inBorderSize 2) (/ inBorderSize 2)
        )
        (set! thePolaroidLayer (car (gimp-layer-new theImage
            polaroidWidth
            polaroidHeight
            RGBA-IMAGE
            "polaroid"
            100
            LAYER-MODE-NORMAL
          ) )
        )
        (gimp-image-insert-layer theImage thePolaroidLayer 0 0)
        (gimp-context-set-background inPolaroidColor)
        (gimp-drawable-fill thePolaroidLayer FILL-BACKGROUND)
        
        ;Polaroid Shadow Layer (number 1)
        (set! theShadowLayer (car (gimp-layer-new theImage
            polaroidWidth
            polaroidHeight
            RGBA-IMAGE
            "shadow"
            inShadowOpacity
            LAYER-MODE-NORMAL
            ) ) 
        )
        (gimp-image-insert-layer theImage theShadowLayer 0 0)
        (gimp-context-set-background inShadowColor)
        (gimp-drawable-fill theShadowLayer FILL-BACKGROUND)
        
        ;Picture Border Layer
        (set! thePictureBorder (levhita-duplicate-layer theImage thePicture))
        (gimp-item-set-name thePictureBorder "picture border")
        (gimp-brightness-contrast thePictureBorder -127 0)
        (gimp-layer-resize-to-image-size thePictureBorder)
        (plug-in-gauss 1 theImage thePictureBorder 3 3 0)
        
        ;Final Resize
        (if (< inDropShadowRadius inOffsetX) ;X offset bigger than shadow
            (begin
                (set! fullWidth(+ inOffsetX (+ inDropShadowRadius polaroidWidth)))
                (set! realXOffset 0)
            )
            (begin
                (set! fullWidth (+ polaroidWidth (* inDropShadowRadius 2)))
                (set! realXOffset (- inDropShadowRadius inOffsetX))
            )
        )
        (if (< inDropShadowRadius inOffsetY) ;X offset bigger than shadow
            (begin
                (set! fullHeight(+ inOffsetY (+ inDropShadowRadius polaroidHeight)))
                (set! realYOffset 0)
            )
            (begin
                (set! fullHeight (+ polaroidHeight (* inDropShadowRadius 2)))
                (set! realYOffset (- inDropShadowRadius inOffsetY))
            )
        )
        (gimp-image-resize theImage fullWidth fullHeight realXOffset realYOffset)
        
        ;Background Layer
        (if (= inFlatten TRUE)
            (begin
                (set! theBackgroundLayer (car (gimp-layer-new theImage
                    fullWidth
                    fullHeight
                    RGBA-IMAGE
                    "background"
                    100
                    LAYER-MODE-NORMAL
                    ) )
                )
                (gimp-image-insert-layer theImage theBackgroundLayer 0 0)
                (gimp-context-set-background inBackgroundColor)
                (gimp-drawable-fill theBackgroundLayer FILL-BACKGROUND)
            )
            ()
        )
        
        ;Polaroid Shadow Layer (number 2)
        (gimp-layer-resize-to-image-size theShadowLayer)
        (gimp-layer-translate theShadowLayer inOffsetX inOffsetY)
        (plug-in-gauss 1 theImage theShadowLayer inDropShadowRadius inDropShadowRadius 0)
        
        ;Layer re-ordering
        (gimp-image-raise-layer-to-top theImage theShadowLayer)
        (gimp-image-raise-layer-to-top theImage thePolaroidLayer)
        (gimp-image-raise-layer-to-top theImage thePictureBorder)
        (gimp-image-raise-layer-to-top theImage thePicture)
        
        ;Should I flatten the Image?
        (if (= inFlatten TRUE)
            (gimp-image-flatten theImage)
            ()
        )
        ;Final Cleanup
        (if (= inCopy TRUE)
            (begin
                (gimp-image-clean-all theImage)
                (gimp-display-new theImage)
            )
            ()
        )
        
        ;Ends the undo queque
        (if (= inCopy FALSE)
            (gimp-image-undo-group-end theImage)
            ()
        )
        (gimp-displays-flush)
    )
)
	
;Function that duplicates a layer
(define (levhita-duplicate-layer image layer)
    (let* ((dup-layer (car (gimp-layer-copy layer 1))))
        (gimp-image-add-layer image dup-layer 0)
        dup-layer ; return dup layer
    )
)
	
;Register function on Gimp

(script-fu-register "script-fu-gorilas-polaroid"
    "Gorilas Polaroid 2..."
    "Do a polaroid style image like the one seen in el hombre que comia diccionarios http:www.elhombrequecomiadiccionarios.com/una-de-mis-semanas-favoritas-del-ano \nfile:gorilas-polaroid.scm"
    "Argel Arias"
    "2007 at Magallanes TI"
    "March 10 2007"
    "RGB* GRAY*"
    SF-IMAGE "The image" 0
    SF-DRAWABLE "The layer" 0
    SF-ADJUSTMENT _"Border size" '(30 0 300 1 10 0 1)
    SF-ADJUSTMENT _"Bottom border size" '(50 0 300 1 10 0 1)
    SF-COLOR _"Polaroid Color" '(242 242 242)
    SF-ADJUSTMENT _"Shadow X offset" '(2 0 4096 1 10 0 1)
    SF-ADJUSTMENT _"Shadow Y offset" '(2 0 4096 1 10 0 1)
    SF-ADJUSTMENT _"Blur radius" '(7 0 1024 1 10 0 1)
    SF-COLOR _"Color" '(0 0 0)
    SF-ADJUSTMENT _"Opacity" '(80 0 100 1 10 0 0)
    SF-TOGGLE _"Work on copy" TRUE
    SF-TOGGLE _"Flatten image" FALSE
    SF-COLOR _"Background Color" '(255 255 255)
)

;Register function on menu structure
(script-fu-menu-register "script-fu-gorilas-polaroid" "<Toolbox>/Script-Fu/Decor")
; end of file