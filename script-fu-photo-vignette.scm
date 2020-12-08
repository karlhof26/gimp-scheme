; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Photo Vignette script  for GIMP 2.10.22
;
;
; Tags: photo, vignette, effect
;
; Author statement:
;
;
;   - Changelog -
;  
;  Version 1.1 
;    - Made the script compatible with GIMP 2.10.22
; --------------------------------------------------------------------
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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; define vignette function
(define (script-fu-photo-vignette img drawable color opacity f_radius f_strength)
    (let* (
            (saveForegroundColor (car (gimp-context-get-foreground)))
            (imgWidth (car (gimp-image-width img)))
            (imgHeight (car (gimp-image-height img)))
            (newWidth)
            (newHeight)
            (maxLength)
            (minLength) ; unused
            (buffer)
            (xPos)
            (yPos)
            (newLayer)
            
            (gausradius 0)
          )
        
        ; ---------------------------------------------------------------
        (gimp-image-undo-group-start img)
        ; ---------------------------------------------------------------
        ; check inputs
        (set! f_strength (- 100 f_strength))
        ; ---------------------------------------------------------------
        ; convert gray/indexed images to rgb color
        (if (= (car (gimp-drawable-is-rgb drawable)) FALSE)
            (gimp-image-convert-rgb img)
        )
        ; set foreground color to black
        (gimp-context-set-foreground color)
        ; get max/min length
        (if (< imgHeight imgWidth)
            (begin
                (set! maxLength imgWidth)
                (set! minLength imgHeight)
            )
            (begin
                (set! maxLength imgHeight)
                (set! minLength imgWidth)
            )
        )
        ; calc buffer
        (set! buffer (* maxLength (/ f_radius 100)))
        ; calc new size
        (if (>= buffer 0)
            (begin
                (set! newHeight (+ maxLength (* 2 buffer)))
                (set! newWidth (+ maxLength (* 2 buffer)))
            )
            (begin
                (set! newHeight maxLength)
                (set! newWidth maxLength)
            )
        )
        
        ; calc resize position
        (set! xPos (/ (- newWidth imgWidth) 2))
        (set! yPos (/ (- newHeight imgHeight) 2))
        
        ; ---------------------------------------------------------------
        ; resize to new size
        (gimp-image-resize img newWidth newHeight xPos yPos)
        ; create new layer
        (set! newLayer (car (gimp-layer-new
            img
            (car (gimp-image-width img))
            (car (gimp-image-height img))
            RGBA-IMAGE
            "Vignette"
            100
            LAYER-MODE-NORMAL
            )
            )
        )
        
        (gimp-image-insert-layer img newLayer 0 -1)
        (gimp-selection-none img)
        (gimp-drawable-fill newLayer FILL-TRANSPARENT)
        ; ---------------------------------------------------------------
        ; create circle selection
        ;(gimp-ellipse-select
        ;    img
        ;    (abs (/ buffer 2))
        ;    (abs (/ buffer 2))
        ;    (+ maxLength buffer)
        ;    (+ maxLength buffer)
        ;    0 ;operation
        ;    0 ;antialias-bool
        ;    0 ;feather-bool
        ;    0 ;feather-radius
        ;)
        
        (gimp-image-select-ellipse
            img
            CHANNEL-OP-REPLACE
            (abs (/ buffer 2))
            (abs (/ buffer 2))
            (+ maxLength buffer)
            (+ maxLength buffer)
        )
        
        
        ; invert selection
        (gimp-selection-invert img)
        ;----------------------------------------------------------------
        ; fill selection with color
        (gimp-edit-fill newLayer FILL-FOREGROUND)
        (gimp-selection-clear img)
        
        ; Blur the vignette
        (if (> f_strength 0)
            (begin
                (set! gausradius (* maxLength (/ f_strength 100)))
                ;(gimp-message (number->string gausradius))
                (if (> gausradius 500)
                    (begin
                        (set! gausradius (/ gausradius 2))
                        (if (> gausradius 350)
                            (begin
                                (set! gausradius (/ gausradius 2))
                            )
                        )
                        
                    )
                )
                ; if still gr than 280 then drop it some more
                (if (> gausradius 280)
                            (begin
                                (set! gausradius (- gausradius 150))
                            )
                )
                ;(gimp-message (number->string gausradius))
                (plug-in-gauss-rle 1 img newLayer gausradius 1 1)
            )
        )
        
        ; set layer opacity
        (gimp-layer-set-opacity newLayer opacity)
        ; ---------------------------------------------------------------
        ; restore default image size
        (gimp-image-resize img imgWidth imgHeight (- xPos) (- yPos))
        ; shrink layer to image size
        (gimp-layer-resize-to-image-size newLayer)
        ; restore colors
        (gimp-context-set-foreground saveForegroundColor)
        ; ---------------------------------------------------------------
        (gimp-image-undo-group-end img)
        ; ---------------------------------------------------------------
        ; display result
        (gimp-displays-flush img)
        ; return values
        (list img drawable newLayer)
    )
)
; register vignette function
(script-fu-register
    "script-fu-photo-vignette" ;func name
    "Vignettierung (Randabschattung) ..." ;menu label
    "Creates a vignette around a photo. \nfile:script-fu-photo-vignette.scm" ;description
    "Christoph Zirkelbach" ;author
    "Christoph Zirkelbach" ;copyright notice
    "July 21, 2008" ;date created
    "" ;image type that the script works on
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Colour / Farbe" '(0 0 0)
    SF-ADJUSTMENT "Opacity / Deckkraft" '(70 0 100 1 10 1 0)
    SF-ADJUSTMENT "Radius (negative allowed)" '(2 -75 25 1 10 0 0)
    SF-ADJUSTMENT "Blur Strength / Härte" '(75 0 100 1 10 0 0)
)

(script-fu-menu-register "script-fu-photo-vignette" "<Toolbox>/Script-Fu/Decor/")


; end of script

