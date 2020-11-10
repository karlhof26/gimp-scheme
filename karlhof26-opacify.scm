; GPL v3
;
; modified by karlhof26
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;
; Changed on June 6 2020 by karlhof26
; Updated for GIMP 2.10.18

(define (opacify-aa image drawable)
    (let* (
            (result 5)
            (lay-width (car (gimp-drawable-width drawable)))
            (lay-height (car (gimp-drawable-height drawable)))
            (resultlayer (car (gimp-layer-new image lay-width lay-height RGBA-IMAGE "OpacifyLayer" 100 LAYER-MODE-NORMAL)))
            (threshlayer (car (gimp-layer-copy drawable TRUE)))
            (resultlayer2 (car (gimp-layer-copy drawable TRUE)))
          )
        (gimp-image-undo-group-start image)
        (gimp-context-push)
        
        (gimp-image-insert-layer image threshlayer 0 -1)
        (gimp-drawable-threshold threshlayer HISTOGRAM-VALUE 0.42 1.0)
        (plug-in-colortoalpha 1 image threshlayer '(0 0 0))
        (gimp-layer-set-opacity threshlayer 46.1) ; was 40.1
        (gimp-displays-flush)
        
        (gimp-image-insert-layer image resultlayer2 0 1)
        ;(set! result (car (gimp-edit-copy drawable)))
        ;(gimp-message (number->string result))
        ;(gimp-edit-paste resultlayer FALSE)
        ;(gimp-floating-sel-anchor resultlayer)
        (gimp-layer-set-opacity resultlayer2 60.1)
        (gimp-layer-set-mode resultlayer2 LAYER-MODE-SCREEN)
        (gimp-drawable-posterize resultlayer2 5)
        ; 
        
        ;(gimp-image-clean-all image)
        (gimp-context-pop)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        ;(list image drawable)
        ;
        ;(gimp-selection-clear image)
    )
)

(script-fu-register "opacify-aa"
    "<Image>/Script-Fu3/MTools/Opacify"
    "Opacify the image. Threshold the drawable, opacity change and paste the result to a new drawable. \nfile:opacify.scm"
    "Karl Hofmeyr"
    "2020 Karl Hofmeyr"
    "12 November 2020"
    "*"
    SF-IMAGE "Input Image"          0
    SF-DRAWABLE "Input Drawable"    0
)
; check for if already applied
; doesn't work if more than 2 main colors

;end of script