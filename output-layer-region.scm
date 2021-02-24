; (c) 2011 nanahositento, under the MIT license.

;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Output Layer Region updated for Gimp 2.10.20
; Copyright (c) 2011 nanahositento
;
; Tags: layer metadata
;
; Author statement:
;
; Outputs metada for each layer to a CSV file. 
;
; --------------------------------------------------------------------
; 
; --------------------------------------------------------------------
;   - Changelog -
;  Version 1.   
;    - Made the script compatible with GIMP 2.10.22
;
; --------------------------------------------------------------------
;
;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files 
;(the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to 
; do so, subject to the following conditions:
;
;The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
;WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-output-layer-region
    image
    drawable
    dirname
    filename)
    
    (define (get-layer-region layer)
        (let* (
                (name (car (gimp-layer-get-name layer)))
                (offset (gimp-drawable-offsets layer))
                (x (car offset))
                (y (cadr offset))
                (w (car (gimp-drawable-width layer)))
                (h (car (gimp-drawable-height layer))))
                (string-append
                    name ","
                    (number->string x) "," (number->string y) ","
                    (number->string w) "," (number->string h))
        )
    )
    
    (define (for-each-layer layers i num file)
        (if (< i num)
            (let* ((layer (vector-ref layers i)))
                (display (get-layer-region layer) file)
                (display "\n" file)
                (for-each-layer layers (+ i 1) num file)
            )
        )
    )
    
    (let* (
            (layer_num (car (gimp-image-get-layers image)))
            (layers (cadr (gimp-image-get-layers image)))
            (path (string-append dirname "/" filename))
            (file (open-output-file path))
          )
        
        (for-each-layer layers 0 layer_num file)
        (close-output-port file)
    )
)

(script-fu-register
    "script-fu-output-layer-region"
    "Output layer region to file"
    "Output the layer region metadata data to a CSV file. Shows x y width height for each layer. \nfile:output-layer-region.scm"
    "nanahositento"
    "(c) 2011 nanahositento"
    "2011-06-12"
    "*"
    SF-IMAGE    "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-DIRNAME  "Output directory" ""
    SF-STRING   "File name" "list.csv")

(script-fu-menu-register "script-fu-output-layer-region"
    "<Toolbox>/Script-Fu/Setup/Layer")

;end of script