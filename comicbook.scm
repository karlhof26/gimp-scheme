
;*************************************************************************************** 
; Comic-Book1 script  for GIMP 2.10.24
; 
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
; ---------------------------------------------------------------------


(define (script-fu-Comic-Book1
            img
            drawable
    )
    
  (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (layer-type (car (gimp-drawable-type drawable)))
            (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL-LEGACY)))
            (layer-temp2 (car (gimp-layer-new img width height layer-type "temp2"  100 LAYER-MODE-NORMAL-LEGACY)))
            (layer-temp3 (car (gimp-layer-new img width height layer-type "temp3"  100 LAYER-MODE-NORMAL-LEGACY)))
       ) 
        
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-image-insert-layer img layer-temp3 0 -1)
    (gimp-image-insert-layer img layer-temp2 0 -1)
     (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp3 0)))
    
    (plug-in-photocopy 1 img layer-temp2 8.0 1.0 0.0 0.8)
    (plug-in-photocopy 1 img layer-temp3 24.54 1.0 0.0 0.8)
    ; was (gimp-levels layer-temp2 0 215 235 1.0 0 255) 
    (gimp-drawable-levels layer-temp2 0 0.843 0.921 1 1.0 0.0 1.0 1) 
    ; was (gimp-levels layer-temp3 0 123 212 .44 0 255) 
    (gimp-drawable-levels layer-temp3 0 0.482 0.831 1 0.44 0.0 1.0 1)
    (gimp-layer-set-mode layer-temp2 3)
    (gimp-layer-set-mode layer-temp3 3)
    ;was (gimp-levels layer-temp1 0 60 220 1.00 0 255)
    (gimp-drawable-levels layer-temp1 0 0.235 0.862 1 1.00 0.0 1.0 1)
    ;was (gimp-levels layer-temp1 0 25 225 2.25 0 255)
    (gimp-drawable-levels layer-temp1 0 0.098 0.882 1 2.25 0.0 1.0 1)
    (plug-in-newsprint 1 img layer-temp1  3 0 0 0 0 15 0 75 0 0 0 2)
    (gimp-image-merge-down img layer-temp3 0)
    (gimp-image-merge-down img layer-temp2 0)
    
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (= (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied; was eqv?
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Comic-Book")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    (gimp-message "Done!")
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-Comic-Book1"
    "<Image>/Script-Fu/Artistic/Comic-Book1..."
    "Creates a Comic Book Effect. \nfile:comicbook.scm"
    "Joe Kitella <joe.kitella@gmail.com>"
    "Joe Kitella"
    "2009, May"
    "RGB* GRAY*"
    SF-IMAGE      "Image"               0
    SF-DRAWABLE   "Drawable"            0
)

;*************************************************************************************** 
; Comic-Book2 script  for GIMP 2.2
; Copyright (C) 2009 Joe Kitella <joe.kitella@gmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-Comic-Book2
            img
            drawable
    )
    
    (gimp-image-undo-group-start img)
    
  (let* (
        (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (old-selection (car (gimp-selection-save img)))
        (image-type (car (gimp-image-base-type img)))
        (layer-type (car (gimp-drawable-type drawable)))
        (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 LAYER-MODE-NORMAL-LEGACY)))
        (layer-temp3 (car (gimp-layer-new img width height layer-type "temp3"  100 LAYER-MODE-NORMAL-LEGACY)))
       ) 
    
    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection FILL-WHITE)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-insert-layer img layer-temp1 0 -1)
    (gimp-image-insert-layer img layer-temp3 0 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp3 0)))
    
    (plug-in-photocopy 1 img layer-temp3 24.54 1.0 0.0 0.8)
    ; was (gimp-levels layer-temp3 0 123 212 .44 0 255)
    (gimp-drawable-levels layer-temp3 0 0.482 0.831 0 0.44 0.0 1.0 0)    
    (gimp-layer-set-mode layer-temp3 3)
    ; was (gimp-levels layer-temp1 0 60 220 1.00 0 255)
    (gimp-drawable-levels layer-temp1 0 0.235 0.862 1 1.00 0.0 1.0 1)
    (plug-in-cartoon 1 img layer-temp1 7.0 0.180)
    (plug-in-newsprint 1 img layer-temp1  3 0 0 0 0 15 0 75 0 0 0 2)
    (gimp-image-merge-down img layer-temp3 0)
    
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))
    
    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
            (gimp-edit-clear layer-temp1)
        )
    )
    
    (gimp-layer-set-name layer-temp1 "Comic-Book")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    (gimp-message "Done!")
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-Comic-Book2"
  "<Image>/Script-Fu/Artistic/Comic-Book2..."
  "Creates a Comic Book Effect. Type2 variation.  \nfile:comicbook.scm"
  "Joe Kitella <joe.kitella@gmail.com>"
  "Joe Kitella"
  "2009, May"
  "RGB* GRAY*"
  SF-IMAGE      "Image"             0
  SF-DRAWABLE   "Drawable"          0
)
