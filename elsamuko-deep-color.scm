; The GIMP -- an image manipulation program 
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
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
; along with this program; if not, write to the Free Software 
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
; 
; Copyright (C) 2010 elsamuko <elsamuko@web.de> 
;

(define (elsamuko-deep-color aimg adraw
                            color1 color2
                            desaturation overlay
                            gamma multiply
                            redtint grain
                            edge extra-dark)
  (let* (
            (img (car (gimp-item-get-image adraw)))
            (owidth (car (gimp-image-width img)))
            (oheight (car (gimp-image-height img)))
            (desat-layer (car (gimp-layer-copy adraw FALSE)))
            (multiply-layer (car (gimp-layer-copy adraw FALSE)))
            (multiply-layer2 (car (gimp-layer-copy adraw FALSE)))
            (multiply-layer2-mask (car (gimp-layer-create-mask multiply-layer2 ADD-MASK-WHITE)))
            (edge-layer (car (gimp-layer-copy adraw FALSE)))
            (red-layer  0)
            (overlay-layer1 (car (gimp-layer-copy adraw FALSE)))         
            (overlay-layer2 (car (gimp-layer-copy adraw FALSE)))         
            (yellow-layer (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            RGBA-IMAGE
                                            "Yellow Multiply" 
                                            10 
                                            LAYER-MODE-MULTIPLY-LEGACY)))
            (sepia-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Sepia" 
                                           50 
                                           LAYER-MODE-GRAIN-MERGE)))
            (grain-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           1
                                           "Grain" 
                                           100 
                                           LAYER-MODE-OVERLAY-LEGACY)))
            (grain-layer-mask (car (gimp-layer-create-mask grain-layer ADD-MASK-WHITE)))
         )
    
    ; init
    (define (set-pt a index x y)
      (begin
        (aset a (* index 2) x)
        (aset a (+ (* index 2) 1) y)
        )
    )
    (define (splineValue)
      (let* (
                (a (cons-array 6 'double))
            )
        (set-pt a 0 0.0 0.0)
        (set-pt a 1 0.502 (/ grain 255))
        (set-pt a 2 1.0 0.0)
        a
      )
    )
    
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
    )
    
    ;some more red
    (gimp-image-insert-layer img edge-layer 0 0)
    (gimp-item-set-name edge-layer "Edge Amp")
    (gimp-color-balance edge-layer TRANSFER-MIDTONES TRUE 20 0 0)
    (gimp-color-balance edge-layer TRANSFER-HIGHLIGHTS TRUE 20 0 0)
    
    (gimp-layer-set-mode edge-layer LAYER-MODE-GRAIN-EXTRACT)
    (gimp-edit-copy-visible img)
    (set! red-layer (car (gimp-layer-new-from-visible img img "Red")))
    (gimp-image-insert-layer img red-layer 0 0)
    (gimp-layer-set-mode red-layer LAYER-MODE-GRAIN-MERGE)
    (gimp-layer-set-opacity red-layer redtint)
    (gimp-drawable-invert red-layer FALSE)
    (gimp-item-set-visible edge-layer FALSE)
    
    ;set desaturated and multiply layer
    (gimp-image-insert-layer img desat-layer 0 -1)
    (gimp-item-set-name desat-layer "Desaturated")
    (gimp-layer-set-mode desat-layer LAYER-MODE-NORMAL-LEGACY)
    (gimp-layer-set-opacity desat-layer 100)
    (gimp-drawable-hue-saturation desat-layer HUE-RANGE-ALL 0 0 desaturation 2) ; overlap was 0
    
    (gimp-image-insert-layer img multiply-layer 0 -1)
    (gimp-item-set-name multiply-layer "Multiply 1")
    (gimp-layer-set-mode multiply-layer LAYER-MODE-MULTIPLY)
    (gimp-layer-set-opacity multiply-layer multiply)
    (gimp-drawable-hue-saturation multiply-layer HUE-RANGE-ALL 0 0 desaturation 5) ; overlap was 0
    
    (if(= extra-dark TRUE)
       (begin
         (gimp-image-insert-layer img multiply-layer2 0 -1)
         (gimp-item-set-name multiply-layer2 "Multiply 2")
         (gimp-layer-set-mode multiply-layer2 LAYER-MODE-MULTIPLY)
         (gimp-layer-set-opacity multiply-layer2 100)
         (gimp-drawable-desaturate multiply-layer2 DESATURATE-LUMINANCE)
         (gimp-layer-add-mask multiply-layer2 multiply-layer2-mask)
         
         (gimp-selection-all img)
         (gimp-edit-copy multiply-layer2)
         (gimp-floating-sel-anchor (car (gimp-edit-paste multiply-layer2-mask TRUE)))
         )
       )
    
    ;set overlay layers
    (gimp-image-insert-layer img overlay-layer1 0 -1)
    (gimp-item-set-name overlay-layer1 "Overlay 1")
    (gimp-layer-set-mode overlay-layer1 LAYER-MODE-OVERLAY)
    (gimp-layer-set-opacity overlay-layer1 overlay)
    (gimp-drawable-desaturate overlay-layer1 DESATURATE-LUMINANCE)
    (gimp-drawable-levels overlay-layer1 HISTOGRAM-VALUE 0.25 0.6 TRUE gamma 0.0 1.0 TRUE)  ; was 70 150 gamma
    
    (gimp-image-insert-layer img overlay-layer2 0 -1)
    (gimp-item-set-name overlay-layer2 "Overlay 2")
    (gimp-layer-set-mode overlay-layer2 LAYER-MODE-OVERLAY)
    (gimp-layer-set-opacity overlay-layer2 overlay)
    (gimp-drawable-desaturate overlay-layer2 DESATURATE-LUMINANCE)
    (gimp-drawable-levels overlay-layer2 HISTOGRAM-VALUE 0.25 0.61 TRUE gamma 0.0 1.0 TRUE)
    
    ;set yellow multiply layer
    (gimp-image-insert-layer img yellow-layer 0 -1)
    (gimp-selection-all img)
    (gimp-context-set-foreground color2)
    (gimp-edit-bucket-fill yellow-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
    
    ;set sepia grain merge layer
    (gimp-image-insert-layer img sepia-layer 0 -1)
    (gimp-selection-all img)
    (gimp-context-set-foreground color1)
    (gimp-edit-bucket-fill sepia-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
    
    ;move red tint layer to top
    (gimp-image-raise-item-to-top img red-layer)
    
    ;add grain
    (if (> grain 0)
       (begin
         ;fill new layer with neutral gray
         (gimp-image-insert-layer img grain-layer 0 -1)
         (gimp-drawable-fill grain-layer FILL-TRANSPARENT)
         (gimp-context-set-foreground '(128 128 128))
         (gimp-selection-all img)
         (gimp-edit-bucket-fill grain-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
         (gimp-selection-none img)
         
         ;add grain and blur it
         (plug-in-hsv-noise 1 img grain-layer 2 0 1 100) ; was 2 0 0 100
         (plug-in-gauss 1 img grain-layer 0.5 0.5 1)
         (gimp-layer-add-mask grain-layer grain-layer-mask)
         
         ;select the original image, copy and paste it as a layer mask into the grain layer
         (gimp-selection-all img)
         (gimp-edit-copy adraw)
         (gimp-floating-sel-anchor (car (gimp-edit-paste grain-layer-mask TRUE)))
         
         ;set color curves of layer mask, so that only gray areas become grainy
         (gimp-drawable-curves-spline grain-layer-mask  HISTOGRAM-VALUE  6 (splineValue))
         )
       )
    
    
    ;edge detection
    (if (= edge TRUE)
        (begin
            (gimp-image-raise-item-to-top img edge-layer)
            (gimp-item-set-visible edge-layer TRUE)
            (plug-in-sobel TRUE img edge-layer TRUE TRUE TRUE)
            (gimp-drawable-levels-stretch edge-layer)
            (gimp-drawable-desaturate edge-layer DESATURATE-LUMINANCE)         
            (gimp-drawable-invert edge-layer FALSE)
            (gimp-layer-set-mode edge-layer LAYER-MODE-MULTIPLY)
            (gimp-layer-set-opacity edge-layer 50)
         )
       )
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    ; we used an array so do a garbage collection
    (gc)
  )
)

(script-fu-register "elsamuko-deep-color"
    "Deep color"
    "Cinematic Deep Color effect. \nfile: elsamuko-deep-color.scm"
    "elsamuko <elsamuko@web.de>"
    "elsamuko"
    "04/03/10"
    "RGB*"
    SF-IMAGE       "Input image"          0
    SF-DRAWABLE    "Input drawable"       0
    SF-COLOR       "Grain Merge"           '(160 141 105)
    SF-COLOR       "Multiply"              '(241 229  31)
    SF-ADJUSTMENT  "Desaturation"          '(-60 -100 0  1 10 0 0)
    SF-ADJUSTMENT  "Overlay Opacity"       '( 60   0 100 1 10 0 0)
    SF-ADJUSTMENT  "Overlay Gamma"         '(  2  1 3 0.1 0.2 1 0)
    SF-ADJUSTMENT  "Multiply Opacity"      '( 50   0 100 1 10 0 0)
    SF-ADJUSTMENT  "Red Tint Opacity"      '( 80   0 100 1 10 0 0)
    SF-ADJUSTMENT  "Grain Strength"        '(128   0 255 1 10 0 0)
    SF-TOGGLE      "Edge Amplification"     TRUE
    SF-TOGGLE      "Extra Darkening"        FALSE
)

(script-fu-menu-register "elsamuko-deep-color" "<Toolbox>/Script-Fu/Artistic")

;end of script