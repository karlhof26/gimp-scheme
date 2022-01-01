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

(define (elsamuko-sunny-landscape aimg adraw
                                  brightnessIn
                                  ylight glight
                                  desat
                                  sharpen
                                  sunshine sky
                                  num1 num2
                                  wb)
  (let* ((img (car (gimp-item-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         
         (working-layer (car (gimp-layer-copy adraw TRUE)))
         (extract-layer (car (gimp-layer-copy adraw FALSE)))
         (wb-layer 0)
         (desat-layer 0)
         (tmplayer1 0)         
         (tmplayer2 0)         
         (color-layer 0)
         (brightness (- 60 brightnessIn))
         (extra 0) ;(not yet used)
         (offset1 (* oheight (/ num1 100)))
         (offset2 (* oheight (/ num2 100)))
         
         (overlay-layer (car (gimp-layer-new img
                                             owidth 
                                             oheight
                                             1
                                             "Yellow Cast" 
                                             60 
                                             LAYER-MODE-OVERLAY)))
         (overlay-layer-mask (car (gimp-layer-create-mask overlay-layer ADD-MASK-WHITE)))
         
         
         (sub-blue-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              1
                                              "Sub Blue" 
                                              100 
                                              LAYER-MODE-NORMAL)))
         (sub-blue-layer-mask (car (gimp-layer-create-mask sub-blue-layer ADD-MASK-WHITE)))
         
         (top-blue-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              1
                                              "Top Blue" 
                                              65 
                                              LAYER-MODE-NORMAL)))
         (top-blue-layer-mask (car (gimp-layer-create-mask top-blue-layer ADD-MASK-WHITE)))
         
         (cloud-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           1
                                           "Clouds" 
                                           60 
                                           LAYER-MODE-SCREEN)))
         (cloud-layer-mask (car (gimp-layer-create-mask cloud-layer ADD-MASK-WHITE)))
         
         
         )
    
    ; init
    (define (set-pt a index x y)
      (begin
        (aset a (* index 2) x)
        (aset a (+ (* index 2) 1) y)
      )
    )
    (define (spline-brightness)
      (let* (
                ;(a (cons-array 8 'byte)))
                (a (cons-array 8 'double))
            )
        ;(set-pt a 0 0 0)
        ;(set-pt a 1 brightness 33)
        ;(set-pt a 2 127 199)
        ;(set-pt a 3 255 255)
        
        (set-pt a 0 0.0 0.0)
        (set-pt a 1 (/ brightness 256) 0.13)
        (set-pt a 2 0.50 0.65)
        (set-pt a 3 1.0 1.0)
        
        a
      )
    )
    
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )
    
    ;extra color layer (not yet used)
    (if(= extra 1)
       (begin
         (gimp-image-insert-layer img extract-layer 0 0)
         (gimp-drawable-desaturate extract-layer DESATURATE-LIGHTNESS)
         (gimp-layer-set-mode extract-layer LAYER-MODE-GRAIN-EXTRACT)
         (gimp-edit-copy-visible img)
         (set! color-layer (car (gimp-layer-new-from-visible img img "Color") ))
         (gimp-image-insert-layer img color-layer 0 0)
         (gimp-layer-set-mode color-layer LAYER-MODE-GRAIN-MERGE)
         (gimp-item-set-visible color-layer FALSE)
         (gimp-image-remove-layer img extract-layer)
         )
       )
    
    ;sharpen + adjust contrast
    (gimp-image-insert-layer img working-layer 0 -1)
    (if(> sharpen 0) (plug-in-unsharp-mask 1 img working-layer 1 sharpen 5))
    (gimp-item-set-name working-layer "Process Copy")
    ;;(gimp-drawable-curves-spline working-layer HISTOGRAM-VALUE 8 (spline-brightness))
    (gimp-drawable-curves-spline working-layer HISTOGRAM-VALUE 8 (spline-brightness))
    ;(gimp-curves-spline working-layer HISTOGRAM-VALUE 8 (spline-brightness))
    ;hues
    (if(> ylight 0) (gimp-drawable-hue-saturation working-layer HUE-RANGE-YELLOW 0 ylight 0 0))
    (if(> glight 0) (gimp-drawable-hue-saturation working-layer HUE-RANGE-GREEN 0 glight 0 0))
    
    ;desaturate a bit
    (set! desat-layer (car (gimp-layer-copy working-layer FALSE)))
    (gimp-image-insert-layer img desat-layer 0 -1)
    (gimp-item-set-name desat-layer "Desaturate")
    (gimp-drawable-desaturate desat-layer DESATURATE-LIGHTNESS)
    (gimp-layer-set-opacity desat-layer desat)
    
    ;add some sunshine
    (gimp-image-insert-layer img overlay-layer 0 -1)
    (gimp-context-set-foreground sunshine)
    (gimp-selection-all img)
    (gimp-edit-bucket-fill overlay-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
    (gimp-edit-copy working-layer)
    (gimp-layer-add-mask overlay-layer overlay-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste overlay-layer-mask TRUE)))
    (gimp-drawable-invert overlay-layer-mask TRUE)
    (gimp-drawable-levels-stretch overlay-layer-mask)
    (gimp-selection-none img)
    
    
    ;add 1st artificial sky 
    (gimp-image-insert-layer img sub-blue-layer 0 -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground sky)
    (gimp-edit-blend sub-blue-layer BLEND-FG-TRANSPARENT
                     LAYER-MODE-NORMAL GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset2 0 offset1)
    (gimp-edit-copy working-layer)
    (gimp-layer-add-mask sub-blue-layer sub-blue-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste sub-blue-layer-mask TRUE)))
    (gimp-drawable-levels-stretch sub-blue-layer-mask)
    (gimp-selection-none img)
    
    ;add clouds
    (gimp-image-insert-layer img cloud-layer 0 -1)
    (gimp-edit-copy working-layer)
    (gimp-layer-add-mask cloud-layer cloud-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste cloud-layer-mask TRUE)))
    (gimp-drawable-levels-stretch cloud-layer-mask)
    (gimp-selection-none img)
    (plug-in-solid-noise 1 img cloud-layer 0 0 (rand 1000) 4 3.5 7.0)
    (gimp-context-set-foreground sky)
    (gimp-selection-all img)
    (gimp-edit-bucket-fill cloud-layer BUCKET-FILL-FG LAYER-MODE-OVERLAY 100 0 FALSE 0 0)
    (gimp-selection-none img)
    
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-background '(0 0 0))
    (gimp-edit-blend cloud-layer BLEND-FG-BG-RGB
                     LAYER-MODE-MULTIPLY-LEGACY GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     FALSE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
    
    ;add 2nd artificial sky
    (gimp-image-insert-layer img top-blue-layer 0 -1)
    (gimp-edit-copy sub-blue-layer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste top-blue-layer TRUE)))
    (gimp-layer-add-mask top-blue-layer top-blue-layer-mask)
    (gimp-edit-copy sub-blue-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste top-blue-layer-mask TRUE)))
    
    ;more color (not yet used)
    (if(= extra 1)
       (begin
         (gimp-image-raise-item-to-top img color-layer)
         (gimp-item-set-visible color-layer TRUE)
         )
       )
    
    ;white balance result
    (if(= wb 1)
       (begin
         (gimp-edit-copy-visible img)
         (set! tmplayer1 (car (gimp-layer-new-from-visible img img "Temp 1")))
         (set! tmplayer2 (car (gimp-layer-new-from-visible img img "Temp 2")))
         (gimp-image-insert-layer img tmplayer1 0 -1)
         (gimp-image-insert-layer img tmplayer2 0 -1)
         (gimp-drawable-levels-stretch tmplayer1)
         (gimp-layer-set-mode tmplayer2 LAYER-MODE-GRAIN-EXTRACT-LEGACY)
         (gimp-edit-copy-visible img)
         (set! wb-layer (car (gimp-layer-new-from-visible img img "White Balance")))
         (gimp-image-insert-layer img wb-layer 0 -1)
         (gimp-layer-set-mode wb-layer LAYER-MODE-GRAIN-MERGE-LEGACY)
         (gimp-image-remove-layer img tmplayer1)
         (gimp-image-remove-layer img tmplayer2)
         (gimp-layer-set-opacity wb-layer 60)
         )
       )
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-sunny-landscape"
                    "Sunny Landscape"
                    "Artificial sunshine effect. \nfile:elsamuko-sunny-landscape.scm"
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "30/03/10"
                    "RGB*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    SF-ADJUSTMENT  "Brightness"           '(10 0 30 1 5 0 0)
                    SF-ADJUSTMENT  "Yellow Lightness"     '(5 0 30 1 5 0 0)
                    SF-ADJUSTMENT  "Green Lightness"      '(5 0 30 1 5 0 0)
                    SF-ADJUSTMENT  "Desaturate"           '(30 0 100 5 10 0 0)
                    SF-ADJUSTMENT  "Sharpen"              '(0.5 0 1 0.1 0.2 1 0)
                    SF-COLOR       "Sunshine Color"       '(255 255 0)
                    SF-COLOR       "Sky Color"            '(85 125 204)
                    SF-ADJUSTMENT  "Sky Gradient Begin"   '(20 -100 200 1 10 1 0)
                    SF-ADJUSTMENT  "Sky Gradient End"     '(60 -100 200 1 10 1 0)
                    SF-TOGGLE      "White Balance Result"  TRUE
                    ;SF-COLOR      _"Cloud Color"          '(0 0 255)
                    ;SF-ADJUSTMENT _"Yellow Hues"          '(20 0 50 1 5 0 0)                
                    ;SF-ADJUSTMENT _"Red Hues"             '(30 0 50 1 5 0 0)                
                    )

(script-fu-menu-register "elsamuko-sunny-landscape" "<Image>/Script-Fu/Light and Shadow")
