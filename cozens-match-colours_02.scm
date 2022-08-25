; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Match colors script  for GIMP 2.4
; Copyright (C) 2007 Kevin Cozens
;
; Tags: colors
;
; Author statement: 
;
;This script matches the colours in one single layer image to the colours in
;another image. It is an implementation of the method described by xooorx at:
;http://www.flickr.com/groups/gimpusers/discuss/72157600180607090/
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


(define (script-fu-match-colors img drawable master-img decompose)
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (master-img2 (car (gimp-image-duplicate master-img)))
            (layer-copy 0)
            (decomposed-new 0)
            (decomposed-old 0)
            (layers 0)
            
            (buffer)
            (layer-copy1 0)
            (layer-copy2 0)
            (decomposetype "")
        )
        
        ; Start undo group    
        (gimp-image-undo-group-start img)
        ;(gimp-image-undo-group-start master-img)
        
        (cond
            ((= decompose 0)
                (set! decomposetype "YCbCr_ITU_R470_256")
            )
            ((= decompose 1)
                (set! decomposetype "LCH")
            )
            ((= decompose 2)
                (set! decomposetype "RGB")
            )
            ((= decompose 3)
                (set! decomposetype "HSL")
            )
            ((= decompose 4)
                (set! decomposetype "YCbCr_ITU_R709_256")
            )
            (else 
                (set! decomposetype "YCbCr_ITU_R470")
            ); 
        )
        ;Add checks to make sure each image only has a single layer.  
        ;(gimp-message (number->string width))
        
        ; step1
        (gimp-image-scale master-img width height)
        ;(gimp-image-scale master-img 50 50)
        
        ;Step 0
        (gimp-selection-all img)
        (set! layer-copy (car (gimp-layer-new master-img width height RGBA-IMAGE "layer-copie" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer master-img layer-copy 0 1)
        ;(gimp-message "step0.1")
        (set! buffer (car (gimp-edit-named-copy-visible img "jpg buffer")))
        ;(gimp-message "step0.2")
        (set! layer-copy (car (gimp-edit-named-paste layer-copy buffer TRUE)))
        
        ;(set! layer-copy2 (car (gimp-layer-new master-img width height RGBA-IMAGE "layer-copy2" 100 LAYER-MODE-NORMAL)))
        ;(gimp-image-insert-layer master-img layer-copy2 0 -1)
        ;(set! layer-copy2 (car (gimp-floating-sel-anchor layer-copy)))
        
        ;(layer (car (gimp-image-flatten new-image)))
        ;(gimp-displays-flush)
        
        
        
        ;(set! layer-copy (car (gimp-layer-new-from-drawable drawable master-img)))
        ;(gimp-image-insert-layer master-img layer-copy 0 -1)
        ;(gimp-item-set-name layer-copy2 "master-img layercopy")
        (gimp-item-set-name layer-copy "master-img layercopy crash now")
        ;(gimp-display-new master-img)
        ;(gimp-displays-flush)
        ;(quit)
        
        ;Step 2
        ;(gimp-message "step2")
        (gimp-image-convert-indexed master-img CONVERT-DITHER-FS CONVERT-PALETTE-GENERATE 256  ; was 256
                                                     FALSE TRUE "xkh") ; was convert dither-none
        
        
        ;Step 3
        ;Check that copy returned true
        
        ;(gimp-displays-flush)
        ;(quit)
        
        ;Step 4
        ;(gimp-message "step4")
        (gimp-image-convert-rgb master-img)
        ;(gimp-image-raise-item master-img layer-copy)
        (set! layer-copy (car (gimp-image-flatten master-img)))
        
        ;(gimp-displays-flush)
        ;(quit)
        
        (set! decomposed-new (car (plug-in-decompose RUN-NONINTERACTIVE
                                                 master-img layer-copy
                                                 decomposetype TRUE)))
        
        
        (gimp-displays-flush)
        ;(quit)
        
        ;(gimp-image-delete master-img)  ;We are done with this copy
        
        ;Step 5
        (set! drawable (car (gimp-image-flatten img)))
        (set! decomposed-old (car (plug-in-decompose RUN-NONINTERACTIVE
                                                 img drawable
                                                 decomposetype TRUE)))
        
        ;Step 6
        (set! layers (cadr (gimp-image-get-layers decomposed-old)))
        (gimp-image-remove-layer decomposed-old (aref layers 1))
        (gimp-image-remove-layer decomposed-old (aref layers 2)) 
        
        
        
        ;Step 7 and 8
        (set! layers (cadr (gimp-image-get-layers decomposed-new)))
        (set! layer-copy1 (car (gimp-layer-new-from-drawable (aref layers 1)
                                                        decomposed-old)))
        (gimp-image-insert-layer decomposed-old layer-copy1 0 1)
        ;(plug-in-blur RUN-NONINTERACTIVE decomposed-old layer-copy)
        (plug-in-mblur RUN-NONINTERACTIVE decomposed-old layer-copy1 0 5 0 (/ width 2) (/ height 2))
        ;(gimp-drawable-threshold layer-copy1 HISTOGRAM-VALUE 0.40 0.95)
        (gimp-drawable-levels-stretch layer-copy1)
        
        (set! layer-copy2 (car (gimp-layer-new-from-drawable (aref layers 2)
                                                         decomposed-old)))
        (gimp-image-insert-layer decomposed-old layer-copy2 0 2)
        (plug-in-blur RUN-NONINTERACTIVE decomposed-old layer-copy2)
        (gimp-drawable-levels-stretch layer-copy2)
        (gimp-drawable-levels layer-copy2 HISTOGRAM-VALUE 0.0 1.0 TRUE 1.65 0.0 1.0 FALSE)
        ;(plug-in-mblur RUN-NONINTERACTIVE decomposed-old layer-copy 0 5 0 1 1)
        
        ;(gimp-image-delete decomposed-new)  ;We are done with this image 
        
        ;(gimp-displays-flush)
        ;(gimp-display-new decomposed-old)
        ;(gimp-display-new decomposed-new)
        ;(gimp-displays-flush)
        ;(quit)
        
        ;Step 9
        (set! layers (cadr (gimp-image-get-layers decomposed-old)))
        (set! decomposed-new (car (plug-in-drawable-compose RUN-NONINTERACTIVE
                                                        decomposed-old
                                                        drawable ; not used
                                                        (aref layers 0)
                                                        (aref layers 1)
                                                        (aref layers 2)
                                                        ;-1
                                                        "HSL"
                                                         )))
        
        (gimp-image-delete decomposed-old)  ;We are done with this image
      ;  (gimp-display-new decomposed-old)
        (gimp-display-new decomposed-new)
        
        (gimp-image-undo-group-end img)
        ;(gimp-image-undo-group-end master-img)
        (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-match-colors"
    "Combine Mix Colors..."
    "Combine the colors of a single layer image to the colors of another single layer image. Note that elements from the source image are transferred. The images are mixed together. e.g. same image with a complex curve applied mixed with original produces a mixture. \nfile: cozens-match-colours_02.scm"
    "Kevin Cozens <kevin@ve3syb.ca>"
    "Kevin Cozens"
    "September 12, 2007"
    "RGB*"
    SF-IMAGE    "Image"             0
    SF-DRAWABLE "Drawable"          0
    SF-IMAGE    "Image to fetch colors from (mix)"    0
    SF-OPTION  "Decompose Type"  '("YCbCr_ITU_R470_256" "LCH" "RGB" "HSL" "YCbCr_ITU_R709_256")
)

(script-fu-menu-register "script-fu-match-colors"
                            "<Toolbox>/Script-Fu/Colors/")

; end of script