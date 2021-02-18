;;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; RGB Swapscale script  for GIMP 2.10.22
; Created by Saul Goode
;
; Tags: color, selection, scale
;
; Author statement:
;
;; SWAPSCALE allows the swapping of the RGB channels as well as selectively scaling them.
;; It is most useful when working on a duplicate of a layer and then changing the Mode or
;; Opacity to control the result. If there is no current selection then the entire layer
;; is processed. Note: scaling can be in the negative direction (-100% scale = INVERT)
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


(define (script-fu-rgb-swapscale image drawable scale-red scale-green scale-blue swap-rotate)
  ;; "(apply-curve layer percent scales a layer to a percent (0-100) of its original
  (define (apply-curve layer percent)
    (let* (
            (curve (cons-array 256 'double))
            (index 255)
        )
        ;(gimp-message "apply curves")
        (while (>= index 0)
            (aset curve index (/ (min 255 (trunc (+ (* index (/ percent 100)) 0.5))) 255))
            (set! index (- index 1))
        )
        (gimp-drawable-curves-explicit layer HISTOGRAM-VALUE 256 curve)
    )
  )
    
  (let* (
            (rgb-images 0)
            (red 0)
            (green 0)
            (blue 0)
            (new-image 0)
            (new-layer 0)
            (old-foreground (car (gimp-context-get-foreground)))
        )
        
        (gimp-image-undo-group-start image)
        (if (= (car (gimp-drawable-type drawable)) RGB-IMAGE)
            (gimp-layer-add-alpha drawable)
        )
        
        (if (= (car (gimp-drawable-type drawable)) RGBA-IMAGE)
            (set! rgb-images (plug-in-decompose RUN-NONINTERACTIVE image drawable "RGBA" 0))
        )
        
        
        (set! red (car (gimp-image-get-active-layer (car rgb-images))))
        (set! green (car (gimp-image-get-active-layer (cadr rgb-images))))
        (set! blue (car (gimp-image-get-active-layer (caddr rgb-images))))
        
        (if (< scale-red 0)
            (gimp-invert red)
        )
        (if (< scale-green 0)
            (gimp-invert green)
        )
        (if (< scale-blue 0)
            (gimp-invert blue)
        )
        (set! scale-red (abs scale-red))
        (set! scale-green (abs scale-green))
        (set! scale-blue (abs scale-blue))
        
        (apply-curve red scale-red)
        (apply-curve green scale-green)
        (apply-curve blue scale-blue)
        
        (set! red (car rgb-images))
        (set! green (cadr rgb-images))
        (set! blue (caddr rgb-images))
        ;(gimp-message "Compose start")
        (cond
            ((= swap-rotate 1) ;; swap blue and green
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE red image blue green (car (last rgb-images)) "RGBA"))) ;; RBG
            )
            ((= swap-rotate 2) ;; swap red and blue
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE blue image green red (car (last rgb-images)) "RGBA"))) ;; BGR
            )
            ((= swap-rotate 3) ;; swap green and red
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE green image red blue (car (last rgb-images)) "RGBA"))) ;; GRB
            )
            ((= swap-rotate 5) ;; rotate-right
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE blue image red green (car (last rgb-images)) "RGBA"))) ;; BRG
            )
            ((= swap-rotate 6) ;; rotate-right
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE green image blue red (car (last rgb-images)) "RGBA"))) ;; GBR
            )
            ( TRUE ;; RGB: don't swap or rotate
                (set! new-image (car (plug-in-compose RUN-NONINTERACTIVE red image green blue (car (last rgb-images)) "RGBA"))) ;; GBR
            )
        )
        (set! drawable (car (gimp-image-get-active-layer image)))
        (gimp-selection-all new-image)
        (if (= 1 (car (gimp-edit-copy (car (gimp-image-get-active-layer new-image)))))
            (begin
                (set! new-layer (car (gimp-edit-paste drawable TRUE)))
                (gimp-floating-sel-anchor new-layer)
            )
        )
        (gimp-image-delete red)
        (gimp-image-delete green)
        (gimp-image-delete blue)
        (gimp-image-undo-group-end image)
        (gimp-context-set-foreground old-foreground)
        (gimp-displays-flush)
        (gc) ; garbage collect; an array was used
  )
)

(script-fu-register "script-fu-rgb-swapscale"
    "<Toolbox>/Script-Fu/Photo/Color/RGB swapscale..."
    "Swaps and scales RGB channels on RGBA or RGB images. \nfile:goode-rgb-swapscale.scm"
    "Saul Goode"
    "Saul Goode"
    "March 2006"
    "RGBA RGB"
    SF-IMAGE      "SF-IMAGE"            0
    SF-DRAWABLE   "SF-DRAWABLE"         0
    SF-ADJUSTMENT "SCALE RED (negative to invert)"      '( 100 -100 100 1 10 0 0)
    SF-ADJUSTMENT "SCALE GREEN (negative to invert)"    '( 100 -100 100 1 10 0 0)
    SF-ADJUSTMENT "SCALE BLUE (negative to invert)"     '( 100 -100 100 1 10 0 0)
    SF-OPTION     "SWAP/ROTATE" '("No Swap Or Rotate (RGB->RGB)"
                                             "Swap Blue & Green (RGB->RBG)"
                                             "Swap Red & Blue (RGB->BGR)"
                                             "Swap Green & Red (RGB->GRB)"
                                             "------------------------"
                                             "Rotate Right (RGB->BRG)"
                                             "Rotate Left (RGB->GBR)" )
)

;; EOF ;;
