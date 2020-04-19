; GIMP Layer Effects  
; Copyright (c) 2011 Andy Meneely
; me@andymeneely.com

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define (math-round input)
   (floor (+ input 0.5))
)

(define (math-ceil input)
  (if (= input (floor input))
    input
    (+ (floor input) 1)
  )
)

(define (get-layer-pos img layer)
  (let* (
            (layerdata (gimp-image-get-layers img))
            (numlayers (car layerdata))
            (layerarray (cadr layerdata))
            (i 0)
            (pos -1)
        )
    (while (< i numlayers)
      (if (= layer (vector-ref layerarray i))
        (begin
            (set! pos i)
            (set! i numlayers)
        )
        (set! i (+ i 1))
      )
    )
    pos
  )
)

(define (add-under-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (+ (get-layer-pos img oldlayer) 1))
)

(define (add-over-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (get-layer-pos img oldlayer))
)

(define (draw-blurshape img drawable size initgrowth sel invert color)
  (let* (   (k initgrowth)
            (currshade 0)
            (i 0)
        )
     (gimp-message "y")
     (while (< i size)
        (gimp-message "z")
        (if (> k 0)
            (gimp-selection-grow img k)
            (if (< k 0)
                (gimp-selection-shrink img (abs k))
            )
        )
        (gimp-message "a")
        (if (= invert 1)
            (set! currshade (math-round (* (/ (- size (+ i 1)) size) 255)))
            (set! currshade (math-round (* (/ (+ i 1) size) 255)))
        )
        (gimp-message "b")
       ; (gimp-context-set-foreground (list currshade currshade currshade))
       ; (if (= (car (gimp-selection-is-empty img)) 0)
       ;     (gimp-edit-fill drawable 0) ; fill with foreground
       ; )
       ; (gimp-image-select-item sel)
      ;  (set! k (- k 1))
      ;  (set! i (+ i 1))
        (gimp-message "c")
     )
  )
)


(define (script-fu-chaoticbits-border-shadow
                img
                origlayer
                color
                width
                iterations)
                
   ;Back up original stuff going in
   (let* (
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (layername (car (gimp-drawable-get-name origlayer)))
            (imgheight (car (gimp-drawable-height origlayer)))
            (imgwidth (car (gimp-drawable-width origlayer)))
            (layer-type (car (gimp-drawable-type origlayer)))            
            (newlayer (car (gimp-layer-new img imgwidth imgheight layer-type "border shadow" 100 LAYER-MODE-NORMAL-LEGACY)))
                               ; (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3))
                               ; (string-append layername " border shadow") 100 0)))
            (blurperiter (/ width iterations))
            (blur_width width)
            (blur_iter 1)
            (sel (car (gimp-selection-save img)))
            (secondlayer (car (gimp-layer-new img imgwidth imgheight layer-type "secondlayer" 100 LAYER-MODE-NORMAL-LEGACY)))
            (newwidth 0)
            (offsetamt 0)
            (jumpamt 0)
            (gap 0)
        )
        (gimp-image-undo-group-start img)
        ;(gimp-edit-copy origlayer)
        ; (set! origlayer (car (gimp-image-flatten img)))
           
        ;Add the new layer above the current
        ;previously -1 not 1
        (gimp-image-insert-layer img newlayer 0 -1) 
        (gimp-layer-set-mode newlayer LAYER-MODE-MULTIPLY) ; Multiply mode
        (gimp-image-insert-layer img secondlayer 0 -1)
        (gimp-layer-set-mode secondlayer 3)
        ;Set color
        (gimp-context-set-foreground color) 
        
        ;New from Karl
        (if (= (car (gimp-selection-is-empty img)) 0)
            (gimp-selection-all img)
            ;;(sel (car (gimp-selection-all img)))
           ;;(gimp-image-select-rectangle img CHANNEL-OP-REPLACE imgwidth imgheight (- imgwidth 100) (- imgheight 100))
        )
        (set! offsetamt (* width 1))
        
        (gimp-image-select-rectangle img CHANNEL-OP-REPLACE offsetamt offsetamt (- imgwidth (* offsetamt 2)) (- imgheight (* offsetamt 2)))
     ;  (gimp-image-select-rectangle img CHANNEL-OP-REPLACE 0 0 (- imgwidth (* 2 width)) (- imgheight (* 2 width)))
       (gimp-selection-invert img)
       (gimp-edit-bucket-fill newlayer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 255 FALSE 0 0)
      ; (sel (car(gimp-image-get-selection img)))
      ; (draw-blurshape img newlayer 20 10 sel 1)
        (set! blurperiter (/ blurperiter 2))
        
        ;Loop over the iterations
        (while (< blur_iter (+ iterations 1)) 
            ;Select from original layer
            ;(gimp-image-select-item img CHANNEL-OP-REPLACE newlayer) 
            ;(gimp-selection-invert img)
            
            ;Fill inverted layer
        ;    (gimp-edit-bucket-fill newlayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)
            
            ;Blur
            ;(gimp-selection-none img) 
            (plug-in-gauss-rle2 1 img newlayer blur_width blur_width) 
            
            (gimp-message "x")
            ;Iterate
            (set! blur_iter (+ blur_iter 1))
            (set! blur_width (* blurperiter blur_iter ))
        )
        
      ;  (set! sel (car(gimp-image-get-selection img)))
      ;   (gimp-image-select-rectangle img CHANNEL-OP-REPLACE 45 45 (- imgwidth (* 2 45)) (- imgheight (* 2 45)))
      ;   (gimp-selection-invert img)
      ;   (gimp-edit-bucket-fill-full newlayer FG-BUCKET-FILL NORMAL-MODE 80 15 FALSE TRUE 0 0 0)
      ;  (set! sel (car(gimp-image-get-selection img)))
      ;  (draw-blurshape img newlayer 20 10 sel 0 color)
           (define thirdlayer (car(gimp-layer-copy newlayer 1)))
           (gimp-image-add-layer img thirdlayer -1)
           (gimp-layer-set-mode thirdlayer 3)
         ;  (gimp-layer-scale thirdlayer 200 220 TRUE)
         ;  (gimp-layer-scale thirdlayer (- imgwidth (- width blur_width)) (- imgheight (- width blur_width)) TRUE)
            (gimp-message "starting")
            (set! jumpamt (+ jumpamt (* width 0.7)))
            (set! gap (+ gap (* width 0.2)))
            (set! newwidth (+ offsetamt jumpamt))
            (gimp-image-select-rectangle img CHANNEL-OP-REPLACE newwidth newwidth (- imgwidth (* newwidth 2)) (- imgheight (* newwidth 2)))
            (gimp-selection-invert img)
            (gimp-edit-bucket-fill thirdlayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)
            ;blur it
            (plug-in-gauss-rle2 1 img thirdlayer gap gap)
            (plug-in-gauss-rle2 1 img thirdlayer jumpamt jumpamt)
            (plug-in-gauss-rle2 1 img thirdlayer width width)
            
            (set! newwidth (+ offsetamt gap))
            (gimp-image-select-rectangle img CHANNEL-OP-REPLACE newwidth newwidth (- imgwidth (* newwidth 2)) (- imgheight (* newwidth 2)))
            (gimp-selection-invert img)
            (gimp-edit-clear thirdlayer)
            
           ; (draw-blurshape img newlayer 20 10 sel 0 color)
        ;Delete from original layer
        ;(gimp-selection-layer-alpha origlayer)
        ;(gimp-selection-invert img)
        ;(gimp-edit-clear newlayer)
        
        ;Restore original settings
        ;(gimp-palette-set-foreground origfgcolor)
        ;(gimp-selection-load origselection)
       ; (gimp-image-set-active-layer img origlayer)
       (gimp-displays-flush)
       (gimp-image-undo-group-end img)
  )
)

(script-fu-register "script-fu-chaoticbits-border-shadow"
            "Double Border blurred"
            "Adds a blurred shadow from the edges of a double border"
            "Karl Hofmeyr"
            "Karl Hofmeyr based on a script by Andy Meneely but highy modified"
            "August 2016"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"	            0
            SF-DRAWABLE     "Drawable"          0
            SF-COLOR        "Color"             '(0 0 0)
            SF-ADJUSTMENT   "Width"             '(50 0 500 1 10 0 0)
            SF-ADJUSTMENT   "Blur Iterations"   '(4 1 10 1 1 0 0)
)

(script-fu-menu-register "script-fu-chaoticbits-border-shadow" "<Image>/Script-Fu2/Borders")

; end of file 