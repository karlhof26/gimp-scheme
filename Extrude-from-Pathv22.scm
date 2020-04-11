; Extrude from Path rel 0.2.2
; Created by Graechan 
 
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
; Rel 0.02 - Fix to prohibit pre 2-8-14 Gimps attempting to set brush or brush-size
; Rel 0.2.1 - Added menu item Cap Type
; Rel 0.2.2 - Added an optional Fadeout effect
; 
(define (gimp-version-meets? check)
  (let* (
            (c (map string->number (strbreakup check ".")))
            (v (map string->number (strbreakup (car (gimp-version)) ".")))
        )
        (if (> (car v) (car c)) #t
            (if (< (car c) (car v)) #f
                (if (> (cadr v) (cadr c)) #t
                    (if (< (cadr v) (cadr c)) #f
                        (if (>= (caddr v) (caddr c)) #t #f)
                    )
                )
            )
        )
  )
)

;    (if (gimp-version-meets? "2.8.0")
;    (begin
; ;script blurb
; )
; )
;
(define (get-all-real-layers image)
    (define (get-children group)
      (let loop ((children (vector->list (cadr (gimp-item-get-children group))))
                 (sub-layers '()) )
        (if (null? children)
          (reverse sub-layers)
          (loop (cdr children)
                (if (zero? (car (gimp-item-is-group (car children))))
                  (cons (car children) sub-layers)
                  (append sub-layers (get-children (car children))) )))))
    (let loop ((top-layers (vector->list (cadr (gimp-image-get-layers image))))
               (all-layers '()) )
      (if (null? top-layers)
        all-layers
        (loop (cdr top-layers)
              (if (zero? (car (gimp-item-is-group (car top-layers))))
                (append all-layers (list (car top-layers)))
                (append all-layers (get-children (car top-layers)))) 
        )
      )
    )
)
;
;
(define (script-fu-extrude-from-path image inDraw
                                    depth
                                    ext-direction
                                    shadow-offset
                                    shadow-blur
                                    shadow-opacity
                                    stroke-brush
                                    brush-size
                                    stroke-type
                                    3d-color
                                    3d-gradient
                                    rev
                                    cap
                                    cap-color
                                    fadeout
                                    allow-resize)
                                    
    (gimp-image-undo-group-start image)
    ;-----------------------------------------------------------------------------------------------------conditional start	
    (let ((handler (car (gimp-message-get-handler))))
    (gimp-message-set-handler 0) ;{ MESSAGE-BOX (0), CONSOLE (1), ERROR-CONSOLE (2) }
    (cond ((= (car (gimp-image-get-active-vectors image)) -1) 
    (gimp-message "No active path Found")
    (gimp-image-undo-group-end image))
    (else
    
 (let* (    
            (img-width (car (gimp-image-width image)))
            (img-height (car (gimp-image-height image)))			
            (width (car (gimp-drawable-width inDraw)))
            (height (car (gimp-drawable-height inDraw)))
            (offx (car (gimp-drawable-offsets inDraw)))
            (offy (cadr (gimp-drawable-offsets inDraw)))
            (drawable (car (gimp-layer-new image width height RGBA-IMAGE "Extrusion" 100 LAYER-MODE-NORMAL)))
            (bkg-layer 0)
            (cap-layer 0)
            (shadow 0)
            (pasted 0)
            (mask 0)
            (layerList 0)
            (inVectors (car (gimp-image-get-active-vectors image))) ;find the active path
            (vectorName (car  (gimp-item-get-name inVectors)))
            (num-strokes (car (gimp-vectors-get-strokes inVectors))) ;get the number of strokes
            (stroke-list (vector->list (cadr (gimp-vectors-get-strokes inVectors))))
            (stroke-id 0) ;get the stroke id
            (vectors 0)   ;vectors flag
            (off-x 0)     ;offx setting flag
            (off-y 0)     ;offy setting flag
            (cnt depth)  ;depth flag
            (all-vectors 0)   ;all-vectors flag(gimp-context-get-gradient)
            (cap-vectors 0)
            (samples 0)
            ;(rev (cond ((= rev FALSE) TRUE)
            ;           ((= rev TRUE) FALSE)))
            (gr (gimp-gradient-get-uniform-samples 3d-gradient depth rev))
            (grn (car gr))  ; length of sample array.
            (gra (vector->list (cadr gr))) ; array of color samples (R1,G1,B1,A1, R2,....)
            (r 0)
            (g 0)
            (b 0)
            (a 0)
            (f 100)    ;brush opacity
            (f-dec 0)  ;fadeout amount
            (gradc 0)
            (brush-copy 0)
        )
    ;-----------------------------------------------------------------------------------------------------basic startup
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (if (not (defined? 'gimp-image-get-item-position)) (gimp-context-set-dynamics "Dynamics Off"))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;----------------------------------------------------------------------------------------------------------set exising visible layers invisible
    (set! layerList (let loop ((layers (get-all-real-layers image))
                           (layerList '()) )
                  (if (null? layers)
                    (reverse layerList)
                    (loop (cdr layers)
                          (if (zero? (car (gimp-drawable-get-visible (car layers))))
                            layerList
                            (cons (car layers) layerList) 
                          )
                    )
                  )
               )
    )
    (set! layerList (delq inDraw layerList))
                            
    (map (lambda (x) (gimp-drawable-set-visible x FALSE)) layerList)
    
    (gimp-image-add-layer image drawable (+ (car (gimp-image-get-layer-position image inDraw)) 0)) ;stack 0=above 1=below
    (gimp-layer-set-offsets drawable offx offy)
    (gimp-layer-resize drawable (+ width (* depth 2)) (+ height (* depth 2)) depth depth)
    
    
    ;-------------------------------------------------------------------------------------------------------------------create all paths
    (set! off-x (round (cos (* ext-direction (/ 3.14 180)))))
    (set! off-y (round (sin (* ext-direction (/ 3.14 180)))))	
    (gimp-vectors-set-visible inVectors FALSE)
    (while (> cnt 0)
        (set! vectors (car (gimp-vectors-copy inVectors)))
        (gimp-image-insert-vectors image vectors 0 -1)
        (if (= cnt depth) (set! cap-vectors vectors))
        (while (not (null? stroke-list))
            (set! stroke-id (car stroke-list))   
            (gimp-vectors-stroke-translate inVectors stroke-id off-x off-y)
            (set! stroke-list (cdr stroke-list))
        )
        
        (set! stroke-list (vector->list (cadr (gimp-vectors-get-strokes inVectors))))
        
        (set! cnt (- cnt 1))
    ) 
    ;--------------------------------------------------------------------------------------------------------------------stroke all paths	
    (gimp-selection-none image)
    (cond ((gimp-version-meets? "2.8.14")
        (gimp-context-set-brush (car stroke-brush))
        (gimp-context-set-brush-size brush-size))
        (else 
            (gimp-context-set-brush "1. Pixel")
            (gimp-context-set-brush-size 3)
        )
    ) ;endcond
    (if (= stroke-type 0) (gimp-context-set-foreground 3d-color))
    
    
    (set! all-vectors (vector->list (cadr (gimp-image-get-vectors image))))
    (set! all-vectors (delq inVectors all-vectors))
    
    (set! cnt depth)
    
   (while (> cnt 0)
        (set! vectors (car all-vectors))
        (gimp-image-set-active-vectors image vectors)
        (set! r (round (* 255 (car gra))))
        (set! g (round (* 255 (cadr gra))))
        (set! b (round (* 255 (caddr gra))))
        (set! a (* 100 (cadddr gra)))
        (cond 
            ((= stroke-type 1) 
            (gimp-context-set-foreground (list r g b))
            (gimp-context-set-opacity (cond ((= fadeout TRUE) (- 100 (- a f-dec))) (else a))));------------------set opacity for gradient fadeout
            ((= stroke-type 0) 
            (gimp-context-set-opacity (cond ((= fadeout TRUE) (- 100 (- f f-dec))) (else f))));------------------set opacity for color fadeout
        );endcond
        (gimp-edit-stroke-vectors drawable vectors)
        (set! gra (cddddr gra)) ; skip 4 items (r, g, b, a) to next color)
        (set! all-vectors (cdr all-vectors))
        (if (= fadeout TRUE)
            (begin ;------------------------------------------------------------------------set fadeout decriment amount
                (if (= cnt depth) (set! f-dec (/ f depth)))
                (if (< cnt depth) (set! f-dec (+ f-dec (/ f depth))))
            )
        );endif	
        (set! cnt (- cnt 1))
    );endwhile
    
    
   
    ;-------------------------------------------------------------------------------------------------------------fill top path for cap	
    (if (= cap 0)
        (begin
            (set! cap-layer (car (gimp-layer-new image (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable)) RGBA-IMAGE "Cap" 100 LAYER-MODE-NORMAL)))
            (gimp-image-add-layer image cap-layer (+ (car (gimp-image-get-layer-position image drawable)) 0)) ;stack 0=above 1=below
            (gimp-layer-set-offsets cap-layer (car (gimp-drawable-offsets drawable)) (cadr (gimp-drawable-offsets drawable)))
            (gimp-vectors-to-selection cap-vectors 2 TRUE FALSE 0 0)
            (gimp-context-set-foreground cap-color)
            (gimp-edit-fill cap-layer FILL-FOREGROUND)
            (gimp-selection-none image)
        )
    ) ;endif
    (if (= cap 1) 
        (begin
            (set! cap-layer (car (gimp-layer-copy inDraw TRUE)))
            (gimp-image-add-layer image cap-layer (+ (car (gimp-image-get-layer-position image drawable)) 0)) ;stack 0=above 1=below
            (gimp-vectors-to-selection cap-vectors 2 TRUE FALSE 0 0)
            (gimp-selection-invert image)
            (gimp-edit-clear cap-layer)
            (gimp-selection-none image)
            (gimp-item-set-name cap-layer "Cap")
        )
    )
    ;--------------------------------------------------------------------------------------------------------------------------drop shadow
    (gimp-drawable-set-visible inDraw FALSE)
    (set! shadow (car (gimp-layer-new image (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable)) RGBA-IMAGE "Drop Shadow" shadow-opacity LAYER-MODE-NORMAL)))
    (gimp-image-add-layer image shadow (+ (car (gimp-image-get-layer-position image drawable)) 1)) ;stack 0=above 1=below
    (gimp-layer-set-offsets shadow (car (gimp-drawable-offsets drawable)) (cadr (gimp-drawable-offsets drawable)))
    (gimp-edit-copy-visible image)
    (set! pasted (car (gimp-edit-paste shadow FALSE)))
    (gimp-floating-sel-anchor pasted)
    (gimp-selection-layer-alpha shadow)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill shadow FILL-FOREGROUND)
    (gimp-layer-translate shadow
        (cond ((not (= off-x 0)) (-(* off-x shadow-offset))) (else 0)) 
        (cond ((not (= off-y 0)) (-(* off-y shadow-offset))) (else 0))
    )
    (gimp-selection-translate image 
        (cond ((not (= off-x 0)) (-(* off-x shadow-blur))) (else 0)) 
        (cond ((not (= off-y 0)) (-(* off-y shadow-blur))) (else 0))
    )
    (set! mask (car (gimp-layer-create-mask shadow ADD-SELECTION-MASK)))
    (gimp-layer-add-mask shadow mask)
    (gimp-selection-none image)
    (if (> shadow-blur 0) (begin
            (plug-in-gauss-rle2 RUN-NONINTERACTIVE image mask shadow-blur shadow-blur)
            (plug-in-gauss-rle2 RUN-NONINTERACTIVE image shadow shadow-blur shadow-blur)
            )
    )
    (gimp-layer-remove-mask shadow MASK-APPLY)
    (if (< cap 2) (gimp-drawable-set-visible inDraw TRUE))
    
    (map (lambda (x) (gimp-drawable-set-visible x TRUE)) layerList);----------set previous visible layers back to visible
    
    ;------------------------------------------------------------------------------------------------------------------------allow resize
    (set! bkg-layer (car (gimp-layer-new image img-width img-height RGBA-IMAGE "resize-layer" 100 LAYER-MODE-NORMAL)))
    (gimp-image-add-layer image bkg-layer (+ (car (gimp-image-get-layer-position image inDraw)) 1)) ;stack 0=above 1=below
    
    (if (= allow-resize TRUE)
        (begin
            (gimp-edit-fill bkg-layer FILL-BACKGROUND)
            (gimp-image-set-active-layer image drawable)
            (plug-in-autocrop-layer 1 image drawable)
            (gimp-image-resize-to-layers image)
            (gimp-layer-resize-to-image-size drawable)
            (gimp-layer-resize-to-image-size inDraw)
            (if (= cap 0) (gimp-layer-resize-to-image-size cap-layer))
            (gimp-layer-resize-to-image-size shadow)
            (gimp-image-remove-layer image bkg-layer)
        )
        (begin
            (gimp-edit-fill bkg-layer FILL-BACKGROUND)
            (plug-in-autocrop 1 image bkg-layer)
            (gimp-layer-resize-to-image-size inDraw)
            (gimp-layer-resize-to-image-size shadow)
            (gimp-image-remove-layer image bkg-layer)
        )
    ) ;endif
    
    (if (= cap 0) (gimp-image-set-active-layer image cap-layer))
    ;--------------------------------------------------------------------------------------------remove the vectors
    (set! all-vectors (vector->list (cadr (gimp-image-get-vectors image))))
    (set! all-vectors (delq cap-vectors all-vectors))
    (while (not (null? all-vectors))
        (set! vectors (car all-vectors))
        (gimp-image-remove-vectors image vectors)
        (set! all-vectors (cdr all-vectors))
    ) ;endwhile
    (gimp-item-set-name cap-vectors vectorName);---------------------------------------------rename the cap-vectors	
    ;--------------------------------------------------------------------------------------------finish the script	
    
    (gimp-context-pop)
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
 ) ;end variables
    
 )) ;end ConditionalStart
    (gimp-message-set-handler handler))
    
) ;end procedure 

(script-fu-register "script-fu-extrude-from-path"            
    "Extrude from Path"
    "Script will create an extrusion from a closed Path. Settings are included to allow you to modify your outputs appearance.
    \n file: Extrude-from-Pathv22.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Aug 2016"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-ADJUSTMENT "3d Depth(pixels)"     '(20 0 150 1 10 0 0)
    SF-ADJUSTMENT  "Extrusion Direction" '(45 0 360 45 45 0 0)
    SF-ADJUSTMENT "Shadow Offset"        '(8 0 50 1 5 0 0)
    SF-ADJUSTMENT "Shadow Blur"          '(15 0 50 1 5 0 0)
    SF-ADJUSTMENT "Shadow Opacity"       '(80 0 100 1 10 0 0)
    SF-BRUSH      "3d Sroke Brush"       '("1. Pixel" 100 44 0)
    SF-ADJUSTMENT "Brush Size"           '(3 1 100 1 1 0 1)
    SF-OPTION     "Stroke Type"          '("Color" "Gradient")
    SF-COLOR      "Stroke Color"             "Blue"
    SF-GRADIENT   "Stroke Gradient"          "Shadows 1"
    SF-TOGGLE     "Reverse the Gradient"     FALSE
    SF-OPTION     "Cap Type"                 '("Color" "Original Image" "Transparent")
    SF-COLOR      "Cap Color"                "Red"
    SF-TOGGLE     "Add Fadeout Effect"       FALSE
    SF-TOGGLE     "Allow resizing"           TRUE
    
)

(script-fu-menu-register "script-fu-extrude-from-path" "<Toolbox>/Script-Fu2/Paths/")
  
;--------------------------------------------------------------------------------------------------
 