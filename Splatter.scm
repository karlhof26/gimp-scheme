; Splatter rel 0.01 
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
;
;
; Include layer Procedure
(define (include-layer image newlayer oldlayer stack)	;stack 0=above 1=below
    (cond ((defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
            (gimp-image-insert-layer image newlayer (car (gimp-item-get-parent oldlayer)) 
            (+ (car (gimp-image-get-item-position image oldlayer)) stack))                                     ;For GIMP 2.8 
          )
          (else
           (gimp-image-add-layer image newlayer (+ (car (gimp-image-get-layer-position image oldlayer)) stack)) ;For GIMP 2.6 
          )
    ) ;end cond
) ;end include layer procedure

(define (script-fu-splatter image drawable
                            canvas-color
                            splatter-gradient
                            mode
                            keep-selection-in
                            conserve
        )
                          
    (gimp-image-undo-group-start image)  
    
 (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (original-width width)
            (original-height height)
            (area (* 1000 1000))
            (paint-layer (car (gimp-layer-new image width height RGBA-IMAGE "Paint-Layer" 100 mode)))
            (alpha (car (gimp-drawable-has-alpha drawable)))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (cond ((defined? 'gimp-image-get-item-position) (car (gimp-item-get-name drawable)))
                    (else (car (gimp-drawable-get-name drawable)))))
            (active-gradient (car (gimp-context-get-gradient)))
            (active-fg (car (gimp-context-get-foreground)))
            (active-bg (car (gimp-context-get-background)))
            (keep-selection keep-selection-in)
            (selection-channel 0)
            (ver 2.8)
            (x1 0)
            (y1 0)
            (iwidth 0)
            (iheight 0)
            (isize 0)
            (cnt 0)
            
            (orig_cnt 0)
            (selection_deets 0)
            (last_cnt 0)
        )
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
    (gimp-context-set-foreground canvas-color)
    (gimp-context-set-background '(255 255 255))
    
    (if (= alpha FALSE) (gimp-layer-add-alpha drawable))
    
    ;;;;check that a selection was made if not make one	
    (if (= sel TRUE) (set! keep-selection FALSE))
    (if (= sel TRUE)
        (begin
            (cond ((= ver 2.8) (gimp-image-select-item image 2 drawable)) 
                (else (gimp-selection-layer-alpha drawable))
            ) ;endcond
        )
    )
    
    ;;;;create selection-channel (gimp-selection-load selection-channel)
    (set! selection-channel (car (gimp-selection-save image)))
    (cond ((= ver 2.8) (gimp-item-set-name selection-channel "selection-channel"))
        (else (gimp-drawable-set-name selection-channel "selection-channel"))
    ) ;endcond
    
    (gimp-image-set-active-layer image drawable)
    (gimp-edit-fill drawable FILL-FOREGROUND)
    
    ;   (gimp-selection-none image)
    (set! x1 (cadr (gimp-drawable-mask-intersect drawable))) ;x coordinate of upper left corner of selection 
    (set! y1 (caddr (gimp-drawable-mask-intersect drawable))) ;y coordinate of upper left corner of selection 
    (set! iwidth (cadddr (gimp-drawable-mask-intersect drawable))) ;x width of the intersection
    (set! iheight (cadr (cdddr (gimp-drawable-mask-intersect drawable)))) ;y height of the intersection
    (set! isize (min iwidth iheight))
    (set! cnt (- (round (/ isize 30)) 1))   
    (set! orig_cnt (- (round (/ isize 30)) 1))   
    
    
    ;;;;begin the script	
    (include-layer image paint-layer drawable 0) ;stack 0=above 1=below
    (gimp-image-select-rectangle image 2 x1 y1 iwidth iheight)
    
    (if (= ver 2.8) (gimp-context-set-dynamics "Random Color"))
    (gimp-context-set-paint-method "gimp-paintbrush")
    (gimp-context-set-brush "Sparks")
    (gimp-context-set-brush-default-size)
    (gimp-context-set-gradient splatter-gradient)
    (gimp-edit-stroke paint-layer)
    (while (> cnt 4) ; was > 0
        ;(gimp-message (number->string cnt))
        (gimp-selection-shrink image 30)
        (if (= (car (gimp-selection-is-empty image)) FALSE)
            (begin
                (gimp-edit-stroke paint-layer)
                (set! last_cnt cnt)
            )
            (begin
                ;(gimp-message "already too small")
            )
        )
        
        
        (set! cnt (- cnt 1))
    )
    ;one more to finish
    
    (gimp-image-select-rectangle image 2 x1 y1 iwidth iheight)
    (gimp-selection-shrink image (* (- orig_cnt last_cnt) 30))
    
    (gimp-selection-shrink image 42)
    
    ;(set! selection_deets (cadr (gimp-selection-bounds image)))
    ;(gimp-message (number->string selection_deets))
        
    (gimp-context-set-foreground '(250 0 0))
    (if (= (car (gimp-selection-is-empty image)) FALSE) (gimp-edit-stroke paint-layer))
    
    (gimp-displays-flush)
    
    
    (gimp-selection-none image)
    (gimp-selection-load selection-channel)
    (gimp-selection-shrink image 1)
    (gimp-selection-invert image)
    (gimp-edit-clear paint-layer)
    (gimp-selection-none image)
    
    ;(gimp-curves-spline paint-layer 0 10 #(0 0 86 50 128 129 172 207 255 255))
    (gimp-drawable-curves-spline paint-layer 0 10 #(0.0 0.0 0.33 0.196 0.5016 5058 0.674 0.811 1.0 1.0))
    (if (= conserve FALSE)
        (begin
            (set! drawable (car (gimp-image-merge-down image paint-layer EXPAND-AS-NECESSARY)))
        )
    )
    (if (= keep-selection-in TRUE)
        (begin
            (gimp-selection-load selection-channel)
        )
    )
    ;;;;finish the script
    
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    
 )
) 

(script-fu-register "script-fu-splatter"            
    "Splatter"
    "Splatters Paint over layer or selection. Note that only some layer modes may work. \nfile:Splatter.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "May 2014"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-COLOR      "Canvas Color"                "Black"
    SF-GRADIENT   "Paint Splatter Gradient"     "Tropical Colors"
    SF-ENUM "Paint Splatter LayerMode" '("LayerModeEffects" "hardlight-mode")
    SF-TOGGLE     "Keep selection"              FALSE
    SF-TOGGLE     "Keep the Layers"             FALSE
)

(script-fu-menu-register "script-fu-splatter" "<Toolbox>/Script-Fu/Patterns")

; end of script


