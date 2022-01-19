; Beveled Star rel 0.03 
; Created by Graechan
; Created following an excellent tutorial from PegLeg44 
; http://www.gimpchat.com/viewtopic.php?f=23&t=8202
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
; Rel 0.02 - Bugfix for Typo
; Rel 0.03 - Fix for missing procedure
;
; Gradients blend direction list
(define list-blend-dir '("Left to Right" "Top to Bottom" "Diagonal to centre" "Diagonal from centre"))
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
;
;find layer by name proceedure
(define (find-layer-by-name image layerName)
  (let* (
           (layerList (vector->list (cadr (gimp-image-get-layers image))))    
           (wantedLayerId -1)
		   (layerId 0)
           (layerText "")
        )
       
        (while (not (null? layerList))
          (set! layerId (car layerList))
            (set! layerText (car (gimp-drawable-get-name layerId))) 
            (if (string=? layerText layerName) (set! wantedLayerId layerId))          
          (set! layerList (cdr layerList)))        
        (if (= -1 wantedLayerId) (error (string-append "Could not find a layer with name:- " layerName)))
        (list wantedLayerId)
  ) ;endvariables
) ;end find layer by name procedure
; 
(define (script-fu-beveled-star 
                               final-size
							   points
							   spline
							   shape
							   color
							   apply-bkg
							   bkg-color
							   keep-selection
							   conserve
							   )
							   
							  

 (let* (
            (size (* shape 100))
			(width size)
			(height size)
			(image (car (gimp-image-new width height RGB)))
			(layer (car (gimp-layer-new image width height RGBA-IMAGE "Layer" 100 LAYER-MODE-NORMAL)))
			(bump (car (gimp-layer-new image width height RGBA-IMAGE "Bump" 100 LAYER-MODE-DODGE)))
			(star (car (gimp-layer-new image width height RGBA-IMAGE "Star" 100 LAYER-MODE-MULTIPLY)))
			(bkg (car (gimp-layer-new image 400 400 RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
			(visible 0)
			(ver 2.8)
			(per (* 4 size))
            (rad (/ per 5))
			(outline (/ spline 2))
			(x 0)
			(y 0)
            (vectors 0)
			(brushName    "outlineBrush")			
        )
    ;(gimp-message "Good to go")
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-image-undo-group-start image)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (gimp-context-set-dynamics "Dynamics Off")
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    (gimp-context-set-interpolation INTERPOLATION-CUBIC)
    
    
    (gimp-image-insert-layer image layer 0 0)
    
    ;;;;begin the script
    (gimp-drawable-fill layer FILL-BACKGROUND)
    ;;;;-----------------------------------------------------------------------------------define new brush for drawing operation 
            ;(gimp-brush-new brushName)
            ;(gimp-brush-set-shape brushName BRUSH-GENERATED-CIRCLE)    
            ;(gimp-brush-set-spikes brushName 2)
            ;(gimp-brush-set-hardness brushName 1.00)                   
            ;(gimp-brush-set-aspect-ratio brushName 1.0)
            ;(gimp-brush-set-angle brushName 0.0)                       
            ;(gimp-brush-set-spacing brushName 1.0)
            (gimp-context-set-brush "Circle (01)")
            (if (< outline 1)
                (gimp-context-set-brush-size 1.1)
                (gimp-context-set-brush-size outline) ;else           
            )
            
            
            (gimp-context-set-foreground '(0 0 0))
            (gimp-context-set-paint-method "gimp-paintbrush")
            
            (the-star-path image ;-------------------------------------------------------------create the star path
                 layer
                 "star" ;name
                 (/ width 2) ;x
                 (/ height 2) ;y
                 (/ size 2) ;oradius
                 0 ;iradius
                 points ;points
                 0) ;rotation
                
                
    (set! vectors (car (gimp-image-get-active-vectors image)))
    (gimp-edit-stroke-vectors layer vectors)
    (gimp-image-remove-vectors image vectors)
    
    ;--------apply Peglegs majic
    ;(gimp-context-set-sample-threshold-int 0)
    (gimp-context-set-antialias FALSE)
    (gimp-image-select-color image 2 layer '(255 255 255))
    
    (gimp-selection-invert image)
    (gimp-edit-fill layer FILL-FOREGROUND)
    
    (gimp-selection-border image 100)
    (gimp-selection-feather image 5)
    (gimp-edit-fill layer FILL-FOREGROUND)
    
    (include-layer image bump layer 0)	;stack 0=above 1=below
    (gimp-drawable-fill bump FILL-BACKGROUND)
    
    (gimp-edit-blend bump  BLEND-FG-BG-RGB LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 
        (/ size 2) (/ size 2) (/ size 2) size)
    (set! visible (car (gimp-layer-new-from-visible image image "visible")))
    (include-layer image visible bump 1)	;stack 0=above 1=below
    (gimp-layer-set-mode bump LAYER-MODE-SCREEN)
    (set! bump (car (gimp-image-merge-down image bump EXPAND-AS-NECESSARY)))
    (set! bump (car (gimp-image-merge-down image bump EXPAND-AS-NECESSARY)))
    (gimp-item-set-name bump "BumpMap")
    
    (include-layer image star bump 0)	;stack 0=above 1=below
    (gimp-context-set-foreground color)
    (gimp-edit-fill star FILL-FOREGROUND)
    
    (gimp-selection-none image)
    (gimp-drawable-invert bump FALSE)
    
    (cond ((= apply-bkg TRUE) ;-----------------------------------------------------------------------------------apply the background
        (include-layer image bkg bump 1)	;stack 0=above 1=below
        (gimp-image-resize-to-layers image)
        (gimp-context-set-foreground bkg-color)
        (script-fu-line-nova image bkg 40 1 50 30)  
        (gimp-image-select-item image 2 bkg) 
        
        (gimp-edit-fill bkg FILL-FOREGROUND)
        (gimp-selection-none image)
        (gimp-layer-scale bkg size size FALSE)
        (gimp-image-resize-to-layers image)
        
        (gimp-image-select-ellipse image 2 0 0 width height)
        
        (gimp-selection-invert image)
        (gimp-edit-clear bkg)
        (gimp-selection-none image)
       )
    ) ;endcond
    
    (plug-in-bump-map RUN-NONINTERACTIVE image star bump 135 45 3 0 0 0 0 TRUE FALSE 1) ;-----------------------------bumpmap and clear layers
    
    
    (gimp-context-set-sample-threshold-int 15)
    (gimp-context-set-antialias TRUE)
    (gimp-image-select-color image 2 bump '(0 0 0))
    
    (gimp-edit-clear star)
    (gimp-edit-clear bump)
    (gimp-selection-invert image)
    (plug-in-gauss-rle2 RUN-NONINTERACTIVE image star 5 5)
    (gimp-image-scale image final-size final-size)
    
    ;;;;finish the script
    ;(gimp-brush-delete brushName)
    
    (if (= keep-selection FALSE)
        (gimp-selection-none image)
    )
    (if (= conserve FALSE)
        (begin
            (if (= apply-bkg TRUE)
                (set! star (car (gimp-image-merge-down image star EXPAND-AS-NECESSARY)))
            )
            (set! star (car (gimp-image-merge-down image star EXPAND-AS-NECESSARY)))
        )
    )
    (gimp-item-set-name star "Star")
    
    
    (gimp-display-new image)
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    (gc)
    
 )
) 

(script-fu-register "script-fu-beveled-star"        		    
  "Beveled Star"
  "Instructions"
  "Graechan"
  "Graechan - http://gimpchat.com"
  "Aug 2013"
  ""
  SF-ADJUSTMENT "Size" '(400 0 1500 1 10 0 1)
  SF-ADJUSTMENT	"Number of points"      '(5 3 12 1 5 0 1)
  SF-ADJUSTMENT "Outline Width"          '(10 1 30 1 1 0 1)
  SF-ADJUSTMENT "Star Shape - depth of spaces"            '(8 2 20 1 1 0 0)
  SF-COLOR      "Star Color"            "Blue"
  SF-TOGGLE     "Apply a Background"    TRUE
  SF-COLOR      "Background Color"      "Yellow"
  SF-TOGGLE     "Keep selection"        FALSE
  SF-TOGGLE     "Keep the Layers"   FALSE
)

(script-fu-menu-register "script-fu-beveled-star" "<Toolbox>/Script-Fu/Shapes")
;
(define (translate-point x y angle distance)
  (let* ((ang (* angle (/ (* 4 (atan 1.0)) 180)))
    (nx (+ (* distance (cos ang)) x))
    (ny (+ (* distance (sin ang)) y)))
    (list nx ny)
  )
)
;
(define (the-star-path img
			     drawable
			     name
			     x
			     y
			     oradius
			     iradius
			     points
			     rotation
			     )
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-vectors-new img name)))
	 (pointstwo (* points 2))
	 (radius iradius)
	 (init (- rotation 90))
	 (deg 0)
	 (i 1)
	 (p (translate-point x y init oradius))
	 (strokeid (car (gimp-vectors-bezier-stroke-new-moveto pathvector (car p) (cadr p)))))
    (while (< i pointstwo)
      (set! deg (+ (* (/ i pointstwo) 360) init))
      (set! p (translate-point x y deg radius))
      (gimp-vectors-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! i (+ i 1))
      (if (= radius oradius)
	(set! radius iradius)
	(set! radius oradius)
      )
    )
    (gimp-vectors-stroke-close pathvector strokeid)
    (gimp-image-add-vectors img pathvector -1)
    (gimp-image-set-active-vectors img pathvector)
    
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)
