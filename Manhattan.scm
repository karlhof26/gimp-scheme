; Manhattan rel 0.01 
; Created by Graechan from Tutorial by Issabella at http://www.gimpchat.com/viewtopic.php?f=23&t=9901
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
; Rel 0.02 - Updated for GIMP-2.10.22
;
(define (script-fu-manhattan image drawable
                               radius
                                hrot
                                vrot
                                conserve
        )
            
    (gimp-image-undo-group-start image)						  
    
 (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (original-width width)
            (original-height height)
            (area (* 1000 1000))
            (sphere (car (gimp-layer-new-from-drawable drawable image)))
            (blur-layer (car (gimp-layer-new-from-drawable drawable image)))
            (alpha (car (gimp-drawable-has-alpha drawable)))
            (sel (car (gimp-selection-is-empty image)))
            (x1 (/ width 2))
            (y1 (/ height 2))
            (x2 0)
            (y2 0)
        )
        
        (gimp-context-push)
        (gimp-context-set-paint-method "gimp-paintbrush")
        (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        (if (= alpha FALSE) (gimp-layer-add-alpha drawable))
        
        ;;;;begin the script
        (let ((stack 0)) ;stack 0=above 1=below 
            (gimp-image-insert-layer image sphere (car (gimp-item-get-parent drawable)) 
                (+ (car (gimp-image-get-item-position image drawable)) stack))
        )                                     
        
        (gimp-item-set-name sphere "Sphere")      
        (if (= alpha FALSE) (gimp-layer-add-alpha sphere))
        
        (cond ((> width height)
                (gimp-layer-scale-full sphere height height TRUE 3)
              )
              (else (gimp-layer-scale-full sphere width width TRUE 3))
        )
        
        (set! x2 (car (gimp-drawable-width sphere)))
        (set! y2 (car (gimp-drawable-height sphere)))
        
        (plug-in-map-object RUN-NONINTERACTIVE image sphere 
                1                ; Type of mapping (0=plane,1=sphere,2=box,3=cylinder)
            0.5 0.5 2.0	         ; viewpoint
            0.5 0.5 0.0          ; object pos ; was 0.5 0.5 0.0
            1.0 0.0 0.0          ; first axis
            0.0 1.0 0.0          ; 2nd axis
            0.0 hrot vrot        ; axis rotation
            0 '(255 255 255)     ; Type of lightsource (0=point,1=directional,2=none)and Lightsource color (r,g,b) 
            -0.5 -0.5 2		     ; light position
            -1.0 -1.0 1.0	     ; light direction
            0.3 1.2 0.7 0.0 27.0 ; material (amb, diff, refl, spec, high)
            TRUE                 ; antialias
            FALSE                ; tile
            FALSE                ; new image
            TRUE                 ; transparency
            radius               ; Sphere/cylinder radius (only used when maptype=1 or 3)
            0.5 0.5 0.5          ; box size
            1.00                 ; unused parameter Cylinder length
            -1                   ; Box front face (set these to -1 if not used)
            -1                   ; Box back face
            -1                   ; Box top face 
            -1                   ; Box bottom face
            -1                   ; Box left face
            -1                   ; Box right face
            -1                   ; Cylinder top face (unused parameter)
            -1)                  ; Cylinder bottom face (unused parameter)
            
            ;(plug-in-autocrop-layer 1 image sphere)
            
            
        (let ((stack 0)) ;stack 0=above 1=below 
            (gimp-image-insert-layer image blur-layer (car (gimp-item-get-parent drawable)) 
                (+ (car (gimp-image-get-item-position image drawable)) stack))
        )
        
        (gimp-item-set-name blur-layer "Motion Blur")      
        (if (= alpha FALSE) (gimp-layer-add-alpha blur-layer))
        
        (plug-in-mblur 1 ;run-mode 
                    image 
                    blur-layer ;drawable 
                    2 ;Type of motion blur { LINEAR (0), RADIAL (1), ZOOM (2) } 
                    20 ;length 
                    10 ;angle 
                    (/ width 2) ;center-x 
                    (/ height 2)) ;center-y
        
        ;;;;create the mask	
        
        (let ((blur-mask (car (gimp-layer-create-mask blur-layer ADD-WHITE-MASK)))) 
            (gimp-layer-add-mask blur-layer blur-mask)
            
            (gimp-edit-blend blur-mask  BLEND-FG-TRANSPARENT LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE x1 y1 x2 y2)	
            ;  (gimp-layer-remove-mask blur-layer MASK-APPLY)
        ) ;endlet
        (if (= conserve FALSE)
            (begin
                (set! drawable (car (gimp-image-merge-down image blur-layer EXPAND-AS-NECESSARY)))
                (set! drawable (car (gimp-image-merge-down image sphere EXPAND-AS-NECESSARY)))
            )
        ) ;endif
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        
    )
) 

(script-fu-register "script-fu-manhattan"               
    "Manhattan Sphere"
    "Transforms the centre portion of a wide image into a sphere. Interesting when the image is of a cityscape. \nfile:Manhattan.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "2012"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-ADJUSTMENT "Sphere Radius" '(.30 .1 2 .01 .1 2 0)
    SF-ADJUSTMENT "Sphere Horizontal Rotation" '(0 -108 108 .1 10 1 0)
    SF-ADJUSTMENT "Sphere Vertical Rotation" '(0 -108 108 .1 10 1 0)
    SF-TOGGLE     "Keep the Layers"   TRUE
)

(script-fu-menu-register "script-fu-manhattan" "<Image>/Script-Fu/Effects")

;end of script