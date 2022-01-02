; Metallica rel 0.02.2
; Created by Graechan
; Thanks to NIXNINE for the tutorial this script follows
; You will need to install GMIC to run this Scipt
; GMIC can be downloaded from http://sourceforge.net/projects/gmic/files/ 
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
; Rel 0.02 - update 26-10
; Rel 0.2.1 - update 27-10
; Rel 0.2.2 - update 28-10
;
(define (script-fu-metallica                                      
                            text                                       
                            font-in 
                            size
                            justify
                            grow
                            letter-spacing
                            line-spacing
                            type
                            plating
                                      3d-fgd-height
                                      x-angle
                                      bev-width
                                      depth                                      
                                      bkg-type 
                                      pattern
                                      bkg-color
                                      conserve)
  (let* (         
         (image (car (gimp-image-new 256 256 RGB)))
         
         (border (/ size 4))
         (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
         (size-layer (car (gimp-text-fontname image -1 0 0 text border TRUE size PIXELS font)))
         (final-width (car (gimp-drawable-width size-layer)))
         (final-height (car (gimp-drawable-height size-layer)))
         (text-layer 0)
         (width 0)
         (height 0)
         (justify (cond ((= justify 0) 2)
                        ((= justify 1) 0)
                        ((= justify 2) 1)))
         (inner-bevel-layer 0)
         (bkg-layer 0) 
         (azimuth 135)
         (elevation 35)         
         (postblur 3.0)
         (lmode 0)
         (up 1)
         (inner 1)         
         (shape 0)
         (prenoise 0)        
         (opacity 100)
         (gloss 10)
         (ialpha 0)
         (metal-layer 0)
         (shadow-size 8)
         (shadow-opacity 50)
         )
    (gimp-context-push)
    
    ;;;;adjust the size-layer
    (gimp-text-layer-set-justification size-layer justify)
    (gimp-text-layer-set-letter-spacing size-layer letter-spacing)
    (gimp-text-layer-set-line-spacing size-layer line-spacing)
    (set! final-width (car (gimp-drawable-width size-layer)))
    (set! final-height (car (gimp-drawable-height size-layer)))	
    
    
    ;;;;Add the text layer for a temporary larger Image size
    (cond ((= type 0) (set! text-layer (car (gimp-text-fontname image -1 0 0 text 158 TRUE 632 PIXELS font))))	      
        (else (set! text-layer (car (gimp-text-fontname image -1 0 0 text -1 TRUE 250 PIXELS font)))))
    ;;;;adjust text 
    (gimp-text-layer-set-justification text-layer justify)
    (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
    (gimp-text-layer-set-line-spacing text-layer line-spacing)
    (set! width (car (gimp-drawable-width text-layer)))
    (set! height (car (gimp-drawable-height text-layer)))    
    (gimp-image-remove-layer image size-layer)
    (gimp-image-resize-to-layers image)    
    (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer image bkg-layer 0 1)
    
    ;;;;Expand the font if needed
    (if (> grow 0)
        (begin
            (gimp-selection-layer-alpha text-layer)
            (gimp-edit-clear text-layer)
            (gimp-selection-grow image grow)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill text-layer FILL-FOREGROUND)
            (gimp-selection-none image)
        )
    )
     
    ;;;;Add new layer with Bevel
    (if (= type 0)
        (begin
            (gimp-selection-layer-alpha text-layer)
            (gimp-image-set-active-layer image text-layer)    
            (set! inner-bevel-layer (car (gimp-layer-new image width height RGBA-IMAGE "Inner bevel" 100 LAYER-MODE-NORMAL)))
            (gimp-image-insert-layer image inner-bevel-layer 0 -1)
            (gimp-image-set-active-layer image inner-bevel-layer)    
            (gimp-context-set-foreground '(0 0 0))
            (gimp-context-set-background '(255 255 255))
            (gimp-edit-fill inner-bevel-layer FILL-FOREGROUND)    
            (gimp-selection-shrink image 1)
            (gimp-selection-feather image bev-width)
            (gimp-selection-shrink image (- (/ bev-width 2) 1))
            (gimp-edit-fill inner-bevel-layer FILL-BACKGROUND)
            (gimp-selection-all image)
            (gimp-drawable-invert inner-bevel-layer TRUE)
            (plug-in-emboss RUN-NONINTERACTIVE image inner-bevel-layer azimuth elevation depth 1);Emboss
            
            ;;;;gloss the bevel-layer    
            (gimp-curves-spline inner-bevel-layer 0 16 #(0 0 63 73 95 125 127 31 156 188 191 151 223 227 255 255))  
            (plug-in-gauss-rle RUN-NONINTERACTIVE image inner-bevel-layer postblur 1 1)
            
            ;;;;Clean up the layers
            (gimp-selection-layer-alpha text-layer)
            (gimp-selection-invert image)
            (gimp-edit-clear inner-bevel-layer)
            (gimp-selection-invert image)
            
            
            
            ;;;;Color the bevel layer     
            ; *******************************************start GMIC Metallic look
            
            
            
            (let *(
                    ;; Matching variables
                    (metal (car (gimp-layer-copy inner-bevel-layer TRUE)))
                           (plate 0)			
                )
                
                ;; Add a layer
                (gimp-image-insert-layer image metal 0 -1)
                                
                     
                ;; name the layer
                (gimp-drawable-set-name metal "Metal")
                     
                     (if (= plating 0) (set! plate 2))
                     (if (= plating 1) (set! plate 3))
                     (if (= plating 2) (set! plate 1))
                     (if (= plating 3) (set! plate 0))
                     (if (= plating 4) (set! plate 4))			
                        
                ;; Render Metallic look using G'MIC.
                (plug-in-gmic-qt 1 image metal 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_tk_metallic 1,0,"
                                (number->string plate)  
                    )
                )         				
            )
            (gimp-image-remove-layer image text-layer)
            (gimp-selection-none image) 
                      
            ; *******************************************end Metallic look
        )
    )
    
    
    ; *******************************************start 3 Dimensional Image
    ;
    (if (= type 1)
        (begin
            
            (let* (
                    ;; Matching variables
                    (3d_fgd (car (gimp-layer-copy text-layer TRUE)))
                    (txt-width (car (gimp-drawable-width text-layer)))
                    (txt-height (car (gimp-drawable-height text-layer)))
                    (3d-height 3d-fgd-height)
                    (plate 0)                           
                )
                
                ;; Add a layers
                (gimp-image-insert-layer image 3d_fgd -1)
                
                ;; name the layers
                (gimp-item-set-name 3d_fgd "3D_fgd")
                
                
                ;; Render extrude3d using G'MIC.
                (plug-in-gmic-qt 1 image 3d_fgd 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                            "-fx_extrude3d " 
                                (number->string 3d-height) ",512,0.6,"
                                (number->string txt-width) ","
                                (number->string txt-height) ",0.88,"
                                (number->string x-angle) ",0,0,45,0,0,-100,0.50,0.70,4"                                
                    )
                )
                (gimp-selection-layer-alpha 3d_fgd)
                (if (= plating 0) (set! plate 2))
                (if (= plating 1) (set! plate 3))
                (if (= plating 2) (set! plate 1))
                (if (= plating 3) (set! plate 0))
                (if (= plating 4) (set! plate 4))			
                
                ;; Render Metallic look using G'MIC.
                (plug-in-gmic-qt 1 image 3d_fgd  1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_tk_metallic 1,0,"
                                (number->string plate)  
                    )
                )         
                    (gimp-image-remove-layer image text-layer)
                    (gimp-selection-none image)                      
            )
            ; *******************************************end 3 Dimensional Image
        )
    )         
    
    ;;;;Set Background Type and fill        
    (if (= bkg-type 1);;Pattern
        (begin
            (gimp-context-set-pattern pattern)
            (gimp-drawable-fill bkg-layer PATTERN-FILL)
        )
    )
    (if (= bkg-type 0);;Color
        (begin
            (gimp-context-set-background bkg-color)
            (gimp-drawable-fill bkg-layer BACKGROUND-FILL)
        )
    )
    
    ;;;;Finish the Image   
    ;;Scale Image to it's original size and merge the layers
    (gimp-image-scale-full image final-width final-height 2)
    (set! metal-layer (car (gimp-image-get-active-layer image)))
    
    (gimp-display-new image)
    (the-metallica-shine image metal-layer shadow-size shadow-opacity conserve)
    (if (= conserve FALSE)
        (begin
            (set! metal-layer (car (gimp-image-merge-visible-layers image  CLIP-TO-IMAGE)))
            (gimp-drawable-set-name metal-layer "Metallica")        
        )
    ) ;endif
    
    (gimp-context-pop)   
    (gimp-displays-flush)
    
  )
)
  
(script-fu-register "script-fu-metallica"
  "Metallica"
  "Create an image with a Metallic text Logo, (uses GMIC)"
  "Graechan"
  "Graechan"
  "October 2011"
  ""  
  SF-TEXT     "Text"               "Metallica!"  
  SF-FONT       "Font"               "Sans Bold"  
  SF-ADJUSTMENT "Font size (pixels)" '(100 25 600 1 1 0 1)
  SF-OPTION "Justify" '("Centered" "Left" "Right")
  SF-ADJUSTMENT "Expand the Font if needed" '(0 0 10 1 1 0 0)
  SF-ADJUSTMENT "Letter Spacing" '(0 -100 100 1 5 0 0)
  SF-ADJUSTMENT "Line Spacing" '(0 -100 100 1 5 0 0)
  SF-OPTION "Image Type" '("Bevelled" "3 Dimensional")
  SF-OPTION "Plating Type" '("Copper" "Bronze" "Gold" "Silver" "Blue Steel")
  SF-ADJUSTMENT "3D-Text-height" '(70 1 256 1 10 0 0)
  SF-ADJUSTMENT "'X' Angle" '(45 0 270 0.1 5 1 0)
  SF-ADJUSTMENT "Bevel Width" '(50 1 100 1 10 0 0)
  SF-ADJUSTMENT "Bevel Depth" '(30 1 60 1 10 0 0)  
  SF-OPTION "Background Type" '("Color" "Pattern" "None")
  SF-PATTERN    "Pattern"            "Pine?"
  SF-COLOR      "Background color"         '(0 0 0)
  SF-TOGGLE     "Keep the Layers"   FALSE
)
  
(script-fu-menu-register "script-fu-metallica" "<Image>/Script-Fu/Logos")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define (the-metallica-shine image drawable
                              shadow-size
                              shadow-opacity
                              conserve)
                              

    (let* (
            ;(image (car (gimp-image-duplicate inImage)))
            (image-layer (car (gimp-image-get-active-layer image)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (sel (car (gimp-selection-is-empty image)))
            (img-layer 0)
            (img-channel 0)
            (bkg-layer 0)
            (shadow-layer 0)
            (tmp-layer 0)
        )
        
        
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (gimp-layer-add-alpha image-layer)
        (if (= sel TRUE) (gimp-selection-layer-alpha image-layer))
        (set! img-layer (car (gimp-layer-new image width height RGBA-IMAGE "img-layer" 100  LAYER-MODE-GRAIN-MERGE)))
        (gimp-image-add-layer image img-layer -1)
        (gimp-drawable-fill img-layer  FILL-BACKGROUND)
        (gimp-edit-fill img-layer FILL-FOREGROUND)
        ;;;;create channel
        (gimp-selection-save image)
        (set! img-channel (car (gimp-image-get-active-drawable image)))	
        (gimp-channel-set-opacity img-channel 100)	
        (gimp-drawable-set-name img-channel "img-channel")
        (gimp-image-set-active-layer image img-layer)	
        (gimp-drawable-set-name image-layer "Original Image")
        
        ;;;;create the background layer    
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-add-layer image bkg-layer 1)	
        
        ;;;;apply the image effects
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image img-layer 12 12)
        (plug-in-emboss RUN-NONINTERACTIVE image img-layer 225 84 10 TRUE)	
        (gimp-selection-invert image)
        (gimp-edit-clear img-layer)
        (gimp-selection-invert image)
        (plug-in-colortoalpha RUN-NONINTERACTIVE image img-layer '(254 254 254));;fefefe
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image img-channel 15 15)
        (plug-in-blur RUN-NONINTERACTIVE image img-layer)
        (gimp-image-set-active-layer image bkg-layer)
        (plug-in-displace RUN-NONINTERACTIVE image bkg-layer 8 8 TRUE TRUE img-channel img-channel 1)
        
        ;;;;create the shadow
        (if (> shadow-size 0)
            (begin
                (script-fu-drop-shadow image img-layer shadow-size shadow-size shadow-size '(0 0 0) shadow-opacity FALSE)
                (set! tmp-layer (car (gimp-layer-new image width height RGBA-IMAGE "temp" 100 LAYER-MODE-NORMAL)))
                (gimp-image-add-layer image tmp-layer -1)
                (gimp-image-raise-layer image tmp-layer)
                (gimp-image-merge-down image tmp-layer CLIP-TO-IMAGE)
                (set! shadow-layer (car (gimp-image-get-active-drawable image)))
                (gimp-image-lower-layer image shadow-layer)
                (gimp-image-lower-layer image bkg-layer)
            )
        )
        (gimp-image-set-active-layer image image-layer)
        (if (= conserve FALSE) (gimp-image-merge-visible-layers image  EXPAND-AS-NECESSARY))
        
        (gimp-selection-none image)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        ;(gimp-display-new image)
        (gimp-displays-flush)
        
    )
)

;end of script
