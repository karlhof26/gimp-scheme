; cobblestone-mosaic-2.6 Rel. 0.6 Created by graechan
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
;    This Script follows an excelent tutorial by lylejk 
;    at http://www.gimpchat.com/viewtopic.php?f=10&t=3562
;    You will need to install the plugin GMIC to use the script 
;    available here https://sourceforge.net/projects/gmic/files/
;
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release 
; Rel 0.02 - Added menu items 'tile-type' 'tile-size' 'tile height' tile-spacing' and 'polish'
; Rel 0.03 - Added Gloss to menu
; Rel 0.04 - Added menu adjustments fo Gloss (glowing,brightness,shine and polish)
; Rel 0.05 - Added code to respect selections and fixed 2 more bugs
; Rel 0.06 - Bug fixes
;;;;
(define (script-fu-cobblestone-mosaic image drawable
                                            tile-type
                                            tile-size
                                            tile-height
                                            tile-spacing
                                            tile-neatness
                                            tile-allow-split
                                            col-dpth
                                            gloss
                                            glow
                                            bright
                                            shine
                                            polish
                                            grout-height
                                            bluramount
											conserve)
							  

 (let* (
            (image-layer (car (gimp-image-get-active-layer image)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (sel (car (gimp-selection-is-empty image)))
            (type (car (gimp-image-base-type image)))
            (orig-channel 0)
            (grout-layer 0)
            (blur-layer 0)
            (noise-layer 0)
            (noise1-layer 0)
            (selblur-layer 0)
            (gloss-layer 0)
            (image-channel 0)
            
            (wasgloss FALSE)
        )
        
        
    (gimp-context-push)
    (gimp-image-undo-group-start image)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;;;;convert image to RGB and add alpha channel    
    (if (> (car (gimp-image-base-type image)) 0) (gimp-image-convert-rgb image))
    (gimp-layer-add-alpha image-layer)
    
    ;;;;save the selection from the original image to channel
    (if (= sel FALSE)
        (begin
            (gimp-selection-save image)
            (set! orig-channel (car (gimp-image-get-active-drawable image)))
            (gimp-channel-set-opacity orig-channel 100) 
            (gimp-drawable-set-name orig-channel "Original-channel")
        )
    )
    ;;;;create a selection and save to channel 	
    (if (= sel TRUE)
        (begin
            (gimp-selection-layer-alpha image-layer)
            (gimp-selection-save image)
            (set! orig-channel (car (gimp-image-get-active-drawable image)))	
            (gimp-channel-set-opacity orig-channel 100)	
            (gimp-drawable-set-name orig-channel "Original-channel")
        )
    )
    
    ;;;;create mosaic	
    
    (plug-in-mosaic 1 image image-layer tile-size tile-height tile-spacing tile-neatness tile-allow-split 135.0 0.20 TRUE TRUE tile-type 0 FALSE)
    
    
    ; *******************************************start GMIC Morpho
    
    
    
    (let*
        (
            ;; Matching variables
            (gmic-layer (car (gimp-layer-copy image-layer TRUE)))                           			
        )
        
        ;; Add a layer
        (gimp-image-insert-layer image gmic-layer 0 1)
        
        
        ;; name the layer
        (gimp-drawable-set-name gmic-layer "GMIC Layer")
                     
                     
        
        ;; Render  using G'MIC.
        (plug-in-gmic-qt 1 image gmic-layer 1 0
                (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        ;"-morpho 1,6,0,2,0,1"
                        "-fx_morphopaint 1,6,0,2,0,1"
                )
        )
        ;; Render smoothing using G'MIC.
        (plug-in-gmic-qt 1 image gmic-layer 1 0
                (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_smooth_anisotropic 60,1,1,4,1,1,30,2,0,1,1,1,1"
                )
        )
    )
    
    ; *******************************************end GMIC Morpho
    (if (> bluramount 0)
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image image-layer bluramount bluramount) ; was 5 5
    )
    (set! image-layer (car (gimp-image-merge-down image image-layer CLIP-TO-IMAGE)))
    (gimp-drawable-set-name image-layer "Image Layer")
    
    ;;;;create the grout-layer    
    (set! grout-layer (car (gimp-layer-new image width height RGBA-IMAGE "Grout Layer" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer image grout-layer 0 1)
    (gimp-context-set-foreground '(127 127 127))
    (gimp-edit-fill grout-layer FILL-FOREGROUND)
    
    (if (> grout-height 0)
        (plug-in-bump-map 1 image grout-layer image-layer 135 45 grout-height 0 0 0 0 TRUE TRUE 0)
    )
    
    (gimp-selection-layer-alpha image-layer)
    (gimp-edit-clear grout-layer)
    (gimp-selection-none image)
    (set! blur-layer (car (gimp-layer-copy grout-layer TRUE)))
    (gimp-image-insert-layer image blur-layer 0 -1)
    (gimp-drawable-set-name blur-layer "Blur Layer")
    (plug-in-gauss-rle2 RUN-NONINTERACTIVE image blur-layer 5 5)
    
    (if (> grout-height 0)
        (plug-in-bump-map 1 image grout-layer blur-layer 135 45 grout-height 0 0 0 0 TRUE TRUE 0)
    )
    
   ; (gimp-image-remove-layer image blur-layer)
    
    ;;;;create the noise-layer    
    (gimp-selection-load orig-channel)
    (set! noise-layer (car (gimp-layer-new image width height RGBA-IMAGE "Noise Layer" 100 LAYER-MODE-MULTIPLY)))
    (gimp-image-insert-layer image noise-layer 0 -1)
    (gimp-context-set-background '(255 255 255))
    (gimp-edit-fill noise-layer FILL-BACKGROUND)
    (plug-in-rgb-noise 1 image noise-layer FALSE TRUE 0.50 0.50 0.50 0.00)
    (set! noise1-layer (car (gimp-layer-copy noise-layer TRUE)))
    (gimp-image-insert-layer image noise1-layer 0 -1)
    (gimp-drawable-set-name noise1-layer "Noise1 Layer")
    (set! noise-layer (car (gimp-image-merge-down image noise1-layer CLIP-TO-IMAGE)))
    (gimp-layer-set-mode noise-layer LAYER-MODE-MULTIPLY)
    (gimp-selection-none image)
    
    (if (> col-dpth 0)
        (begin 
            (gimp-image-set-active-layer image image-layer)
            (set! selblur-layer (car (gimp-layer-copy image-layer TRUE)))
            (gimp-image-insert-layer image selblur-layer 0 -1)
            (gimp-drawable-set-name selblur-layer "selblur Layer")
            (plug-in-sel-gauss 1 image selblur-layer 5.00 50)
            (gimp-layer-set-mode selblur-layer LAYER-MODE-GRAIN-MERGE)
            (gimp-layer-set-opacity selblur-layer col-dpth)	
        )
    )
    
    (if (= gloss TRUE)
        (begin 
            ;;;;create the gloss-layer
            (set! gloss-layer (car (gimp-layer-new image width height RGBA-IMAGE "Gloss Layer" 100 LAYER-MODE-SCREEN)))
            (gimp-image-insert-layer image gloss-layer 0 -1)
            (gimp-selection-layer-alpha image-layer)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill gloss-layer FILL-FOREGROUND)
            
            ;;;;create channel
            (gimp-selection-save image)
            (set! image-channel (car (gimp-image-get-active-drawable image)))    
            (gimp-channel-set-opacity image-channel 100)      
            (gimp-drawable-set-name image-channel "image-channel")
            (gimp-image-set-active-layer image image-layer)
            (gimp-selection-none image)
            
            (plug-in-lighting 1 
                    image              ; IMAGE
                    gloss-layer        ; DRAWABLE
                    image-channel      ; BUMP MAP
                    0                  ; ENVIRONMENT MAP
                    TRUE               ; ENABLE BUMPMAPPING
                    FALSE              ; ENABLE ENVMAPPING
                    0                  ; TYPE OF MAPPING
                    1                  ; TYPE OF LIGHTSOURCE 
                    '(255 255 255)     ; LIGHTSOURCE COLOR
                    -10                ; LIGHTSOURCE POS X
                    0                  ; LIGHTSOURCE POS Y
                    0.5                ; LIGHTSOURCE POS Z
                    -10                ; LIGHTSOURCE DIR X
                    0                  ; LIGHTSOURCE DIR Y
                    0.5                ; LIGHTSOURCE DIR Z
                    glow               ; AMBIANT INTENSITY (GLOWING)
                    bright             ; DIFFUSE INTENSITY (BRIGHT)
                    bright             ; DIFFUSE REFLECTIVITY (INTENSITY)
                    shine              ; SPECULAR REFLECTIVITY (SHINY)
                    polish               ; HIGHLIGHT (POLISHED)
                    TRUE               ; ANTIALIASING
                    FALSE              ; CREATE NEW IMAGE
                    FALSE)	           ; MAKE BACKGROUND TRANSPARENT 
            (set! wasgloss TRUE)
        )
    )
    
    
    (if (or (= conserve FALSE) (= type 2)) 
        (begin
            (set! image-layer (car (gimp-image-merge-down image selblur-layer CLIP-TO-IMAGE)))
            (if (= wasgloss TRUE) (set! image-layer (car (gimp-image-merge-down image gloss-layer CLIP-TO-IMAGE))))
            (set! grout-layer (car (gimp-image-merge-down image noise-layer CLIP-TO-IMAGE)))
            (set! image-layer (car (gimp-image-merge-down image image-layer CLIP-TO-IMAGE)))
            (gimp-drawable-set-name image-layer "Cobblestone Mosaic")
            
            (if (= type 2) (gimp-image-convert-indexed image 0 4 255 FALSE FALSE "Web"))
           
        )
    )
    (if (= type 1) (gimp-image-convert-grayscale image))
    
    
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    
 )
) 



(script-fu-register "script-fu-cobblestone-mosaic"                
                    "Cobblestone Mosaic-2.6"
                     "Needs GMIC installed to run,if you use an indexed image you cannot 'keep the layers' \nfile:Cobblestone-MosaicA_02.scm"
                    "Graechan"
                    "Graechan"
                    "January 2012"
                    "RGB* GRAY* INDEXED*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-OPTION     "Tile-Type" '("SQUARES" "HEXAGONS" "OCTAGONS")
                    SF-ADJUSTMENT "Tile-Size" '(25 5 100 .1 10 1 0)
                    SF-ADJUSTMENT "Tile-Height" '(2 1 50 .1 10 1 0)
                    SF-ADJUSTMENT "Tile-Spacing" '(10 1 50 .1 10 1 0)
                    SF-ADJUSTMENT "Tile-Neatness" '(.45 0 1 .01 1 2 0)
                    SF-TOGGLE     "Allow Tile-Splitting"   FALSE
                    SF-ADJUSTMENT "Color-Depth" '(75 0 100 1 10 0 0)
                    SF-TOGGLE     "Add Gloss"   TRUE
                    SF-ADJUSTMENT "Glowing" '(5 0 100000 .01 1 2 1)
                    SF-ADJUSTMENT "Brightness" '(1 0 100000 .01 1 2 1)
                    SF-ADJUSTMENT "Shiny" '(5 0 100000 .01 1 2 1)
                    SF-ADJUSTMENT "Polish" '(30 0 100000 .01 1 2 1)
                    SF-ADJUSTMENT "Grout Height" '(10 0 65 1 10 0 0)
                    SF-ADJUSTMENT "Blur amount" '(5 0 100 1 10 0 0)
                    SF-TOGGLE     "Keep the Layers 'except for indexed images'"   FALSE
)

(script-fu-menu-register "script-fu-cobblestone-mosaic" "<Toolbox>/Script-Fu/Artistic/")

; end of script