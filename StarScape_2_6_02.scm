; Star Scape
; Script-Fu for GIMP 2.6, by LightningIsMyName 
; 
; This Script is meant to create a star scape pattern
; 
; This script was written by LightningIsMyName
; Email: lightningismyname(at)gmail(dot)com  
; My Home Page: http://lightningismyname.deviantart.com/
;
; This Program is a free software, and you may use it for any purpose, comercial and non-commercial.
; You may not sell or redistribute this software alone or inside a bundle unless it's for FREE.
; You may modify this script only if:
; 1. You keep this message with my email, homepage, description, and release log.
; 2. You share alike with the same licensing, and without any profit.
;
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; 
; Release Log: Version 1.0 - September 2007
;              Initial release, for GIMP 2.4
;              Thanks to saulgoode from the GIMP Talk forums for helping me to debug this script
;
;              Version 1.1 - February 2009
;              Another release, for GIMP 2.6.
;              Updated the menu registration of the script

(define (script-fu-starscape width height cloudopt seed XDensity YDensity incolor)
    
    (let*
        (
            (img (car (gimp-image-new width height RGB)))
            (layerone (car (gimp-layer-new img width height RGB-IMAGE "Star Scape" 100 LAYER-MODE-NORMAL)))
            (layerclouds (car (gimp-layer-new img width height RGB-IMAGE "Clouds1" 50 LAYER-MODE-HARDLIGHT-LEGACY)))  ;18 is Hardlight mode
            (layercloudscopy (car 
               (gimp-layer-new
               img
               width 
               height
               RGB-IMAGE
               "Clouds2"
               50
               18)))
            (floating) ;Define new variable for later usage
            (seed2 21)
        )
        
        (gimp-image-undo-group-start img)
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255)) 
        
        ;Add Black Layer
        (gimp-image-insert-layer img layerone 0 0)
        
        ;Select All
        ;(gimp-rect-select img 0 0 width height ADD 0 0)
        (gimp-image-select-rectangle img CHANNEL-OP-ADD 0 0 width height)
        
        ;Fill With Black - Had problms using the fill function so i used colorize instead
        (gimp-drawable-colorize-hsl layerone 0 0 -100)
        
        ;In Order to create the Stars we will use the hurl function on the black layer
        (if (= seed 0)
            (begin
                ;(gimp-message "setting random seed")
                (set! seed2 0)
                (srand (car (gettimeofday)))
                (set! seed (rand 600123))
                (gimp-message (string-append "random seed:" (number->string seed)))
            )
        )
        (plug-in-randomize-hurl 1 img layerone 1 1 FALSE seed)
        
        ;Desaturate the noise to achieve grayscale stars
        (gimp-drawable-desaturate layerone DESATURATE-LUMINANCE)
        
        (if (< cloudopt 2)
            (begin
                ;Add the Cloud Layers
                (gimp-image-insert-layer img layerclouds 0 0)
                ;(gimp-image-insert-layer img layercloudscopy 0 0)
                
                ;Erase Junk from the clouds layers
                (gimp-edit-clear layerclouds) 
                ;(gimp-edit-clear layercloudscopy) 
                
                ;Adding the clouds to the layer    
                (if (= seed2 0)
                    (begin
                        (set! seed (rand 300123))
                    )
                )
                (plug-in-solid-noise 1 img layerclouds 0 0 seed 15 XDensity YDensity)
                
                ;copy the clouds
                ;(gimp-edit-copy layerclouds)
                
                (if (< cloudopt 1)
                    (begin
                        (set! layercloudscopy (car (gimp-layer-copy layerclouds TRUE)))
                        (gimp-image-insert-layer img layercloudscopy 0 -1)
                        
                        ;paste the clouds - define as a new layer called floating
                        ;(set! floating (car (gimp-edit-paste layercloudscopy 1)))
                        
                        ;Attach floating to second cloud layer
                        ;(gimp-floating-sel-attach floating layercloudscopy)
                        
                        
                        ;rotate cloud layer
                        (set! floating (car (gimp-item-transform-scale layercloudscopy 0 0 height width))) ; note the reversed height width
                        
                        (gimp-item-transform-rotate-simple floating 2 FALSE 0 0)
                        (gimp-item-transform-flip-simple floating 1 FALSE 0)
                        (gimp-item-transform-flip-simple floating 1 TRUE 0)
                        
                        (gimp-floating-sel-anchor floating)
                        ;(gimp-layer-scale layercloudscopy 600 600 FALSE)
                        ;(plug-in-rotate 1 img layercloudscopy 2 FALSE)
                        ;(gimp-display-new img)
                        ;(quit)
                    )
                )
            )
        )
        ;merge all layers
        (set! layerone (car (gimp-image-merge-visible-layers img 1)))
        
        ;Select All
        ;(gimp-rect-select img 0 0 width height ADD 0 0)
        (gimp-image-select-rectangle img CHANNEL-OP-ADD 0 0 width height)
        
        ;set active forground color and fill layer with color on soft light mode
        (gimp-context-set-foreground incolor)
        (gimp-edit-bucket-fill layerone 0 19 100 0 0 0 0) 
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))   
        (gimp-displays-flush img)
        (gimp-image-undo-group-end img)
        ;Display Final result 
        (gimp-display-new img)
        
     )
)


(script-fu-register  "script-fu-starscape"
              "<Image>/File/Create/Render/Star Scape..."
              "Draw a StarScape with a determined color, density and size. \n file: StarScape_2_6_02.scm"
              "LightningIsMyName (LIMN)"
              "LightningIsMyName (LIMN)"
              "September 2007"
              ""
              SF-VALUE      "Image Width (px)"      "300"
              SF-VALUE      "Image Height (px)"     "300"
              SF-OPTION     "Cloud Option"          '("Full Clouds" "1 Cloud layer" "No Clouds")
              SF-ADJUSTMENT "Clouds - Random Seed 0=randomise"      '(0 0 1294967295 1 10 0 1)
              SF-ADJUSTMENT "Clouds Density - X Size"               '(4 0.1 16 0.1 1 1 1)
              SF-ADJUSTMENT "Clouds Density - Y Size"               '(4 0.1 16 0.1 1 1 1)
              SF-COLOR      "Color"                                 '(21 76 212)
)

; end of script