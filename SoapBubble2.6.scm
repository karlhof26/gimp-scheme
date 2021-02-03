;This Script Fu is meant to create a Soap Bubble 
;For any info/idea/suggestions email me at <moozpan@walla.com>
;Original tutorial for making the soap buble can be found at www.gimptalk.com
;Origianl Tutorial by ClayOgre
;I take credit only for making the tutorial Script Fu Compatiable, finding a similar automatic way and writing the script itself
;Updated for Gimp 2.6 by GnuTux :http://www.gimpchat.com
;Change menu location to Filters/Render - GnuTux
;Version 1.2
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
; Rel 1.2 - Initial Release

(define (script-fu-soap width height manual seed turb blur rotation merge)
        
    (let*
          (
            (img (car (gimp-image-new (* 2 width) (* 2 height) RGB)))
            
            (bubble1 (car (gimp-layer-new img (* 2 width) (* 2 height) RGB-IMAGE "Bubble - Color 1" 35 LAYER-MODE-NORMAL)))
            (rim1 (car (gimp-layer-new img (* 2 width) (* 2 height) RGB-IMAGE "White Rim" 35 LAYER-MODE-NORMAL)))
            (rim2 (car (gimp-layer-new img (* 2 width) (* 2 height) RGB-IMAGE "Black Rim" 35 LAYER-MODE-NORMAL)))
            (light (car (gimp-layer-new img (* 2 width) (* 2 height) RGB-IMAGE "HighLight" 35 LAYER-MODE-NORMAL)))
            (shade (car (gimp-layer-new img (* 2 width) (* 2 height) RGB-IMAGE "Outer Shade" 100 LAYER-MODE-NORMAL)))
            
            (bubble2) ; to be defined later
            (temp)
            (chan1)
            (chan2)
            (chan3)
            (space)
            
          )
        ;Push Context
        (gimp-context-push)
        
        ;adding base layer
        (gimp-image-insert-layer img bubble1 0 -1)
        (gimp-layer-add-alpha bubble1)
        
        ;begining the real work - colring and making the bubbles
        
        (plug-in-plasma 1 img bubble1 seed turb)
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        (plug-in-gauss 1 img bubble1 blur blur 0)
        
        (if (= manual 1)
            (begin
                ;this option is now Alternate warp method
                ; IWARP IS Deprecated
                ;;;(plug-in-iwarp 0 img bubble1)
            )
        )
        
        (if  (= manual 0) 
            (begin 
                (gimp-layer-set-opacity bubble1 75)
                
                ;Duplicating the first layer on grain extract mode 
                
                (set! bubble2(car (gimp-layer-new-from-drawable bubble1 img)))
                (gimp-image-add-layer img bubble2 35)
                (gimp-layer-add-alpha bubble2)
                (gimp-layer-set-mode bubble2 20)
                (gimp-layer-set-opacity bubble2 100)
                
                ;Since in the original tutorial we have iWarp and it can't be done automatically in GIMP, here is a substetute that gave me a good result. we willl do it on bubble2
                ;at the end it will be merged with bubble1 and it will be set to bubble1 
                
                (plug-in-waves 1 img bubble2 16.16 0 35.63 2 0)
                (plug-in-whirl-pinch 1 img bubble2 309 0.632 0.822)
                
                ;(gimp-display-new img)
                ;(quit)
                
                (plug-in-illusion 1 img bubble2 64 0)
                (plug-in-gauss 1 img bubble2 (/ blur 2) (/ blur 2) 0)
                
                (set! bubble1(car (gimp-image-merge-visible-layers img 1)))
                (gimp-layer-set-opacity bubble1 35)
                (plug-in-whirl-pinch 1 img bubble1 -134 -0.643 1.351)
            )
        )
        (gimp-levels bubble1 0 30 222 0.86 0 255)
        
        ;Now, duplicating layer one like in the original tutorial
        (if (= manual 1) 
            (begin 
                (set! bubble2(car (gimp-layer-new-from-drawable bubble1 img)))
                (gimp-image-add-layer img bubble2 0)
                (gimp-layer-add-alpha bubble2)
                
                (plug-in-waves 1 img bubble2 13.16 0 45.63 2 0) ; was 16.16 0 35.63 2 0
                (plug-in-whirl-pinch 1 img bubble2 271 0.732 0.486) ; was 309 0.632 0.822
                
                ;Now, Mapping to object. the second layer is mapped in a rotation for more depth.
                
                (plug-in-map-object 1 img bubble2 1 0.5 0.5 2.0 0.5 0.5 0.0 1.0 0.0 0.0 0.0 1.0 0.0 0.0 -90 0.0 3 '(255 255 255)-0.5 -0.5 2.0 -1.0 -1.0 1.0 0.3 1.0 0.5 0.0 27.0 TRUE FALSE FALSE TRUE 0.25 1.0 1.0 1.0 1.0 -1 -1 -1 -1 -1 -1 -1 -1)
            )
        )
        (plug-in-map-object 1 img bubble1 1 0.5 0.5 2.0 0.5 0.5 0.0 1.0 0.0 0.0 0.0 1.0 0.0 0.0 90 0.0 3 '(255 255 255)-0.5 -0.5 2.0 -1.0 -1.0 1.0 0.3 1.0 0.5 0.0 27.0 TRUE FALSE FALSE TRUE 0.25 1.0 1.0 1.0 1.0 -1 -1 -1 -1 -1 -1 -1 -1)
        
        ;(gimp-display-new img)
        ;Here is the rest if not manually
        (if (= manual 0)
            (begin
                (set! bubble2(car (gimp-layer-new-from-drawable bubble1 img)))
                (gimp-image-add-layer img bubble2 0)
                (gimp-layer-add-alpha bubble2)
                (plug-in-polar-coords 1 img bubble2 100 0 0 1 0)
                
                (set! temp(car (gimp-layer-new-from-drawable bubble2 img)))
                (gimp-image-add-layer img temp 0)
                (gimp-drawable-transform-flip-simple temp 1 1 1 1)
                (set! bubble2(car (gimp-image-merge-down img temp 1)))
                (plug-in-polar-coords 1 img bubble2 100 0 0 1 1)
                (gimp-fuzzy-select bubble2 1 1 0 2 TRUE 0 0 0)
                (gimp-edit-clear bubble2)
                (gimp-selection-none img)
                (gimp-layer-scale bubble2 width height 1)
                (plug-in-gauss 1 img bubble2 (/ blur 2) (/ blur 2) 0)
                (plug-in-whirl-pinch 1 img bubble2 114 1 1)
                
                
            )
        )
        
        ;Adding the other layers, adding them alpha and clearing
        
        (gimp-image-add-layer img rim2 0)
        (gimp-layer-add-alpha rim2)
        (gimp-edit-clear rim2)
        
        (gimp-image-add-layer img rim1 0)
        (gimp-layer-add-alpha rim1)
        (gimp-edit-clear rim1)
        
        (gimp-image-add-layer img light 0)
        (gimp-layer-add-alpha light)
        (gimp-edit-clear light)
        
        (gimp-image-add-layer img shade 0)
        (gimp-layer-add-alpha shade)
        (gimp-edit-clear shade)
        
        ;making the channels/paths
        
        (gimp-selection-layer-alpha bubble1)
        (set! chan1(car (gimp-selection-save img))) ;whole bubble
        (gimp-selection-shrink img 5)
        (set! chan2(car (gimp-selection-save img))) ;bubble shrink, blur
        (gimp-selection-none img)
        (plug-in-gauss 1 img chan2 6 6 0)
        
        ;making the white rim
        
        (gimp-selection-load chan1)
        (gimp-selection-combine chan2 1) ;mode 1 is substract
        (gimp-edit-fill rim1 2)
        
        ;making the black rim
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-brush "Circle Fuzzy (07)")
        (gimp-selection-load chan1)
        (gimp-edit-stroke rim2)
        (gimp-selection-invert img)
        (gimp-edit-clear rim2)
        
        ;making the highlights
        
        (gimp-ellipse-select img (* 0.73 width) (* 0.6 height) (* 0.54 width) (* 0.3 height) 2 TRUE 0 0)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-blend light 2 0 0 100 0 0 FALSE FALSE 0 0 0 width (* 0.6 height) width (* 0.85 height))
        
        ;making the second highlight
        
        (gimp-selection-load chan1)
        (gimp-ellipse-select img (* width 0.5) (* height 0.32) width (* 1.02 height) 1 TRUE 0 0)
        (set! chan3(car (gimp-selection-save img)))
        (gimp-selection-none img)
        (plug-in-gauss 1 img chan3 30 30 0)
        (gimp-selection-load chan3)
        (gimp-selection-combine chan1 3)
        (gimp-edit-fill light 2)
        
        ;making shade/glow around the bubble
        (gimp-selection-load chan1)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill shade 0)
        (gimp-selection-none img)
        (plug-in-gauss 1 img shade 30 30 0)
        (gimp-selection-load chan1)
        (gimp-edit-clear shade)
        (gimp-selection-none img)
        
        ;rotating the higlight
        
        (set! rotation(* (/ rotation 180) 3.14159))	;converting to radians
        (gimp-drawable-transform-rotate-default light rotation 1 width height 1 0)
        
        ;finanlly: merging the layers
        
        (if  (= merge 1)
            (gimp-image-merge-visible-layers img 1)
            ()
        )
        
        (gimp-display-new img)
        
        ;Restore Gimp Context
        (gimp-context-pop)
    )
)


(script-fu-register "script-fu-soap"
            "<Toolbox>/Script-Fu/Render/Soap Bubble2..."
            "Draw a Soap Bubble. Most values can be determined by the user. although there are also deafult values provided for simple use of the script. \nfile:SoapBubble2.6.scm"
            "LightningIsMyName (LIMN) and ClayOgre"
            "LightningIsMyName (LIMN)"
            "November 2007"
            ""
            SF-VALUE      "Bubble Width (px)"   "200"
            SF-VALUE      "Bubble Height (px)"  "200"
            SF-TOGGLE     "Alternate Warp method" FALSE
            SF-ADJUSTMENT "Random Seed"         '(0 0 429496729 1 10 0 1)  ;for some reason i can't fet the max value of the randpm seed to it's true max of 4294967295 like in gimp 2.2. please fix it if you can.
            SF-ADJUSTMENT "Turbulence"         '(3.2 0.1 7 0.1 1 1 1)
            SF-ADJUSTMENT "Blur Factor"         '(8 0 50 0.1 1 1 1)
            SF-ADJUSTMENT "HighLight Rotation"         '(0 -180 180 0.1 1 1 1)
            SF-TOGGLE "Merge all layers when done" TRUE
            
)

; end of script