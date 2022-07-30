; shine alpha-to-logo rel 0.01
; Created by Graechan
;  
;   
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
; Rel 0.02 - Updated by karlhof26 to wor with Gimp 2.10.18
;
;

(define (script-fu-shine image drawable
                              shadow-size
                              shadow-opacity
                              keep-selection-in
                              conserve)
                              

 (let* (
            (image-layer (car (gimp-image-get-active-layer image)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (sel (car (gimp-selection-is-empty image)))
            (alpha (car (gimp-drawable-has-alpha image-layer)))
            (keep-selection keep-selection-in)
            (layer-name (car (gimp-drawable-get-name image-layer)))
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
        
        (if (= alpha FALSE) (gimp-layer-add-alpha image-layer))
        
        (if (= sel TRUE) (set! keep-selection FALSE))
        (if (= sel TRUE) (gimp-selection-layer-alpha image-layer))
        
        (set! img-layer (car (gimp-layer-new image width height RGBA-IMAGE "Shine-layer" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image img-layer 0 -1)
        (gimp-drawable-fill img-layer  FILL-BACKGROUND)
        (gimp-edit-fill img-layer FILL-FOREGROUND)
        
       ; (gimp-message "line 68")
        ;(gimp-displays-flush)
        ;(quit)
        
        ;;;;create channel
        (gimp-selection-save image)
        (set! img-channel (car (gimp-image-get-active-drawable image)))
        (gimp-channel-set-opacity img-channel 100) 
        (gimp-drawable-set-name img-channel "img-channel")
        (gimp-image-set-active-layer image image-layer) ; was img-layer
        (gimp-drawable-set-name image-layer "Original Image")
        
        ;;;;create the background layer    
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 1)
        
       ; (gimp-message "line 84")
        (gimp-displays-flush)
        ;(quit)
        
        ;;;;apply the image effects
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        ;added by karlhof26 to make it work
        (plug-in-hsv-noise RUN-NONINTERACTIVE image img-layer 6 12 90 240)
        
        
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image img-layer 8 8); was img-layer and 12 12
        
        (plug-in-emboss RUN-NONINTERACTIVE image img-layer 225 84 10 TRUE) ; was img-layer
        ;(gimp-displays-flush)
        ;(quit)
        
        ;(plug-in-gauss-rle2 RUN-NONINTERACTIVE image image-layer 12 12); was img-layer
        ;(plug-in-emboss RUN-NONINTERACTIVE image image-layer 225 84 10 TRUE) ; was img-layer
        
       ; (gimp-message "line 92")
        (gimp-displays-flush)
       ; (quit)
        
        (gimp-selection-invert image)
        (gimp-edit-clear img-layer)
        (gimp-selection-invert image)
        (plug-in-colortoalpha RUN-NONINTERACTIVE image img-layer '(254 254 254));;fefefe 
        
        ;(gimp-message "line 111")
        (gimp-displays-flush)
       ; (quit)
       
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image img-channel 15 15)
        
        (plug-in-blur RUN-NONINTERACTIVE image img-layer)
        
        (gimp-image-set-active-layer image bkg-layer)
        (plug-in-displace RUN-NONINTERACTIVE image bkg-layer 8 8 TRUE TRUE img-channel img-channel 1)
        
        (gimp-displays-flush)
       ; (quit)
        
        (gimp-image-remove-layer image bkg-layer)
        
        (gimp-displays-flush)
       ; (quit)
       
        ;;;;create the shadow
        (if (> shadow-size 0)
            (begin
                ;(gimp-message "inside shadow")
                (script-fu-drop-shadow image img-layer shadow-size shadow-size shadow-size '(0 0 0) shadow-opacity FALSE)
                
                (gimp-displays-flush)
               ; (quit)
                
                (set! tmp-layer (car (gimp-layer-new image width height RGBA-IMAGE "temp" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image tmp-layer 0 -1)
                (gimp-image-raise-layer image tmp-layer)
                (gimp-image-merge-down image tmp-layer CLIP-TO-IMAGE)
                (set! shadow-layer (car (gimp-image-get-active-drawable image)))
                (gimp-image-lower-layer image shadow-layer)
               ; (gimp-message "shadow done")
            )
        )
        
        (if (= conserve FALSE)
            (begin
                (if (> shadow-size 0)
                    (set! img-layer (car (gimp-image-merge-down image img-layer EXPAND-AS-NECESSARY)))
                )
                (set! image-layer (car (gimp-image-merge-down image img-layer EXPAND-AS-NECESSARY)))
            )
        )
        (gimp-drawable-set-name image-layer layer-name)
        
        (if (= keep-selection FALSE) (gimp-selection-none image))
        (gimp-image-remove-channel image img-channel)
        (if (and (= conserve FALSE) (= alpha FALSE) (gimp-layer-flatten image-layer)))
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        ;(gimp-display-new image)
        (gimp-displays-flush)
        (gimp-message "Good finish OK")
    )
)

(script-fu-register "script-fu-shine"            
                    "Shine"
                     "Shine the Image (Image must have transparent background). There must be a selection. \n file:Shine_02.scm"
                    "Graechan"
                    "Graechan"
                    "1/6/2012"
                    "RGB*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-ADJUSTMENT "Shadow Size" '(8 0 30 1 10 0 0)
                    SF-ADJUSTMENT "Shadow Opacity" '(50 0 100 1 10 0 0)
                    SF-TOGGLE     "Keep selection"          FALSE
                    SF-TOGGLE     "Keep layers?" FALSE
)

(script-fu-menu-register "script-fu-shine" "<Toolbox>/Script-Fu/Effects/")


; end of script