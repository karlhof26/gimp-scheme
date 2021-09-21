;A script that creates a comic template. 
;
;
; Comic Template rel 0.02 
; Created by karlhof26
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
; Rel 0.02 - Updated for Gimp 2.10.18

(define (comicpanels-kh width height horiz vert border )
    ;set image size 
    
    (let* (
            (page-width (+ (* (+ width border) horiz) border))
            (page-height (+ (* (+ height border) vert) border))
            (img (car (gimp-image-new page-width page-height 0 )))
            (layer (car (gimp-layer-new img page-width page-height 0 "Comic" 100 0)))
            (flag 0)
            (flagv 0)
        )
        
        
        (gimp-image-insert-layer img layer 0 0)
        ;add alpha channel to layer
        (gimp-layer-add-alpha layer)
        ;set resolution to 300dpi for printing 
        
        (gimp-image-set-resolution img 300 300)
        
        (gimp-edit-clear layer)
        ;set forground color to white and fill background
        (gimp-context-set-foreground '(255 255 255))
        
        (gimp-drawable-fill layer FILL-FOREGROUND)
        
        ;select rectangles  for panels
        (while (< flag horiz)
            (set! flagv 0)
            (while (< flagv vert)
                
                ;(gimp-rect-select img (+ border (* width flag) (* border flag)) (+ border (* height flagv) (* border flagv)) width height 0 0 0 )
                (gimp-image-select-rectangle img CHANNEL-OP-ADD (+ border (* width flag) (* border flag)) (+ border (* height flagv) (* border flagv)) width height)
                (set! flagv (+ flagv 1))
            )
            ; Increment the flag
            (set! flag (+ flag 1))
        )
        ;(gimp-rect-select img border border width height 0 0 0 )
        (gimp-image-select-rectangle img CHANNEL-OP-ADD border border width height)
        ;(gimp-rect-select img (+ border width border) border width height 0 0 0 )
        (gimp-image-select-rectangle img CHANNEL-OP-ADD (+ border width border) border width height )
        ;(gimp-rect-select img border (+ border height border) width height 0 0 0 )
        (gimp-image-select-rectangle img CHANNEL-OP-ADD border (+ border height border) width height )
        ;(gimp-rect-select img (+ border width border) (+ border height border) width height 0 0 0 )
        (gimp-image-select-rectangle img CHANNEL-OP-ADD (+ border width border) (+ border height border) width height)
        
        
        ;set forground colour to black and stroke the selection to outline the panels
        (gimp-context-set-foreground '(0 0 0))
        
        ;(gimp-edit-stroke layer)
        (gimp-context-set-paint-method "gimp-paintbrush")
        (gimp-context-set-stroke-method STROKE-PAINT-METHOD)
        (gimp-context-set-brush "Circle (05)") ; was brushTemp variable
        
        ;(gimp-brush-set-shape brushTemp BRUSH-GENERATED-CIRCLE)
        ;(gimp-brush-set-hardness brushTemp 0.99)
        ;(gimp-brush-set-radius brushTemp (+ (/ inStroke 2) 1.0))
        ;(gimp-brush-set-spacing brushTemp 20.0)
        ;(gimp-brush-set-spikes brushTemp 2)
        ;(gimp-brush-set-aspect-ratio brushTemp 1.0)
        ;(gimp-brush-set-angle brushTemp 1.0) 
                
        (gimp-drawable-edit-stroke-selection layer)
        
        (gimp-edit-clear layer)
        
        (gimp-selection-none img)
        
        ;display image
        (gimp-display-new img)
    )
)

(script-fu-register "comicpanels-kh"
    "Comic Panels"
    "Creates a comic template. \n file:comicpanels_0.scm"
    "karlhof26"
    "2020 karlhof26"
    "Marhc  2020"
    ""
    SF-VALUE   "Panel width"        "300" 
    SF-VALUE   "Panel height"       "300" 
    SF-VALUE   "horizontal-panels"  "2"
    SF-VALUE   "vertical-panels"    "2"
    SF-VALUE   "border"             "50"
    
)

(script-fu-menu-register "comicpanels-kh"
                         "<Toolbox>/Script-Fu/Artistic/")
                         
; end of script