; Asphalt rel 0.01a
; Created by Graechan; updated by Karl Hofmeyr (karlhof26)
; You will need to install GMIC to run this Scipt
; GMIC can be downloaded from http://gmic.eu 
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
(define (display_message 
                 text_message                              ; Message to display
                 output_destination                        ; MESSAGE-BOX (0), CONSOLE (1), ERROR-CONSOLE (2)
        )
   (let* (
          (handler_state (car (gimp-message-get-handler))) ; Save current message handler state
         )
          (gimp-message-set-handler output_destination)    ; Set message handler to the desired output destination    
          (gimp-message text_message)                      ; Display the message
          (gimp-message-set-handler handler_state)         ; Restore message handler to saved state 
   ) ;end let
) ; end define display-message procedure
;
(define (script-fu-asphalt image drawable
                                 cracks-opacity
                                 cracks-density
                                 merge)

 (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (asphalt (car (gimp-layer-new image width height RGBA-IMAGE "Asphalt" 100 LAYER-MODE-NORMAL)))
            (alpha (car (gimp-drawable-has-alpha drawable)))
        )
    (gimp-image-undo-group-start image)    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;;;;begin the script	
    
    (cond ((defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
            (gimp-image-insert-layer image asphalt (car (gimp-item-get-parent drawable)) 
            (+ (car (gimp-image-get-item-position image drawable)) 0))
           )                                             ;For GIMP 2.8 
           (else (gimp-image-add-layer image asphalt (+ (car (gimp-image-get-layer-position image drawable)) 0))) ;For GIMP 2.6 
    ) ;end cond
    
    (gimp-edit-fill asphalt FILL-FOREGROUND)
    
    (asphalt-difference-clouds image asphalt)
    (gimp-displays-flush)
    (set! asphalt (car (gimp-image-get-active-layer image)))
    
    (plug-in-hsv-noise 1 image asphalt 1 180 0 224)
    
    (plug-in-sparkle 
                     1 ;run-mode{ RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) } 
                     image   ; Input image
                     asphalt ;Input drawable 
                     0.100   ;Luminosity threshold (0.0 - 1.0) 
                     0.34    ;Flare intensity (0.0 - 1.0)
                     2       ;Spike length (in pixels)
                     5       ;# of spike points
                     14      ;Spike angle (0-360 degrees, -1: random) 
                     0.07    ;Spike density (0.0 - 1.0)
                     0       ;Transparency (0.0 - 1.0) 
                     0       ;Random hue (0.0 - 1.0)
                     0       ;Random saturation (0.0 - 1.0)
                     FALSE   ; Preserve luminosity (TRUE/FALSE)
                     FALSE   ;Inverse (TRUE/FALSE)
                     FALSE   ;Add border (TRUE/FALSE)
                     2)      ;Color of sparkles: { NATURAL (0), FOREGROUND (1), BACKGROUND (2) }
        
    (plug-in-emboss RUN-NONINTERACTIVE image asphalt 220 89 28 TRUE) ;emboss setting:TRUE=Emboss or FALSE=Bumpmap
    
    (plug-in-gauss-rle2 RUN-NONINTERACTIVE image asphalt 2 2)
    
  (if (not (defined? 'plug-in-gmic-qt))        ;Check for [G'MIC]
    (begin
      (gimp-message "Your pattern is incomplete because\nThe plugin G'MIC is not installed\nG'MIC is available at http:gmic.eu")
    )
    (begin ;else
            
            ; *******************************************[G'MIC] Cracks
            
            (let*
                (
                    ;; Matching variables
                    (cracks (car (gimp-layer-new image width height RGBA-IMAGE "Cracks" cracks-opacity LAYER-MODE-OVERLAY)))
                )
                
                ;;(gimp-message "started cracks")
                ;; Add a layer
                (cond ( (defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
                        (gimp-image-insert-layer image cracks (car (gimp-item-get-parent asphalt)) 
                            (+ (car (gimp-image-get-item-position image asphalt)) 0)
                        )
                      )                                           ;For GIMP 2.8 
                    (else (gimp-image-add-layer image cracks (+ (car (gimp-image-get-layer-position image asphalt)) 0))) ;For GIMP 2.6 
                ) ;end cond
                
                (gimp-edit-fill cracks FILL-FOREGROUND)
                
                ;; Render Cracks using G'MIC.
                (plug-in-gmic-qt 1 image cracks 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "fx_cracks " ; prev gimp_cracks
                            (number->string cracks-density) ",0,255,255,255,230,0,0"; previously 0 and no 1 at end; 255,1,0,40,1
                                  
                    )
                )
                ;(gimp-drawable-invert cracks FALSE)
                ; Merge the cracks layer with the layer below
                (if (= merge TRUE) (set! asphalt (car (gimp-image-merge-down image cracks EXPAND-AS-NECESSARY))))
                ;(gimp-message "thru cracks")
            
            )
                      
            ; *******************************************end [G'MIC] Cracks
            
    ) ; End else for [G'MIC] Check
  );endif for [G'MIC] Check
    
    (if (= merge TRUE) 
        (begin ; Merge the asphalt layer with the layer below
            (set! drawable (car (gimp-image-merge-down image asphalt EXPAND-AS-NECESSARY)))
            (if (= alpha FALSE) (gimp-layer-flatten drawable))
        )
    ) ;endif
    
    ;;(gimp-message "finishing")
    (gimp-displays-flush)
    (gimp-context-pop)
    
 ) ;end variables
    (gimp-image-undo-group-end image)
) ;endprocedure

(script-fu-register "script-fu-asphalt"              
    "Asphalt"
    "Creates a pattern of Asphalt on drawable or within selection if one exists, requires the GMIC plugin to complete the pattern. \nfile:Asphalt_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Feb 2014"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-ADJUSTMENT "Cracks Opacity" '(60 0 100 1 10 0 0)
    SF-ADJUSTMENT "Cracks Density" '(12 .1 20 .1 1 1 0)
    SF-TOGGLE     "Merge the Layers"   FALSE
)

(script-fu-menu-register "script-fu-asphalt" "<Toolbox>/Script-Fu/Patterns")

(define (asphalt-difference-clouds image drawable)

  (let* ( (draw-offset-x (car (gimp-drawable-offsets drawable)))
          (draw-offset-y (cadr (gimp-drawable-offsets drawable)))
          (has-sel       (car (gimp-drawable-mask-intersect drawable)))
          (sel-offset-x  (cadr (gimp-drawable-mask-intersect drawable)))
          (sel-offset-y  (caddr (gimp-drawable-mask-intersect drawable)))
          (width         (cadddr (gimp-drawable-mask-intersect drawable)))
          (height        (caddr (cddr (gimp-drawable-mask-intersect drawable))))
          (type          (car (gimp-drawable-type-with-alpha drawable)))
          (diff-clouds   (car (gimp-layer-new image width height type
                                              "Clouds" 100 LAYER-MODE-DIFFERENCE)))
          (offset-x      0)
          (offset-y      0)
        )
        
    (gimp-image-undo-group-start image)
    
    ; Add the cloud layer above the current layer
    (cond ((defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
            (gimp-image-insert-layer image diff-clouds (car (gimp-item-get-parent drawable))                           ;For GIMP 2.8
            (+ (car (gimp-image-get-item-position image drawable)) 0)))
            (else (gimp-image-add-layer image diff-clouds (+ (car (gimp-image-get-layer-position image drawable)) 0))) ;For GIMP 2.6 
    ) ;end cond
    
    
    ; Clear the layer (so there are no noise in it)
    (gimp-drawable-fill diff-clouds FILL-TRANSPARENT)
    
    ; Selections are relative to the drawable; adjust the final offset
    (set! offset-x (+ draw-offset-x sel-offset-x))
    (set! offset-y (+ draw-offset-y sel-offset-y))
    
    ; Offset the clouds layer
    (cond ((defined? 'gimp-item-is-layer)
        (if (gimp-item-is-layer drawable)
            (gimp-layer-translate diff-clouds offset-x offset-y)))
        (else
            (if (gimp-drawable-is-layer drawable)
            (gimp-layer-translate diff-clouds offset-x offset-y)))
    ) ;endcond
    ; Show the solid noise dialog
    (plug-in-solid-noise 1 image diff-clouds 0 0 1 15 0.4 0.4)
    
    ; Merge the clouds layer with the layer below
    (gimp-image-merge-down image diff-clouds EXPAND-AS-NECESSARY)

    (gimp-image-undo-group-end image)

    (gimp-displays-flush)
  )
)

; end of script