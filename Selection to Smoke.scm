; Selection to Smoke rel 0.01 
; Created by Graechan from tutorial by he4rty at http://www.gimpchat.com/viewtopic.php?f=23&t=9035
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
          (set! layerText (cond ((defined? 'gimp-image-get-item-position) (car (gimp-item-get-name layerId)))
                            (else (car (gimp-drawable-get-name layerId)))))
          (if (string=? layerText layerName) (set! wantedLayerId layerId))          
          (set! layerList (cdr layerList))) ;endwhile        
        (if (= -1 wantedLayerId) (error (string-append "Could not find a layer with name:- " layerName)))
        (list wantedLayerId)
  ) ;end variables
) ;end find layer by name proceedure
;
(define (script-fu-selection-to-smoke image layer)							  

 (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (smoke-layer 0)
            (noise-layer 0)
            (copy-layer 0)
            (sel (car (gimp-selection-is-empty image)))
            (selection-channel 0)
            (outline 10)
            (ver 2.8)
            (offx 0)
            (offy 0)
            (handler (car (gimp-message-get-handler)))
        )
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    (gimp-context-set-gradient "Incandescent")
    
    ;;;;begin the script	
    ;;;;check for selection [chksel]	
    (cond 
        ((= sel TRUE)
            (gimp-message-set-handler 0)
            (error "No Selection Found")
            (gimp-message-set-handler handler))
        (else
            (gimp-image-undo-group-start image)
            ;;;;create selection-channel (gimp-selection-load selection-channel)	
            (set! selection-channel (car (gimp-selection-save image)))
            (gimp-image-set-active-layer image layer)
            
            (set! smoke-layer (car (gimp-layer-new image width height RGBA-IMAGE "Smoke" 100 LAYER-MODE-NORMAL)))
            (include-layer image smoke-layer layer 0)	;stack 0=above 1=below  
            (gimp-drawable-fill smoke-layer FILL-FOREGROUND)
            (gimp-edit-fill smoke-layer FILL-BACKGROUND)
            
            (gimp-image-select-color image 2 smoke-layer '(255 255 255))
            
            (gimp-selection-grow image (* outline 2.5))
            (gimp-selection-invert image)
            (gimp-edit-clear smoke-layer)
            (gimp-selection-none image)
            (plug-in-autocrop-layer 1 image smoke-layer)
            (set! offx (car (gimp-drawable-offsets smoke-layer)))
            (set! offy (cadr (gimp-drawable-offsets smoke-layer)))  
            (plug-in-gauss-rle2 RUN-NONINTERACTIVE image smoke-layer 10 10)
            
            ;;;;create the solid noise layer	
            (set! noise-layer (car (gimp-layer-new image (car (gimp-drawable-width smoke-layer)) (car (gimp-drawable-height smoke-layer)) RGBA-IMAGE "Noise" 100 DARKEN-ONLY-MODE)))
            (include-layer image noise-layer smoke-layer 0)	;stack 0=above 1=below
            (gimp-layer-set-offsets noise-layer offx offy)
            (plug-in-solid-noise 1 image noise-layer FALSE FALSE 0 1 4 4)
            
            (set! smoke-layer (car (gimp-image-merge-down image noise-layer EXPAND-AS-NECESSARY)))
            (plug-in-c-astretch 1 image smoke-layer)
            (gimp-progress-pulse)
            (plug-in-cubism 1 image smoke-layer 10 2.5 FALSE)
            
    ;;;;[G'MIC] Dream smoothing : -gimp_dreamsmooth 3,0,1,0.8,0,0.8,1,24,0
    ;; Render Dream smoothing using G'MIC.
                (plug-in-gmic-qt 1 image smoke-layer 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_dreamsmooth 3,0,1,0.8,0,0.8,1,24,0"
                                 
                    )
                )
            
            (gimp-displays-flush)
            ;;;;create the smoke layer copies
            (set! copy-layer (car (gimp-layer-copy smoke-layer TRUE)))
            (include-layer image copy-layer smoke-layer 0)	;stack 0=above 1=below
            
            (gimp-layer-set-mode copy-layer LAYER-MODE-SCREEN)
            (gimp-layer-set-opacity copy-layer 50)
            (set! smoke-layer (car (gimp-image-merge-down image copy-layer EXPAND-AS-NECESSARY)))
            (plug-in-gradmap 1 image smoke-layer)
            (set! copy-layer (car (gimp-layer-copy smoke-layer TRUE)))
            (include-layer image copy-layer smoke-layer 0)	;stack 0=above 1=below
            
            (gimp-progress-pulse)
            
            (plug-in-colorify 1 image copy-layer '(0 0 255))
            (gimp-layer-set-mode copy-layer LAYER-MODE-GRAIN-MERGE)
            (set! smoke-layer (car (gimp-image-merge-down image copy-layer EXPAND-AS-NECESSARY)))
            (gimp-drawable-desaturate smoke-layer DESATURATE-LUMINANCE)
            (plug-in-c-astretch 1 image smoke-layer)
            (plug-in-colortoalpha 1 image smoke-layer '(0 0 0))
            (gimp-layer-set-opacity smoke-layer 75)
            
            (set! copy-layer (car (gimp-layer-copy smoke-layer TRUE)))
            (include-layer image copy-layer smoke-layer 0)   ;stack 0=above 1=below
            (plug-in-neon 1 image copy-layer 5 0)
            (plug-in-gauss-rle2 RUN-NONINTERACTIVE image copy-layer 5 5)
            (gimp-layer-set-mode copy-layer LAYER-MODE-SCREEN)
            (gimp-layer-set-opacity copy-layer 50)
            
            (set! smoke-layer (car (gimp-image-merge-down image copy-layer EXPAND-AS-NECESSARY)))
            (if (= sel FALSE) (gimp-image-remove-channel image selection-channel))
            
            
            (gimp-displays-flush)
            (gimp-image-undo-group-end image)
            (gimp-context-pop)
            
        ) ;endelse chksel
    ) ;endcond chksel
    
 )
)

(script-fu-register "script-fu-selection-to-smoke"        		    
    "Selection to Smoke"
    "Creates a Smoke effect inside a selection. \nfile:Selection to Smoke.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Nov 2013"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
)

(script-fu-menu-register "script-fu-selection-to-smoke" "<Image>/Script-Fu2/Select")


