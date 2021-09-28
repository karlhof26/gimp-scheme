; Pencil Shade rel 0.06
; Created by Graechan 
;
 ; You will need to install GMIC to run the the 'wisps Effect' Script included
; GMIC can be downloaded from http://sourceforge.net/projects/gmic/files/
; ----Note! 'Pencil Shade' does not require 'GMIC' but 'Wisps' does---- 
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
; ----Location of 'Pencil Shade' is Filters/Artistic/Pencil Shade----
; ----Location of 'Wisps' is Script-Fu/Effects/Wisps----
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
; Rel 0.02 - additional settings added and refined to use current PDB CALLS FOR 2.6 AND 2.8
; Rel 0.03 - Bugfix to remove final depreciated procedures
; Rel 0.04 - Bugfix to fix Typo 'oops!'
; Rel 0.05 - Added 'Wisps' Script [Thanks to GnuTux]
; Rel 0.06 - Updated 'Wisps' to include changes to Gmic Dreamsmooth filter
;
; Include layer Procedure
(define (pencil-shade-include-layer image newlayer oldlayer stack)	;stack 0=above 1=below
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
(define (pencil-shade-find-layer-by-name image layerName)
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
(define (script-fu-pencil-shade image drawable 
                                      inblurshade
                                      outblurshade
                                      inblurline
									  outblurline
									  iterations
									  val
									  line-opacity
									  shade-opacity
									  keep-selection
									  conserve)

        (gimp-image-undo-group-start image)		
    
  (let* ( 
            (shadeLayer (car (gimp-image-get-active-layer image)))
            (lineLayer (car (gimp-layer-copy shadeLayer 1)))
            (alpha (car (gimp-drawable-has-alpha drawable)))
            (layer-name (cond ((defined? 'gimp-image-get-item-position) (car (gimp-item-get-name drawable)))
                        (else (car (gimp-drawable-get-name drawable)))))
            (ver 2.8)
            (i iterations)
            (matrix 0)
        )
        (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
        
        (gimp-context-push)
        (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        (pencil-shade-include-layer image lineLayer shadeLayer 0)	;stack 0=above 1=below
        (cond ((= ver 2.8) (gimp-item-set-name shadeLayer "Shade"))
            (else (gimp-drawable-set-name shadeLayer "Shade"))
        ) ;endcond
        (cond ((= ver 2.8) (gimp-item-set-name lineLayer "Line"))
            (else (gimp-drawable-set-name lineLayer "Line"))
        ) ;endcond
        (gimp-layer-set-mode lineLayer LAYER-MODE-MULTIPLY)
        (if (= (car (gimp-drawable-is-rgb lineLayer)) TRUE) (gimp-drawable-desaturate lineLayer DESATURATE-LUMINANCE))
        (gimp-image-set-active-layer image shadeLayer)
        (if (= (car (gimp-drawable-is-rgb shadeLayer)) TRUE) (gimp-drawable-desaturate shadeLayer DESATURATE-LUMINANCE))
        
        (plug-in-apply-canvas RUN-NONINTERACTIVE image shadeLayer 2 1)
        
        (gimp-layer-flatten shadeLayer)
        (plug-in-dog 1 image shadeLayer inblurshade outblurshade TRUE TRUE)
        
        ;;;;create the pencil shade matrix array	
        (set! matrix (cons-array 25 'long))    
        (aset matrix 0 val) (aset matrix 1 -1.0) (aset matrix 2 -1.0) (aset matrix 3 -1.0) (aset matrix 4 -1.0)        
        (aset matrix 5 -1.0) (aset matrix 6 val) (aset matrix 7 -1.0) (aset matrix 8 -1.0) (aset matrix 9 -1.0)        
        (aset matrix 10 -1.0) (aset matrix 11 -1.0) (aset matrix 12 val) (aset matrix 13 -1.0) (aset matrix 14 -1.0)        
        (aset matrix 15 -1.0) (aset matrix 16 -1.0) (aset matrix 17 -1.0) (aset matrix 18 val) (aset matrix 19 -1.0)
        (aset matrix 20 -1.0) (aset matrix 21 -1.0) (aset matrix 22 -1.0) (aset matrix 23 -1.0) (aset matrix 24 val)		
        
        (while (> i 0)
            (plug-in-convmatrix RUN-NONINTERACTIVE image 
                shadeLayer 
                25 ;argc-matrix [The number of elements in the following array. Should be always 25.]  
                matrix
                FALSE ;alpha-alg [Enable weighting by alpha channel]
                50 ;divisor
                0 ;offset
                5 ;argc-channels [The number of elements in following array. Should be always 5.]
                #(1 1 1 1 0) ;channels
                2 ;bmode ;[Mode for treating image borders] { EXTEND (0), WRAP (1), CLEAR (2) }
            )
            (set! i (- i 1))
        ) ;endwhile
        
        (gimp-image-set-active-layer image lineLayer)
        (gimp-layer-flatten lineLayer)
        (plug-in-dog RUN-NONINTERACTIVE image lineLayer inblurline outblurline TRUE TRUE)
        (plug-in-unsharp-mask RUN-NONINTERACTIVE image lineLayer 4 4 5)
        (gimp-layer-set-opacity lineLayer line-opacity)
        
        (set! drawable (car (gimp-layer-copy shadeLayer FALSE)))
        (pencil-shade-include-layer image drawable shadeLayer 0)	;stack 0=above 1=below
        (gimp-layer-set-mode drawable LAYER-MODE-MULTIPLY)
        (gimp-layer-set-opacity drawable shade-opacity)
        (set! shadeLayer (car (gimp-image-merge-down image drawable EXPAND-AS-NECESSARY)))
        
        (if (= conserve FALSE)
            (begin
                (set! drawable (car (gimp-image-merge-down image lineLayer EXPAND-AS-NECESSARY)))
                (cond ((= ver 2.8) (gimp-item-set-name drawable (string-append layer-name "\n-Pencil Shade")))
                      (else (gimp-drawable-set-name drawable (string-append layer-name "\n-Pencil Shade")))
                ) ;endcond
                (if (= keep-selection FALSE) (gimp-selection-none image))
                (cond ((= alpha FALSE) (gimp-layer-flatten drawable))
                      (else (gimp-layer-add-alpha drawable))
                ) ;endcond
            )
        ) ;endif
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        
    )
)

(script-fu-register      
    "script-fu-pencil-shade"
    "Pencil Shade..."
    "Creates a pencil drawing using a shaded style. \nfile:Pencil-Shade.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "July 2013"
    "RGB* GRAY*"
    SF-IMAGE      "Image" 0
    SF-DRAWABLE   "Drawable" 0
    SF-ADJUSTMENT "Inner Blur Shade" '(70 1 100 1 5 0 0)
    SF-ADJUSTMENT "Outer Blur Shade" '(8 1 100 1 5 0 0)
    SF-ADJUSTMENT "Inner Blur Line" '(3 1 100 1 5 0 0)
    SF-ADJUSTMENT "Outer Blur Line" '(1 1 100 1 5 0 0)
    SF-ADJUSTMENT "Matrix Iteratioons" '(2 1 10 1 1 0 0)
    SF-ADJUSTMENT "Matrix Value" '(16.0 0 25 .1 1 1 0)
    SF-ADJUSTMENT "Opacity Line" '(70 0 100 1 5 0 0)
    SF-ADJUSTMENT "Darken Shade" '(0 0 100 1 5 0 0)
    SF-TOGGLE     "Keep selection"          FALSE
    SF-TOGGLE     "Keep the Layers"   FALSE
)
(script-fu-menu-register "script-fu-pencil-shade" "<Toolbox>/Script-Fu/Artistic")

(define (script-fu-wisps image layer
                               inblurshade
                               iterations
                               cpu
                               spacial
                               keep-selection-in
                                conserve
                                )
    
    (gimp-image-undo-group-start image)     
    
  (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (final-width width)
            (final-height height)
            (area (* 1000 1000))
            (alpha (car (gimp-drawable-has-alpha layer)))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (cond ((defined? 'gimp-image-get-item-position) (car (gimp-item-get-name layer)))
                        (else (car (gimp-drawable-get-name layer)))))
            (keep-selection keep-selection-in)
            (selection-channel 0)
            (bkg-layer 0)
            (ver 2.8)
            (lineLayer 0)
            (shadeLayer 0)
        )
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (cond ((defined? 'gimp-context-set-dynamics) (gimp-context-set-dynamics "Dynamics Off")))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;;;;scale image to given area if required	
    (gimp-image-scale image 
        (max 1 (min 262144 (round (* width (sqrt (/ area (* width height)))))))
        (max 1 (min 262144 (round (* height (sqrt (/ area (* width height))))))))
    (set! width (car (gimp-image-width image)))
    (set! height (car (gimp-image-height image)))
    
    
    (set! bkg-layer (car (gimp-layer-copy layer TRUE)))
    (pencil-shade-include-layer image bkg-layer layer 1)	;stack 0=above 1=below
    (cond ((= ver 2.8) (gimp-item-set-name bkg-layer "Background"))
          (else (gimp-drawable-set-name bkg-layer "Background"))
    ) ;endcond
    
    (if (= alpha FALSE) (gimp-layer-add-alpha layer))
    
    
    ;;;;check that a selection was made if not make one	
    (if (= sel TRUE) (set! keep-selection FALSE))
    (if (= sel TRUE)
        (begin
            (cond ((= ver 2.8) (gimp-image-select-item image 2 layer)) 
                  (else (gimp-selection-layer-alpha layer))
            ) ;endcond
            (gimp-selection-shrink image 1)
        )
    ) ;endif
    
    ;;;;create selection-channel (gimp-selection-load selection-channel)	
    (set! selection-channel (car (gimp-selection-save image)))
    (cond ((= ver 2.8) (gimp-item-set-name selection-channel "selection-channel"))
          (else (gimp-drawable-set-name selection-channel "selection-channel"))
    ) ;endcond
    (gimp-image-set-active-layer image layer)
    
    ;;;;begin the script
    (gimp-displays-flush)
    (script-fu-pencil-shade image layer 
                                      inblurshade
                                      8 ;outblurshade
                                      3 ;inblurline
                                      1 ;outblurline
                                      iterations
                                      16 ;val
                                      70 ;line-opacity
                                      0 ;shade-opacity
                                      TRUE ;keep selection
                                      TRUE) ;conserve
    
    (set! lineLayer (car (pencil-shade-find-layer-by-name image "Line")))
    (set! shadeLayer (car (pencil-shade-find-layer-by-name image "Shade")))
    
    ;;;;prepare the layers (GnuTux)	
    (gimp-layer-set-mode lineLayer LAYER-MODE-NORMAL)
    (gimp-layer-set-opacity lineLayer 100)
    (cond ((= ver 2.8) (gimp-image-raise-item image shadeLayer)) ;for2.8
          (else (gimp-image-raise-layer image shadeLayer))             ;for2.6
    ) ;endcond
    (gimp-layer-set-mode shadeLayer LAYER-MODE-DIFFERENCE-LEGACY)
    (set! lineLayer (car (gimp-image-merge-down image shadeLayer EXPAND-AS-NECESSARY)))
    (gimp-colorize lineLayer 187 70 -15) ; +7 to blue, +20 sat, -15 lightness
    (gimp-displays-flush)
    
    ;=====================================================
    ; Add Effect to Pencil Script (GnuTux)
    ;=====================================================
        		    				
                
                ;; Render dreamsmooth using G'MIC.
                (plug-in-gmic-qt 
                    1
                    image lineLayer 1 0
                    (string-append
                        "-v - " ; To have a silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fx_dreamsmooth 2,1,1,0.8,0,0.8,"
                        (number->string cpu) "," 
                        (number->string spacial) ",0"
                                 
                    )
                )
    (set! layer (car (gimp-image-get-active-layer image)))
    (gimp-selection-invert image)
    (gimp-edit-clear layer)
    (gimp-selection-invert image)
    
    ;end [GMIC]
    ;=========================================================
    
    ;;;;finish the script
    (gimp-image-scale image final-width final-height)	
    (if (= conserve FALSE) (set! layer (car (gimp-image-merge-down image layer EXPAND-AS-NECESSARY))))
    (cond ((= ver 2.8) (gimp-item-set-name layer (string-append layer-name "\n-Wisps")))
          (else (gimp-drawable-set-name layer (string-append layer-name "\n-Wisps")))
    ) ;endcond
    (if (= keep-selection FALSE) (gimp-selection-none image))
    (if (= conserve FALSE) (gimp-image-remove-channel image selection-channel))
    (if (and (= conserve FALSE) (= alpha FALSE)) (gimp-layer-flatten layer))		
    
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    
 )
)

(script-fu-register "script-fu-wisps"              
    "Pencil Shade Wisps..."
    "Creates wisps effect using 'Pencil Shade' and 'Gmic'. \nfile:Pencil-Shade.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "2012"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-ADJUSTMENT "Inner Blur Shade"                    '(25 1 100 1 5 0 0)
    SF-ADJUSTMENT "Matrix Iteratioons"                  '(3 1 10 1 1 0 0)
    SF-OPTION     "Number of threads (CPU's)"           '("Auto" "1 Thread" "2 Threads" "4 Threads" "8 Threads" "16 Threads")
    SF-ADJUSTMENT "Spacial overlap"                     '(24 0 256 1 10 0 0)
    SF-TOGGLE     "Keep selection"                      FALSE
    SF-TOGGLE     "Keep the Layers"                     FALSE
)

(script-fu-menu-register "script-fu-wisps" "<Toolbox>/Script-Fu/Artistic")


