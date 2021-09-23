; This script takes origin from the Gimp-chat topic Woodcut-Hedcutish-Effect (thanks to PatDavid).
; This script has been carefully tested by Issabella and adjusted to solve the resulting weaknesses (many thanks to my friend) 
;
; Flow implemented to get the final result 
; ======================================== 
; preliminary steps:
; -----------------
; P01. convert image to RGB if GRAI
; P02. auto adjust levels of base image
; P03. save adjusted base image in a buffer
;
; first part (A) (mainly following PatDavid's tutorial):
; ------------------------------------------------------
; A01. desaturate the image
; A02. duplicate the layer
; A03. added by DN: apply Emboss filter
; A04. apply G'MIC/Graphic Novel filter, 
;      skipping local normalization
;      and adjusting Pencil amplitude       		                  
;      set layer opacity to 75%                             
; A05. create a new layer from visible
;      apply Filters>Artistic>Engrave, but only on Wood 					 
;      set Line width = 1/x of image height, 
;      set Line type=black, Blur radius=1
; A06. apply G'MIC/Deformation>Random
;      note:set amplitute to 2 except for Metal texture (only 1)        				 
;      added by DN: apply also G'MIC Mighty Details (reduced effect for Metal texture)
;      set layer mode to multiply (was Darken in the tutorial)
; A07. add a layer mask, using the desaturated base layer created at step A01
;      reduce contrast and increase lightness of the mask 
; A08. added by DN a call to G'MIC/gimp_light_relief
; A09. create a new layer from visible
; A10. add a layer mask, using again the desaturated base layer 
; A11. adjust level to brighten it up a bit 
;      DN=used level-mid-point=1.75 instead of setting white point=175 	 
; A12. create again a new layer from visible
; A13. set BG colour to white
; A14. apply Filter>Distorts>Erase every other row
;      set options to Columns and Fill-with-BG-colour
; A15. set layer mode to overlay
;      added by DN a call to G'MIC/Light Glow 
;      to cover the tutorial action "paint manually some white with a soft brush"
; A16. set opacity to 70
;
; second part (B) (ADDED by DN) 
; -----------------------------
;      this part is executed only if requested by the user (default=yes)
;      textures must be available/selectable
; preliminary step: restore image type to RGB if altered by engrave filter (on Wood)
; B01. rescale the patterm to the image size to limit unwanted effects
; B02. auto adjust levels of the pattern
;      apply G'MIC light releif on plain metal
; B03. duplicate the pattern layer
;      set 2nd pattern layer mode differently on the base of the texture type
;           COLOR for Wood, Bricks, Rusted Metal 
;           LIGHTEN for Leather
;           ADDITION for Plain Metal
; B04. duplicate again the pattern layer
;      set 3rd pattern layer to mode MULTIPLY, opacity 33
; B05. create a new layer and paste the buffer saved in the preliminary phase
;      set mode to overlay
; B06. create a threshold layer (160-255)
;      blur it, reduce contrast and increase lightness
;      set opacity to 65
; B07. use the threshold layer as a mask for the top (original buffer)
; B08. duplicate the layer with the original buffer (but remove the mask)
; B09. apply G'MIC watershed on first original buffer
;      cartoonize the second original buffer (8 colors)
;
; last step: flatten the image if requested by the user (default=yes)
; -------------------------------------------------------------------
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;

; Each pixel row of original (once scaled down if "pre-scaled" not selected) becomes a line (group of rows), so start with a rather small image (eg. 300x200)

; ==============================================================

; derived from rr_make_seamless.scm (fixed the options)
; by Rob Antonishen

(define (script-fu-rr-embed-seamless img inLayer p-name)
  (let* (
            (width (car (gimp-image-width img)))
            (height (car (gimp-image-height img)))
            (imgTemp (car (gimp-image-duplicate img)))
            (layerTemp (car (gimp-image-flatten imgTemp)))
            (imgWorking (car (gimp-image-duplicate imgTemp)))
            (layerWorking 0)
            (float 0)
            (layerBlur 0)
            (layerBlur2 0)
            (layerColor 0)
            (layerColor2 0)
            (buffname "seamlessbuff")
            (inFlatten 0.0) ; fixed
            (inCenter 1) 	; fixed 
            (inOrient 0) 	; fixed
            (d-dir "")
            (p-name2 "")
        )
    
    ; it begins here
    ;(gimp-message "rr_embed_seamless started OK")
    ;(gimp-image-undo-disable imgWorking)
    (gimp-message-set-handler 2)
    ;(gimp-message (number->string height))
    ;(gimp-message (number->string width))
    
    (set! imgWorking (car (plug-in-tile RUN-NONINTERACTIVE imgTemp layerTemp (* 2 width) (* 2 height) TRUE)))  ;create a tiled copy
    
    (set! layerWorking (car (gimp-image-get-active-layer imgWorking))) ; get the layer
    (set! inFlatten (- 1.0 inFlatten))
    ;(gimp-display-new img)
    ;(gimp-display-new imgWorking)
    
    
    (if (or (= inOrient 0) (= inOrient 1))
        (begin
            ;build the selection (vertical)
            ;(gimp-message "line 133")
            ;(gimp-message (number->string (- (* width 0.50) (* width 0.07))))
            (gimp-context-set-feather TRUE)
            (gimp-context-set-feather-radius (round (* width 0.02)) (round (* width 0.02)))
            (gimp-selection-none img)
            ;(gimp-ellipse-select img (- (* width 0.50) (* width 0.07)) (* height 0.05) (* width 0.14) (* height 0.45) CHANNEL-OP-REPLACE TRUE TRUE (* width 0.02))
            (gimp-image-select-ellipse img CHANNEL-OP-REPLACE (- (* width 0.50) (* width 0.07)) (* height 0.05) (* width 0.14) (* height 0.45)  )
            (gimp-ellipse-select img (- (* width 0.50) (* width 0.07)) (* height 0.50) (* width 0.14) (* height 0.45) CHANNEL-OP-ADD TRUE TRUE (* width 0.02))
            (gimp-ellipse-select img (- (* width 0.50) (* width 0.125)) (* height 0.05) (* width 0.25) (* height 0.45) CHANNEL-OP-ADD TRUE TRUE (* width 0.10))
            (gimp-ellipse-select img (- (* width 0.50) (* width 0.125)) (* height 0.50) (* width 0.25) (* height 0.45) CHANNEL-OP-ADD TRUE TRUE (* width 0.10))
            (gimp-ellipse-select img (- (* width 0.50) (* width 0.02)) (* height 0.00) (* width 0.04) (* height 1.00) CHANNEL-OP-ADD TRUE TRUE (* width 0.02))
            (gimp-ellipse-select img (- (* width 0.50) (* width 0.04)) (* height 0.20) (* width 0.08) (* height 0.60) CHANNEL-OP-ADD TRUE TRUE (* width 0.08))
            (gimp-ellipse-select img (- (* width 0.53) (* width 0.06)) (* height 0.15) (* width 0.12) (* height 0.20) CHANNEL-OP-ADD TRUE TRUE (* width 0.02))
            (gimp-ellipse-select img (- (* width 0.47) (* width 0.06)) (* height 0.65) (* width 0.12) (* height 0.20) CHANNEL-OP-ADD TRUE TRUE (* width 0.02))
            ; copy it
            (set! buffname (car (gimp-edit-named-copy-visible img buffname))) 
            ;paste it in
            (set! float (car (gimp-edit-named-paste layerWorking buffname FALSE)))
            (set! float (car (gimp-drawable-transform-2d-default float 0 0 1 1 0 0 (* height -0.5) FALSE FALSE)))
            (gimp-floating-sel-anchor float)
            
            (set! float (car (gimp-edit-named-paste layerWorking buffname FALSE)))
            (set! float (car (gimp-drawable-transform-2d-default float 0 0 1 1 0 0 (* height 0.5) FALSE FALSE)))
            (gimp-floating-sel-anchor float)
        )
    )
    
    (if (or (= inOrient 0) (= inOrient 2))
        (begin
            ;(gimp-message "line 161")
            ;build the selection (horizontal)
            (gimp-context-set-feather TRUE)
            (gimp-context-set-feather-radius (round (* height 0.02)) (round (* height 0.02)))
            
            (gimp-selection-none img)
            ;(gimp-message (number->string (round (* height 0.02))))
            ;(gimp-message (number->string (round (* width 0.05))))
            ;(gimp-message (number->string (round (* width 0.45))))
            ;(gimp-message (number->string (round (- (* height 0.50) (* height 0.07)))))
            (gimp-image-select-ellipse img CHANNEL-OP-REPLACE  2 2 25 25)
            
            
            ;(gimp-ellipse-select img (* width 0.05) (- (* height 0.50) (* height 0.07)) (* width 0.45) (* height 0.14) CHANNEL-OP-REPLACE TRUE TRUE (* height 0.02))
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (round (* width 0.05)) (round (- (* height 0.50) (* height 0.07))) (round (* width 0.45)) (round (* height 0.14)) )
            
            
            
            ;(gimp-ellipse-select img (* width 0.50) (- (* height 0.50) (* height 0.07)) (* width 0.45) (* height 0.14) CHANNEL-OP-ADD TRUE TRUE (* height 0.02))
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.50) (- (* height 0.50) (* height 0.07)) (* width 0.45) (* height 0.14))
            ;(gimp-message "both new selects done")
            
            (gimp-context-set-feather-radius (round (* height 0.10)) (round (* height 0.10)))
            ;(gimp-ellipse-select img  CHANNEL-OP-ADD (* width 0.05) (- (* height 0.50) (* height 0.125)) (* width 0.45) (* height 0.25) CHANNEL-OP-ADD TRUE TRUE (* height 0.10))
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.05) (- (* height 0.50) (* height 0.125)) (* width 0.45) (* height 0.25) )
            
            (gimp-context-set-feather-radius (round (* height 0.10)) (round (* height 0.10)))
            ;(gimp-ellipse-select img (* width 0.50) (- (* height 0.50) (* height 0.125)) (* width 0.45) (* height 0.25) CHANNEL-OP-ADD TRUE TRUE (* height 0.10))
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.50) (- (* height 0.50) (* height 0.35)) (* width 0.45) (* height 0.25) )
            
            ;(gimp-message "2more new selects done")
            
            (gimp-context-set-feather-radius (round (* height 0.08)) (round (* height 0.08)))
            ;(gimp-ellipse-select img (* width 0.20) (- (* height 0.50) (* height 0.04)) (* width 0.60) (* height 0.08) CHANNEL-OP-ADD TRUE TRUE (* height 0.08))
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.20) (- (* height 0.50) (* height 0.04)) (* width 0.65) (* height 0.08) )
            
            ;(gimp-message "a new selects done")
            ;(gimp-displays-flush)
            ;(gimp-display-new img)
            ;(quit)
            
            (gimp-context-set-feather-radius (round (* height 0.02)) (round (* height 0.02)))
            ;(gimp-ellipse-select img (* width 0.00) (- (* height 0.50) (* height 0.02)) (* width 1.00) (* height 0.04) CHANNEL-OP-ADD TRUE TRUE (* height 0.02))
            ;(gimp-ellipse-select img (* width 0.15) (- (* height 0.47) (* height 0.06)) (* width 0.20) (* height 0.12) CHANNEL-OP-ADD TRUE TRUE (* height 0.02))
            ;(gimp-ellipse-select img (* width 0.65) (- (* height 0.53) (* height 0.06)) (* width 0.20) (* height 0.12) CHANNEL-OP-ADD TRUE TRUE (* height 0.02))
            
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.00) (- (* height 0.50) (* height 0.02)) (* width 1.00) (* height 0.04)  )
            (gimp-image-select-ellipse img CHANNEL-OP-ADD (* width 0.15) (- (* height 0.47) (* height 0.06)) (* width 0.20) (* height 0.12) )
            (gimp-image-select-ellipse img  CHANNEL-OP-ADD (* width 0.65) (- (* height 0.53) (* height 0.06)) (* width 0.20) (* height 0.12) )
            
            ;(gimp-message "line 218")
            ; copy it
            
            (set! buffname (car (gimp-edit-named-copy-visible img buffname)))  
            ;(gimp-message "line 222")
            ;(gimp-message buffname)
            
            ;(gimp-message "line 225")
            
            ;paste it in
            (set! float (car (gimp-edit-named-paste layerWorking buffname FALSE)))
            ;(set! float (car (gimp-drawable-transform-2d-default float 0 0 1 1 0 (* width -0.5) 0 FALSE TRANSFORM-RESIZE-ADJUST)))
             (set! float (car (gimp-item-transform-2d float 0 0 1 1 0 (* width -0.5) 0 )))
            (gimp-floating-sel-anchor float)
            ;(gimp-message "first transform done")
            
            (set! float (car (gimp-edit-named-paste layerWorking buffname FALSE)))
            ;(set! float (car (gimp-drawable-transform-2d-default float 0 0 1 1 0 (* width 0.5) 0 FALSE TRANSFORM-RESIZE-ADJUST)))
            (set! float (car (gimp-item-transform-2d float 0 0 1 1 0 (* width 0.5) 0 )))
            (gimp-floating-sel-anchor float)
            
            ;(gimp-message "line 239")
            ;(gimp-displays-flush)
            ;(gimp-display-new img)
            ;(gimp-display-new imgWorking)
            
            
        )
    )
    
    (if (eq? inCenter TRUE)
        (begin
            ;(gimp-message "line 250")
            ;build the selection (center)
            (gimp-selection-none img)
            (gimp-context-set-feather-radius (* (min height width) 0.10) (* (min height width) 0.10))
            ;(gimp-ellipse-select img (- (* width 0.50) (* width 0.20)) (- (* height 0.50) (* height 0.20)) (* width 0.40) (* height 0.40) CHANNEL-OP-REPLACE TRUE TRUE (* (min height width) 0.10))
            (gimp-image-select-ellipse img CHANNEL-OP-REPLACE (- (* width 0.50) (* width 0.20)) (- (* height 0.50) (* height 0.20)) (* width 0.40) (* height 0.40)  )
            
            ; copy it
            (set! buffname (car (gimp-edit-named-copy-visible img buffname)))
            
            (gimp-selection-none img)
            
            ;paste it in
            (set! float (car (gimp-edit-named-paste layerWorking buffname FALSE)))
            (gimp-floating-sel-anchor float)
        )
    )
    ;(gimp-message "line 267")
    
    (gimp-selection-none img)
    (gimp-selection-none imgWorking)
         
    ;high pass filter with preserve DC
    (set! layerBlur (car (gimp-layer-copy layerWorking FALSE)))
    (gimp-image-insert-layer imgWorking layerBlur 0 -1)
    
    ;blur
    (if (> inFlatten 0)
        (plug-in-gauss-rle 1 imgWorking layerBlur (* (min height width) inFlatten) 1 1) ;2
    )  
    (gimp-image-set-active-layer imgWorking layerBlur)  ;top layer
    
    ;get the average colour of the input layer
    (set! layerColor (car (gimp-layer-copy layerWorking FALSE)))
    (gimp-image-insert-layer imgWorking layerColor 0 -1)
    ;(gimp-message "line 285")
    
    (gimp-context-set-foreground (list (car (gimp-histogram layerColor HISTOGRAM-RED 0 255)) (car (gimp-histogram layerColor HISTOGRAM-GREEN 0 255)) (car (gimp-histogram layerColor HISTOGRAM-BLUE 0 255))))
    (gimp-drawable-fill layerColor FILL-FOREGROUND) ;3
    (gimp-image-set-active-layer imgWorking layerColor) 
    ;(gimp-message "line 290")
    
    ;copy the solid colour layer
    (set! layerColor2 (car (gimp-layer-copy layerColor FALSE)))
    (gimp-image-insert-layer imgWorking layerColor2 0 -1)    
    (gimp-layer-set-mode layerColor LAYER-MODE-SUBTRACT)
    (gimp-image-set-active-layer imgWorking layerColor2)
    
    ;copy the blurred layer
    (set! layerBlur2 (car (gimp-layer-copy layerBlur FALSE)))
    (gimp-image-insert-layer imgWorking layerBlur2 0 -1)   
    (gimp-layer-set-mode layerBlur2 LAYER-MODE-SUBTRACT)
    
    (gimp-image-set-active-layer imgWorking layerBlur2)
    
    (set! layerBlur (car (gimp-image-merge-down imgWorking layerColor 0)))
    (set! layerBlur2 (car (gimp-image-merge-down imgWorking layerBlur2 0)))
    ;(gimp-message "line 307")
    
    (gimp-layer-set-mode layerBlur LAYER-MODE-SUBTRACT)
    (gimp-layer-set-mode layerBlur2 LAYER-MODE-ADDITION)
    
    (set! layerWorking (car (gimp-image-merge-down imgWorking layerBlur 0)))
    (set! layerWorking (car (gimp-image-merge-down imgWorking layerBlur2 0)))

    ;crop the image
    (gimp-image-crop imgWorking width height (* width 0.5) (* height 0.5)) ;1
    ;and offset
    (gimp-drawable-offset layerWorking TRUE 0 (/ width 2) (/ height 2))
    
    ;done
    ;(gimp-display-new imgWorking)
    
    
    ; new stuff (DN)  = store the new seamless pattern
    ; ------------------------------------------------
    (set! d-dir (car (gimp-gimprc-query "gimp_dir"))) ; gimp_dir  
    (set! p-name2 (string-append "C:\\Users\\YOURNAME\\Gimp-2.10\\" "\\patterns\\"
                        p-name
                        ".pat")
      )
                ; ; d-dir;  ; was fwd slashess 
                
    ;(gimp-message "line 333 - ready to save")
    ;(gimp-display-new imgWorking)
    ;(gimp-message p-name2)
    ;(gimp-message "line 279")
    ;(gimp-message p-name)
    ;(gimp-message "line 281")
    ;(gimp-message d-dir)
    ;No saving of the pattern - Gimp is not run in Admin mode
    (file-pat-save RUN-NONINTERACTIVE imgWorking layerWorking "C:\\Workspace\\temppat1woodcarve.pat" "C:\\Workspace\\temppat1woodcarve" "temppat1woodcarve")
    ;(file-pat-save RUN-NONINTERACTIVE imgWorking layerWorking p-name2 p-name2 p-name)
    ;(file-pat-save-internal RUN-NONINTERACTIVE imgWorking layerWorking p-name2 "" p-name)
    (gimp-patterns-refresh)
    (gimp-message "Seamless pattern has been created and stored in C:\\workspace. Please move it to Patterns folder.") 
    ;(gimp-context-set-pattern p-name)
    (gimp-context-set-pattern "Wood")
    (gimp-message "Pattern set to Wood anyway.") 
    
    ; end new stuff
    ; -------------
    
    
    ;(gimp-display-new imgWorking)
    ;(gimp-image-undo-enable imgWorking)
    ;(gimp-displays-flush)
    ;(gimp-message "line 357 - rr-embed-seamless end ok")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(define (script-fu-woodcut-carved-effect
            theOldImage
            theOldLayer
            doPattern                   ; addition by DN
            PatternType                 ; addition by DN
            Pattern                     ; addition by DN
            PatternAction               ; addition by DN
            doFlatten
   )

; Initialize an undo, so the process can be undone with a single undo
     (gimp-image-undo-group-start theOldImage)

; Variable init
    (let*
     (
      (imageType (car (gimp-image-base-type theOldImage)))
      (imageWidth (car (gimp-image-width theOldImage)))
      (imageHeight (car (gimp-image-height theOldImage)))
      (setMeanValue 0)
      (GraphicNovelPencil 16)
      (GraphicNovelOpacity 75)
      (HeightDivisor 100)                   ; to calculate the EngraveLineWidth 
      (DeformationAmplitude 2)
      (OverlayDistortsOpacity 70)
      (realBufferName "a")
      (theDesaturatedLayer 0)
      (theDuplicatedLayer 0)
      (LevelsMidPoint 1.75)
      (theGraphicLayer 0)
      (theGraphicLayer2 0)
      (engraveHeight 0)
      (theEngravedLayer 0)
      (theDeformedLayer)
      (theMergedLayer 0)
      (theMask 0)
      (theNewFromVisible1 0)
      (theMask1 0)
      (theNewFromVisible2 0)
      (thePatternLayer 0)
      (patternWidth 0)
      (patternHeight 0)
      (temp-image 0)
      (temp-layer 0)
      (the2PatternLayer 0)
      (the3PatternLayer 0)
      (histoValues 0)
      (meanValue 0)
      (patternOpacity 0)
      (floatingBuffer 0)
      (theTopLayer 0)
      (theTopLayerDup 0)
      (theThresholdLayer 0)
      (theMask2 0)
     )

    (gimp-message-set-handler 2)


; =================
; PRELIMINARY STEPS
; -----------------
    
; convert image to mode RGB is GRAY
; ---------------------------------
    (if (= imageType GRAY)
       (gimp-image-convert-rgb theOldImage)
    )
    ;(gimp-message "line 431")
    
    ; autocalculate HISTOGRAM-VALUE midpoint based on average luminosity
    ; ==================================================================
    (set! histoValues (gimp-drawable-histogram theOldLayer HISTOGRAM-VALUE 0.0 1.0))
    (set! meanValue (car histoValues))
    (set! setMeanValue (/ (- 256 meanValue) 128))       ; the difference
    (set! setMeanValue (/ (+ setMeanValue 1.00) 2))     ; half the difference
    (set! setMeanValue (* setMeanValue setMeanValue))   ; power of the difference
    ;(gimp-message (string-append "Calculated Set Mid Point-3: " (number->string setMeanValue)))
    ; setting new mid point of levels before processing the base image  
    ;(gimp-levels theOldLayer HISTOGRAM-VALUE 0 255 setMeanValue 0 255)
    ;(gimp-message (number->string setMeanValue)) ; added by karlhof26
    (set! setMeanValue (/ setMeanValue 2))
    (gimp-drawable-levels theOldLayer HISTOGRAM-VALUE 0.0 1.0 TRUE setMeanValue 0.0 1.0 TRUE)
    
    ;  conserve the adjusted base image as a buffer
    (set! realBufferName (car (gimp-edit-named-copy theOldLayer "myBufferName")))
    ;(gimp-message realBufferName)
    
    ; ===============    
    ; START OF PART A
    ; ---------------
    
    ; step A01
    ; ========
    (gimp-drawable-desaturate theOldLayer DESATURATE-LUMINOSITY)       ; was 1
    (set! theDesaturatedLayer (car (gimp-image-get-active-drawable theOldImage)))
    (gimp-item-set-name theDesaturatedLayer "Base Layer Desaturated")
    ;(gimp-message "line 458")
    ; step A02
    ; ========
    (set! theDuplicatedLayer (car (gimp-layer-new-from-drawable theDesaturatedLayer theOldImage )))
    (gimp-image-insert-layer theOldImage theDuplicatedLayer 0 0)
    (gimp-item-set-name theDuplicatedLayer "preGraphic Layer")
    (gimp-layer-add-alpha theDuplicatedLayer)
    
    ; step A03   (added by DN)
    ; ========
    (plug-in-emboss 1 theOldImage theDuplicatedLayer 210 110 11 1)
    
    ; step A04  
    ; ========
    (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
       (string-append
        "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
            "-fx_graphic_novelfxl 1,0,0,0,0,0,1.24,"  
            (number->string GraphicNovelPencil)
                ",0,0,0.5,0.78,1.92,0,0,9,1,1,1,0.5,0.8,4.50"    
       )
    )
    (set! theGraphicLayer (car (gimp-image-get-active-drawable theOldImage)))
    (gimp-item-set-name theGraphicLayer "GraphicNovel Layer")
    (gimp-layer-set-opacity theGraphicLayer GraphicNovelOpacity)
    ;(gimp-message "line 483")
    
    ; step A05
    ; ========
    (set! theGraphicLayer2 (car (gimp-layer-new-from-visible theOldImage theOldImage "preEngrave Layer")))
    (gimp-image-insert-layer theOldImage theGraphicLayer2 0 0)
    (gimp-layer-add-alpha theGraphicLayer2)
    ; ------------------------------------------------------------------------------------------ 	
    ; warning: the filter lew-engrave requires strictly an integer as height parameter
    ; thus you may use "quotient"...
    ; alternatively you may use "trunc":
    ; ---------------------------------- 	
    (set! engraveHeight (trunc (/ imageHeight HeightDivisor)))   ; (trunc (/x y))" returns a TRUE INTEGER
    ; attention, please: engraving effect applied only for Wood patterns    
    (if (= PatternType 0) ; wood
        (begin
            ;(gimp-message "Pattern type 0")
            (lew-engrave theOldImage theGraphicLayer2 0
                          engraveHeight 1.5 0 2 TRUE 1 FALSE )
            (set! theEngravedLayer (car (gimp-image-get-active-drawable theOldImage)))
            (gimp-item-set-name theEngravedLayer "Engraved Layer")
        )
        (begin
            ;(gimp-message "Pattern type 1-4")
            (set! theEngravedLayer (car (gimp-image-get-active-drawable theOldImage)))
            (gimp-item-set-name theEngravedLayer "Non-Engraved Layer")  ; else reflect this on the name
      )  
    )
    ;(gimp-message "line 511")
    
    ; step A06
    ; ========
    ; attention, please: deformation effect reduced only for Plain Metal patterns
    (if (= PatternType 3) ; metal
        (set! DeformationAmplitude (/ DeformationAmplitude 2))  ; reduce amplitude of deformation
    )
    ; do a random deform
    (plug-in-gmic-qt 1 theOldImage theEngravedLayer 1 0 ; 1=input only active layer
       (string-append
            "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
            "-deform "
            (number->string DeformationAmplitude)
       )
    )
    ; added by DN: apply G'MIC Mighty Details
    ; ---------------------------------------
    ; attention, please: mighty details effect reduced only for Plain Metal patterns
    (if (= PatternType 3) ; metal
        (plug-in-gmic-qt 1 theOldImage theEngravedLayer 1 0 ; 1=input only active layer
            (string-append
                "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                "-fx_mighty_details 15,0.5,15,1,3,0"
       )
      ) ; as above is applied in case of metal texture
        (begin
            (plug-in-gmic-qt 1 theOldImage theEngravedLayer 1 0 ; 1=input only active layer
                (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_mighty_details 25,1,25,1,3,0"
                )
            ) ; this is applied in case of other textures
        )
    )
    (set! theDeformedLayer (car (gimp-image-get-active-drawable theOldImage)))
    (gimp-item-set-name theDeformedLayer "Deformed Layer")
    (gimp-layer-set-mode theDeformedLayer LAYER-MODE-MULTIPLY)        
    ;(gimp-message "line 549")
    
    ;
    ;
    ; step A07
    ; ========
    (set! theMask (car (gimp-layer-create-mask theDeformedLayer ADD-MASK-WHITE)))
    (gimp-layer-add-mask theDeformedLayer theMask)
    (gimp-edit-copy theDesaturatedLayer)
    (gimp-image-set-active-layer theOldImage theDeformedLayer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste theMask FALSE)))
    ;(gimp-brightness-contrast theMask 0 -64)                ; first reduce contrast
    (gimp-drawable-brightness-contrast theMask 0.0 -0.250)                ; first reduce contrast
    ;(gimp-brightness-contrast theMask 32 0)                 ; then increase brightness
    (gimp-drawable-brightness-contrast theMask 0.125 0.0)                 ; then increase brightness
    
    ; step A08
    ; ========    
    ; added by DN : light_relief 
    (plug-in-gmic-qt 1 theOldImage theDeformedLayer 1 0; 1=input only active layer
       (string-append
        "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
        "-fx_light_relief 0.3,0.2,0.2,0,1,0.5,0.5,5,0.5,0,0"
       )
    )
    ;(gimp-message "line 574")
     
    ; step A09
    ; ========
    (set! theNewFromVisible1 (car (gimp-layer-new-from-visible theOldImage theOldImage "Visible 1")))
    (gimp-image-insert-layer theOldImage theNewFromVisible1 0 0)
    
    ; step A10
    ; ========
    (set! theMask1 (car (gimp-layer-create-mask theNewFromVisible1 ADD-MASK-WHITE)))
    (gimp-layer-add-mask theNewFromVisible1 theMask1)
    (gimp-edit-copy theDesaturatedLayer)
    (gimp-image-set-active-layer theOldImage theNewFromVisible1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste theMask1 FALSE)))
    ;(gimp-brightness-contrast theMask1 0 -64)                ; first reduce contrast
    (gimp-drawable-brightness-contrast theMask 0.0 -0.250)
    ;(gimp-brightness-contrast theMask1 32 0)                 ; then increase brightness
    (gimp-drawable-brightness-contrast theMask 0.125 0.0)
    (gimp-layer-set-mode theNewFromVisible1 LAYER-MODE-DARKEN-ONLY-LEGACY)
    ;(gimp-message "line 593")
      
      
    ; step A11
    ; ========
    ; attention: logic modified by DN: using midpoint instead of highpoint  
    ;(gimp-levels theNewFromVisible1 HISTOGRAM-VALUE 0 255 LevelsMidPoint 0 255)
    ;(gimp-message (number->string LevelsMidPoint))
    (gimp-drawable-levels theNewFromVisible1 HISTOGRAM-VALUE 0.0 1.0 TRUE LevelsMidPoint 0.0 1.0 TRUE)
    
    ; step A12
    ; ========
    (set! theNewFromVisible2 (car (gimp-layer-new-from-visible theOldImage theOldImage "Visible 2")))
    (gimp-image-insert-layer theOldImage theNewFromVisible2 0 0)
    
    ; step A13
    ; ========
    (gimp-context-set-background '(255 255 255))
    
    ; step A14
    ; ========
    ;(gimp-message "line 558 - erase rows")
    (script-fu-erase-rows theOldImage theNewFromVisible2 1 0 1)
    ;(gimp-message "line 616")
    
    ; step A15
    ; ========
    (gimp-layer-set-mode theNewFromVisible2 LAYER-MODE-OVERLAY)
    ; added by DN
    (plug-in-gmic-qt 1 theOldImage theNewFromVisible2 1 0; 1=input only active layer
       (string-append
        "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
        "-fx_lightglow 30,0.5,8,0.8,0"
       )
    )
    ;(gimp-message "line 628")
    (gimp-displays-flush)
    
    ; step A16
    ; ========
    (gimp-layer-set-opacity theNewFromVisible2 OverlayDistortsOpacity)    
    ; check/adjust levels mean
    (set! histoValues (gimp-drawable-histogram theNewFromVisible2 HISTOGRAM-VALUE 0.0 1.0))
    (set! meanValue (car histoValues))
    (set! setMeanValue (/ (- 256 meanValue) 128))       ; the difference
    (set! setMeanValue (/ (+ setMeanValue 2.00) 3))     ; one third the difference
    (set! setMeanValue (* setMeanValue setMeanValue))   ; power of the difference
    ;(gimp-message (string-append "Calculated Set Mid Point-3: " (number->string setMeanValue)))
    ; setting new mid point  
    ;(gimp-levels theNewFromVisible2 HISTOGRAM-VALUE 0 255 setMeanValue 0 255)
    (gimp-drawable-levels theNewFromVisible2 HISTOGRAM-VALUE 0.0 1.0 TRUE setMeanValue 0.0 1.0 TRUE)
    ;(gimp-message "line 644")
    
    ; -------------    
    ; END OF PART A
    ; =============
    
    ; ===============    
    ; START OF PART B
    ; ---------------
    ; add a texture if required
    ; =========================
    (if (= doPattern TRUE)
        (begin
            ;(gimp-message "line 657")
            (if (= PatternType 0) ; wood                 ; (the image mode was changed by the engrave filter)
                (gimp-image-convert-rgb theOldImage)
            )
            ; step B01
            ; ========
            (set! patternWidth (car (gimp-pattern-get-info Pattern)))
            (set! patternHeight (cadr (gimp-pattern-get-info Pattern)))	
            (set! thePatternLayer (car (gimp-layer-new theOldImage patternWidth patternHeight
                0 "Choosen texture" 100 LAYER-MODE-DARKEN-ONLY)))
            (gimp-image-insert-layer theOldImage thePatternLayer 0 0)
            (gimp-context-set-pattern Pattern)
            (gimp-drawable-fill thePatternLayer FILL-PATTERN)
            (gimp-layer-set-mode thePatternLayer LAYER-MODE-DARKEN-ONLY-LEGACY)
            ;(gimp-message "line 671")
            
            ; new stuff
            ; ---------
            (if (= PatternAction 0) ; rescale
                (gimp-item-transform-scale thePatternLayer 0 0 imageWidth imageHeight)
            )
            (if (= PatternAction 1) ; make seamless
                (begin
                    (set! temp-image (car (gimp-image-new patternWidth patternHeight RGB)))
                    (set! temp-layer (car (gimp-layer-new-from-drawable thePatternLayer temp-image)))
                    (gimp-image-insert-layer temp-image temp-layer 0 0)
                    ;(gimp-message "line 683")
                    ; deactivated for now
                    ;(gimp-message "off to script-fu-rr-embed-seamless")
                    (script-fu-rr-embed-seamless temp-image temp-layer "temppat1")
                    ;(file-pat-save-internal 1 temp-image temp-layer "temp-pat" "temp-pat" "temp-pat") ; khof26 addition
                    ;(gimp-message "line 688")
                    ;(gimp-message (car (gimp-context-get-pattern)))
                    (gimp-item-transform-scale thePatternLayer 0 0 imageWidth imageHeight)
                    (gimp-drawable-fill thePatternLayer FILL-PATTERN)
                )
            )
            ;(gimp-message "line 694")
            (if (= PatternAction 2) ; do nothing on the pattern, hence...
                (begin
                    (gimp-item-transform-scale thePatternLayer 0 0 imageWidth imageHeight) ; first resize...
                    (gimp-drawable-fill thePatternLayer FILL-PATTERN)   ; ...then refill again with the original pattern
                )
            )
            ; end of new stuff
            ; ----------------
            
            ; step B02
            ; ========
            (set! histoValues (gimp-drawable-histogram thePatternLayer HISTOGRAM-VALUE 0.0 1.0))
            (set! meanValue (car histoValues))
            (set! setMeanValue (/ (- 256 meanValue) 128))       ; the difference
            (set! setMeanValue (/ (+ setMeanValue 2.00) 3))     ; one third the difference
            (set! setMeanValue (* setMeanValue setMeanValue))   ; power of the difference
            ;(gimp-message (string-append "Calculated Set Mid Point-3: " (number->string setMeanValue)))
            ; setting new mid point  
            ;(gimp-levels thePatternLayer HISTOGRAM-VALUE 0 255 setMeanValue 0 255)
            (gimp-drawable-levels thePatternLayer HISTOGRAM-VALUE 0.0 1.0 TRUE setMeanValue 0.0 1.0 TRUE)
            ;(gimp-message "line 715")
            
            ; light_relief on plain metal
            (if (= PatternType 3) ; metal
                    (plug-in-gmic-qt 1 theOldImage theDeformedLayer 1 0 ; 1=input only active layer
                        (string-append
                            "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                            "-fx_light_relief 0.1,0.5,0.25,0,2,0.5,0.5,5,0.5,0,0"
                        )
                    )
            )
            ;(gimp-message "line 726")
            ; step B03
            ; ======== 
            (set! the2PatternLayer (car (gimp-layer-new-from-drawable thePatternLayer theOldImage )))
            (gimp-image-insert-layer theOldImage the2PatternLayer 0 0)
            (gimp-item-set-name the2PatternLayer "Choosen texture (2)")
            (gimp-layer-add-alpha the2PatternLayer)
            (if (= PatternType 0) ; wood
                (begin
                    (gimp-layer-set-mode the2PatternLayer LAYER-MODE-HSL-COLOR-LEGACY)
                    (gimp-layer-set-opacity the2PatternLayer 85.0)
                )
            )
            ;(gimp-message "line 739")
                (if (= PatternType 1) ; bricks
                    (begin
                        (gimp-layer-set-mode the2PatternLayer LAYER-MODE-HSL-COLOR-LEGACY)
                        (gimp-layer-set-opacity the2PatternLayer 75.0)
                    )
                )
                (if (= PatternType 2) ; leather
                    (begin
                        (gimp-layer-set-mode the2PatternLayer LAYER-MODE-LIGHTEN-ONLY)
                        (gimp-layer-set-opacity the2PatternLayer 70.0)
                    )           
                )
                (if (= PatternType 3) ; metal
                    (begin
                        (gimp-layer-set-mode the2PatternLayer LAYER-MODE-ADDITION-LEGACY)
                        (gimp-layer-set-opacity the2PatternLayer 60.0)
                    )           
                )
                (if (= PatternType 4) ; rusty
                    (begin
                        (gimp-layer-set-mode the2PatternLayer LAYER-MODE-HSL-COLOR-LEGACY)
                        (gimp-layer-set-opacity the2PatternLayer 60)
                    )           
                )
            ;(gimp-message "line 764")
            
            ; step B04
            ; ======== 
            (set! the3PatternLayer (car (gimp-layer-new-from-drawable thePatternLayer theOldImage )))
            (gimp-image-insert-layer theOldImage the3PatternLayer 0 0)
            (gimp-item-set-name the3PatternLayer "Choosen texture (3)")
            (gimp-layer-add-alpha the3PatternLayer)
            (set! histoValues (gimp-drawable-histogram the3PatternLayer HISTOGRAM-VALUE 0.0 1.0))
            (set! meanValue (car histoValues))
            (set! patternOpacity (/ (* meanValue 30) 100))             ; 30% of the mean valuye
            (gimp-layer-set-mode the3PatternLayer LAYER-MODE-MULTIPLY)
            (gimp-layer-set-opacity the3PatternLayer patternOpacity)
            ;(gimp-message "line 777")
            
            ; step B05
            ; ======== 
            
            (set! theTopLayer (car (gimp-layer-new-from-visible theOldImage theOldImage "original buffer Layer")))
            (gimp-image-add-layer theOldImage theTopLayer 0)
            (gimp-layer-add-alpha theTopLayer)
            (gimp-floating-sel-anchor (car (gimp-edit-named-paste theTopLayer realBufferName FALSE)))
            (gimp-layer-set-mode theTopLayer LAYER-MODE-OVERLAY)
            
            ; step B06
            ; ========
            (set! theThresholdLayer (car (gimp-layer-new-from-drawable theTopLayer theOldImage )))
            (gimp-image-insert-layer theOldImage theThresholdLayer 0 0)
            ;(gimp-threshold theThresholdLayer 160 255)
            (gimp-drawable-threshold theThresholdLayer HISTOGRAM-VALUE 0.627 1.0)
            (plug-in-gauss 1 theOldImage theThresholdLayer 5 5 0)
            
            ;(gimp-brightness-contrast theMask1 0 -64)                ; first reduce contrast
            (gimp-drawable-brightness-contrast theMask 0.0 -0.250)
            ;(gimp-brightness-contrast theMask1 32 0)                 ; then increase brightness
            (gimp-drawable-brightness-contrast theMask 0.125 0.0)
            (gimp-drawable-set-name theThresholdLayer "Threshold Layer")
            ;    (gimp-drawable-set-visible theThresholdLayer FALSE)             ; on/off as you like
            (gimp-layer-set-opacity theThresholdLayer 50)
            (gimp-layer-set-mode theThresholdLayer LAYER-MODE-MULTIPLY)
            ;(gimp-message "line 804")
            
            ; step B07
            ; ========       
            (set! theMask2 (car (gimp-layer-create-mask theTopLayer ADD-MASK-WHITE)))
            (gimp-layer-add-mask theTopLayer theMask2)
            (gimp-edit-copy theThresholdLayer)
            (gimp-image-set-active-layer theOldImage theTopLayer)
            (gimp-floating-sel-anchor (car (gimp-edit-paste theMask2 FALSE)))
            
            ; step B08
            ; ======== 
            (set! theTopLayerDup (car (gimp-layer-new-from-drawable theTopLayer theOldImage )))
            (gimp-image-insert-layer theOldImage theTopLayerDup 0 0)
            (gimp-item-set-name theTopLayerDup "duplicated Top Layer")
            (gimp-layer-remove-mask theTopLayerDup MASK-DISCARD)
            (gimp-layer-set-mode theTopLayerDup LAYER-MODE-OVERLAY)
            ;(gimp-message "line 821")
            
            ; step B09
            ; ======== 
            (plug-in-gmic-qt 1 theOldImage theTopLayer 1 0 ; 1=input only active layer
                (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_segment_watershed 5,1,0,0"
                )
            )
            (plug-in-gmic-qt 1 theOldImage theTopLayerDup 1 0 ; 1=input only active layer
                (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-cartoon 3,200,20,0.25,1.5,8"     ; FT4 to 8
                )
            )
            
            ;(gimp-message "line 838")
        ); end begin the part B if requested
    ); end if requested  
    ;(gimp-message "line 841")
    ; =====================================================================================================
    (if (= doFlatten TRUE)
        (begin
            ;(gimp-message "line 755 flatten")
            (gimp-image-flatten theOldImage)
        ) 
    )
    ;Ensure the updated image is displayed now
    (gimp-displays-flush)
    (gimp-message "Good finish OK!")
     
  ) ; end let*
  (gimp-image-undo-group-end theOldImage)
) ; end define

; ==================================================================================================
; =========================== end of the main script definition ====================================
; ==================================================================================================

; ====================================================================================================
; =========================== end of the main script registration ====================================
; ====================================================================================================

;---------------------------------------------------------------------------------------------------------

(define (lew-engrave image drawable pre-process lh gamma interpol option version2 blur early)
    
    (let* (
            (levels-white-thresh 1)   ; threshhold for final levels -- 1 is value used if not version 2
            ; (scale  (pow 255.001 gamma))  ; to scale to 0..1 range -- no longer used
            (width  (car  (gimp-drawable-width drawable)))
            (height (car  (gimp-drawable-height drawable)))
            (fwidth  1)    ; final image values (set later)
            (fheight 1)
            ; (x0     (car  (gimp-drawable-offsets drawable)))
            ; (y0     (cadr (gimp-drawable-offsets drawable)))
            ;        ^^^^ - here we pick the second element of the returned list...
            (mask-layer 0)
            (hack  (+ (trunc (/ lh 2)) 1))  ; need to rotate the mask because of a strange behavior of gimp-layer-scale-full with INTERPOLATION-NONE in 2.6.1, fixed in 2.6.6, so hack undone below
                                           ;              rows
                                           ; scale by 3   2 3 4     2 3 3 4
                                           ; scale by 4   3 4 5     3 4 4 5
                                           ;          5   3
                                           ;          6   4
                                           ;          7   4
                                           ;          8   5
                                           ; So:   +  trunc  / lh 2   1       --  for more info on bug: http://bugzilla.gnome.org/show_bug.cgi?id=576123
         )
        ; (gimp-message "inside Lew-engrave proc")
        ; (gimp-message (strcat "width=" (number->string width 10) " height=" (number->string height 10) ))
        ; (gimp-message (strcat "scale=" (number->string scale 10) " gamma=" (number->string gamma 10) ))
        ; (gimp-message (strcat "option=" (number->string option 10) ))
        ; (gimp-message (strcat "version2=" (number->string version2 10) " early=" (number->string early 10) ))
        
        (set! interpol (- 2 interpol))   ; flip value: 0,1,2 --> 2,1,0, the idea being to put the usual "cubic" as default.
        
        (set! hack 0)            ; make into comment if 2.6.1 hack needed; this line disables hack for 2.6.6, no longer needed
        
        (gimp-image-undo-group-start image)
        
          ; if color, convert to grayscale
          (if (= (car (gimp-drawable-is-gray drawable)) 0)
             (gimp-image-convert-grayscale image)
          )
        
          (if (> version2 0)    ; I think need to treat bools as numbers -- "(if version2 ..." is perhaps just a presence check in TinyScheme?)
             (begin
                (gimp-levels drawable 0 0 255 (/ 1.0 gamma) 0 255)   ; apply reverse gamma on the initial image -- changing output levels here might allow to insure we always have a black or white line
                (set! gamma 1.0)  ; make it 1 so the rest of the code doesn't apply it again, and the mask gets equal-spaced levels
             )
          )
        ;(gimp-message "line 913")
        
          (if (= pre-process 0)          ; scale down if not done prior to calling script (do after gamma adjustment)
             (begin
                (if (< width lh)        ; protection against silly uses
                   (set! width lh)
                )
                (if (< height lh)
                   (set! height lh)
                )
                (gimp-layer-scale drawable (/ width lh) (/ height lh) FALSE)
                (set! width  (car  (gimp-drawable-width drawable)))    ; get the new values for use below
                (set! height (car  (gimp-drawable-height drawable)))
             )
          )
        
          (if (= pre-process 1)          ; scale down if not done prior to calling script (do after gamma adjustment)
             (begin
                (if (< height lh)
                   (set! height lh)
                )
                (gimp-layer-scale drawable width (/ height lh) FALSE)
                (set! height (car  (gimp-drawable-height drawable)))
                (set! fwidth width) ; special case
             )
             (set! fwidth (* width lh))
          )
        
          (set! fheight (* height lh)) ; always
        
          (gimp-image-resize  image  fwidth fheight 0 0)  ; normal case
        
          (if (not (= pre-process 1))   ; don't do horizontal scale if pre-process = downsize vertical
             (gimp-layer-scale-full drawable fwidth  height  FALSE interpol)  ; was INTERPOLATION-CUBIC -- LANCZOS no good because it spills, altho if done first, perhaps not a big issue
          )
          (gimp-context-set-interpolation INTERPOLATION-NONE)
          (gimp-layer-scale drawable fwidth fheight FALSE)
          
          (set! mask-layer (car
            (gimp-layer-new  image  fwidth  fheight  GRAY-IMAGE "Engrave temporary" 100. LAYER-MODE-SUBTRACT)
            ; no-better (gimp-layer-new-from-drawable drawable image)
          ) )
        ;(gimp-message "line 955")
        
          (gimp-image-insert-layer image mask-layer 0 -1)
        
          (gimp-drawable-fill mask-layer FILL-WHITE)
        
          (gimp-image-set-active-layer image mask-layer)
        
          ;(gimp-rect-select image 0 0 1 lh CHANNEL-OP-REPLACE FALSE 0.0)
          (gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 0 1 lh)
        
            (let* ( (y 0) (yp 0) (yprh 0) (ds -1) (di 1) (rgb (cons-array 1 'byte)) (offset 0.03) )
        
                ; offset is to adjust the blacks to get a better gamma (still work-in-progress)
                ; y goes from 0 to (lh-1), with increasing pixel values (in one case below, it goes backwards)
                ; yp is the desired pixel position; for the "center" options, it starts near center, then hops before and after until the edges are reached
                ;                                   for the other two, it tracks y (in one case, y==yp, so it's not even used)
                ; yprh is a rotation of above yp within the lh window, because of a bug in GIMP 2.6.1. In 2.6.6, yprh is in effect same as yp.
                ; di and ds are used to execute the hop (walk the code, no pun, to grok this)
                ; rgb is the greyscale pixel value (notwithstanding the "RBG" name)
                
                (if (= version2 0)
                   (set! offset 0.5) ; this value works better in version 1
                )
                
                (if (= option 0)  ; Center black    (brightest mask line in middle)
                  (begin
                     (set! y (- lh 1))            ; y decreasing for this one (grey scale)
                     (set! yp (trunc (/ lh 2)))
                     (while (>= y 0)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma (the "pow" is NOP in version 2
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (- y 1))
                        (set! yp (+ yp (* ds di)))  ; hop in growing zig-zag
                        (set! di (+ di 1))
                        (set! ds (- 0 ds))
                     )
                  )
               )
               (if (= option 1)  ; Center white    (brightest mask line on one edge)
                  (begin
                     (set! yp (trunc (/ lh 2)))
                     (while (< y lh)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (+ y 1))
                        (set! yp (+ yp (* ds di)))  ; hop in growing zig-zag
                        (set! di (+ di 1))
                        (set! ds (- 0 ds))
                     )
                  )
               )
               (if (= option 2)  ; Black on bottom (brightest mask line at bottom)
                  (begin
                     (while (< y lh)              ; no yp needed here
                        ; (aset rgb 0 (trunc (* 255 (/ (+ offset y) lh))) )         ; linear
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ y hack) lh))  ; rotate hack - was y
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (+ y 1))
                     )
                  )
               )
               (if (= option 3)  ; Black on top    (brightest mask line at top)
                  (begin
                     (set! yp (- lh 1))
                     (while (< y lh)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! yp (- yp 1))
                        (set! y (+ y 1))
                     )
                  )
               )
               ; may want symetrical versions of line in center?
            
            )
            
            (gimp-edit-copy mask-layer)
            
            (gimp-selection-all image)  ; needed for bucket fill it seems
            
            (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0))  ; multi-lingual equivalent of (gimp-context-set-pattern "Clipboard") : "clipboard" is always first
            
            (gimp-edit-bucket-fill mask-layer BUCKET-FILL-PATTERN LAYER-MODE-NORMAL 100. 0. FALSE 0. 0.)
            
            (gimp-selection-none image)
            
            (if (< early 1)     ; skip these last two steps if early exit asked for
                (begin
                    (set! mask-layer (car
                        (gimp-image-merge-down image mask-layer CLIP-TO-IMAGE)   ; returns a new layer!
                        )
                    )
                
                (if (> version2 0)
                   (set! levels-white-thresh (trunc (/ 255 lh) ) )    ; assumes gamma now 1, ie. spacing is regular in the mask values
                )
                
                ;(gimp-levels mask-layer 0 0 levels-white-thresh 1.0 0 255)
                (gimp-drawable-levels mask-layer HISTOGRAM-VALUE 0.0 (/ levels-white-thresh 255) TRUE 1.0 0.0 1.0 TRUE)
                ; (gimp-levels mask-layer 0 0 (trunc (/ 100 lh)) 1.0 0 255)
                
                (if (> blur 1)
                   (plug-in-gauss 1 image mask-layer blur 1.0 1)  ; only horiz
                )
             )
             ;(gimp-message "Early exit: merge down & levels left to do")
          )
            
           (gimp-image-set-active-layer image mask-layer)  ; for engraveDoc -- not sure needed
            
           (gimp-image-undo-group-end image)
            
           ; finally we notify the UI that something has changed.
           
          (gimp-displays-flush)
          ;(gimp-message "exit lew-engrave")
   )
)

; Main registration
(script-fu-register "script-fu-woodcut-carved-effect"
            "<Toolbox>/Script-Fu/Artistic/Creates a woodcut-carved effect"
            "Creates a woodcut-carved effect of an image starting from PatDavid's tutorial. Please note that Make Seamless option creates a pattern and saves it to C:\Workspace but does not use it - None and Rescale work well. Once finished please ensure that you look at the layers as numerous options are possible by setting layer visibility.\n file:DIEGO-GRAECHAN-RobA_Woodcut-Carving-Effect_1.1.scm"
            "Diego Nassetti"
            "Diego Nassetti"
            "December 2014"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-TOGGLE       "Add Pattern"   TRUE
            SF-OPTION       "Pattern Type"         '("Wood" "Bricks" "Leather" "Plain Metal" "Rusty Metal")
            SF-PATTERN      "Pattern"             "Pine"
            SF-OPTION       "Action on Pattern"   '("Rescale" "Make-seamless" "None")
            SF-TOGGLE       "Flatten Image"       FALSE
          
)



; end of script