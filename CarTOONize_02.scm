;*************************************************************************************** 
; Toon Image script  for GIMP 2.6
; Version 1.0.1 - 17-12-2010 added halftone option, check for existence of GREYCstoration
;                            pop up message only.  Filter continues if non-existent. 
;                            Added G'MIC support as per forum request.
; Version 1.0.2 - 17-12-2010 Re-did the Half-tone the same as the comic strip effect
;                            Copyright (C) 2010 John Harris john@grynn.com>  
;                            Based on comic-book script by Joe Kitella <joe.kitella@gmail.com
; Version 1.0.3 - 17-12-2010 Revised Halftone effect again... Coding problem with layers. 
; Version 1.0.4 - 20-12-2010 Added last resort pre-filter of Selective Gaussian Blur as per
;                            forum request (Rob A>), tightened up the parameters from the request at 
;                            5/15 to 4/11. There was too much edge loss at the higher values.
; Version 1.0.5 - 21-12-2010 Added SF-OPTION to select the default smoothing type if all filters
;                            are installed. (Defaults to none installed).   
; Version 1.0.6 - 22-12-2010 Added Despeckle smoothing option, ... to display name, and did all edit-
;                            copy-pastes to a named buffer.  Cleaned up code, removed a section that was
;                            no longer being used. Generates better skin-tone colors than before.
;                 24-12-2010 Added Despeckle to filter register information.  Changed levels for truer colors.
; Version 1.0.7 - 31-12-2010 Added Level of colors to either flatten out the colors, or make more levels.
;                            Fixed the active layer display issue when not using halftone.  Added option to
;                            keep image on existing layer (non haltone). Default is keep. Gimp 2.7 fixes.
; Version 1.0.8 - 31-03-2020 Align for use on Gimp 2.10.18
; Version 1.0.9 - 23.09.2021 Align for use on Gimp 2.10.24
; Version 1.0.10 - 06/03/2022 Enhanced line managment and darkening; Use on Gimp 2.10.30
; ---------------------------------------------------------------------------------------
(define (script-fu-CarTOONize
                        img
                        drawable
                        Prefilter
                        filter-type
                        line-size
                        enable-shadow
                        shadow-intensity
                        cdepth
                        new-layer
                        half-tone
                        dilate
                        dilatecount
                        keepbase
                        flattenbase
    )
    
    (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-selection (car (gimp-selection-save img)))
            (image-type (car (gimp-image-base-type img)))
            (buffer (car (gimp-edit-named-copy drawable "temp-buffer")))
            (toon-img (car (gimp-edit-named-paste-as-new buffer)))
            (layer-temp1 (car (gimp-layer-new toon-img width height 0 "temp1"  100 LAYER-MODE-NORMAL)))
            (layer-temp2 (car (gimp-layer-new toon-img width height 0 "temp2"  100 LAYER-MODE-NORMAL)))
            (layer-temp3 (car (gimp-layer-new toon-img width height 0 "temp3"  100 LAYER-MODE-NORMAL)))
            (layer-temp2a (car (gimp-layer-new toon-img width height 0 "temp2a"  100 LAYER-MODE-NORMAL)))
            (layer-temp4 (car (gimp-layer-new toon-img width height 0 "temp4"  100 LAYER-MODE-NORMAL)))
            (layer-temp5 (car (gimp-layer-new toon-img width height 0 "temp5"  100 LAYER-MODE-NORMAL)))
            (layer-temp6 (car (gimp-layer-new toon-img width height 0 "temp6"  100 LAYER-MODE-NORMAL)))
            (layer-temp7 (car (gimp-layer-new toon-img width height 0 "temp7"  100 LAYER-MODE-NORMAL)))
            (layer-gmic (car (gimp-layer-new toon-img width height 0 "gmic"  100 LAYER-MODE-NORMAL)))
            (layer-cartoonize (car (gimp-layer-new img width height 0 "CarTOONize"  100 LAYER-MODE-NORMAL)))
            (layer-comic (car (gimp-layer-new img width height 0 "Comic-strip"  100 LAYER-MODE-NORMAL)))
            (cdepth (* 4 cdepth))     
            (isfiltered  FALSE)
            (achannel)
            (countDil 0) ; dilation counter
        ) 
        (gimp-image-undo-disable toon-img)
        
        
        (if (eqv? (car (gimp-selection-is-empty toon-img)) TRUE)
            (gimp-drawable-fill old-selection FILL-WHITE)
        ) 
        (gimp-image-insert-layer toon-img layer-gmic 0 -1)
        (gimp-image-insert-layer toon-img layer-temp7 0 -1) 
        (gimp-image-insert-layer toon-img layer-temp6 0 -1) 
        (gimp-image-insert-layer toon-img layer-temp5 0 -1)
        (gimp-image-insert-layer toon-img layer-temp4 0 -1)
        (gimp-image-insert-layer toon-img layer-temp3 0 -1)
        
        (if (= enable-shadow TRUE)
            (begin
                (gimp-image-insert-layer toon-img layer-temp2a 0 -1)
                (gimp-layer-set-mode layer-temp2a 3)
            )
        )
        
        (gimp-image-insert-layer toon-img layer-temp1 0 -1)
        (gimp-image-insert-layer toon-img layer-temp2 0 -1)
        (gimp-drawable-levels-stretch drawable)
        (set! buffer (car (gimp-edit-named-copy drawable "temp-buffer")))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste  layer-gmic buffer 0)))
        (if (= Prefilter TRUE)
            (begin
                (if (= filter-type 2)
                  (begin
                    ;(gimp-message "Filter type = 2")
                    (if (defined? 'plug-in-gmic-qt)
                        (begin
                ;;        (plug-in-greycstoration 1 toon-img layer-gmic 60 .70 1 .6 1.1 .8 30 2 0 0 4 10 15 7 1 2)
                ;;          (plug-in-gmic-qt 1 toon-img layer-gmic 1 0 "-cartoon 3.62,221.23,8.7,0.3345,1.788 8")
                            ;(gimp-message "Gmic fx_smooth_haar")
                            (plug-in-gmic-qt 1 toon-img layer-gmic 1 0 "-fx_smooth_haar 1,10,10,0,0,82")
                            (set! isfiltered TRUE)
                        )
                        (begin
                            (gimp-message "Gmic must be installed to prefilter, no filter will be applied. obtain from http://Gmic.eu")
                        )
                        
                    )
                  )
                )
            )
        )
        (if (= Prefilter TRUE)
          (begin
            (if (= filter-type 3)
              (begin
                (if (= isfiltered FALSE)
                  (begin
                    (if (defined? 'plug-in-gmic-qt)
                      (begin
                        (plug-in-gmic-qt 1 toon-img layer-gmic 1 0 "-fx_smooth_anisotropic 80,.7,1,1.1,1,1.0,30,2,0,1,1,1") ; was 60,.7,1,1.1,1,1,30,2,0,1,1,1,1
                        (set! isfiltered TRUE)
                        ;(gimp-message "fx_smooth_anisotropic done")
                        (gimp-displays-flush)
                        ;(gimp-message "quitting")
                        ;(quit)
                      )
                      (begin
                        (gimp-message "G'MIC must be installed to prefilter, no filter will be applied.")
                      )
                    )
                  )
                )
              )
            )
          )
        )
        
        (if (= Prefilter TRUE)
          (begin
            (if (= filter-type 0)
              (begin
                ;(gimp-message "Filter type is 0")
                (if (= isfiltered FALSE)
                  (begin
                    (if (defined? 'plug-in-sel-gauss)
                        (begin
                            ;(gimp-message "plug-in-sel-gauss go")
                            (plug-in-sel-gauss 1 toon-img layer-gmic 4 11)
                        )
                    )
                  )
                  (begin
                    ;(gimp-message "Is filtered is True line 131")
                  )
                )
              )
            )
          )
        )
        
        (if (= Prefilter TRUE)
            (begin
                (if (= filter-type 1)
                  (begin
                    (if (= isfiltered FALSE)
                      (begin
                        (if (defined? 'plug-in-despeckle)
                            (begin
                                ;(gimp-message "despeckle")
                                (plug-in-despeckle 1 toon-img layer-gmic 7 1 -1 256)
                            )
                            (begin
                                (gimp-message "No despeckle installed")
                            )
                        )
                      )
                    )
                  )
                )
            )
        )
        
        ;(gimp-message "line 186")
        
        
        (set! buffer (car (gimp-edit-named-copy (car (gimp-image-get-active-layer toon-img)) "temp-buffer")))
        
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp1 buffer 0)))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp3 buffer 0)))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp4 buffer 0)))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp7 buffer 0)))
        (if (= enable-shadow TRUE)
            (begin
                (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp2a buffer 0)))
                (gimp-drawable-desaturate layer-temp2a DESATURATE-AVERAGE)
                (gimp-drawable-threshold layer-temp2a HISTOGRAM-VALUE (/ shadow-intensity 255) 1.0)
            )
        )
        ;(gimp-message "line 202")
        
        
        (gimp-layer-set-mode layer-temp3 15)
        (gimp-layer-set-mode layer-temp5 15)
        (gimp-layer-set-mode layer-temp4 3)
        (gimp-layer-set-mode layer-temp6 3)
        (gimp-layer-set-mode layer-temp2 16)
        (gimp-displays-flush)
        
        (gimp-drawable-desaturate layer-temp3 DESATURATE-LIGHTNESS)
        (set! buffer (car (gimp-edit-named-copy layer-temp1 "temp-buffer")))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp2 buffer 0)))
        
        ;(gimp-message "line 216") 
        
        
        (plug-in-gauss 1 toon-img layer-temp2 line-size line-size 0) ; 1)
        (gimp-drawable-invert layer-temp2 FALSE) ; was true
        
        ; added by karlhof26
        (gimp-layer-set-opacity layer-temp2 68)
        ;(gimp-display-new toon-img)
        ;(quit)
        
        (gimp-image-merge-down toon-img layer-temp2 0)
        
        (set! layer-temp1 (car (gimp-image-get-active-layer toon-img)))
         (gimp-drawable-threshold layer-temp1 HISTOGRAM-VALUE (/ (+ 180 (* line-size 1.5)) 255) 1.0) ; was (/ 245 255) 1.0
        ;(gimp-drawable-threshold layer-temp1 HISTOGRAM-VALUE (/ 245 255) 1.0) ; was (/ 245 255) 1.0
        
        (if (= dilate TRUE)
            (begin
                
                ;must invert before dilation
                (gimp-drawable-invert layer-temp1 FALSE)
                
                (gimp-selection-all toon-img)
                
                (set! achannel (car (gimp-image-get-active-channel toon-img)))
                (while (< countDil dilatecount)
                    (plug-in-dilate 1 toon-img layer-temp1 1 achannel 0.99 5 0 255)
                    (set! countDil (+ countDil 1))
                )
                ;(plug-in-dilate 1 toon-img layer-temp1 1 achannel 0.99 10 0 255)
                (gimp-selection-none toon-img)
                (gimp-drawable-invert layer-temp1 FALSE)
            )
        )
        ;(gimp-display-new toon-img)
        ;(quit)
        
        (gimp-layer-set-mode layer-temp1 LAYER-MODE-MULTIPLY)  ;was 3
        
        
        
        (gimp-image-merge-down toon-img layer-temp3 0)
        (set! layer-temp4 (car (gimp-image-get-active-layer toon-img)))
        (set! buffer (car (gimp-edit-named-copy layer-temp4 "temp-buffer")))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp5 buffer 0)))
        (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp6 buffer 0)))
        
        
        
        (gimp-drawable-desaturate layer-temp5 DESATURATE-LIGHTNESS)
        ;(gimp-message "line 267")
        (gimp-image-merge-down toon-img layer-temp5 0)
        (set! layer-temp4 (car (gimp-image-get-active-layer toon-img)))
        (gimp-layer-set-mode layer-temp4 3)
        
        (gimp-displays-flush)
        ;(gimp-display-new toon-img)
        ;(quit)
        
        (gimp-image-raise-layer toon-img layer-temp4)
        (gimp-image-merge-down toon-img layer-temp4 0)
        (set! layer-temp6 (car (gimp-image-get-active-layer toon-img)))
        (gimp-layer-set-mode layer-temp6 14)
        (gimp-image-merge-down toon-img layer-temp6 0)
        (set! layer-temp7 (car (gimp-image-get-active-layer toon-img)))
        ;(gimp-levels layer-temp7 0 0 255 1 0 225)
        (gimp-drawable-levels layer-temp7 HISTOGRAM-VALUE 0.0 1.0 TRUE 1.0 0.0 0.905 TRUE) ; last value was 225
        ;(gimp-message "line 284")
        
        (if (= flattenbase TRUE)
            (gimp-image-flatten toon-img)
        )
        
        (gimp-image-convert-indexed toon-img CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE cdepth FALSE FALSE "x")
        (gimp-image-convert-rgb toon-img)
        
        (set! layer-temp1 (car (gimp-image-get-active-layer toon-img)))
        (set! buffer (car (gimp-edit-named-copy (car (gimp-image-get-active-drawable toon-img)) "temp-buffer" )))
        
        ; These lines here for testing
        ;(gimp-image-insert-layer img layer-cartoonize 0 -1)
        ;(gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-cartoonize buffer 0 )))
        ;(gimp-display-new toon-img)
        (gimp-displays-flush)
        ;(quit)
        
        
        (if (= new-layer TRUE)
            (begin
                ;(gimp-message "new-layer True")
                (gimp-image-insert-layer img layer-cartoonize 0 -1)
                (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-cartoonize buffer 0 )))
                (gimp-image-set-active-layer img layer-cartoonize)
            )
            (begin
                ;(gimp-message "new-layer False")
                (gimp-floating-sel-anchor (car (gimp-edit-named-paste drawable buffer 0 )))
                ;(gimp-display-new toon-img)
            )
        )
        
        ;(gimp-message "line 318")
        (if (= half-tone TRUE)
            (begin
                ;(gimp-message "half-tone start")
                ;(gimp-message "line 322")
                (gimp-image-insert-layer img layer-comic 0 -1)
                (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-comic buffer 0)))
                (plug-in-unsharp-mask 1 img layer-comic 4.0 10.0 1)
                ;(gimp-message "line 326")
                (if (defined? 'plug-in-newsprint)
                    (begin
                        ;(plug-in-newsprint 1 img layer-comic 1 0.3 4 1 0 15 0 75 0 0 0 6)
                        (plug-in-newsprint 1 img layer-comic 4 1 0 15.0 0 75.0 0 4.0 0 0.0 0 7)
                    )
                    (begin
                        (gimp-message "No newsprint plug in") ;else
                    )
                )
            )
        )
        (gimp-image-undo-enable toon-img)
        (if (= keepbase TRUE)
            (begin
                (gimp-display-new toon-img)
            )
            (begin
                (gimp-image-delete toon-img)
            )
        )
        (gimp-displays-flush)
        
        (gimp-image-undo-group-end img)
        (gimp-message "Good finish OK!")
        (gc) ; memory garbage cleanup; due to temp image
  )
)

(script-fu-register
    "script-fu-CarTOONize"
    "<Image>/Script-Fu/Artistic/CarTOONize"
    "Toon a Picture.  Take a standard RGB or Grayscale picture, and cartoonize it.  You can adjust the thickness of the lines, as well as adjust the 'black' shadow intensity.  Now you can adjust the color depth, 1 is the flattest, 10 has the most levels.  Prefilter option requires and uses the G'MIC Filter, Despeckle or the Selective Gaussian Blur.  Set it to FALSE if you don't want noise reduction.  Now have the option to keep it on existing layer (non haltone).\n file:CarTOONize_02.scm"
    "Joe1GK <kgioj@yahoo.com>"
    "Joe1GK"
    "2010, December"
    "RGB* GRAY*"
    SF-IMAGE        "Image"	                     0
    SF-DRAWABLE     "Drawable"                   0
    SF-TOGGLE       "Prefilter (Noise reduction)" TRUE
    SF-OPTION       "Filter type - Noise Reduction"   '("Selective Gaussian Blur Smoothing" "Despeckle Smoothing" "GMIC Smoothing" "G'MIC Anistropic Smoothing")
    SF-ADJUSTMENT   "Line Thickness (threshold black areas)"   '(10 2 30 1 10 0 1)
    SF-TOGGLE       "Black Shadows"             TRUE
    SF-ADJUSTMENT   "Shadow Intensity"          '(35 0 50 1 10 0 1)
    SF-ADJUSTMENT   "Color Levels"              '(4 1 10 1 10 0 1)
    SF-TOGGLE       "Generate as new layer"     TRUE
    SF-TOGGLE       "Halftone"                  TRUE
    SF-TOGGLE       "Broaden lines?"             FALSE
    SF-ADJUSTMENT   "Broaden line iterations"   '(2 1 10 1 2 0 0)
    SF-TOGGLE       "Keep working image (show 2nd image)"           FALSE
    SF-TOGGLE       "Flatten working image"      TRUE
)

; end of file
