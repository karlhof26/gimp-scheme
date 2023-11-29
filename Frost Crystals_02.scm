; Frost Crystals rel 3.3
; Created by Graechan
; from a tutorial thanks to K1TesseraEna http://www.gimpchat.com/viewtopic.php?f=23&t=6143
; Cold Sky background adapted from a Tharice Demand script
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
; Rel 0.02 - changed the look of the Frost Crystals with added settings
; Rel 0.03 - Added 'Transparent' to Background choices in Alpha-to-Logo script and improved script finish
; Rel 0.03.2 - Improved script finish
; Rel 0.03.3 - Bugfix for Typo
(define (script-fu-frost-crystals-logo 
                                      text 
                                    font-in 
                                    font-size
                                    font-color
                                    bkg-type 
                                    pattern
                                    bkg-color
                                    gradient
                                    letter-spacing
                                    size
                                    brush-space
                                    frost-color
                                    lum-threshold 
                                    flare-inten 
                                    spike-len
                                    density
                                    outline-option
                                    outline-color
                                    outline-growth
                                    conserve                                      
            )
                                    
  (let* (
            (offx 0)
            (offy 0)
            (img-width 256) 
            (img-height 256)
            (image (car (gimp-image-new img-width img-height RGB)))         
            (border (/ font-size 4))
            (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
            (size-layer (car (gimp-text-fontname image -1 0 1 text border TRUE font-size PIXELS font)))
            (final-width (car (gimp-drawable-width size-layer)))
            (final-height (car (gimp-drawable-height size-layer)))
            (text-layer 0)
            (outline 0)
            (width img-width)
            (height img-height)
            (text-width 0)
            (text-height 0)
            (selection 0)
            (bkg-layer 0)
            (img 0)
            (layer 0)
            (img-display 0)
            (path 0) 
            
            (layer-color (car (gimp-layer-new image final-width final-height RGBA-IMAGE
                    "Color Layer" 100 LAYER-MODE-OVERLAY)))
        )
        ;(gimp-message "start")
        (gimp-context-push)
        (gimp-context-set-foreground '(0  0  0)) ; was 0 0 0 ; used to set font-color
        (gimp-context-set-background '(255 255 255))
        (gimp-image-insert-layer image layer-color 0 0)
        ;(gimp-display-new image)
        
        ;(gimp-image-insert-layer image size-layer 0 0)
        
        ;;;;Add the text layer for a temporary larger Image size
        (set! text-layer (car (gimp-text-fontname image -1 0 0 text (round (/ 350 4)) TRUE 350 PIXELS font)))
        ;(gimp-message "line91")
        
        ;(gimp-image-insert-layer image text-layer 0 -1)
        (gimp-item-set-name text-layer "text layer")
        (gimp-item-set-name size-layer "size layer")
        ;(gimp-message "line96")
        
        (gimp-text-layer-set-justification text-layer 2)
        (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
        (set! text-width (car (gimp-drawable-width text-layer)))
        (set! text-height (car (gimp-drawable-height text-layer)))    
        (gimp-image-remove-layer image size-layer)  ; kh change
        
        (gimp-image-resize-to-layers image)	
        ;(gimp-message "line105")
        
        ;;;;set the new Image size
        (if (> text-width img-width) (set! width text-width))           
        (if (> text-height img-height) (set! height text-height))
        ;(gimp-message (number->string width))
        
        ;;;;resize the image    
        (gimp-image-resize image width height 0 0)
        
        ;;;;centre the text layer   
        (set! offx (/ (- width text-width) 2))
        (set! offy (/ (- height text-height) 2))    
        (gimp-layer-set-offsets text-layer offx offy)
        
        ;(gimp-message "after centre text layer") 
        
        ;;;;set the text layer    
        (gimp-image-select-item image CHANNEL-OP-ADD text-layer)
        (set! selection (car (gimp-selection-save image)))
        
        (gimp-drawable-fill text-layer FILL-TRANSPARENT)
        (gimp-layer-set-mode text-layer LAYER-MODE-SCREEN)
        (gimp-selection-none image)    
        ;(gimp-message "debug 3")
        ;(gimp-display-new image)
        ;(quit)
        
        ;;;;resize the text-layer ;;;
        (gimp-image-set-active-layer image text-layer)
        (gimp-layer-resize-to-image-size text-layer)    
        
        ;;;;create the brush
        (set! img (car (gimp-image-new size (/ size 12) RGB)))
        (set! layer (car (gimp-layer-new img size (/ size 12) RGBA-IMAGE "Brush" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer img layer 0 0)
        
        (gimp-image-select-ellipse img 2 0 0 size  (/ size 12) )
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(21 45 76))
        (gimp-edit-blend layer BLEND-FG-BG-RGB LAYER-MODE-NORMAL GRADIENT-BILINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE (/ size 2) (/ (/ size 12) 2) size (/ (/ size 12) 2) )
        ;(gimp-message "line146")
        ;(gimp-displays-flush)
        ;(gimp-display-new image)
        
        (gimp-selection-none img) ; kh change
        (catch
            (begin
                (gimp-message "ERROR:: Logobrush not found")
                (quit)
            )
            (set! path (string-append gimp-directory "/brushes/" "logobrush" ".gbr"))
            (file-gbr-save RUN-NONINTERACTIVE img layer path path brush-space "Logo Brush")
            ;(gimp-image-delete img) ; kh change
            ;(gimp-message "line159")
            
            ;;;;stroke the text-layer
            (gimp-brushes-refresh)
            (gimp-context-set-brush "Logo Brush")
            (gimp-context-set-dynamics "Dynamics Random")
        )
        
        ; added by karlhof26
        (gimp-context-set-foreground font-color)
        ;(gimp-context-set-feather 2)
        ;added by karlhof26
        (gimp-context-set-brush-size (* size 2))
        (gimp-context-set-brush-spacing (/ brush-space 100))
        
        ;; deprecated (gimp-selection-load selection) replaced with line below
        (gimp-image-select-item image CHANNEL-OP-REPLACE selection)
        
        ;added by karlhof26
        (gimp-drawable-edit-fill text-layer FILL-FOREGROUND)
        
        (gimp-edit-stroke text-layer)
        
        
        (gimp-selection-none image)
        (plug-in-sparkle 1 image text-layer lum-threshold flare-inten spike-len 4 15 density 0 0 0 FALSE FALSE FALSE 0)
        (gimp-brush-delete "Logo Brush") ; kh chnaged to Logo Brush
        
        ;; resequenced by karlhof26 to allow font to be colored
         ;;;;color the frost
        ;(gimp-message "line186 colorify")
        (gimp-selection-all image)
        (gimp-image-select-item image CHANNEL-OP-SUBTRACT selection)
        (plug-in-colorify RUN-NONINTERACTIVE image text-layer frost-color)
        (gimp-selection-none image)
        
        
        ;;;;Scale Image to it's original size;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (gimp-image-scale-full image final-width final-height 2)
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
        
        
        ;;;;create the background layer    
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 1)
        (gimp-context-set-pattern pattern)
        (gimp-context-set-background bkg-color)
        (gimp-context-set-gradient gradient)
        
        ;(gimp-message "line206")
        ;(gimp-displays-flush)
        ;(gimp-display-new image)
        
        (if (= bkg-type 0)
            (begin
                ;(gimp-message "line212 off to frosty sky")
                (fc-frosty-sky image bkg-layer 230 30 6 32 24 conserve) ; was 200 20 4 32 24
                ;(gimp-message "line214 back")
            )
        )
        (if (= bkg-type 1)
            (gimp-drawable-fill bkg-layer FILL-PATTERN)
        )
        (if (= bkg-type 2)
            (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
        )
        (if (= bkg-type 3)
            (gimp-edit-blend bkg-layer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 0 0 width height)
        )
        
        ; kh debug code
        ;(gimp-image-insert-layer image layer 0 -1)
        ;
        ;(gimp-message "line230")
        ;(gimp-display-new image)
        ;(gimp-display-new img)
        ;(gimp-displays-flush)
        ;(quit)
        
        ;;;;color the frost - Removed here and placed higher by karlhof26
        ;(gimp-message "colorify")
        ;(plug-in-colorify RUN-NONINTERACTIVE image text-layer frost-color)
        
        ;(gimp-message "line240")
        ;(gimp-display-new image)
        ;(quit)
        
        ;;;;add crystals outline
        (if (= outline-option TRUE)
            (begin
                ;(gimp-message "line247 outline")
                (set! outline (car (gimp-layer-new image width height RGBA-IMAGE "Outline" 100 LAYER-MODE-OVERLAY)))
                (gimp-image-insert-layer image outline 0 (+ (car (gimp-image-get-layer-position image text-layer)) 1)) ;stack 0=above 1=below
                ;; depreacted (gimp-selection-layer-alpha text-layer)
                ;(gimp-message "line251")
                ;(gimp-display-new image)
                
                ;(gimp-image-selection-all image)
                (gimp-image-select-item image CHANNEL-OP-REPLACE selection) ; was channel-op-add
                (gimp-selection-grow image outline-growth) ; was 1
                (gimp-context-set-foreground outline-color)
                (gimp-edit-fill outline FILL-FOREGROUND)
                (gimp-selection-none image)
                
                
            )
        )
        (gimp-image-set-active-layer image text-layer)
        
        ;(gimp-message "line266")
        ;(gimp-display-new image)
        ;(quit)
        (gimp-layer-set-mode text-layer LAYER-MODE-NORMAL)
        
        (if (and (= conserve TRUE) (= bkg-type 0))
            (begin
                ;(gimp-message "line272 remove bkglayer")
                (gimp-image-remove-layer image bkg-layer)
            )
        )
        (if (= conserve FALSE)
            (begin
                (set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
                
                ;(set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
                ;
                ;(set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
            )
        ) ;endif
        (gimp-drawable-set-name text-layer text)
        
        
        (gimp-context-pop)
        (gimp-display-new image)
        (gc) ; garbage cleanup; memory cleanup
        
  )
)
  
(script-fu-register "script-fu-frost-crystals-logo"
    "Frost Crystals[logo]..."
    "Create an image with a text layer over a pattern layer. The overlay option may overwhelm the font color.\nfile:Frost Crystals_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "June 2011"
    ""
    SF-TEXT       "Text"    "FROST CRYSTALS"
    SF-FONT       "Font"               "Arial Bold"
    SF-ADJUSTMENT "Font size (pixels)" '(200 10 500 1 1 0 1)
    SF-COLOR      "Font color"         '(0 0 0)
    SF-OPTION "Background Type" '("Cold Sky" "Pattern" "Color" "Gradient" "Transparent")
    SF-PATTERN    "Pattern"            "Pink Marble"
    SF-COLOR      "Background color"         '(0 0 0)
    SF-GRADIENT   "Background Gradient" "Abstract 3"
    SF-ADJUSTMENT "Letter Spacing"          '(20 -100 100 1 10 0 0)
    SF-ADJUSTMENT "Brush Size"              '(83 12 100 1 5 0 0)
    SF-ADJUSTMENT "Brush Spacing"           '(100 1 4900 1 5 0 0)
    SF-COLOR      "Frost Color"             '(255 255 255)
    SF-ADJUSTMENT "Frost-Amount"            '(.025 0 .075 .001 .005 3 0)
    SF-ADJUSTMENT "Frost-Intensity"         '(.15 0 .30 .01 .05 2 0)
    SF-ADJUSTMENT "Frost-Length(pixels)"    '(20 1 100 1 5 0 0)
    SF-ADJUSTMENT "Frost-Density"           '(1 0 1 .01 .1 2 0)
    SF-TOGGLE     "Outline in color"        TRUE
    SF-COLOR      "Outline Color"           '(0 0 0)
    SF-ADJUSTMENT "Outline growth"          '(1 0 20 1 5 0 0)
    SF-TOGGLE     "Keep the Layers"         TRUE
    
)

(script-fu-menu-register "script-fu-frost-crystals-logo" "<Toolbox>/Script-Fu/Logos")

(define (script-fu-frost-crystals-alpha image drawable
                                bkg-type 
                                pattern
                                bkg-color
                                gradient
                                size
                                brush-space
                                frost-color
                                lum-threshold 
                                flare-inten 
                                spike-len
                                density
                                original
                                keep-selection-in
                                conserve
        )
        (gimp-image-undo-group-start image)      
        
  (let* (
            (image-layer (car (gimp-layer-copy (car (gimp-image-get-active-layer image)) TRUE)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (original-width width)
            (original-height height)
            (alpha (car (gimp-drawable-has-alpha (car (gimp-image-get-active-layer image)))))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (car (gimp-drawable-get-name drawable)))
            (keep-selection keep-selection-in)
            (bkg-layer 0)
            (outline 0)
            (selection 0)
            (img 0)
            (layer 0)
            (path 0)
            
            (result 111)
            (result2 0)
        )
        
        (gimp-image-insert-layer image image-layer 0 -1)
        (gimp-drawable-set-name image-layer (string-append layer-name "-" "Frost Crystals"))
        
        (gimp-context-push)
        (gimp-context-set-default-colors)
        
        
        
        
        (if (= alpha FALSE) (gimp-layer-add-alpha image-layer))
        
        ;;;;check that a selection was made if not make one	
        (if (= sel TRUE) (set! keep-selection FALSE))
        (if (= sel TRUE) (gimp-selection-layer-alpha image-layer))
        
        ;;;;create selection-channel (gimp-selection-load selection)
        (set! selection (car (gimp-selection-save image)))
        (gimp-drawable-fill image-layer FILL-TRANSPARENT)
        (if (and (= bkg-type 4) (= original FALSE))
            (gimp-layer-set-visible drawable FALSE)
        )
        (if (or (= original TRUE) (= bkg-type 4))
            (begin
                (gimp-selection-invert image)
                (gimp-edit-clear drawable)
            )
        )
        (gimp-selection-none image)
        
        ;(gimp-display-new image)
        ;(quit)
        
        ;;;;create the brush
        (set! img (car (gimp-image-new size (/ size 12) RGB)))
        (set! layer (car (gimp-layer-new img size (/ size 12) RGBA-IMAGE "Brush" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer img layer 0 0)
        (gimp-image-select-ellipse img 2 0 0 size (/ size 12))
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(21 45 76))
        (gimp-edit-blend layer BLEND-FG-BG-RGB LAYER-MODE-NORMAL  GRADIENT-BILINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE (/ size 2) (/ (/ size 12) 2) size (/ (/ size 12) 2))
        (gimp-displays-flush)
        (gimp-selection-none img)
        (catch
            (begin
                (gimp-message "ERROR - Frost brush creation error")
                (quit)
            )
            
            (set! path (string-append gimp-directory "/brushes/" "frostcrystalbrush" ".gbr"))
            (file-gbr-save RUN-NONINTERACTIVE img layer path path brush-space "frostcrystalbrush")
            (gimp-image-delete img)
            
            ;;;;stroke the layer
            (gimp-brushes-refresh)
            (gimp-context-set-brush "frostcrystalbrush")
            (gimp-context-set-dynamics "Dynamics Random")
        )
        ;added by karlhof26
        (gimp-context-set-brush-size (* size 2))
        (gimp-context-set-brush-spacing (/ brush-space 100))
        
        (gimp-selection-load selection)
        (gimp-edit-stroke image-layer)
        (gimp-selection-none image)
        (plug-in-sparkle 1 image image-layer lum-threshold flare-inten spike-len 4 15 density 0 0 0 FALSE FALSE FALSE 0)
        (gimp-brush-delete "frostcrystalbrush")
        
        ;;;;color the frost 
        (plug-in-colorify RUN-NONINTERACTIVE image image-layer frost-color)
        
        ;;;;create the background layer    
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 1)
        (gimp-context-set-pattern pattern)
        (gimp-context-set-background bkg-color)
        (gimp-context-set-gradient gradient)
        (if (= bkg-type 0)
            (begin
                (fc-frosty-sky image bkg-layer 200 20 4 32 24 FALSE)
                (set! bkg-layer (car (gimp-image-get-active-layer image)))
                (gimp-drawable-set-name bkg-layer "Frosty Sky")
            )
        )
        (if (= bkg-type 1)
            (begin
                (gimp-drawable-fill bkg-layer FILL-PATTERN)
                (gimp-drawable-set-name bkg-layer "Pattern Fill")
            )
        )
         
        (if (= bkg-type 2)
            (begin
                (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
                (gimp-drawable-set-name bkg-layer "Color Fill")
            )
        )
        (if (= bkg-type 3)
            (begin
                (gimp-edit-blend bkg-layer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 0 0 width height)
                (gimp-drawable-set-name bkg-layer "Gradient Fill")
            )
        )
            
        ;;;;color the frost - moved higher by karlhof26
        ;(plug-in-colorify RUN-NONINTERACTIVE image image-layer frost-color)
        
        (if (= original TRUE)
            (begin
                (gimp-image-raise-layer-to-top image drawable)
                (gimp-image-lower-layer image drawable)
            )
        )
        
        ;;;;add crystals outline
        (set! outline (car (gimp-layer-new image width height RGBA-IMAGE "Outline" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image outline 0 (+ (car (gimp-image-get-layer-position image image-layer)) 1)) ;stack 0=above 1=below
        (gimp-selection-layer-alpha image-layer)
        (gimp-selection-grow image 1)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill outline FILL-FOREGROUND)
        (gimp-selection-none image)
        (gimp-image-set-active-layer image image-layer)
        
        ;;;;finish the script
        (if (= conserve FALSE)
            (begin
                (set! image-layer (car (gimp-image-merge-down image image-layer EXPAND-AS-NECESSARY)))
                (if (= original TRUE)
                    (set! drawable (car (gimp-image-merge-down image drawable EXPAND-AS-NECESSARY)))
                )
                (if (= original FALSE)
                    (begin
                        (gimp-image-remove-layer image drawable)
                        (set! drawable (car (gimp-image-get-active-layer image)))
                    )
                )
                (set! drawable (car (gimp-image-merge-down image image-layer EXPAND-AS-NECESSARY)))
                (gimp-drawable-set-name drawable (string-append layer-name "-" "\nFrost Crystals"))
            )
        ) ;endif
        (if (= conserve TRUE)
            (begin
                (gimp-drawable-set-name drawable layer-name)
            )
        )
        (if (and (= conserve TRUE) (= original FALSE))
            (begin
                (gimp-image-remove-layer image drawable)
            )
        )
        
        (if (= keep-selection TRUE)
            (gimp-selection-load selection)
        )
        (gimp-image-remove-channel image selection)
        
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        (gc); garbage; cleanup
        
  )
)

(script-fu-register "script-fu-frost-crystals-alpha"        		    
    "Frost Crystals[alpha]..."
    "Instructions. Requires a layer with transparency. \nfile:Frost Crystals_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "2012"
    "RGB*"
    SF-IMAGE        "image"      0
    SF-DRAWABLE     "drawable"   0
    SF-OPTION       "Background Type" '("Cold Sky" "Pattern" "Color" "Gradient" "Transparent")
    SF-PATTERN      "Pattern"                   "Pink Marble"
    SF-COLOR        "Background color"          '(0 0 0)
    SF-GRADIENT     "Background Gradient"       "Abstract 3"
    SF-ADJUSTMENT   "Brush Size"                '(40 12 100 1 5 0 0)
    SF-ADJUSTMENT   "Brush Spacing"             '(100 1 4900 1 5 0 0)
    SF-COLOR        "Frost Color"               '(255 255 255)
    SF-ADJUSTMENT   "Frost-Amount"              '(.025 0 .075 .001 .005 3 0)
    SF-ADJUSTMENT   "Frost-Intensity"           '(.15 0 .30 .01 .05 2 0)
    SF-ADJUSTMENT   "Frost-Length(pixels)"      '(20 1 100 1 5 0 0)
    SF-ADJUSTMENT   "Frost-Density"             '(1 0 1 .01 .1 2 0)
    SF-TOGGLE       "Keep the Original Image"   TRUE
    SF-TOGGLE       "Keep selection"            TRUE
    SF-TOGGLE       "Keep the Layers"           TRUE
)

(script-fu-menu-register "script-fu-frost-crystals-alpha" "<Toolbox>/Script-Fu/Alpha-to-Logo")
 
(define (fc-frosty-sky image drawable smalls mediums bigs bglum bgcon conserve)
        
        (gimp-image-undo-group-start image)
        (gimp-context-push); Save the foreground & background colors
        
  (let* (            
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (layer-one 0)
            (layer-two)
            (ii 0)
            (ns 0)
            (xs 0)
            (ys 0)
            (nm 0)
            (xm 0)
            (ym 0)
            (nb 0)
            (xb 0)
            (yb 0)
            (lum 0)
            (pixw (cons-array 3 'byte))
        )
        
        (srand (realtime))
        (aset pixw 0 255)
        (aset pixw 1 255)
        (aset pixw 2 255)
        
        
        (set! layer-one (car (gimp-layer-new image width height RGB-IMAGE "Bottom" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image layer-one 0 -1)
        
        ; Set the fg to white, bg to black
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(0 0 0))
        (gimp-drawable-fill layer-one FILL-BACKGROUND)  ;was 1BG ;0 FG, 1 BG, 2 white
        
        ;(gimp-message "smalls")
        ; generating small stars
        (while (< ns smalls)
            (set! ns (+ ns 1))
            (set! xs (+ 2 (rand (- width 5))))
            (set! ys (+ 2 (rand (- height 5))))
            ;(gimp-message (number->string xs))
            (set! lum (+ (rand 160) 32))
            (aset pixw 0 (+ lum (rand 61)))
            (aset pixw 1 (+ lum (rand 61)))
            (aset pixw 2 (+ lum (rand 61)))
            (gimp-drawable-set-pixel layer-one xs ys 3 pixw)
            
            ; preparing the exit of the loop
            (set! ii (+ ii 1))
            (if (> ii 10000) (set! ns smalls))
            (gimp-progress-update (/ ns smalls))
        ) ; end of loop
        
        ; generating medium stars
        ;(gimp-message "mediums")
        (set! ii 0)
        (while (< nm mediums)
            (set! nm (+ nm 1))
            (set! xm (+ 3 (rand (- width 6))))
            (set! ym (+ 3 (rand (- height 6))))
            ;(gimp-message (number->string xm))
            (set! lum (+ (rand 96) 128))
            (aset pixw 0 (+ lum (rand 31)))
            (aset pixw 1 (+ lum (rand 31)))
            (aset pixw 2 (+ lum (rand 31)))
            (gimp-drawable-set-pixel layer-one xm ym 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xm 1) ym 3 pixw)
            (gimp-drawable-set-pixel layer-one xm (+ ym 1) 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xm 1) (+ ym 1) 3 pixw)
            
            ; preparing the exit of the loop
            (set! ii (+ ii 1))
            (if (> ii 10000) (set! nm mediums))
            (gimp-progress-update (/ nm mediums))
        ) ; end of loop
        
        ; generating big stars
        ;(gimp-message "bigs")
        (set! ii 0)
        (while (< nb bigs)
            (set! nb (+ nb 1))
            (set! xb (+ 6 (rand (- width 12))))
            (set! yb (+ 6 (rand (- height 12))))
            ;(gimp-message (number->string xb))
            (set! lum (+ (rand 64) 160))
            (aset pixw 0 (+ lum (rand 29)))
            (aset pixw 1 (+ lum (rand 29)))
            (aset pixw 2 (+ lum (rand 29)))
            ;(gimp-message "bigsets")
            (gimp-drawable-set-pixel layer-one xb yb 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 1) yb 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 2) yb 3 pixw)
            (gimp-drawable-set-pixel layer-one xb (+ yb 1) 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 1) (+ yb 1) 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 2) (+ yb 1) 3 pixw)
            (gimp-drawable-set-pixel layer-one xb (+ yb 2) 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 1) (+ yb 2) 3 pixw)
            (gimp-drawable-set-pixel layer-one (+ xb 2) (+ yb 2) 3 pixw)
            
            ; preparing the exit of the loop
            (set! ii (+ ii 1))
            (if (> ii 10000) (set! nb bigs))
            (gimp-progress-update (/ nb bigs))
        ) ; end of loop
        ;(gimp-message "line679")
        
        (define layer-two (car (gimp-layer-copy layer-one 1)))
        (gimp-image-insert-layer image layer-two 0 -1)
        (gimp-image-set-active-layer image layer-two)
        (plug-in-gauss-rle 1 image layer-two 3 1 1)
        (plug-in-normalize 1 image layer-two)
        (gimp-layer-set-mode layer-two LAYER-MODE-SCREEN)
        
        
        (define layer-three (car (gimp-layer-copy layer-one 1)))
        (gimp-image-insert-layer image layer-three 0 -1)
        (gimp-image-set-active-layer image layer-three)
        (plug-in-gauss-rle 1 image layer-three 6 1 1)
        (plug-in-normalize 1 image layer-three)
        (gimp-layer-set-mode layer-three LAYER-MODE-SCREEN)
        
        (define layer-four (car (gimp-layer-copy layer-one 1)))
        (gimp-image-insert-layer image layer-four 0 -1)
        (gimp-image-set-active-layer image layer-four)
        (plug-in-gauss-rle 1 image layer-four 32 1 1)
        (gimp-levels layer-four 0 0 (- 44 bgcon) 1 0 255)
        (plug-in-gauss-rle 1 image layer-four 16 1 1)
        (gimp-colorize layer-four 236 64 bglum)
        (gimp-layer-set-mode layer-four LAYER-MODE-SCREEN)
        
        (gimp-layer-set-name layer-one "Stars")
        (gimp-layer-set-name layer-two "Stars contour")
        (gimp-layer-set-name layer-three "Stars halo")
        (gimp-layer-set-name layer-four "Sky bg")
        
        (if (= conserve FALSE)
            (begin
                (set! layer-one (car (gimp-image-merge-down image layer-two EXPAND-AS-NECESSARY)))
                (set! layer-one (car (gimp-image-merge-down image layer-three EXPAND-AS-NECESSARY)))
                (set! layer-one (car (gimp-image-merge-down image layer-four EXPAND-AS-NECESSARY)))
                (set! drawable (car (gimp-image-merge-down image layer-one EXPAND-AS-NECESSARY)))
            )
        )
        
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop); Restore the old foreground & background colors
        
        
        
  )
)

;end 