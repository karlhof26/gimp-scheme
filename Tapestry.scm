;
; -*-scheme-*- 
;
; Script-Fu Tapestry v3.0
; A Gimp script which breaks down an image into cross stitch colours.
;
; Janet Fulcher and Adrian Spriddell - February 2011
; An implementation of Saul Goode's indexed-decompose.scm 
;
; This software is in the public domain. 
;
; I have released this script into the wild in the hope that it may 
; prove useful. It comes with absolutely no warranty whatsoever, 
; without even the implied warranty of merchantability or fitness 
; for any particular purpose. See the GNU General Public License for 
; details. You may redistribute and/or modify this script or extract 
; segments from it without prior consent.
;
; Tested with Gimp-2.6. GIMP uses the tiny-scheme interpreter. 
; The Gimp-specific functions are made available to scripts via the
; PDB (Procedural Data Base). You can look up functions in the GIMP
; with the DB-Browser, available from the Script-Fu menu.
;
; Located in the Gimp <IMAGE><FILTERS><ARTISTIC> menu.
;
; "Tapestry v3.0" makes a fresh image to work on with the registration and 
; pattern grids in separate layers so that they may be turned off and on as 
; required. Floss colours are optionally converted to individual layers.
;
; Depending on your source image, it may be a good idea to crank up the 
; brightness and contrast levels before you start and to scale to the 
; size of your canvas. Count the number of stitches to the inch your  
; chosen canvas provides - say 14 stitches per inch. There are 5 pixels  
; to a stitch so 70 pixels per inch. For a canvas 24 by 32 inches, 
; that's 24x70 by 32x70 = an image size of 1680x2240. 
;
; Be sure to save your final image in xcf, bmp or other uncompressed 
; format. Save only in xcf format to preserve layers.
; DO NOT SAVE AS A JPEG - the colours will be much too blurred.
;
; Change-log:
; v2.0 Added registration and guide grids
; v2.1 Added option to convert colours to layers
; v2.2 Work-around for bug in colour picker
; v3.0 Allow user to directly set the final number of colours
; v3.0 No longer needs to have "Pattern5x5" installed
; v3.0 Now works with any source image
;
; To-Do list: 
; Add Anchor and DMC floss colour tags to each layer
; Option to add symbols to each colour - easier to see in hard copy


(define (script-fu-tapestry image drawable numcolors Colour2layers?)
    
    (let* ( 
                
                (x 0)
                (y 0)
                (width 0)
                (height 0)
                (line 0)
                (extent 0)
                (pattern 0)
                (grid 0)
                (nextlayer 0)
                (background 0)
                (new-layer -1)
                (layer -1)
                (index 0)
                (map 0)
                (red 0)
                (green 0)
                (blue 0)
                (scale 128)
                
           )
        
            (gimp-image-undo-disable image)
            ; Shove the current environment on the stack for retrieval later            
            
            (gimp-context-push)
            (gimp-context-set-default-colors)
            
            
            ; Make sure there are no active selactions then copy all the
            ; visible layers of the source image so we don't mess it up.
            
            (gimp-selection-none image) 
            (gimp-edit-copy-visible image)
            (set! image (car (gimp-edit-paste-as-new)) )
            (set! drawable (car (gimp-image-get-active-drawable image)) )
            (gimp-display-new image)
            ;;(gimp-image-undo-disable image)
            
            
            ; Ensure the image size variables are in multiples of 10 
            ; so the grid will be overlaid correctly.
            
            (set! height (round (/ (car (gimp-drawable-height drawable)) 10)) )
            (set! width (round (/ (car (gimp-drawable-width drawable)) 10)) ) 
            (set! height (* 10 height))            
            (set! width (* 10 width))
            
            
            ; Scale the new image dimensions if necessary.
            
            (gimp-image-scale image width height)        
            
            
            ; Pixelise to five pixel squares.
            (plug-in-pixelize RUN-NONINTERACTIVE image drawable 5 )
            (set! background (car (gimp-image-get-active-layer image)) )
            (gimp-displays-flush)           
            
            ; Clear selections and prepare to make layers if required
            (when (equal? Colour2layers? TRUE)
                
                (gimp-progress-set-text _"Converting image")
                (gimp-selection-none image)  
                (set! drawable (car (gimp-image-get-active-drawable image)) )
                (set! layer (car (gimp-image-get-active-layer image)) )
                
                ; Check and convert image to indexed if necessary
                
                (if (= (car (gimp-drawable-is-rgb layer)) 0) 
                    (gimp-image-convert-rgb image)
                )
                (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE numcolors 0 TRUE "bloggs")
                
                ; Get the colourmap
                (gimp-message "start color map")
                (set! map (gimp-image-get-colormap image))
                (set! index 0)
                (set! numcolors (/ (car map) 3))
                (set! map (cadr map))
                (set! scale numcolors) ; For progress bar
                (if (> numcolors 0)
                    ; Start making layers, one for each colour
                    (begin
                        
                        (while (> numcolors 0)
                            
                            (set! red   (fmod (aref map index) 256))
                            (set! green (fmod (aref map (+ 1 index)) 256))
                            (set! blue  (fmod (aref map (+ 2 index)) 256))
                            
                            
                            (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
                                 
                            (set! new-layer (car (gimp-layer-copy layer TRUE)) )
                            (gimp-drawable-set-name new-layer
                                (string-append "RGB=[" (number->string red) ", " 
                                (number->string green) ", " (number->string blue) "]"))
                            
                            (gimp-image-add-layer image new-layer -1)
                            (gimp-selection-invert image)
                            (gimp-edit-clear new-layer)
                            (set! numcolors (- numcolors 1))
                            (set! index (+ index 3))
                            
                            ; Update progress bar
                            (gimp-progress-set-text "Copying colours to layers") 
                            (gimp-progress-update (/ (- scale numcolors) scale))  
                            
                        )     ; End while
                    )     ; End Begin
                )     ; End numcolors
                (gimp-message "end color map")
                
                ; Convert image to (back) to RGB
                
                (gimp-selection-none image)
                (gimp-progress-set-text "Converting image")
                (gimp-image-convert-rgb image)
                (gimp-drawable-fill layer FILL-BACKGROUND)
                
            )     ; End of conditional Colour2layers block
            
            
            ; Make a new layer "pattern" with an alpha channel.
            (set! pattern (car (gimp-layer-new image width height 
                RGBA-IMAGE "Pattern Layer" 100 LAYER-MODE-NORMAL)) )
            (gimp-image-insert-layer image pattern 0 -1)
            (set! drawable (car (gimp-image-get-active-drawable image)) )
            (gimp-drawable-fill pattern FILL-TRANSPARENT)
            
            (gimp-message "overlay grating")
            ; Overlay a grating of 5x5 squares.
            (plug-in-grid RUN-NONINTERACTIVE image drawable 
                1 5 0 '(0 0 0) 255 
                1 5 0 '(0 0 0) 255 
                1 5 0 '(0 0 0) 255)
            
            ; Make another new layer "grid" with an alpha channel.
            (set! grid (car (gimp-layer-new  image  width  height   
                RGBA-IMAGE "Grid Layer" 100 LAYER-MODE-NORMAL)) )    
            (gimp-image-insert-layer image grid 0 -1)
            (set! drawable (car (gimp-image-get-active-drawable image)) )
            (gimp-drawable-fill grid FILL-TRANSPARENT)
            (gimp-displays-flush)
            (gimp-message "grid done") 
            
            ; Set the foreground and background colours
            (gimp-context-set-foreground '(255 255 0))
            (gimp-context-set-background '(255 50 0))
            
            ; Draw a grid of red horizontal reference lines 
            ; on the image one pixel wide every 10 squares.
            ; Starting 50 pixels offset centre and progressing downward.
            
            (gimp-message "draw red reference lines start")
            
            (set! line (+ (* height 0.5) 50))
            (set! extent height)
            (while (< line extent) 
                ;(gimp-rect-select image 0 line width 1 REPLACE 0 0)
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 line width 1 )
                (gimp-edit-fill drawable FILL-BACKGROUND) 
                (set! line (+ line 50))
            )
            
            ; Then start at an offset centre and progress upward.
            
            (set! line (- (* height 0.5) 50))
            (set! extent 0)
            (while (< extent line) 
                ;;(gimp-rect-select image 0 line width 1 REPLACE 0 0)
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 line width 1 )
                (gimp-edit-fill drawable FILL-BACKGROUND)   ;BG-IMAGE-FILL
                (set! line (- line 50))
            )
            
            ; Now draw a similar grid of vertical lines.
            (set! line (+ (* width 0.5) 50))
            (set! extent width)
            (while (< line extent) 
                ;;(gimp-rect-select image line 0 1 height REPLACE 0 0)
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE line 0 1 height )
                (gimp-edit-fill drawable FILL-BACKGROUND) 
                (set! line (+ line 50))
            )  
            
            (set! line (- (* width 0.5) 50))
            (set! extent 0)
            (while (< extent line) 
                ;;(gimp-rect-select image line 0 1 height REPLACE 0 0)
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE line 0 1 height )
                (gimp-edit-fill drawable FILL-BACKGROUND) 
                (set! line (- line 50))
            )
            
            ; Select and fill a yellow vertical line 1 pixel
            ; wide centered on the currently active drawable.
            (gimp-message "select and fill a yellow line")
            (gimp-image-select-rectangle image CHANNEL-OP-REPLACE
                (+ x (* width 0.5))
                (+ y (* height 0.0))
                (/ width width)
                (* height 1.0)
                )
            (gimp-edit-fill drawable FILL-FOREGROUND)
            
            ; Select and fill a yellow horizontal line 1 pixel
            ; wide centered on the currently active drawable.
            
            (gimp-image-select-rectangle image CHANNEL-OP-REPLACE
                (+ x (* width 0.0))
                (+ y (* height 0.5))
                (* width 1.0)
                (/ height height)
                )
            (gimp-edit-fill drawable FILL-FOREGROUND)
            
            (gimp-message "ready to finish")
            
            ; Do the housekeeping
            (gimp-displays-flush)
            (gimp-selection-none image)
            (gimp-image-undo-enable image)
            ;;(gimp-image-undo-group-end image)
            ;;(gimp-context-pop) 
            (gimp-message "finish OK")
            ;;(gimp-progress-end)
            (gimp-displays-flush)
            
    )    ; End of let block
)    ; End of definition block

; Here endeth the procedures.

; Register the function in the GIMPs PDB.
(script-fu-register "script-fu-tapestry"
            "<Image>/Script-Fu/Artistic/Tapestry..."
            "Breaks down an image into cross-stitch colours."
            "Micomicon"
            "Janet Fulcher & Adrian Spriddell"
            "February 2011"    
            "*"
            SF-IMAGE        "Input Image" 0
            SF-DRAWABLE     "Input Drawable" 0
            SF-ADJUSTMENT   "Number of colors (2-128)" '( 8 2 128 1 5 0 0 )
            SF-TOGGLE       "Make a new layer for each colour." 0
            
)

; end of file