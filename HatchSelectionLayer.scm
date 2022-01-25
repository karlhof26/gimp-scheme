; Hatch Selection Layer
; creates a new layer with a hatch pattern of current selection
; author: Tin Tran 
; date: 2015

(define (script-fu-hatch-selection-layer image layer 
              color
              bg-color
              option
              rotate
              percent
              height
         )
  (let* (
            (dummy-variable-to-make-let-happy 0)
            (image-width)
            (image-height)
            (new-width)
            (new-height)
	  (new-layer)
	  (hatch-image)
	  (hatch-layer)
	  (new-vectors)
	  (p1-x)
	  (p1-y)
	  (p2-x)
	  (p2-y)
	  (p-ar (cons-array 12 'double))
	  (linewidth 1.0)
	  (small-linewidth)
	  (brush-name)
	  (rotated)
	  (selection)
	  (temp)
	  ) ;end of variable declaration
	  
        (gimp-context-push)
        (gimp-image-undo-group-start image)                   ;undo-group in one step
        
        
        (set! image-width (car (gimp-image-width image)))
        (set! image-height (car (gimp-image-height image)))
        (set! new-width (* image-width 2))
        (set! new-height (* image-height 2))
        ;START =======================================================
        
        
        ;if percent > 50 we make percent 100-percent and swap foreground and background color
        ;this is to avoid having a circle having to have a diameter greater than the width/height of grid
        ;but it should work for lines and squares as well
        (if (> percent 50)
            (begin
                (set! percent (- 100 percent))
                (set! temp color)
                (set! color bg-color)
                (set! bg-color temp)
            )
            (begin
            )
        )
        (if (= percent 0) ;there's a bug on zero percent for some reason
            (begin
                (set! percent 0.01))
            (begin)
        )
        
        
        
        ;resize canvas
        (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0))  ;*JT*
        (gimp-image-resize image new-width new-height (/ image-width 2) (/ image-height 2))
        ;create new layer
        (set! new-layer (car (gimp-layer-new image new-width new-height
                               RGBA-IMAGE "Hatch Selection" 100 LAYER-MODE-NORMAL)))  ;creates layer
        ;insert above current layer
        ;(gimp-image-insert-layer image new-layer 0 (car (gimp-image-get-item-position image layer)))	
        (gimp-image-insert-layer image new-layer 0 0)	 	  
        ;set that layer to be active layer
        (gimp-image-set-active-layer image new-layer)
        ;converts selection to path
        ;(plug-in-sel2path 1 image layer)  
        ;*********************************
        (set! selection (car (gimp-selection-save image)))
        
        
        ;creates new image and layer to draw hatch on and display it
        (set! height (* height 10));make image 16 times larger
        (set! hatch-image (car (gimp-image-new height height RGB)))
        (set! hatch-layer (car (gimp-layer-new hatch-image height height
                                RGBA-IMAGE "Hatch" 100 LAYER-MODE-NORMAL)))  
        (gimp-image-insert-layer hatch-image hatch-layer 0 0)
        (gimp-image-set-active-layer hatch-image hatch-layer)	  
        ;(gimp-display-new hatch-image)
        
        ;(if (= is_transparent-bg TRUE)
            ;(begin
                ;do nothing to keep background transparent
            ;)
          ;(begin  ; This is the optional 'else' case i.e. code to do if not selected
            ;color background with given bg-color because transparent-bg is not checked
            (gimp-selection-all hatch-image)
            (gimp-context-set-foreground bg-color)
            (gimp-edit-fill hatch-layer FILL-FOREGROUND)
          ;)  
        ; ) ;end of if
        (if (= option 0) ;lines
            (begin
                (set! linewidth (* (/ percent 100.0) height))
                (if (< linewidth 1)
                    (begin
                        (set! linewidth 1)
                    )
                    (begin)
                )
                (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 height linewidth)
                (gimp-context-set-foreground color)
                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
            ) ;end of is_rectangle FALSE
            (begin  
                (if (= option 1) ;option 1 squares
                    (begin 
                        (set! linewidth (sqrt (* (* (/ percent 100.0) height) height)))
                        (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 linewidth linewidth)
                        (gimp-context-set-foreground color)
                        (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                    )
                    (begin ;option 2 circle/dots
                        (if (= option 2)
                            (begin
                                ;calculate linewidth as diameter or dot
                                (set! linewidth (* (sqrt (/ (* (* (/ percent 100.0) height) height) 3.1415)) 2))
                                (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                (gimp-image-select-ellipse hatch-image CHANNEL-OP-REPLACE 0 0 linewidth linewidth)
                                (gimp-context-set-foreground color)
                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                            )
                            (begin 
                                (if (= option 3);option 3 triangles
                                    (begin
                                        (set! linewidth (sqrt (*(* (* (/ percent 100.0) height) height) 2)))
                                        (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                        (gimp-image-select-polygon hatch-image CHANNEL-OP-REPLACE
                                                                                6
                                                                                (list->vector
                                                                                    (list 0 0 linewidth 0 0 linewidth)
                                                                                )
                                        )
                                        (gimp-context-set-foreground color)
                                        (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                    )
                                    (begin 
                                        (if (= option 4) ;option 4 zigzags
                                            (begin
                                                (set! linewidth (* (* (/ percent 100.0) height)))
                                                (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                (gimp-image-select-polygon hatch-image CHANNEL-OP-REPLACE
                                                                                    12
                                                                                    (list->vector
                                                                                        (list 
                                                                                            0 0 
                                                                                            linewidth 0 
                                                                                            height (/ height 2)
                                                                                            linewidth height 
                                                                                            0 height 
                                                                                            (- height linewidth) (/ height 2)
                                                                                        )
                                                                                    )
                                                )
                                                (gimp-context-set-foreground color)
                                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                            )
                                            (begin
                                                (if (= option 5);option 5 fans
                                                    (begin
                                                        (set! linewidth (/ (* (* (/ percent 100.0) height)) 2))
                                                        (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 
                                                                                                                    (/ height 2) linewidth)
                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-ADD (- height linewidth) 0
                                                                                                                    linewidth (/ height 2))
                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-ADD (/ height 2) (- height linewidth)
                                                                                                                    (/ height 2) linewidth)
                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-ADD 0 (/ height 2)
                                                                                                                    linewidth (/ height 2))
                                                        (gimp-context-set-foreground color)
                                                        (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                    )
                                                    (begin
                                                        (if (= option 6) ; option 6 rings
                                                            (begin
                                                                (set! linewidth (* (sqrt (/ (* (* (/ (* percent 1.5) 100.0) height) height) 3.1415)) 2))
                                                                (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                                (gimp-image-select-ellipse hatch-image CHANNEL-OP-REPLACE 0 0 linewidth linewidth)
                                                                (gimp-context-set-foreground color)
                                                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                                (set! small-linewidth (* (sqrt (/ (* (* (/ (* percent 0.5) 100.0) height) height) 3.1415)) 2))
                                                                (if (< small-linewidth 1)(begin(set! small-linewidth 1))(begin))
                                                                (gimp-image-select-ellipse hatch-image CHANNEL-OP-REPLACE  (- (/ linewidth 2) (/ small-linewidth 2))
                                                                                                                        (- (/ linewidth 2) (/ small-linewidth 2))
                                                                                                                        small-linewidth small-linewidth)
                                                                (gimp-context-set-foreground bg-color)
                                                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                            )
                                                            
                                                            (begin
                                                                (if (= option 7)
                                                                    (begin  ;option 7 hollow squares
                                                                        (set! linewidth (sqrt (* (* (/ (* percent 1.5) 100.0) height) height)))
                                                                        (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 linewidth linewidth)
                                                                        (gimp-context-set-foreground color)
                                                                        (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                                        (set! small-linewidth (sqrt (* (* (/ (* percent 0.5) 100.0) height) height)))
                                                                        (if (< small-linewidth 1)(begin(set! small-linewidth 1))(begin))
                                                                        (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE (- (/ linewidth 2) (/ small-linewidth 2))
                                                                                                                                (- (/ linewidth 2) (/ small-linewidth 2))
                                                                                                                                small-linewidth small-linewidth)
                                                                        (gimp-context-set-foreground bg-color)
                                                                        (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                                    )
                                                                    (begin
                                                                        (if (= option 8)
                                                                            (begin
                                                                                
                                                                                ;option 8 crosses
                                                                                (set! linewidth (sqrt (/(* (* (/ percent 100.0) height) height) 5)))
                                                                                (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                                                (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE linewidth 0 linewidth (* linewidth 3))
                                                                                (gimp-image-select-rectangle hatch-image CHANNEL-OP-ADD 0 linewidth (* linewidth 3) linewidth)
                                                                                (gimp-context-set-foreground color)
                                                                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                                            )
                                                                            (begin
                                                                                ;option 9 T's
                                                                                (set! linewidth (sqrt (/(* (* (/ percent 100.0) height) height) 3)))
                                                                                (if (< linewidth 1)(begin(set! linewidth 1))(begin))
                                                                                (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 (* linewidth 2) linewidth)
                                                                                (gimp-image-select-rectangle hatch-image CHANNEL-OP-ADD (/ linewidth 2) 0 linewidth (* linewidth 2))
                                                                                (gimp-context-set-foreground color)
                                                                                (gimp-edit-fill hatch-layer FILL-FOREGROUND)
                                                                            
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                );
            ) ;end of option 1 or 2
        ) 
        (set! height (/ height 10)) ;resize image to original height
        (gimp-image-scale hatch-image height height)	  
        (gimp-selection-all hatch-image) 
        ;copy this hatch layer
        (gimp-edit-copy hatch-layer)
        (gimp-image-delete hatch-image)
        
        ;pattern fill and rotate 
        (gimp-selection-all image)
        (gimp-edit-fill new-layer FILL-PATTERN)
        (set! rotated(car(gimp-item-transform-rotate new-layer (* (/ rotate 180) 3.1415) TRUE 0 0)))
        (gimp-floating-sel-anchor rotated)
        
        ;selects the original selection, cut its inversion
        (gimp-image-select-item image CHANNEL-OP-REPLACE selection)
        (gimp-selection-invert image)
        (gimp-edit-cut new-layer)
        (gimp-selection-invert image)
        
        
        ;removes the channel after we're done
        (gimp-image-remove-channel image selection)
        
        
        ;set visible to true
        (gimp-item-set-visible new-layer TRUE)
        
        (gimp-image-resize image image-width image-height (- 0 (/ image-width 2)) (- 0 (/ image-height 2)))
        (gimp-layer-resize-to-image-size new-layer)
        
        ;DONE ========================================================
        
        
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        (gimp-displays-flush)
  ) ;end of let*
) ;end of define

(script-fu-register
    "script-fu-hatch-selection-layer"         ;function name
    "<Toolbox>/Script-Fu/Create New/Hatch Selection Layer..."    ;menu register
    "Creates A hatched layer of the current selection. \nfile: HatchSelectionLayer.scm"       ;description
    "Tin Tran"                          ;author name
    "copyright info and description"         ;copyright info or description
    "2015"                          ;date
    "RGB*, GRAY*"                        ;mode
    SF-IMAGE      "Image" 0                   
    SF-DRAWABLE   "Layer" 0
    SF-COLOR      "Hatch Color 1" '(255 255 255)
    SF-COLOR      "Hatch Color 2" '(0 0 0)
    SF-OPTION     "Hatch with" '("Lines" "Squares" "Circles" "Triangles" "Zigzags" "Fans" "Rings" "Hollow Squares" "Crosses" "T's")
    SF-ADJUSTMENT "Hatch Rotate Angle" '(45 -360 360 1 10 0 0)
    SF-ADJUSTMENT "Hatch Width (Percentage)" '(50 0 100 1 10 0 0)
    SF-ADJUSTMENT "Hatch Grid Width and Height" '(10 5 100 1 10 0 0)
)

;end of script