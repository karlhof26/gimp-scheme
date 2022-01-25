; Auto Hatch Layers
; creates a new layers of automatic hatching by reducing image down to a certain number of colors
; and then calling HatchSelectionLayer.scm found here: http://gimpchat.com/viewtopic.php?f=9&t=12536
; author: Tin Tran 
; date: 2015
;----------------------------------- sub function


(define (script-fu-auto-hatch-layers-sub-function image layer 
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
                (set! percent 0.01)
            )
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
                (gimp-image-select-rectangle hatch-image CHANNEL-OP-REPLACE 0 0 
                                                                                height linewidth)
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
																(gimp-image-select-ellipse hatch-image CHANNEL-OP-REPLACE 	(- (/ linewidth 2) (/ small-linewidth 2))
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
		  ) ;end of if	
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
;------------------------- END OF SUB FUNCTION
(define (script-fu-auto-hatch image layer 
                hatches
                ;hatch-option
                _lines
                _squares
                _circles
                _triangles
                _zigzags
                _fans
                _rings
                _hollowsquares
                _crosses
                _Ts
                is-components
                start-angle
                rotate-increment
                height
        )
    
    (let* (
            (color-map 0)
            (colors 0)
            (red 0)
            (green 0)
            (blue 0)
            (y 0)
            (value 0.0)
            (new-red 0)
            (new-green 0)
            (new-blue 0)
            (rotate-angle 0)
            (do-option)
            (bg-red)
            (bg-green)
            (bg-blue)
            (bg-value)
            (bg-represented)
            (bg-leftover)
            (max-channel)
            (option (cons-array 255 'byte))
            (option-size 0)
          )
        (gimp-image-undo-disable image); DN = NO UNDO
        ;(gimp-image-undo-group-start image)                   ;undo-group in one step
        ;convert to indexed
        
        ;puts all the options into the option array
        (if (= _lines TRUE)
            (begin
                (aset option option-size 0)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _squares TRUE)
            (begin
                (aset option option-size 1)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _circles TRUE)
            (begin
                    (aset option option-size 2)
                    (set! option-size (+ option-size 1))
                    
            )
            (begin)
        )
        (if (= _triangles TRUE)
            (begin
                    (aset option option-size 3)
                    (set! option-size (+ option-size 1))
                    
            )
            (begin)
        )
        (if (= _zigzags TRUE)
            (begin
                (aset option option-size 4)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _fans TRUE)
            (begin
                (aset option option-size 5)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _rings TRUE)
            (begin
                (aset option option-size 6)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _hollowsquares TRUE)
            (begin
                (aset option option-size 7)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _crosses TRUE)
            (begin
                (aset option option-size 8)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        (if (= _Ts TRUE)
            (begin
                (aset option option-size 9)
                (set! option-size (+ option-size 1))
                
            )
            (begin)
        )
        ; if none is set we'll default to lines
        (if (= option-size 0)
            (begin
                (aset option option-size 0)
                (set! option-size (+ option-size 1))
            )
            (begin)
        )
        
        
        
        
        
        (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE hatches FALSE FALSE "unused palette name")
        ;grabs color map
        (gimp-context-set-sample-threshold 0)
        (set! colors (vector->list (cadr (gimp-image-get-colormap image))))
        
        (gimp-image-convert-rgb image) ;converts it to rgb before we call hatch loop
        
        (set! rotate-angle start-angle)
        
        (set! y hatches) ;loop hatches number of times
        (while (> y 0)
            ;do work here
            (set! red (car colors))
            (set! green (cadr colors))
            (set! blue (caddr colors))
            ;select each color
            (gimp-image-select-color image CHANNEL-OP-REPLACE layer (list red green blue))
            ;calculate values to use for hatch
            (set! value (*(/ (max red green blue) 255.0) 100))
            (if (> value 0) ;if value is non-zero we can calculate new colors
                (begin
                    (set! new-red (* (/ red value) 100))
                    (set! new-green (* (/ green value) 100))
                    (set! new-blue (* (/ blue value) 100))
                )
                (begin ;else
                    (set! new-red 0)
                    (set! new-green 0)
                    (set! new-blue 0)
                ) 
            )
            ;default bg to black and enter calculation phase
            (set! bg-red 0)
            (set! bg-green 0)
            (set! bg-blue 0)
            (if (= is-components TRUE)
                (begin ;recalculate colors and background color so that we have components
                    (set! max-channel (max red green blue)) ;decide which channel to break out based on highest channel
                    (if (= max-channel red)
                        (begin ;do red
                            ;trying to break out red -------------------------------
                            (set! value (* (/ (max green blue) 255.0) 100))
                            (if (> value 0) ;if value is non-zero we can calculate new colors
								 (begin
									(set! new-green (* (/ green value) 100))
									(set! new-blue (* (/ blue value) 100))
								 )
								 (begin ;else
									(set! new-green 0)
									(set! new-blue 0)
								 ) 
							)
							(set! bg-value (- 100 value)) ;bg-value is percentage to be represented by background
							(if (= bg-value 0)
							    (begin (set! bg-value 0.01))
								(begin)
							)
							
							(set! bg-red (* (/ red bg-value) 100))
							
							(if (> bg-red 255)
								(begin
									(set! bg-red 255)
									(set! bg-represented (/ (* 255 bg-value) 100))
									(set! bg-leftover (- red bg-represented))
									(set! new-red (* (/ bg-leftover value) 100))
								)
								(begin
									;bg-red can contain all reds needed so just set new-red to zero
									(set! new-red 0)
								)
							)
							
						;end of breaking out red channel -------------------------------------
						
						)
					    (begin ;else
							(if (= max-channel green)
								(begin ;break out green
								;trying to break out green -------------------------------
									(set! value (*(/ (max red blue) 255.0) 100))
									(if (> value 0) ;if value is non-zero we can calculate new colors
										 (begin
											(set! new-red (* (/ red value) 100))
											(set! new-blue (* (/ blue value) 100))
										 )
										 (begin ;else
											(set! new-red 0)
											(set! new-blue 0)
										 ) 
									)
									(set! bg-value (- 100 value)) ;bg-value is percentage to be represented by background
									(if (= bg-value 0)
										(begin (set! bg-value 0.01))
										(begin)
									)
									(set! bg-green (* (/ green bg-value) 100))
									(if (> bg-green 255)
										(begin
											(set! bg-green 255)
											(set! bg-represented (/ (* 255 bg-value) 100))
											(set! bg-leftover (- green bg-represented))
											(set! new-green (* (/ bg-leftover value) 100))
										)
										(begin
											;bg-green can contain all greens needed so just set new-green to zero
											(set! new-green 0)
										)
									)
								;end of breaking out green channel -------------------------------------
								
								)
						        (begin ;else break out blue
								;trying to break out blue -------------------------------
									(set! value (* (/ (max green red) 255.0) 100))
									(if (> value 0) ;if value is non-zero we can calculate new colors
										 (begin
											(set! new-green (* (/ green value) 100))
											(set! new-red (* (/ red value) 100))
										 )
										 (begin ;else
											(set! new-green 0)
											(set! new-red 0)
										 ) 
									)
									(set! bg-value (- 100 value)) ;bg-value is percentage to be represented by background
									(if (= bg-value 0)
										(begin (set! bg-value 0.01))
										(begin)
									)
							(set! bg-blue (* (/ blue bg-value) 100))
									(if (> bg-blue 255)
										(begin
											(set! bg-blue 255)
											(set! bg-represented (/ (* 255 bg-value) 100))
											(set! bg-leftover (- blue bg-represented))
											(set! new-blue (* (/ bg-leftover value) 100))
										)
										(begin
											;bg-blue can contain all blues needed so just set new-blue to zero
											(set! new-blue 0)
										)
									)
						;end of breaking out blue channel -------------------------------------
								
								)
						
						    )
						)				
					)
				    
					
				)
				(begin ;else just hatch over black
					
				)
			 )
			 ;if value is zero,we get no hatch pattern from breaking components,it's boring
             ; so we'll just try to hatch over black at least we'll get pattern
			 (if (= value 0)
			 
				 (begin
				 ;calculate values to use for hatch
				  (set! value (*(/ (max red green blue) 255.0) 100))
				  (if (> value 0) ;if value is non-zero we can calculate new colors
					 (begin
						(set! new-red (* (/ red value) 100))
						(set! new-green (* (/ green value) 100))
						(set! new-blue (* (/ blue value) 100))
					 )
					 (begin ;else
						(set! new-red 0)
						(set! new-green 0)
						(set! new-blue 0)
					 ) 
				  )
				  ;default bg to black and enter calculation phase
				  (set! bg-red 0)
				  (set! bg-green 0)
				  (set! bg-blue 0)
				 )
				 (begin
				 )
			 )
			 
			 
             ;(if (= hatch-option 8)
			;	(begin
			;	   (set! do-option (remainder y 8))
				;)
				;(begin ;else
				;   (set! do-option hatch-option)
				;)
			; )
			;set do-option based on selected options
			(set! do-option (aref option (remainder y option-size)))
			
			 (script-fu-auto-hatch-layers-sub-function 
			                                 image
											 layer
											 (list new-red new-green new-blue)
											 (list bg-red bg-green bg-blue)
											 do-option
											 rotate-angle
											 value
											 height)
			  
			 (set! rotate-angle (+ rotate-angle rotate-increment))
			  (if (> y 1) ;if y is still valid we set colors to the next colors
			     (begin
                    (set! colors (cdddr colors))
				 )
				 (begin ;else
				 )
			  )
			  
              ;loop control
			  (set! y (- y 1))
		  );end of while 
          (gimp-selection-none image)	
		   (gimp-image-undo-enable image) ;DN = NO UNDO
		   ;(gimp-image-undo-group-end image)                     ;undo group in one step
	       (gimp-displays-flush)
	    )
	
	
    
) ;end of define

(script-fu-register
    "script-fu-auto-hatch"                  ;function name
    "<Toolbox>/Script-Fu/Create New/Hatch Auto Hatch Layers..."                                         ;menu register
    "Creates multiple layers of hatches automatically. \nfile: HatchSelection_AutoHatchLayers.scm"      ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2015"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image" 0                   
    SF-DRAWABLE   "Layer" 0
    SF-ADJUSTMENT "Number of hatches" '(5 2 255 1 10 0 0)
    ;SF-OPTION     "Hatch with" '("Lines" "Squares" "Circles" "Triangles" "Zigzags" "Fans" "Rings" "Hollow Squares" "All")
    SF-TOGGLE     "Hatch with Lines" TRUE
    SF-TOGGLE     "Hatch with Squares" FALSE
    SF-TOGGLE     "Hatch with Circles" FALSE
    SF-TOGGLE     "Hatch with Triangles" FALSE
    SF-TOGGLE     "Hatch with Zigzags" FALSE
    SF-TOGGLE     "Hatch with Fans" FALSE
    SF-TOGGLE     "Hatch with Rings" FALSE
    SF-TOGGLE     "Hatch with Hollow Squares" FALSE
    SF-TOGGLE     "Hatch with Crosses" FALSE
    SF-TOGGLE     "Hatch with T's" FALSE
    SF-TOGGLE     "Break into color components instead of hatching over black" FALSE
    SF-ADJUSTMENT "Hatch Start Angle" '(45 -360 360 1 10 0 0)
    SF-ADJUSTMENT "Hatch Increment Angle For Each Hatch" '(0 -360 360 1 10 0 0)
    SF-ADJUSTMENT "Hatch Grid Width and Height" '(10 5 100 1 10 0 0)
)

;end of script