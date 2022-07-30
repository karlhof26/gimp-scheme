; create visually same effect as tutorial by wbool63
; found at link: http://gimpchat.com/viewtopic.php?f=23&t=10151
; author: Tin Tran
; date: 2014
(define (script-fu-hatched image layer 
              fill-color
              stroke-color
              stroke-width
              shrink-distance
              hatch-color
              hatch-gradient-angle ;angle -180 angle <=180
              hatch-gradient-width ;gradient width 0 
              shadow-offset-X   ;offset x
              shadow-offset-Y   ;offset y
              shadow-blur-radius ;blur-radius
              shadow-color       ;color
              shadow-opacity     ;opacity
         )
  (let* (
            (dummy-variable-to-make-let-happy 0)
            (get-layers-returned-values)
            (layers-visibility-array)     ;array of bytes to store visibility flags
            (layers-name-vector)          ;vector to store layers' names
            (layers-count)
            (layers-array)
            (get-vectors-returned-values)
            (vectors-count)
            (vectors-array)
            (current-vector)
            (current-layer)
            (current-layer-name)
            (visibility-flag)
            (image-width 1)
            (image-height 1)
            (working-layer-name)
            (brush-name)
            (hatched-layer)
            
            (i 0)        ;loop counter
        ) ;end of variable declaration
        (gimp-image-undo-group-start image)                   ;undo-group in one step
        
        (set! image-width (car (gimp-image-width image)))
        (set! image-height (car (gimp-image-height image)))
        (set! working-layer-name (car (gimp-item-get-name layer)))
        (set! get-layers-returned-values (gimp-image-get-layers image))
        (set! layers-count (car get-layers-returned-values))
        (set! layers-array (cadr get-layers-returned-values))
        ;gets layers visibility and names and save it before hiding them all so that we can set it back later
        (set! layers-visibility-array (cons-array layers-count 'byte))
        (set! layers-name-vector (make-vector layers-count 'string))
        (set! i 0)
        (while (< i layers-count)
            (set! current-layer (aref layers-array i))                ;set the current layer we're looking at
            (vector-set! layers-name-vector i (car (gimp-item-get-name current-layer))) ;saves current the layer's name
            (aset layers-visibility-array i (car (gimp-item-get-visible current-layer))) ;saves the visibility of current layer
            (set! i (+ i 1))
        )
        ;hide all layers
        (set! i 0)
        (while (< i layers-count)
            (gimp-item-set-visible (aref layers-array i) FALSE)
            (set! i (+ i 1))
        )
        
        
        (gimp-item-set-visible layer TRUE)   ;show the active layer selected to perform work on
        
        ;START ----------------------------------------------------------------------
        (gimp-image-select-item image CHANNEL-OP-REPLACE layer)
        (plug-in-sel2path RUN-NONINTERACTIVE image layer)
        
        (set! get-vectors-returned-values (gimp-image-get-vectors image))
        (set! vectors-count (car  get-vectors-returned-values))
        (set! vectors-array (cadr get-vectors-returned-values))
        (set! current-vector (aref vectors-array 0))      ;top path, our own created from selection
        ;(set! current-vector (aref vectors-array (- vectors-count 1))) ;bottom path
        (gimp-image-select-item image CHANNEL-OP-REPLACE current-vector)
        (gimp-context-set-foreground fill-color)
        (gimp-edit-fill layer FILL-FOREGROUND)
        
        ;create brush
        (set! brush-name (car(gimp-brush-new "hatch stroke")))
        (gimp-brush-set-hardness brush-name 1)
        (gimp-brush-set-radius brush-name (/ stroke-width 2))
        (gimp-brush-set-shape brush-name BRUSH-GENERATED-CIRCLE)
        (gimp-context-set-brush brush-name)         ;use created brush
        (gimp-context-set-foreground stroke-color)
        ;(gimp-context-set-brush-size 7) ;doesn't seem to affect our own brushing
        (gimp-edit-stroke-vectors layer current-vector)
        
        
        
        ;creates new layer to draw the hatched pattern
        (gimp-selection-shrink image shrink-distance)
        (set! hatched-layer (car (gimp-layer-new image image-width image-height
                                RGBA-IMAGE "Hatched" 100 LAYER-MODE-NORMAL)))  ;creates layer
        ;(gimp-image-insert-layer image hatched-layer 0 0)   
        (gimp-image-insert-layer image hatched-layer 0 (car (gimp-image-get-item-position image layer)))   
        (gimp-image-set-active-layer image hatched-layer)
        (gimp-edit-fill hatched-layer FILL-WHITE)
        (gimp-context-set-background fill-color)
        (gimp-context-set-foreground hatch-color)
        
        ;(python-layer-fx-gradient-overlay 1 ;run mode
        ;                                image
        ;                                hatched-layer        
        ;                                "FG to BG (Hardedge)" ;gradient
        ;                                GRADIENT-LINEAR ;gradient-type
        ;                                REPEAT-TRIANGULAR ;repeat
        ;                                FALSE            ;reverse
        ;                                100        ;opacity
        ;                                LAYER-MODE-NORMAL ;blend mode
        ;                                0 ;center x
        ;                                0 ;center y
        ;                                hatch-gradient-angle ;angle -180 angle <=180
        ;                                hatch-gradient-width ;gradient width 0 
        ;                                FALSE ;merge with layer
        ;)
        (script-fu-layerfx-gradient-overlay   ;run mode  
                                        image
                                        hatched-layer        
                                        LAYER-MODE-NORMAL-LEGACY ; grad blend mode
                                        LAYER-MODE-NORMAL-LEGACY ; piant mode
                                        "FG to BG (Hardedge)" ;gradient
                                        GRADIENT-LINEAR ;gradient-type
                                        2 ;repeat triangular
                                        FALSE            ;reverse
                                        100        ;opacity
                                        0 ;blend mode (cannot use new layer modes ie >28) must use legacy modes
                                        1 ;center x
                                        1 ;center y
                                        hatch-gradient-angle ;angle -180 angle <=180 ; 135
                                        hatch-gradient-width ;gradient width ; 10 
                                        FALSE ;merge with layer
        )
        
        (gimp-selection-none image)
        ;drop shadow below isn't called with run-mode like documented in procedure browser
        (script-fu-drop-shadow image layer
                                shadow-offset-X   ;offset x
                                shadow-offset-Y   ;offset y
                                shadow-blur-radius ;blur-radius
                                shadow-color       ;color
                                shadow-opacity     ;opacity
                                TRUE ;allow resizing
        )
        ;DONE -----------------------------------------------------------------------
        ;set visibility back to how it was before started running script
        (set! i 0)
        (while (< i layers-count)
            (set! current-layer-name (vector-ref layers-name-vector i)) ;grabs current-layer-name as loop runs through saved vector
            (set! current-layer (car(gimp-image-get-layer-by-name image current-layer-name))) ;sets current-layer by names saved in vectors
            (set! visibility-flag (aref layers-visibility-array i)) ;grabs the saved visibility flag
            (gimp-item-set-visible current-layer visibility-flag)         ;sets visibility to originally saved values
            (set! i (+ i 1))
        )
        
        ; set some brush details so user doesn't get too surprised
        (gimp-brush-delete brush-name)        ;deletes the brush we created to draw dots, so it's not in memory 
        (gimp-context-set-brush "2. Hardness 025") ;sets some other brush so it doesn't use clipboard as brush
        (gimp-context-set-brush-size 20) 
        
        
        (gimp-image-undo-group-end image)                     ;undo group in one step 
        (gimp-displays-flush)
  ) ;end of let*
) ;end of define

(script-fu-register
    "script-fu-hatched"         ;function name
    "<Image>/Script-Fu/Texture/Hatched Stripes"    ;menu register
    "Creates Overlay color effect. Stripes of the fill colour and hatch color. Requires a layer with transparency. \nfile:Hatched Stripes.scm"       ;description
    "Tin Tran"                          ;author name
    "copyright info and description"         ;copyright info or description
    "2014"                          ;date
    "RGB*, GRAY*"                        ;mode
    SF-IMAGE      "Image"           0                   
    SF-DRAWABLE   "Layer"           0
    SF-COLOR      "Fill Color"      '(255 255 255)
    SF-COLOR      "Stroke Color"    '(124 66 62)
    SF-ADJUSTMENT "Stroke Width"    '(4 0 100 1 10 0 0)
    SF-ADJUSTMENT "Shrink Distance(from border to hatch)"   '(5 0 100 1 10 0 0)
    SF-COLOR      "Hatch Color"     '(124 66 62)
    SF-ADJUSTMENT "Hatch Gradient Angle"    '(135 -180 180 1 10 0 0) ;angle -180 angle <=180
    SF-ADJUSTMENT "Hatch Gradient Width"    '(10 2 100 1 10 0 0) ;gradient width 
    SF-ADJUSTMENT "Shadow Offset X"         '(4 0 100 1 10 0 0)
    SF-ADJUSTMENT "Shadow Offset Y"         '(4 0 100 1 10 0 0)
    SF-ADJUSTMENT "Shadow Blur Radius"      '(15 0 100 1 10 0 0)
    SF-COLOR      "Shadow Color"            '(124 66 62)
    SF-ADJUSTMENT "Shadow Opacity"          '(60 0 100 1 10 0 0)
    ;SF-COLOR      "Silhouette Color"   '(24 194 244)
    ;SF-TOGGLE     "Apply Curves"       TRUE
    ;SF-ADJUSTMENT "Colorize Hue(used if Apply Curves)"        '(187 0 360 1 10 0 0)
    ;SF-ADJUSTMENT "Colorize Saturation(used if Apply Curves)" '(90 0 100 1 10 0 0)
    ;SF-ADJUSTMENT "Colorize Lightness(used if Apply Curves)"  '(0 -100 100 1 10 0 0)
    ;SF-ADJUSTMENT "Drop Shadow offset X"  '(10 -4096 4096 1 50 0 0)
    ;SF-ADJUSTMENT "Drop Shadow offset Y"  '(10 -4096 4096 1 50 0 0)
)
