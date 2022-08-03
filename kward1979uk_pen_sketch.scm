
(define (pen-kward inimage indraw blur)
    
    (let* (
            (theImage 0)
            (theDraw 0)
            (flush 0)
            (flush-copy 0)
            (second-copy 0)
            (width 0)
            (height 0)
            (white-layer 0)
            (flat-grey 0)
            (outline-copy 0)
            (final-outline 0)
            (copy-count 0)
            (copy 0)
          )
        (set! theImage inimage)
        (set! theDraw indraw)
        (gimp-image-undo-group-start theImage)
        (set! flush (car(gimp-image-flatten theImage)))
        (set! flush-copy (car (gimp-layer-copy flush 1)))
        (gimp-image-insert-layer theImage flush-copy 0 -1)
        (set! second-copy (car (gimp-layer-copy flush 1)))
        (gimp-image-insert-layer theImage second-copy 0 -1)
        (gimp-layer-set-mode second-copy 20)
        (plug-in-gauss 1 theImage second-copy blur blur 0)
        (set! width (car (gimp-drawable-width flush)))
        (set! height (car (gimp-drawable-height flush)))
        (set! white-layer (car (gimp-layer-new theImage width height 1 "white" 100 13)))
        (gimp-drawable-fill white-layer 2)
        (gimp-image-insert-layer theImage white-layer 0 -1)
        (gimp-drawable-set-visible flush 0)
        (set! flat-grey (car(gimp-image-merge-visible-layers theImage 0)))
        
        ;(gimp-levels flat-grey 0 0 128 1 0 255)
        (gimp-drawable-levels flat-grey HISTOGRAM-VALUE 0.0 0.5 TRUE 1.0 0.0 1.0 FALSE)
        
        (set! outline-copy (car(gimp-layer-copy flat-grey 1)))
        (gimp-image-insert-layer theImage outline-copy 0 -1)
        (plug-in-colortoalpha 1 theImage outline-copy '(255 255 255))
        (gimp-layer-set-mode outline-copy 17)
        (set! final-outline (car(gimp-image-merge-visible-layers theImage 0)))
        
        (set! copy-count 1)
        (while (<= copy-count 4 )
            (set! copy (car(gimp-layer-copy final-outline 1)))
            (gimp-image-insert-layer theImage copy 0 -1)
            (gimp-layer-set-mode copy 17)
            (set! copy-count (+ copy-count 1))
        )
        (gimp-layer-set-mode final-outline 17)
        
        (gimp-drawable-set-visible flush 1)
        (gimp-layer-add-alpha flush)
        (gimp-layer-set-opacity flush 70)
        (set! white-layer (car (gimp-layer-new theImage width height 1 "white" 100 0)))
        (gimp-drawable-fill white-layer 2)
        (gimp-image-insert-layer theImage white-layer 0 -1)
        (gimp-image-lower-layer-to-bottom theImage white-layer)
        (gimp-image-flatten theImage)
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    )
) ; end let
 
(script-fu-register     "pen-kward"
    "<Image>/Script-Fu/Kward1979uk/pen-drawn..."
    "This filter changes any image into a image that appears to have been drawn woth ink. \nfile:kward1979uk_pen_sketch.scm"
    "Karl Ward"
    "Karl Ward"
    "OCT 2006"
    ""
    SF-IMAGE      "SF-IMAGE" 0
    SF-DRAWABLE   "SF-DRAWABLE" 0
    SF-ADJUSTMENT "Line thickess" '(25 1 100 1 5 0 0)

)
				
; end of script