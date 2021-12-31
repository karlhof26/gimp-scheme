; -*-scheme-*-
; Mario Valle 2010.  No copyright.  Public Domain.
; Based on Guillermo Maldonado Rule of Thirds
; Tin Tran 2016. added guides to show image borders.
;                so now we can just move/scale layer and seeing
;                what the future crop area would look like
(define (script-fu-guide-gr image drawable)
    (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
        )
        
        (gimp-image-add-hguide image 0)
        (gimp-image-add-hguide image height)
        (gimp-image-add-vguide image 0)
        (gimp-image-add-vguide image width)	
        (gimp-image-add-hguide image (/ (* height 1000) 1618))
        (gimp-image-add-hguide image (/ (* height 382) 1000))
        (gimp-image-add-vguide image (/ (* width 1000) 1618))
        (gimp-image-add-vguide image (/ (* width 382) 1000))
        
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-guide-gr"
    "New Guides _Golden Ratio Guides"
    "Adds Golden Ratio guides plus guides on edges. \nfile:golden-rule_v1pt1.scm"
    "Mario Valle"
    "Mario Valle, 2010"
    "March 2010"
    ""
    SF-IMAGE      "Input Image"      0
    SF-DRAWABLE   "Input Drawable"   0
)

(script-fu-menu-register "script-fu-guide-gr"
                         "<Toolbox>/Script-Fu/Setup/Guides")
    