; 
; V1.0 - Feb-2011 - First working version! (still learning language) - Brian Hahn
;
; Script to copy, shift and rotate a path
;
; Registers in the Path dialog (right click on a path
;
; A complte re-write of a section from 'shapes0.scm' by Lukas Stahl 
;
; License:
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
;
;
(define (script-fu-path-copy-rotate-shift sfImage sfPath sfOffsetX sfOffsetY 
                          sfAngle sfCenterX sfCenterY sfAutocenter sfTimes)
  (let* (
            (varNewPath 0)
            (index 1)
            (centerX sfCenterX)
            (centerY sfCenterY)
        )
    (if (= sfAutocenter TRUE)   ; use center of image for rotation
        (begin
            (set! centerX (/ (car (gimp-image-width sfImage)) 2))
            (set! centerY (/ (car (gimp-image-height sfImage)) 2))
        )
    )
    
    (gimp-image-undo-group-start sfImage)
    
    (while (<= index sfTimes)
        (set! varNewPath (car(gimp-vectors-copy sfPath)))
        (gimp-image-add-vectors sfImage varNewPath -1)
        (gimp-vectors-stroke-rotate varNewPath TRUE centerX centerY (* index sfAngle))
        (gimp-vectors-stroke-translate varNewPath TRUE (* index sfOffsetX) (* index sfOffsetY))
        (gimp-vectors-set-visible varNewPath TRUE)
        (set! index (+ index 1))
    )
    
    (gimp-image-undo-group-end sfImage)
    (gimp-displays-flush)
  );end let*
)

(script-fu-register "script-fu-path-copy-rotate-shift"
        "<Vectors>/Path-Copy-rotate-shift..."
        "Duplicates a path, rotates and shifts the copies. \nfile:path copy rotate shift.scm"
        "Brian Hahn"
        "Brian Hahn"
        "2011"
        "*"
        SF-IMAGE        "Image"             0
        SF-VECTORS      "Vector Object"     0
        SF-ADJUSTMENT   "Offset X (px)"     '(3 -1000 1000 1 10 0 1)
        SF-ADJUSTMENT   "Offset Y (px)"     '(3 -1000 1000 1 10 0 1)
        SF-ADJUSTMENT   "Rotation Angle"    '(0.5 -180 180 0.1 10 2 0)
        SF-ADJUSTMENT   "Rotation Center X"   '(500 0 5000 1 10 0 1)
        SF-ADJUSTMENT   "Rotation Center Y"   '(500 0 5000 1 10 0 1)
        SF-TOGGLE       "Use image center"    TRUE
        SF-ADJUSTMENT   "Number of copies"    '(20 1 1000 1 10 0 1)
)

;emd of script