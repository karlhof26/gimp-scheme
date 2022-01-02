; tapered_stroke_along_path.scm 
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.0 (20080926) 

; Description
;
; paints a tapered stroke along the path
;

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (tapered-stroke-along-path-a img inLayer inPath inStartWidth inEndWidth inSpacing inCurve)

  (let*
    (
        (width (car (gimp-image-width img)))
        (height (car (gimp-image-height img)))
        (brushTemp 0)
        (varRadius (/ inStartWidth 2.0))
        (varFirstStroke (aref (cadr (gimp-vectors-get-strokes inPath)) 0))
        (varPathLength (car (gimp-vectors-stroke-get-length inPath varFirstStroke 1)))
        (varCounter 0)
        (*newpoint* (cons-array 2 'double))
        (varTemp 0)
        (varRepaint 0)
        
        (outcome 0)
    )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    
    ;logging
    ;(gimp-message-set-handler ERROR-CONSOLE)
    ;(gimp-message-set-handler CONSOLE)
    ;(gimp-message-set-handler MESSAGE-BOX)
    ;or start GIMP wwith "gimp --console-messages" to spawn a console box
    ;then use this:
    ;(gimp-message "foobar") 
    
    ;testing for functions defined
    ;(if (defined? 'plug-in-shift) (gimp-message "It Exists") (gimp-message "Doesnt Exist"))
    
    ;Uses: 
    ;gimp-vectors-stroke-get-point-at-dist
    ;gimp-vectors-stroke-get-length
    ;
    
    ;Set up Brush   
    ;(set! brushTemp (car (gimp-brush-new "TaperedStrokeBrush")))
    ;(gimp-brush-set-shape brushTemp BRUSH-GENERATED-CIRCLE)
    ;(gimp-brush-set-hardness brushTemp 1)
    ;(gimp-brush-set-radius brushTemp varRadius)
    ;(gimp-brush-set-spacing brushTemp inSpacing)
    ;(gimp-brush-set-spikes brushTemp 2)
    ;(gimp-brush-set-aspect-ratio brushTemp 1)
    ;(gimp-brush-set-angle brushTemp 0)
    ;(gimp-context-set-brush brushTemp)
    
    (gimp-context-set-brush "Circle (01)")
    (gimp-context-set-brush-size varRadius)
    (gimp-context-set-brush-spacing 20.0)
    
    ;walk the path
    (while (<= varCounter varPathLength)
        (set! varTemp (gimp-vectors-stroke-get-point-at-dist inPath varFirstStroke varCounter 1))
        
        (if (and (<> (list-ref varTemp 0) 0) (<> (list-ref varTemp 1) 0))
            (begin
                (aset *newpoint* 0 (list-ref varTemp 0))   ; set the paint array
                (aset *newpoint* 1 (list-ref varTemp 1))
                (gimp-paintbrush-default inLayer 2 *newpoint*) ; paint point with paintbrush
            )
        )
        (set! varCounter (+ varCounter (* 0.02 varRadius inSpacing))) ; 0.02 is 2/100 to turn the inSpacing to a percent
        (set! varRadius (/ (+ (* (pow (/ varCounter varPathLength) inCurve) (- inEndWidth inStartWidth)) inStartWidth) 1.0))
        
        (if (< varRadius 1)
            (set! varRadius 1.0)
        )
        ;(gimp-message (number->string varRadius))
        
        (set! varRepaint (+ varRepaint 1))
        (if (> varRepaint 25)
            (begin
                (set! varRepaint 0)
                (gimp-displays-flush)
                (gimp-progress-update  (/ varCounter varPathLength))
            )
        )
       ;(set! outcome (car (gimp-brush-set-radius brushTemp (round varRadius))))
       ;(gimp-message (number->string outcome))
       (gimp-context-set-brush-size (round varRadius))
    )
    
    ;(gimp-brush-delete brushTemp)
    (gimp-brushes-refresh)
    ;done
    (gimp-image-undo-group-end img)
    (gimp-progress-end)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gc) ; memory cleanup
  )
)

(script-fu-register "tapered-stroke-along-path-a"
                    "<Toolbox>/Script-Fu2/Paths/Tapered Stroke Path..."
                    "Paint a Tapering Stroke Along the Path in the Current Colour. \nfile:tapered_stroke_along_path_02.scm"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Sept 2008"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-VECTORS    "Path to Stroke"          -1
                    SF-ADJUSTMENT "Stroke Start Width"      '(20 0.2 100 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Stroke End Width"        '(1.0 0.2 200 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Brush Spacing"           '(20 0.1 200 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Taper Exponent (1=linear)"    '(5 0.2 10 1 1 1 SF-SPINNER)
)

;end of script