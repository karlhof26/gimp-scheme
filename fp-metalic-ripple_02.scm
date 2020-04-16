; GIMP - The GNU Image Manipulation Program 
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;
;
;
; Define the Function

(define (fp-script-fu-ripple-texture3
            image      
            drawable
            times
            hAmplitude
            vAmplitude
            period1
            period2
            wave1
            wave2
            hEdge
            vEdge
            hTrue
            vTrue
            )
    (let* (
            (timesnext 0)
        )
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        
        (gimp-layer-add-alpha drawable)
        
        (if (= wave1 0)
            (set! wave1 1)
            (set! wave1 0)
        )
        
        (if (= wave2 0)
            (set! wave2 1)
            (set! wave2 0)
        )
        
        (while (> times 0)
            
            (plug-in-ripple RUN-NONINTERACTIVE image drawable period1 hAmplitude 0 hEdge wave1 TRUE hTrue)
            (plug-in-ripple RUN-NONINTERACTIVE image drawable period2 vAmplitude 1 vEdge wave2 TRUE vTrue)
            (gimp-displays-flush)
            (set! times (- times 1))
        )
        
        (gimp-image-undo-group-end image)
        
        (gimp-context-pop)
        
        (gimp-displays-flush)
        
    )
)

(script-fu-register "fp-script-fu-ripple-texture3"
    "<Image>/Script-Fu2/Render/Metalic/Ripple Texture3..."
    "Create a random ripple texture somewhat similar to Photoshop's Wave Filter. Uses an image as input.  \n file:fp-metalic-ripple_02.scm"
    "Art Wade"
    "Art Wade"
    "March 18, 2010"
    "RGB* GRAY*"
    SF-IMAGE        "Image" 0
    SF-DRAWABLE     "Drawable" 0
    SF-ADJUSTMENT   "Ripple how many times?" '(5 1 1000 1 1 0 1)
    SF-ADJUSTMENT   "Horizontal Amplitude" '(10 -400 400 1 1 0 1)
    SF-ADJUSTMENT   "Vertical Amplitude" '(10 -400 400 1 1 0 1)
    SF-ADJUSTMENT   "Horizontal Period" '(100 1 400 1 1 0 1)
    SF-ADJUSTMENT   "Vertical Period" '(100 1 400 1 1 0 1)
    SF-OPTION       "Horizontal Waveform" '("Sinewave"
                  "Sawtooth")
    SF-OPTION       "Vertical Waveform"    '("Sinewave"
                  "Sawtooth")
    SF-OPTION       "Horizontal Edge Handling"   '("Smear"
                    "Wrap"
                    "Blank")
    SF-OPTION      "Vertical Edge Handling"      '("Smear"
                    "Wrap"
                    "Blank")
    SF-TOGGLE      "Retain Horizontal Tilability?"    TRUE
    SF-TOGGLE      "Retain Vertical Tilability?"      TRUE
)

; end of file