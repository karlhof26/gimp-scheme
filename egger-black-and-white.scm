; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Black & White script(V2.3) for GIMP 2.4
; Original author: Martin Egger (martin.egger@gmx.net)
; 2005, Bern, Switzerland
; Updater: karlhof26
; 2020, UK 
; Tags: photo, b&w, monochrome
;
; Author statement:
;
; You can find more about simulating BW at
; http://epaperpress.com/psphoto/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; 18.11.2007 -  ; Added avaliabillity check for rgb scater plug-in
; 01.03.2020 - updated for Gimp 2.10.18
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.
;

;
(define (script-fu-eg-blackandwhite InImage InLayer InType InNoise InFlatten)
    ;
    ; Save history
    ;
    (gimp-image-undo-group-start InImage)
    
    (let*  (
             (BWLayer (car (gimp-layer-copy InLayer TRUE)))
           )
        (gimp-image-insert-layer InImage BWLayer 0 -1)
        
        ; Select conversion type
        
        (cond
            
            ; B&W (Gimp)
            ;
            ((= InType 0)
                (gimp-drawable-desaturate BWLayer DESATURATE-LUMINANCE)
            )
            ;
            ; B&W (Channel Mixer)
            ;
            ((= InType 1)
                (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.4 0.3 0.3 0 0 0 0 0 0)
            )
            ;
            ; B&W with Red filter
            ;
            ((= InType 2) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.9 0.1 0.0 0 0 0 0 0 0))
            ;
            ; B&W with Orange filter
            ;
            ((= InType 3)
                (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.78 0.22 0.0 -0.2 0 0 -0.2 0 0)
            )
            ;
            ; B&W with Yellow filter
            ;
            ((= InType 4) (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.6 0.28 0.12 -0.1 0 0 -0.1 0 0))
            ;
            ; B&W with Green Filter
            ;
            ((= InType 5)
                (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.1 0.7 0.2 -0.2 0 0 -0.2 0 0)
            )
            ;
            ; B&W (Lithographic film)
            ;
            ((= InType 6)
                (begin
                    (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE 0.4 0.3 0.3 0 0 0 0 0 0)
                    ;(gimp-brightness-contrast BWLayer 0 110)
                    (gimp-drawable-brightness-contrast BWLayer 0.0 0.431)
                )
            )
            ;
            ; B&W (Orthochromatic film)
            ;
            ((= InType 7)
                (plug-in-colors-channel-mixer TRUE InImage BWLayer TRUE -1.1 1.05 1.05 0 0 0 0 0 0)
            )
        )
        ;
        ; Add noise to the image, if we need to
        ;
        (if (and (= InNoise TRUE) (defined? 'plug-in-scatter-rgb) )
            (begin
                (plug-in-scatter-rgb TRUE InImage BWLayer FALSE FALSE 0.1 0.1 0.1 0)
            )
            (begin
                ;(gimp-message "using noise plugin")
            )
        )
        (if (= InNoise TRUE)
            (begin
                ; this is the noise if no scatter plugin
                (plug-in-rgb-noise 1 InImage BWLayer FALSE FALSE 0.1 0.1 0.1 0.5)
                
            )
        )
        ;
        ; Flatten the image, if we need to
        ;
        (cond
            ((= InFlatten TRUE) (gimp-image-merge-down InImage BWLayer CLIP-TO-IMAGE))
            ((= InFlatten FALSE) (gimp-drawable-set-name BWLayer "BlackWhite"))
        )
    )
    
    ; Finish work
    ;
    (gimp-image-undo-group-end InImage)
    (gimp-displays-flush)
    (gc) ; garbage cleanup; memory cleanup
)

; Register the function with the GIMP
;
(script-fu-register "script-fu-eg-blackandwhite"
    "<Toolbox>/Script-Fu/Colors/Black and White Conversions"
    "Black and White conversions. \nfile:egger-black-and-white.scm"
    "Martin Egger martin.egger@gmx.net"
    "2005, Martin Egger, and karlhof26"
    "01.03.2020"
    "RGB*"
    SF-IMAGE       "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-OPTION      "Which B&W conversion"
            '(
                    "B&W (Gimp)"
                    "B&W (Channel Mixer)"
                    "B&W + RED filter"
                    "B&W + ORANGE filter"
                    "B&W + YELLOW filter"
                    "B&W + GREEN filter"
                    "B&W (Lithographic film)"
                    "B&W (Orthochromatic film)"
            )
    SF-TOGGLE    "Film grain simulation "       FALSE
    SF-TOGGLE    "Flatten Image"                FALSE
)

; end of script 