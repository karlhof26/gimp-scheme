;
; Add film grain, 2.10.32
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
; (C) 2022, Karl Hofmeyr - tweet me @karlhof26
;
; You can find more about adding realistic film grain to BW images at
; http://www.outbackphoto.com/workflow/wf_95/essay.html
;
; This script was tested with Gimp 2.10.32
; This is a variation from the 02b version.
;
; New versions will be distributed from http://registry.gimp.org/ only
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function
;
(define (script-fu-Eg-AddFilmGrain-a InImage InLayer InFlatten InShadCol InNoiseAmt InConvol InShadRad InMid1Blur InMid2Blur InShad1Blur InShad2Blur)
    ;
    ; Save history
    ;
    (gimp-image-undo-group-start InImage)
    (if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
    (gimp-context-set-foreground '(150 150 151))
    ;
    (let*   (
                (Midtone1Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 1 - midtones" 60 LAYER-MODE-OVERLAY)))
                (Midtone2Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 2 - midtones" 60 LAYER-MODE-OVERLAY)))
                (ShadHL1Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 1 - shadows" 60 LAYER-MODE-OVERLAY)))
                (ShadHL2Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 2 - shadows" 60 LAYER-MODE-OVERLAY)))
            )
        (gimp-drawable-fill Midtone1Layer FILL-FOREGROUND)
        (gimp-drawable-fill Midtone2Layer FILL-FOREGROUND)
        (gimp-drawable-fill ShadHL1Layer FILL-FOREGROUND)
        (gimp-drawable-fill ShadHL2Layer FILL-FOREGROUND)
        (gimp-image-insert-layer InImage Midtone1Layer 0 -1)
        (gimp-image-insert-layer InImage Midtone2Layer 0 -1)
        (gimp-image-insert-layer InImage ShadHL1Layer 0 -1)
        (gimp-image-insert-layer InImage ShadHL2Layer 0 -1)
        ;
        (plug-in-hsv-noise TRUE InImage Midtone1Layer InConvol 0 0 (+ 50 InNoiseAmt))
        (plug-in-hsv-noise TRUE InImage Midtone2Layer InConvol 0 0 (+ 50 InNoiseAmt))
        ;
        (gimp-context-set-antialias TRUE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 2 2)
        
        (gimp-context-set-sample-threshold-int InShadRad)
        (gimp-context-set-sample-criterion 0)
         
        (gimp-context-set-sample-merged TRUE)
        (gimp-image-select-color InImage CHANNEL-OP-REPLACE InLayer InShadCol)
        (plug-in-hsv-noise TRUE InImage ShadHL1Layer InConvol 0 0 (+ 50 InNoiseAmt))
        (plug-in-hsv-noise TRUE InImage ShadHL2Layer InConvol 0 0 (+ 50 InNoiseAmt))
        (gimp-selection-none InImage)
        ;
        (plug-in-gauss TRUE InImage Midtone1Layer InMid1Blur InMid1Blur TRUE)
        (plug-in-gauss TRUE InImage Midtone2Layer InMid2Blur InMid2Blur TRUE)
        (plug-in-gauss TRUE InImage ShadHL1Layer InShad1Blur InShad1Blur TRUE)
        (plug-in-gauss TRUE InImage ShadHL2Layer InShad2Blur InShad2Blur TRUE)
        ;
        ; Flatten the image, if we need to
        ;
        (cond
            ((= InFlatten TRUE) 
                (begin
                    (gimp-image-merge-down InImage Midtone1Layer CLIP-TO-IMAGE)
                    (gimp-image-merge-down InImage Midtone2Layer CLIP-TO-IMAGE)
                    (gimp-image-merge-down InImage ShadHL1Layer CLIP-TO-IMAGE)
                    (gimp-image-merge-down InImage ShadHL2Layer CLIP-TO-IMAGE)
                )
            )
            ((= InFlatten FALSE) 
                (begin
                    (gimp-image-set-active-layer InImage InLayer)
                )
            )
        )
    )
    ;
    ; Finish work
    ;
    (gimp-image-undo-group-end InImage)
    (gimp-displays-flush)
    ;
    (gc) ; memory cleanup; garbage cleanup
)
;

(script-fu-register "script-fu-Eg-AddFilmGrain-a"
    "Add film grain to BW - v02a"
    "Add realistic film grain to BW images. More configurable. Noise has base of 50 added. \nfile:egger-AddFilmGrain_02a.scm"
    "Martin Egger (martin.egger@gmx.net)"
    "Martin Egger, Bern, Switzerland"
    "29.02.2012"
    "RGB* GRAY*"
    SF-IMAGE        "The Image"         0
    SF-DRAWABLE     "The Layer"         0
    SF-TOGGLE       "Flatten Image"     FALSE
    SF-COLOR        "Shadow Color"      '(35 35 35)
    SF-ADJUSTMENT   "Noise Amount"                  '(123 1 200 1 10 0 0)
    SF-ADJUSTMENT   "Convolution Amount"            '(3 1 8 1 2 0 0)
    SF-ADJUSTMENT   "Shadow Selection Range"        '(18 1 255 1 10 0 0)
    SF-ADJUSTMENT   "Midtone 1 Blur Radius"         '(5.0 0.5 50.0 0.5 0 2 0)
    SF-ADJUSTMENT   "Midtone 2 Blur Radius"         '(3.5 0.5 10.0 0.5 0 2 0)
    SF-ADJUSTMENT   "Shadow 1 Blur Radius"          '(7.0 0.5 50.0 0.5 0 2 0)
    SF-ADJUSTMENT   "Shadow 2 Blur Radius"          '(3.5 0.5 10.0 0.5 0 2 0)
)

(script-fu-menu-register "script-fu-Eg-AddFilmGrain-a" "<Toolbox>/Script-Fu/Distorts/Grain")
; end of script
