; The GIMP -- an image manipulation program   
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; along with this program; if not, write to the Free Software 
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2010 elsamuko <elsamuko@web.de>
;


(define (elsamuko-photochrom aimg adraw
                             color1 color2
                             contrast bw-merge
                             num1 num2
                             dodge retro)
  (let* (
            (img (car (gimp-item-get-image adraw)))
            (owidth (car (gimp-image-width img)))
            (oheight (car (gimp-image-height img)))
            (offset1 (* oheight (/ num1 100)))
            (offset2 (* oheight (/ num2 100)))
            (dodge-layer (car (gimp-layer-copy adraw FALSE)))
            (contrast-layer1 (car (gimp-layer-copy adraw FALSE)))
            (contrast-layer2 (car (gimp-layer-copy adraw FALSE)))
            (bw-screen-layer (car (gimp-layer-copy adraw FALSE)))         
            (bw-merge-layer (car (gimp-layer-copy adraw FALSE)))         
            (lum-layer     (car (gimp-layer-copy adraw FALSE)))
            (extra-layer  0)
            (merge-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Grain Merge" 
                                           50 
                                           LAYER-MODE-GRAIN-MERGE)))
            (merge-mask (car (gimp-layer-create-mask merge-layer ADD-MASK-WHITE)))
            (screen-layer (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            RGBA-IMAGE
                                            "Screen" 
                                            10 
                                            LAYER-MODE-SCREEN)))
            (screen-mask (car (gimp-layer-create-mask screen-layer ADD-MASK-WHITE)))
            (multiply-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              RGBA-IMAGE
                                              "Multiply" 
                                              10 
                                              LAYER-MODE-MULTIPLY)))
            (multiply-mask (car (gimp-layer-create-mask multiply-layer ADD-MASK-WHITE)))
            (retro-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Retro 1" 
                                           60 
                                           LAYER-MODE-MULTIPLY)))
            (floatingsel 0)
            (retro-mask (car (gimp-layer-create-mask retro-layer ADD-MASK-WHITE)))
            (retro-layer2 (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            RGBA-IMAGE
                                            "Retro 2" 
                                            20 
                                            LAYER-MODE-SCREEN)))
            (gradient-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              RGBA-IMAGE
                                              "Gradient Overlay" 
                                              100 
                                              LAYER-MODE-OVERLAY)))
         )
        
        ; init
        (gimp-context-push)
        (gimp-image-undo-group-start img)
        (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
            (gimp-image-convert-rgb img)
        )
        
        ;set extra color layer
        (gimp-image-insert-layer img lum-layer 0 0)
        (gimp-item-set-name lum-layer "Luminosity")
        (gimp-drawable-desaturate lum-layer DESATURATE-LIGHTNESS)
        (gimp-layer-set-mode lum-layer LAYER-MODE-GRAIN-EXTRACT)
        (gimp-edit-copy-visible img)
        (set! extra-layer (car (gimp-layer-new-from-visible img img "Extra Color")))
        (gimp-image-insert-layer img extra-layer 0 0)
        (gimp-layer-set-mode extra-layer LAYER-MODE-GRAIN-MERGE)
        (gimp-layer-set-opacity extra-layer 50)
        (gimp-item-set-visible lum-layer FALSE)
        
        ;set BW screen layer
        (gimp-image-insert-layer img bw-screen-layer 0 -1)
        (gimp-item-set-name bw-screen-layer "BW Screen")
        (gimp-layer-set-mode bw-screen-layer LAYER-MODE-SCREEN)
        (gimp-layer-set-opacity bw-screen-layer 50)
        (gimp-drawable-desaturate bw-screen-layer DESATURATE-LUMINANCE)
        
        ;set BW merge layer
        (gimp-image-insert-layer img bw-merge-layer 0 -1)
        (gimp-item-set-name bw-merge-layer "BW Merge")
        (gimp-layer-set-mode bw-merge-layer LAYER-MODE-GRAIN-MERGE)
        (gimp-layer-set-opacity bw-merge-layer bw-merge)
        (gimp-drawable-desaturate bw-merge-layer DESATURATE-LUMINANCE)
        (gimp-drawable-curves-spline bw-merge-layer HISTOGRAM-VALUE 6 #(0.0 0.61 0.36 0.18 1.0 1.0))  ;0 144 88 42 255 255
        
        ;set contrast layers
        (gimp-image-insert-layer img contrast-layer1 0 -1)
        (gimp-item-set-name contrast-layer1 "Contrast1")
        (gimp-layer-set-mode contrast-layer1 LAYER-MODE-OVERLAY)
        (gimp-layer-set-opacity contrast-layer1 contrast)
        (gimp-drawable-desaturate contrast-layer1 DESATURATE-LUMINANCE)
        
        (gimp-image-insert-layer img contrast-layer2 0 -1)
        (gimp-item-set-name contrast-layer2 "Contrast2")
        (gimp-layer-set-mode contrast-layer2 LAYER-MODE-OVERLAY)
        (gimp-layer-set-opacity contrast-layer2 contrast)
        (gimp-drawable-desaturate contrast-layer2 DESATURATE-LUMINANCE)
        
        ;set dodge layer
        (gimp-image-insert-layer img dodge-layer 0 -1)
        (gimp-item-set-name dodge-layer "Dodge")
        (gimp-layer-set-mode dodge-layer LAYER-MODE-DODGE)
        (gimp-layer-set-opacity dodge-layer 50)
        
        ;set merge layer
        (gimp-image-insert-layer img merge-layer 0 -1)
        (gimp-selection-all aimg)
        (gimp-context-set-foreground color1)
        (gimp-edit-bucket-fill merge-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        (gimp-layer-add-mask merge-layer merge-mask)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-blend merge-mask BLEND-FG-BG-RGB
                     LAYER-MODE-NORMAL GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
        
        ;set screen layer
        (gimp-image-insert-layer img screen-layer 0 -1)
        (gimp-selection-all aimg)
        (gimp-context-set-foreground color1)
        (gimp-edit-bucket-fill screen-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        (gimp-layer-add-mask screen-layer screen-mask)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-blend screen-mask BLEND-FG-BG-RGB
                     LAYER-MODE-NORMAL GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
        
        ;set multiply layer
        (gimp-image-insert-layer img multiply-layer 0 -1)
        (gimp-selection-all aimg)
        (gimp-context-set-foreground color2)
        (gimp-edit-bucket-fill multiply-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        (gimp-layer-add-mask multiply-layer multiply-mask)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-blend multiply-mask BLEND-FG-BG-RGB
                     LAYER-MODE-NORMAL GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
        
        ;optional retro colors
        (if(= retro TRUE)
            (begin
                       ;yellow with mask
                       (gimp-image-insert-layer img retro-layer 0 -1)
                       (gimp-selection-all aimg)
                       (gimp-context-set-foreground '(251 242 163))
                       (gimp-edit-bucket-fill retro-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
                       (gimp-layer-add-mask retro-layer retro-mask)
                       (gimp-edit-copy contrast-layer1)
                       (set! floatingsel (car (gimp-edit-paste retro-mask TRUE)))
                       (gimp-floating-sel-anchor floatingsel)
                       
                       ;rose
                       (gimp-image-insert-layer img retro-layer2 0 -1)
                       (gimp-selection-all aimg)
                       (gimp-context-set-foreground '(232 101 179))
                       (gimp-edit-bucket-fill retro-layer2 BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
                       
                       ;gradient overlay
                       (gimp-image-insert-layer img gradient-layer 0 -1)
                       (gimp-context-set-foreground '(255 255 255))
                       (gimp-context-set-background '(0 0 0))
                       (gimp-edit-blend gradient-layer BLEND-FG-BG-RGB
                                        LAYER-MODE-NORMAL GRADIENT-LINEAR
                                        100 0 REPEAT-NONE
                                        FALSE FALSE 1 0
                                        TRUE 0 offset1 0 offset2)
                       
                       ;deactivate orange layers
                       (gimp-item-set-visible merge-layer FALSE)
                       (gimp-item-set-visible screen-layer FALSE)
                       (gimp-item-set-visible multiply-layer FALSE)
                       (gimp-displays-flush)
            )
        )
        
        ;dodge b/w
        (if(= dodge TRUE)
            (begin
                       (gimp-drawable-desaturate dodge-layer DESATURATE-LUMINANCE)
                       (gimp-item-set-visible extra-layer FALSE)
            )
        )
        
        ; tidy up
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gimp-context-pop)
    )
)

(script-fu-register "elsamuko-photochrom"
    "Photochrom"
    "Photochrom effect from 1890. \n file:elsamuko-photochrom.scm"
    "elsamuko <elsamuko@web.de>"
    "elsamuko"
    "26/02/10"
    "RGB*"
    SF-IMAGE       "Input image"          0
    SF-DRAWABLE    "Input drawable"       0
    SF-COLOR       "Screen & Grain Merge"  '(255 128 0)
    SF-COLOR       "Multiply"              '(255 68 112)
    SF-ADJUSTMENT  "Contrast"              '(60  0 100 1 10 0 0)
    SF-ADJUSTMENT  "B/W Merging"           '(60  0 100 1 10 0 0)
    SF-ADJUSTMENT  "Gradient Begin Offset" '(0 -100 200 1 10 0 0)
    SF-ADJUSTMENT  "Gradient End Offset"   '(100 -100 200 1 10 0 0)
    SF-TOGGLE      "B/W Dodging"            FALSE
    SF-TOGGLE      "Retro"                  FALSE
)

(script-fu-menu-register "elsamuko-photochrom" "<Image>/Script-Fu/Artistic")

;end of script