; The GIMP -- an image manipulation program 
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
; Copyright (C) 2005 Francois Le Lay <mfworx@gmail.com>
;
; Version 0.3 - Changed terminology, settings made more user-friendly
; Version 0.2 - Now using radial blending all-way
; Version 0.1 - Rectangular Selection Feathering doesn't look too good
; 
;
; Usage: 
;
; - Vignetting softness: The vignette layer is scaled with a 
;   default size equal to 1.5 the image size. Setting it to 2
;   will make the vignetting softer and going to 1 will make
;   the vignette layer the same size as the image, providing
;   for darker blending in the corners.
;
; - Saturation and contrast have default values set to 20 and act 
;   on the base layer.
;
; - Double vignetting: when checked this will duplicate the Vignette 
;   layer providing for a stronger vignetting effect.
;
;
; October 23, 2007
; Script made GIMP 2.4 compatible by Donncha O Caoimh, donncha@inphotos.org
; Download at http://inphotos.org/gimp-lomo-plugin/
;
; Updated by elsamuko <elsamuko@web.de> 
; http://registry.gimp.org/node/7870
;

(define (elsamuko-lomo aimg adraw avig asat acon
                       sharp wide_angle gauss_blur
                       motion_blur grain c41 
                       invertA invertB
                       adv is_black
                       centerx centery aradius)
  (let* ( (img (car (gimp-item-get-image adraw)))
          (draw (car (gimp-layer-copy adraw FALSE))) 
          (owidth (car (gimp-image-width img)))
          (oheight (car (gimp-image-height img)))
          (halfwidth (/ owidth 2))
          (halfheight (/ oheight 2))
          (endingx 0)
          (endingy 0)
          (blend_x 0)
          (blend_y 0)
          
          (imgLAB 0)
          (layersLAB 0)
          (layerA 0)
          (layerB 0)
          (drawA 0)
          (drawB 0)
          
          (MaskImage 0)
          (MaskLayer 0)
          (OrigLayer 0)
          (HSVImage 0)
          (HSVLayer 0)
          (SharpenLayer 0)
          (Visible 0)
          
          (cyan-layer 0)
          (magenta-layer 0)
          (yellow-layer 0)
          (blue-layer 0)
          (blue-layer-mask 0)
          
          (amiddle (/ (+ owidth oheight) 2))
          (multi (/ aradius 100))
          (radius (* multi amiddle))
          (x_black (+ (- halfwidth  (* multi (/ amiddle 2))) (* owidth (/ centerx 100))))
          (y_black (- (- halfheight (* multi (/ amiddle 2))) (* oheight (/ centery 100))))
          (vignette (car (gimp-layer-new img
                                         owidth 
                                         oheight
                                         1
                                         "Vignette" 
                                         100 
                                         LAYER-MODE-OVERLAY)))
          (hvignette (car (gimp-layer-new img
                                          owidth 
                                          oheight
                                          1
                                          "Vignette" 
                                          100 
                                          LAYER-MODE-OVERLAY)))
          (overexpo (car (gimp-layer-new img
                                         owidth 
                                         oheight
                                         1
                                         "Over Exposure" 
                                         80 
                                         LAYER-MODE-OVERLAY)))
          (black_vignette (car (gimp-layer-new img
                                               owidth 
                                               oheight
                                               1
                                               "Black Vignette" 
                                               100 
                                               LAYER-MODE-NORMAL)))
          (grain-layer (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            1
                                            "Grain" 
                                            100 
                                            LAYER-MODE-OVERLAY)))
          (grain-layer-mask (car (gimp-layer-create-mask grain-layer ADD-MASK-WHITE)))
          (newacon 0)
          (layerC 0)
          )
    
    ; init
    (set! blend_x (+ halfwidth  (* owidth  (/ centerx 100))))
    (set! blend_y (- halfheight (* oheight (/ centery 100))))
    
    (define (set-pt a index x y)
      (begin
        (aset a (* index 2) x)
        (aset a (+ (* index 2) 1) y)
        )
    )
    (define (splineValue)
      (let* (
              (a (cons-array 6 'double))
              (newgrain 0)
            )
        (set! newgrain (/ grain 255))
        (set-pt a 0 0.0 0.0)
        (set-pt a 1 0.5 newgrain)
        (set-pt a 2 1.0 0.1)
        a
        )
    )
    
    ;(gimp-message (number->string (car (gimp-drawable-is-gray adraw ))))
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
    )
    
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    (gimp-image-insert-layer img draw 0 -1)
    (gimp-item-set-name draw "Process Copy")
    
    ; adjust contrast, saturation
    (set! newacon (+ 0 (* (/ acon 40) 0.18))) ; mutiplier must be less than 0.5
    (gimp-drawable-brightness-contrast draw 0.0 newacon)
    (gimp-drawable-hue-saturation draw HUE-RANGE-ALL 0.0 0.0 asat 0.0)
   ;; (gimp-drawable-hue-saturation draw HUE-RANGE-ALL 0.0 0.0 15.0 0.0)
    
    ;wide angle lens distortion
    (if (> wide_angle 0)
        (begin
            (plug-in-lens-distortion 1 img draw 0 0 wide_angle 0 9 0)
        )
    )
    
    ;gauss blur as general focusing error
    (if (> gauss_blur 0)
        (begin
            (plug-in-gauss 1 aimg draw gauss_blur gauss_blur 0)
        )
    )
    
    ;motion blur as corner fuzziness
    (if (> motion_blur 0)
        (begin
            (plug-in-mblur 1 img draw 2 motion_blur 0 blend_x blend_y)
        )
    )
    
    (gimp-message(number->string c41))
    
    (if (= c41 0)
        (begin
            (gimp-message "Neutral")
        )
    )
    ;add c41-effect
    ;old red from djinn (http://registry.gimp.org/node/4683)
    (if (= c41 1)
        (begin
                  (gimp-drawable-curves-spline draw  HISTOGRAM-VALUE 8 #(0.0 0.0 0.31 0.34 0.61 0.82 1.0 1.0))  ;;(0 0 68 64 190 219 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED   8 #(0.0 0.0 0.18 0.31 0.68 0.51 1.0 1.0)) ;;(0 0 39 93 193 147 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 6 #(0.0 0.0 0.31 0.325 1.0 0.89))   ;;(0 0 68 70 255 207)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  6 #(0.0 0.0 0.315 0.311 1.0 0.76))         ;;(0 0 94 94 255 199)
        )
    )
    
    ;xpro green from lilahpops (http://www.lilahpops.com/cross-processing-with-the-gimp/)
    (if (= c41 2)
        (begin
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED  10 #(0.0 0.0 0.29 0.305 0.46 0.68 0.67 0.92 1.0 1.0))  ;;(0 0 80 84 149 192 191 248 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 8 #(0.0 0.0 0.32 0.43 0.495 0.92 1.0 1.0)) ;;(0 0 70 81 159 220 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  4 #(0.0 0.105 1.0 0.95)) ;;(0 27 255 213)
        )
    )
    
    ;blue
    (if (= c41 3)
        (begin
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED   4 #(0.0 0.23 1.0 0.94)) ;;(0 62 255 229)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 8 #(0.0 0.0 0.245 0.18 0.73 0.96 1.0 1.0)) ;;(0 0 69 29 193 240 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  8 #(0.0 0.11 0.42 0.20 0.67 0.96 1.0 1.0))  ;;(0 27 82 44 202 241 255 255)
        )
    )
    
    ;intense red
    (if (= c41 4)
        (begin
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED   6 #(0.0 0.0 0.38 0.61 0.955 1.0)) ;;(0 27 82 44 202 241 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 6 #(0.0 0.0 0.53 0.419 0.956 1.0)) ;;(0 0 136 107 240 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  6 #(0.0 0.0 0.53 0.419 1.0 0.96))   ;;(0 0 136 107 255 246)
        )
    )
    
    ;movie (from http://tutorials.lombergar.com/achieve_the_indie_movie_look.html)
    (if (= c41 5)
        (begin
                  (gimp-drawable-curves-spline draw  HISTOGRAM-VALUE 4 #(0.156 0.000 1.000 1.000)) ;;40 0 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED   6 #(0.000 0.000 0.498 0.615 1.000 1.000)) ;;(0  0 127 157 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 4 #(0.000 0.031 1.000 1.000)) ;;(0  8 255 255)
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  6 #(0.000 0.000 0.498 0.415 1.000 0.960)) ;;(0  0 127 106 255 245)
        )
    )
    
    ;vintage-look script from mm1 (http://registry.gimp.org/node/1348)
    (if (= c41 6)
        (begin
                  ;Yellow Layer
                  (set! yellow-layer (car (gimp-layer-new img owidth oheight RGB "Yellow" 100  LAYER-MODE-MULTIPLY)))
                  (gimp-image-insert-layer img yellow-layer 0 -1)
                  (gimp-context-set-background '(251 242 163))
                  (gimp-drawable-fill yellow-layer FILL-BACKGROUND)
                  (gimp-layer-set-opacity yellow-layer 59)
                  
                  ;Magenta Layer
                  (set! magenta-layer (car (gimp-layer-new img owidth oheight RGB "Magenta" 100  LAYER-MODE-SCREEN)))
                  (gimp-image-insert-layer img magenta-layer 0 -1)
                  (gimp-context-set-background '(232 101 179))
                  (gimp-drawable-fill magenta-layer FILL-BACKGROUND)
                  (gimp-layer-set-opacity magenta-layer 20)
                  
                  ;Cyan Layer 
                  (set! cyan-layer (car (gimp-layer-new img owidth oheight RGB "Cyan" 100  LAYER-MODE-SCREEN)))
                  (gimp-image-insert-layer img cyan-layer 0 -1)
                  (gimp-context-set-background '(9 73 233))
                  (gimp-drawable-fill cyan-layer FILL-BACKGROUND)
                  (gimp-layer-set-opacity cyan-layer 17)
        )
    )
    
    ;LAB from Martin Evening (http://www.photoshopforphotographers.com/pscs2/download/movie-06.pdf)
    (if (= c41 7)
        (begin
                  (set! drawA  (car (gimp-layer-copy draw FALSE)))
                  (set! drawB (car (gimp-layer-copy draw FALSE)))
                  (gimp-image-insert-layer img drawA 0 -1)
                  (gimp-image-insert-layer img drawB 0 -1)
                  
                  (gimp-item-set-name drawA "LAB-A")
                  (gimp-item-set-name drawB "LAB-B")
                  
                  ;decompose image to LAB and stretch A and B
                  (set! imgLAB (car (plug-in-decompose 1 img drawA "LAB" TRUE)))
                  (set! layersLAB (gimp-image-get-layers imgLAB))
                  (set! layerA (aref (cadr layersLAB) 1))
                  (gimp-drawable-levels-stretch layerA)
                  (plug-in-recompose 1 imgLAB layerA)
                  
                  (set! imgLAB (car (plug-in-decompose 1 img drawB "LAB" TRUE)))
                  (set! layersLAB (gimp-image-get-layers imgLAB))
                  (set! layerB (aref (cadr layersLAB) 2))
                  (gimp-drawable-levels-stretch layerB)
                  (plug-in-recompose 1 imgLAB layerB)
                  
                  (gimp-image-delete imgLAB)
                  
                  ;set mode to color mode
                  (gimp-layer-set-mode drawA LAYER-MODE-OVERLAY) ;; COLOR-MODE
                  (gimp-layer-set-mode drawB LAYER-MODE-OVERLAY) ;; COLOR-MODE
                  (gimp-layer-set-opacity drawA 40)
                  (gimp-layer-set-opacity drawB 40)
                  
                  ;blur
                  (plug-in-gauss 1 img drawA 2.5 2.5 1)
                  (plug-in-gauss 1 img drawB 2.5 2.5 1)
        )
    )
    
    ;light blue
    (if (= c41 8)
        (begin
                  (gimp-message "option 8")
                  (gimp-drawable-curves-spline draw  HISTOGRAM-RED   6 #(0.0 0.0 0.603 0.552 0.909 1.000)) ; 0 0 154 141 232 255
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 8 #(0.0 0.0 0.254 0.188 0.792 0.843 1.000 1.000)) ; 0 0 65 48 202 215 255 255
                  (gimp-drawable-curves-spline draw  HISTOGRAM-GREEN 4 #(0.0 0.082 1.000 1.000)) ; 0 21 255 255
                  (gimp-drawable-curves-spline draw  HISTOGRAM-BLUE  8 #(0.0 0.0 0.266 0.349 0.635 0.807 0.917 1.000)) ; 0 0 68 89 162 206 234 255
                  (gimp-drawable-levels draw HISTOGRAM-VALUE
                               0.1 1.0 TRUE ;input   ;;25 255 ;input 
                               1.25   ;gamma
                               0.0 1.0 TRUE) ;output   ;;;0 255 ;output
        )
    )
    
    ;redscale
    (if (= c41 9)
        (begin
                  ;Blue Layer
                  (set! blue-layer (car (gimp-layer-copy draw TRUE)))
                  (gimp-image-insert-layer img blue-layer 0 -1)
                  (gimp-item-set-name blue-layer "Blue Filter")
                  (gimp-layer-set-opacity blue-layer 40)
                  (gimp-layer-set-mode blue-layer LAYER-MODE-SCREEN)
                  (plug-in-colors-channel-mixer 1 img blue-layer TRUE
                                                0.0 0.1 2.0 ;R
                                                0.0 0.1 0.1 ;G
                                                0.0 0.0 0.0 ;B
                                                )
                  (set! blue-layer-mask (car (gimp-layer-create-mask blue-layer ADD-MASK-COPY)))
                  (gimp-layer-add-mask blue-layer blue-layer-mask)
                  
                  (gimp-context-set-background '(0 0 255))
                  (gimp-drawable-fill blue-layer FILL-BACKGROUND)
                  
                  (gimp-drawable-curves-spline draw HISTOGRAM-RED   6 #(0.0 0.0 0.498 0.745 1.000 1.000))   ;;(0 0 127 190 255 255)
                  (gimp-drawable-curves-spline draw HISTOGRAM-GREEN 6 #(0.0 0.0 0.498 0.243 0.941 1.0000))  ;;(0 0 127  62 240 255)
                  (gimp-drawable-curves-spline draw HISTOGRAM-BLUE  4 #(0.0 0.0 1.0 0.0))   ;;(0 0 255 0)
        )
    )
    
    ;retro bw
    (if (= c41 10)
        (begin
                  (gimp-message "option 10")
                  (gimp-drawable-desaturate draw DESATURATE-LUMINANCE)
                  
                  ;(gimp-curves-spline draw HISTOGRAM-RED   4 #(0 15 255 255))
                  (gimp-drawable-curves-spline draw HISTOGRAM-RED   4 #(0.0 0.08 1.0 1.0))  ;;;(0 15 255 255))
                  
                  
                  (gimp-drawable-curves-spline draw HISTOGRAM-BLUE  6 #(0.0 0.0 0.99 0.90 1.0 1.0)) ;;(0 0 255 230)
                  
                  
                  (gimp-drawable-curves-spline draw HISTOGRAM-VALUE 8 #(0.0 0.0 0.247 0.203 0.749 0.792 1.0 1.0)) ;;(0 0 63 52 191 202 255 255)
                  (gimp-displays-flush)
                  ;(quit)
        )
    )
    
    ;paynes bw
    (if (= c41 11)
        (begin
                  (gimp-drawable-desaturate draw DESATURATE-LUMINANCE)
                  (gimp-drawable-colorize-hsl draw 215.0 11.0 0.0)
        )
    )
    
    ;sepia
    (if (= c41 12)
        (begin
                  (gimp-drawable-desaturate draw DESATURATE-LUMINANCE) ;;DESATURATE-LUMINOSITY
                  (gimp-drawable-colorize-hsl draw 30.0 25.0 10.0)
        )
    )
    
    ;set some funky colors
    (if (= invertA TRUE)
        (begin
                          (set! imgLAB (car (plug-in-decompose 1 img draw "LAB" TRUE)))
                          (set! layersLAB (gimp-image-get-layers imgLAB))
                          (set! layerA (aref (cadr layersLAB) 1))
                          (gimp-drawable-invert layerA TRUE)
                          (plug-in-recompose 1 imgLAB layerA)
        )
    )
    (if (= invertB TRUE)
        (begin
                          (set! imgLAB (car (plug-in-decompose 1 img draw "LAB" TRUE)))
                          (set! layersLAB (gimp-image-get-layers imgLAB))
                          (set! layerB (aref (cadr layersLAB) 2))
                          (gimp-drawable-invert layerB TRUE)
                          (plug-in-recompose 1 imgLAB layerB)
        )
    )
    
    (gimp-displays-flush)
                  
    ;add two blending layers
    (gimp-context-set-foreground '(0 0 0)) ;black
    (gimp-context-set-background '(255 255 255)) ;white
    (gimp-image-insert-layer img overexpo 0 -1)
    (gimp-image-insert-layer img vignette 0 -1)
    (gimp-drawable-fill vignette FILL-TRANSPARENT)
    (gimp-drawable-fill overexpo FILL-TRANSPARENT)
    
    ;compute blend ending point depending on image orientation
    (if (> owidth oheight) 
        (begin
          (set! endingx owidth)
          (set! endingy halfheight)
        )
        (begin
          (set! endingx halfwidth)
          (set! endingy oheight)
        )
    )
    
    (gimp-message "404")
    ;let's do the vignetting effect
    ;apply a reverse radial blend on layer
    ;then scale layer by "avig" factor with a local origin
    ;if double vignetting is needed, duplicate layer and set duplicate opacity to 80%
    (gimp-edit-blend vignette 2 0 2 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE blend_x blend_y endingx endingy)
    (gimp-layer-scale vignette (* owidth avig) (* oheight avig) 1)
    
    (plug-in-spread 1 img vignette 50.1 50.0)
    
    (if (= adv TRUE) 
        (begin 
           (set! hvignette (car (gimp-layer-copy vignette 0)))
           (gimp-layer-set-opacity hvignette 80)
           (gimp-image-insert-layer img hvignette 0 -1)
           (gimp-layer-resize-to-image-size hvignette)
        )
    )
    (gimp-layer-resize-to-image-size vignette)
    
    (gimp-message "line444")
    (gimp-displays-flush)
    
    ;let's do the over-exposure effect
    ;swap foreground and background colors then
    ;apply a radial blend from center to farthest side of layer
    (gimp-context-swap-colors)
    (gimp-edit-blend overexpo 2 0 2 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE blend_x blend_y endingx endingy)
    (plug-in-spread 1 img overexpo 50.1 50.1)
    
    (gimp-message "line454")
    ;adding the black vignette
    ;selecting a feathered circle, invert selection and fill up with black
    (if (= is_black TRUE) 
        (begin 
           (gimp-message "insside is_black")
           (gimp-image-insert-layer img black_vignette 0 -1)
           (gimp-drawable-fill black_vignette FILL-TRANSPARENT)
           (gimp-image-select-ellipse img CHANNEL-OP-REPLACE x_black y_black radius radius)
           (gimp-selection-feather img (* radius 0.2))
           (gimp-selection-invert img)
           (gimp-context-set-foreground '(0 0 0))
           (gimp-edit-bucket-fill black_vignette BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
           (gimp-selection-none img)
        )
    )
    
    (gimp-message "line471")
    (gimp-displays-flush)
    
    ;add grain
    (if (> grain 0) 
        (begin 
            (gimp-message "add grain")
            ;fill new layer with neutral gray
            (gimp-image-insert-layer img grain-layer 0 -1)
            (gimp-drawable-fill grain-layer FILL-TRANSPARENT)
            (gimp-context-set-foreground '(128 128 128))
            (gimp-selection-all img)
            (gimp-edit-bucket-fill grain-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 1 FALSE 0 0)
            (gimp-selection-none img)
            
            ;add grain and blur it
            (plug-in-hsv-noise 1 img grain-layer 2 0 0 100)
            (plug-in-gauss 1 img grain-layer 0.5 0.5 1)
            (gimp-layer-add-mask grain-layer grain-layer-mask)
            
            (gimp-message "475") 
            ;select the original image, copy and paste it as a layer mask into the grain layer
            (gimp-selection-all img)
            (gimp-edit-copy-visible img)
            (gimp-floating-sel-anchor (car (gimp-edit-paste grain-layer-mask TRUE)))
            
            ;set color curves of layer mask, so that only gray areas become grainy
            (gimp-drawable-curves-spline grain-layer-mask HISTOGRAM-VALUE 6 (splineValue))
        )
    )
    
    (gimp-message "line 502")
    (gimp-message "line503")
    (gimp-displays-flush)
    
    
    ;sharpness layer
    (if (> sharp 0)
        (begin
            (gimp-message "sharp gr 0")
            (if (> grain 0)
                (begin
                    (gimp-message "hide grain layer if sharp>0")
                    (gimp-item-set-visible grain-layer FALSE)
                )
            )
            (gimp-message "sharpening")
            (gimp-edit-copy-visible aimg)
            (set! Visible (car (gimp-layer-new-from-visible aimg aimg "Visible")))
            (gimp-image-insert-layer aimg Visible 0 -1)
            
            (set! MaskImage (car (gimp-image-duplicate aimg)))
            (set! MaskLayer (cadr (gimp-image-get-layers MaskImage)))
            (set! OrigLayer (cadr (gimp-image-get-layers aimg)))
            (set! HSVImage (car (plug-in-decompose TRUE aimg Visible "HSV" TRUE))) ;; was Value
            (set! HSVLayer (gimp-image-get-layers HSVImage))
            (gimp-message "Line 527")
     ;       (set! layerC (aref (cadr HSVLayer) 2))
            ;;(set! HSVLayer (aref HSVImage 0)) ; (aref HSVImage)
            (set! SharpenLayer (car (gimp-layer-copy Visible TRUE)))
            (gimp-message "Line 531")
            
            (gimp-displays-flush)
            
            
            ;;;smart sharpen from here: http: registry.gimp.org node 108
            (gimp-image-insert-layer img SharpenLayer 0 -1)
            (gimp-selection-all HSVImage)
            (gimp-message "Line 531")
            
            (set! layerC (aref (cadr HSVLayer) 2))
            (gimp-edit-copy layerC) ;;(aref HSVLayer 0)
            
            (gimp-message "Line 541")
            
            ;(gimp-image-delete HSVImage)
            (gimp-floating-sel-anchor (car (gimp-edit-paste SharpenLayer FALSE)))
            (gimp-layer-set-mode SharpenLayer LAYER-MODE-HSV-VALUE) ; VALUE-MODE
            
            ;good to here
            (gimp-displays-flush)
            
            (plug-in-edge 1 MaskImage (aref MaskLayer 0) 6.1 1 0)
            (gimp-drawable-levels-stretch (aref MaskLayer 0))
            (gimp-image-convert-grayscale MaskImage)
            (plug-in-gauss 1 MaskImage (aref MaskLayer 0) 6 6 1)
            (let* ((SharpenChannel (car (gimp-layer-create-mask SharpenLayer ADD-MASK-WHITE)))
                  )
                (gimp-layer-add-mask SharpenLayer SharpenChannel)
                (gimp-selection-all MaskImage)
                (gimp-edit-copy (aref MaskLayer 0))
                (gimp-floating-sel-anchor (car (gimp-edit-paste SharpenChannel FALSE)))
                (gimp-image-delete MaskImage)
                (plug-in-unsharp-mask TRUE img SharpenLayer 1 sharp 0)
                (gimp-layer-set-opacity SharpenLayer 80)
                (gimp-layer-set-edit-mask SharpenLayer FALSE)
            )
            (gimp-message "Line 568")
            
            
            (gimp-item-set-name SharpenLayer "Sharpen")
            (gimp-image-remove-layer aimg Visible)
            (if (> grain 0)
                (begin
                    (gimp-item-set-visible grain-layer TRUE)
                    (gimp-image-lower-item aimg SharpenLayer)
                )
            )
            (gimp-displays-flush)
            ;(quit)
        )
    )
    
    (gimp-message "Good Finish OK")
    ;tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gc) ; an array was used so garbage clean
  )
)

(script-fu-register "elsamuko-lomo"
                    "Lomo..."
                    "Do a lomo effect on image. Do not use InvertLAB with B&W options.
file:elsamuko-lomo_02.scm"
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "15/02/05"
                    "*"
                    SF-IMAGE       "Input image"           0
                    SF-DRAWABLE    "Input drawable"        0
                    SF-ADJUSTMENT  "Vignetting Softness"   '(1.5 1 2 0.1 0.5 1 0)
                    SF-ADJUSTMENT  "Saturation"            '(15 -100 100  1 5 1 0)
                    SF-ADJUSTMENT  "Contrast"              '(22 0 40 1 5 1 0)
                    SF-ADJUSTMENT  "Sharpness"             '(0.8 0 2 0.1 0.2 1 0)
                    SF-ADJUSTMENT  "Wide Angle Distortion" '(5 0 13 0.1 0.5 1 0)
                    SF-ADJUSTMENT  "Gauss Blur"            '(1 0  5 0.1 0.5 1 0)
                    SF-ADJUSTMENT  "Motion Blur"           '(3 0  5 0.1 0.5 1 0)
                    SF-ADJUSTMENT  "Grain"                 '(128 0 255 1 20 0 0)
                    SF-OPTION     "Colors"                '("Neutral"
                                                             "Old Red"
                                                             "XPro Green"
                                                             "Blue"
                                                             "XPro Autumn"
                                                             "Movie"
                                                             "Vintage"
                                                             "Xpro LAB"
                                                             "Light Blue"
                                                             "Redscale"
                                                             "Retro B/W"
                                                             "Paynes B/W"
                                                             "Sepia")
                    SF-TOGGLE      "Invert LAB-A"          FALSE
                    SF-TOGGLE      "Invert LAB-B"          FALSE
                    SF-TOGGLE      "Double Vignetting"     TRUE
                    SF-TOGGLE      "Black Vignetting"      FALSE
                    SF-ADJUSTMENT  "   X-Shift%"        '(0 -50  50 1 10 0 0)
                    SF-ADJUSTMENT  "   Y-Shift%"        '(0 -50  50 1 10 0 0)
                    SF-ADJUSTMENT  "   Radius%"         '(115 0 200 1 20 0 0)
)

(script-fu-menu-register "elsamuko-lomo" "<Toolbox>/Script-Fu/Light and Shadow")

;end of script  