; The GIMP -- an image manipulation program  
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Advanced Line Art (Sobel) script  for GIMP 2.3.4
; Copyright (C) 2001-2005 Lasm <lasm@rocketmail.com>
;  http://www.godsimmediatecontact.com
;  http://www.godsdirectcontact.org
;  http://www.raindesigninc.com
;
;  Latest scripts available at
;  http://sourceforge.net/projects/lasmz/
;  http://groups.yahoo.com/group/script-fu/
;
; Welcome to the Line Art Coffee House
; This Line Art Reference Model script is for coffee-connisseurs only
; If it doesn't work for your images, perhaps you prefer the Bubble Tea House next door ?
; line-art.scm - lasm's famous Line Art script a.k.a. Grand Mother's Coffee Line-Art script
; Dedication - to my mother (1917-2002)
; who passed away during the design of version 3.0 This will be the last release.
;
; --------------------------------------------------------------------
; version 1.0  by Lasm 2001/03/12 <lasm@rocketmail.com>
;     - Initial relase
; version 2.0  by Lasm 2002/01/01 <lasm@rocketmail.com>
;     - moved to Script-Fu->Lasm's FX Effects->Line Art
;     - added automatic adjustment mode as default
;     - changed default color to blue
;     - Added polychromatic option, flatten image option
;     - Added 5 new coffee-art styles 
;             1) Java - slow, thick, black, heavy texture 
;             2) Capuccino - slow, black, creamy, heavy texture 
;             3) Expresso - fast, white, medium texture 
;             4) Mocha - fast, thin, black, medium texture
;             5) Latte - fast, white, creamy, medium texture 
;     - Added option to turn texture layer on or off
; version 2.1  by Lasm 2002/01/11 <lasm@rocketmail.com>
;     - re-order options above color selector
;     - coffee-name now includes blending options
;     - Added "Post Processing Decaffeinator" works only in flatten mode, requires plugin-grain
; version 3.0  by Lasm 2002/01/24 <lasm@rocketmail.com>
;     - Added pre-processing "Water Temperature" option requires plug-in-convmatrix
;     - coffee-name in flatten mode includes "Water Temperature" 
; version 3.1  by Lasm 2002/01/26 <lasm@rocketmail.com>
;     - coffee-name in flatten mode includes manual adjustment numbers 
; version 3.2  by Lasm 2005/10/05 <lasm@rocketmail.com>
;     - updated to work in Gimp version 2.2 
; version 3.3  by Lasm 2005/10/05 <lasm@rocketmail.com>
;     - updated to work in Gimp version 2.3.4 
;	- changed default color to light-grey
; version 3.4  by Lasm 2005/11/16 <lasm@rocketmail.com>
;     - code re-organised to keep procedure names private
;	  - changed static constants with script name prefix
;	  - import to sourceforge/cvs
; version 3.5  by Lasm 2005/11/22 <lasm@rocketmail.com>
;     - change dependency on plug-in-grain to plug-in-dilate
;
; --------------------------------------------------------------------
; You are free to use/distribute this program provided the 
; comments are left intact.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Instructions on using this script
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. The pre-processing water boiler is used to lighten the final image and produce
;     a more refined, smooth line in the line-art. The default value of 80 degC means no
;     pre-processing smoothing operation is carried out. Each susbsequent temperature greater
;     than 80 degC produces stronger smoothing effect.This option is useful for "Java"
;     and "Capuccino" coffee as they tend to produce very dark images.
; 2. Conversely, the post-processing Decaffienator produces darker final image. It is an
;     operation carried out after flattening all visible layers in the image. Use it if the final image
;     is too light. This is particularly useful for the light, creamy Latte coffee, but all coffee   ;  
;     types will benefit from it as well.
; 3. Texture can be turned off for certain images, which is better off without it. Similarly for
;     polychromatic option. 
; 4. Polychromatic and line-art color are mutually exclusive options. To change line art color
;     from blue to something else, simply click on the line art color bar and select the color
;     you want. For black color, all sliders in the color selector should have zero value
; 5. Very colorful line-art is often produced by turning on polychromatic and turning off texture. 
; 6. Use flatten option when you are satisfied with the result or turn it off if you want to experiment
;     with the layers.
; 7. The automatic adjustment is useful most of the time, so leave it as it is, unless your
;     image needs manual adjustment for lightness/contrast etc.
; 8. Java and Capuccino styles tend to take longer to produce as they use the Gimpressionist
;     plugin. Expresso appears to give reasonably good results most of the time.
;
; That's all folks. Have fun with this script !
; Another Grandmother Coffee House production.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Advanced Line Art  function:
;
; Requires:
;   plug-in-sobel
;   plug-in-gauss-rle
;   plug-in-gimpressionist
;   plug-in-dilate
;   plug-in-convmatrix
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-lalas-lineart img 
                inLayer
                tempmode
                ppdecaf?
                flatten?
                texture?
                polychrome?
                art-color
                stmode
                opmode
                brightness
                contrast
                lightness
                saturation)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to create names of Coffee Beans and blending options
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation)
      (let* (
                (brt "9")
                (crt "a")
                (ltn "b")
                (stn "c")
                (bcls "d")
                (r    "e")
                (g    "f")
                (b    "bl")
                (rgb  "h")
             )
        ;(gimp-message "line 141")
        (set! brt (number->string brightness 10))
        (set! crt (number->string contrast 10))
        (set! ltn (number->string lightness 10))
        (set! stn (number->string saturation 10))
        (set! bcls (string-append "-adj" "(" brt "," crt "," ltn "," stn ")"))
        (set! r (number->string(car art-color)))
        (set! g (number->string(cadr art-color)))
        (set! b (number->string(caddr art-color)))
        (set! rgb (string-append "-c" "(" r "," g "," b ")"))
        (string-append
            (cond 
                ((= stmode 0) "Java")
                ((= stmode 1) "Capuccino")
                ((= stmode 2) "Expresso")
                ((= stmode 3) "Mocha")
                ((= stmode 4) "Latte"))
            (cond 
                ((= tempmode 0) "")
                ((= tempmode 1) "-85 degC")
                ((= tempmode 2) "-90 degC")
                ((= tempmode 3) "-95 degC"))
            (if  (= opmode 1)
                bcls
                "") 
            (if  (eqv? texture? TRUE)
                "-texture" "") 
            (if  (eqv? polychrome? TRUE)
                "-polychrome" 
                rgb)
            (if  (eqv? ppdecaf? TRUE) 
                "-Decaf" "")
        )
      )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to create a new layer 
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (copylayer layer layername)
        (let* (
                (new 0)
              )
            (set! new (car (gimp-layer-copy layer 1))) ; Add an alpha channel
            (gimp-drawable-set-name new layername)
            new
        )
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to return matrix array
    ;  with thanks to Iccii script
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (get-matrix matrix-list)
        (define (list-ref l n) (nth n l))
        (let* (
                (count 0)
                (matrix (cons-array 25 'double))
              )
            (while (< count 25)
                (aset matrix count (list-ref matrix-list count))
                (set! count (+ count 1))
            )
            matrix
        )
    ) ; Return the matrix array
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to return channels array
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (get-channels drawable gray? red? green? blue? alpha?)
        (let* (
                (channels (cons-array 5 'long))
                (gray 0)
                (red 0)
                (green 1)
                (blue 2)
                (alpha 0)
              )
            (if (eqv? (car (gimp-drawable-is-gray drawable)) TRUE)
                (set! gray (if (eqv? gray? TRUE) 1 0))
                (set! gray 0)
            )
            (if (eqv? (car (gimp-drawable-is-rgb drawable)) TRUE)
                (begin
                    (set! red   (if (eqv? red?   TRUE) 1 0))
                    (set! green (if (eqv? green? TRUE) 1 0))
                    (set! blue  (if (eqv? blue?  TRUE) 1 0)))
                (begin
                    (set! red   0)
                    (set! green 0)
                    (set! blue  0)))
            (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
                (set! alpha   (if (eqv? alpha? TRUE) 1 0))
                (set! alpha   0))
            (aset channels 0 gray)
            (aset channels 1 red)
            (aset channels 2 green)
            (aset channels 3 blue)
            (aset channels 4 alpha)
        channels)
    ) ; Return the channel array
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Main function
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;(set! matrix-list                                               ; prepare pre-processing water
    ;       (list   0   0   0   0   0
    ;               0   1   2   1   0
    ;               0   2   4   2   0
    ;               0   1   2   1   0
    ;               0   0   0   0   0))        
    ;(set! channels                                                 ; prepare smooth boiling water
    ;       (list   0   1   1   1   0))
   
  (let* (
            (width (car (gimp-drawable-width inLayer)))
            (height (car (gimp-drawable-height inLayer)))
            (color-layer (car (gimp-layer-new img width height RGBA-IMAGE "Color Line Art" 100 LAYER-MODE-NORMAL)))
            (bg-layer (copylayer inLayer "White Layer"))
            (grey-layer (copylayer bg-layer "Grey Layer"))
            (sobel-layer (copylayer grey-layer "Sobel Layer"))
            (impress-layer (copylayer sobel-layer "Saturation Layer"))
            (black-layer (copylayer impress-layer "Black Line Art"))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (matrix-list                                               ; prepare pre-processing water
                (list   0   0   0   0   0
                        0   1   2   1   0
                        0   2   4   2   0
                        0   1   2   1   0
                        0   0   0   0   0))
            (channels                                                 ; prepare smooth boiling water
                (list   0   1   1   1   0))
            (capuccino-layer (copylayer grey-layer "Cappuccino Layer"))
            (sobel-layer1 0)
            (sobel-layer2 0)
            (sobel-layer3 0)
            (sobel-layer4 0)
            (coffee-layer 0)
            (coffee-layer1 0)
            (coffee-layer2 0)
            (latte-layer 0)
            (latte-layer1 0)
            (latte-layer2 0)
        )
        
        (gimp-image-undo-group-start img)
        
        ;; Real work goes in here
        (gimp-image-insert-layer img bg-layer 0 -1)
        (gimp-image-set-active-layer img bg-layer)
        (gimp-edit-fill bg-layer FILL-WHITE)
        (if  (= opmode 0) 
            (begin
                ;(gimp-message "line 305 Automatic")
                (gimp-image-insert-layer img grey-layer 0 -1)
                (gimp-context-set-background '(128 128 128))
                (gimp-edit-fill grey-layer FILL-BACKGROUND)                      ;  Grey Layer fill
                (gimp-layer-set-mode grey-layer LAYER-MODE-ADDITION-LEGACY)     ;   Addition Layer Mode
                (gimp-image-insert-layer img sobel-layer 0 -1)
                (if  (> tempmode 0) 
                    (begin
                        (let* (
                                (matrix (get-matrix matrix-list))
                                (index 5)
                                
                                (channels (get-channels sobel-layer FALSE TRUE TRUE TRUE FALSE))
                              )
                              
                            ;(gimp-message "line 320")
                            (set! index 0)
                            (while (<  index tempmode)
                                (plug-in-convmatrix 1 img sobel-layer 25 matrix TRUE      ;;  thanks to Iccii script
                                    16 0 5 channels 0)
                                (set! index (+ index 1))
                            )
                        ) 
                    )
                )
                (gimp-drawable-brightness-contrast sobel-layer (/ 10 255) (/ -9 255))
                (gimp-image-set-active-layer img grey-layer)
                (gimp-image-merge-down img grey-layer  EXPAND-AS-NECESSARY)
            )
        )
        ;(gimp-message "line 335")
        (if  (= opmode  1) 
            (begin
                ;(gimp-message "line 338 manual")
                (gimp-image-insert-layer img sobel-layer 0 -1)
                    (if  (> tempmode 0) 
                        (begin
                            (let* ((matrix (get-matrix matrix-list))
                                (channels (get-channels sobel-layer FALSE TRUE TRUE TRUE FALSE)))
                                (set! index 0)
                                (while (<  index tempmode)
                                    (plug-in-convmatrix 1 img sobel-layer 25 matrix TRUE      ;;  thanks to Iccii script
                                        16 0 5 channels 0)
                                    (set! index (+ index 1))
                                )
                            ) 
                        )
                    )
                ;(gimp-brightness-contrast sobel-layer brightness contrast)
                (gimp-drawable-brightness-contrast sobel-layer (/ brightness 255) (/ contrast 255))
                ;(gimp-hue-saturation sobel-layer 0 0 lightness saturation)
                (gimp-drawable-hue-saturation sobel-layer HUE-RANGE-ALL 0 (/ lightness 100) (/ saturation 100) 0)
            )
        )
        
        (gimp-image-set-active-layer img sobel-layer)
        ;(plug-in-sobel 1 img sobel-layer TRUE TRUE TRUE)
        (plug-in-edge 1 img sobel-layer 5.1 0 0)
        ;
        ;(gimp-drawable-invert sobel-layer FALSE)
        ;(gimp-message "line 361")
        (gimp-displays-flush)
        ;(gimp-message "line363")
        
        
        
        (if  (= stmode 1)                                                                 ;  Capuccino - 5 spoons of steamed milk please
            (begin
                (set! capuccino-layer (car (gimp-image-merge-down img sobel-layer  EXPAND-AS-NECESSARY)))
                (set! sobel-layer1 (copylayer capuccino-layer "Sobel Layer 1"))
                (gimp-image-insert-layer img sobel-layer1 0 -1)
                (plug-in-gauss-rle2 1 img sobel-layer1 5.05 5.1)
                (gimp-layer-set-mode sobel-layer1 LAYER-MODE-SCREEN-LEGACY)      ; Screen Layer Mode
                (set! sobel-layer2 (copylayer sobel-layer1 "Sobel Layer 2"))
                (gimp-image-insert-layer img sobel-layer2 0 -1)
                (set! sobel-layer3 (copylayer sobel-layer2 "Sobel Layer 3"))
                (gimp-image-insert-layer img sobel-layer3 0 -1)
                (set! sobel-layer4 (copylayer sobel-layer3 "Sobel Layer 4"))
                (gimp-image-insert-layer img sobel-layer4 0 -1)
            )
        )
        
        (if  (<= stmode 1)     
            (if  (eqv? texture? TRUE)                                                        ; slow art brewing, go for a long Java coffee break
                (begin
                    ;(gimp-message "line 388")
                    ;(gimp-message "stmode lt 1 and texture true")
                    (gimp-image-insert-layer img impress-layer 0 -1)
                    (gimp-item-set-name impress-layer "impress-layer")
                    (gimp-image-set-active-layer img impress-layer)
                    (gimp-brightness-contrast impress-layer brightness contrast)
                    (plug-in-gimpressionist 1 img impress-layer "Crosshatch") ; impressionist cannot run in non-interactive mode yet
                    (gimp-layer-set-mode impress-layer LAYER-MODE-HSL-COLOR-LEGACY) ; was LAYER-MODE-HSV-SATURATION-LEGACY
                )
            )   ; Saturation Layer Mode
        )          ; end of  (and
        ;(gimp-message "line 399")
        ;(gimp-displays-flush)
        ;(gimp-message "line401")
        
        
        (if  (> stmode 1)                                                                           ; expresso coffee - instant art gratification
            (begin
                ;(gimp-message "line 406")
                (gimp-image-set-active-layer img sobel-layer)
                (gimp-layer-set-mode sobel-layer LAYER-MODE-DIFFERENCE)     ; Difference Layer Mode
                (set! sobel-layer1 (copylayer sobel-layer "Sobel Layer 1"))
                (gimp-image-insert-layer img sobel-layer1 0 -1)
                (gimp-layer-set-mode sobel-layer1 LAYER-MODE-DIFFERENCE)   ; Difference Layer Mode
                (set! sobel-layer2 (copylayer sobel-layer1 "Sobel Layer 2"))
                (gimp-image-insert-layer img sobel-layer2 0 -1)
                (gimp-layer-set-mode sobel-layer2 LAYER-MODE-DIFFERENCE)   ; Difference Layer Mode
                (gimp-drawable-set-visible inLayer FALSE)
                
                (if  (or (= stmode 3) (= stmode 4))                                           ; mocha or latte
                    (begin
                        ;(gimp-message "line 419")
                        (set! latte-layer (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))) 
                        (gimp-drawable-desaturate latte-layer DESATURATE-LUMINANCE)
                        (gimp-drawable-levels latte-layer HISTOGRAM-VALUE 0.52 1.0 TRUE 1.0 0.0 1.0 TRUE) ; was 128 and VALUE-LUT (so maybe RGB but buggy)
                    )
                )
                
                (if  (= stmode 4)                                                                       ; latte coffee - with a little skimmed milk
                    (begin
                        ;(gimp-message "line 428")
                        (set! latte-layer1 (copylayer latte-layer "Latte Layer 1"))
                        (gimp-image-insert-layer img latte-layer1 0 -1)
                        (plug-in-gauss-rle2 1 img latte-layer1 5 4.9)
                        (gimp-layer-set-mode latte-layer1 LAYER-MODE-MULTIPLY)          ; Multiply Layer Mode
                        (set! latte-layer2 (copylayer latte-layer1 "Latte Layer 2"))
                        (gimp-image-insert-layer img latte-layer2 0 -1)
                        (plug-in-gauss-rle2 1 img latte-layer2 10 10.5)
                        (gimp-layer-set-mode latte-layer2 LAYER-MODE-SCREEN-LEGACY)
                    )
                )           ; Screen Layer Mode
                
                (if  (eqv? texture? TRUE) 
                    (begin 
                        (gimp-image-insert-layer img impress-layer 0 -1)
                        (gimp-layer-set-mode impress-layer LAYER-MODE-HSV-SATURATION-LEGACY)        ; Saturation Layer Mode
                    )
                )
                (gimp-drawable-set-visible inLayer TRUE)
            )
        )  ; end of  expresso coffee (if  (> stmode 1)
        ;(gimp-message "line 449")
        
        (gimp-displays-flush)
        ;(gimp-message "line452")
        
        
        (gimp-image-insert-layer img black-layer 0 -1)
        (gimp-image-set-active-layer img black-layer)
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-fill black-layer FILL-BACKGROUND)                          ; Black Line Art
        (gimp-layer-set-mode black-layer LAYER-MODE-HSL-COLOR)                ; Color Layer Mode
        
        (gimp-image-insert-layer img color-layer 0 -1)
        (gimp-image-set-active-layer img color-layer)
        (gimp-context-set-foreground art-color)
        (gimp-edit-fill color-layer FILL-FOREGROUND)                           ; Color Line Art
        (gimp-layer-set-mode color-layer LAYER-MODE-HSL-COLOR)                   ; Color Layer Mode
        ;(gimp-message "line 467")
        
        (if  (eqv? polychrome? TRUE) 
            (begin
                (gimp-layer-add-alpha inLayer)
                (gimp-image-raise-layer-to-top img inLayer)
                (gimp-layer-set-mode inLayer LAYER-MODE-HSL-COLOR)
            )
        )               ; Color Layer Mode
        ;(gimp-message "line 475")
        (gimp-displays-flush)
        
        
        (if  (or (eqv? flatten? TRUE)
                (eqv? ppdecaf? TRUE))                      ; Post processing coffee decaffeinator requires flatten mode
            (begin
                (set! coffee-layer (car (gimp-image-flatten img)))
                (gimp-drawable-set-name coffee-layer (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation))
                (if  (eqv? ppdecaf? TRUE) 
                    (begin
                        ;(gimp-message "line 486")
                        (set! coffee-layer1 (copylayer coffee-layer "Coffee Layer 1"))
                        (gimp-image-insert-layer img coffee-layer1 0 -1)
                        (gimp-layer-set-mode coffee-layer1 LAYER-MODE-MULTIPLY-LEGACY)      ; Multiply Layer Mode
                        ;(gimp-message "line 490")
                        (set! coffee-layer2 (copylayer coffee-layer1 "Coffee Layer 1"))
                        (gimp-image-insert-layer img coffee-layer2 0 -1)
                        (set! coffee-layer (car (gimp-image-flatten img)))
                        ;(gimp-message "line 494")
                        (gimp-drawable-set-name coffee-layer (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation))
                        
                        (if (null? (defined? 'plug-in-dilate))
                            (begin
                                ;(gimp-message "line 499 no dilate")
                            )
                            (begin
                                (plug-in-dilate 1 img coffee-layer 1 HISTOGRAM-VALUE 1.0 7 0 255))
                            )
                    )
                )
            )
        )
        ;(gimp-message "line 508")
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
        (gimp-message "Good finish OK")
  )
)


(script-fu-register "script-fu-lalas-lineart"
    "<Toolbox>/Script-Fu/Effects/Advanced Line Art Sobel"
    "Lasms famous Line Art effect for photographs. This works on any RGB image. The technique may not be
    effective when used on images with large areas of high saturation. Flatten to record script settings in layer name.\nfile:lalas_line-art.scm"
    "lasm"
    "Copyright 2020, lasm, karlhof26"
    "March 12, 2001"
    "RGB*"
    SF-IMAGE            "The Image"             0
    SF-DRAWABLE         "The Layer"             0
    SF-OPTION           "Water Temperature"    '("80 deg C" "85 deg C" "90 deg C" "95 deg C")
    SF-TOGGLE           "Post Processing Decaffeinator"          FALSE
    SF-TOGGLE           "Flatten Image"        TRUE
    SF-TOGGLE           "Texture"              TRUE
    SF-TOGGLE           "Polychromatic"        FALSE
    SF-COLOR            "Line Art Color"      '(220 219 219)  ;220 219 219
    SF-OPTION           "Art Style"            '("Java" "Capuccino" "Expresso" "Mocha" "Latte")
    SF-OPTION           "Adjustment"           '("Automatic" "Manual")
    SF-ADJUSTMENT       "Brightness"                 '(10 -127 127 1 10 0 0)
    SF-ADJUSTMENT       "Contrast"                   '(-9 -127 127 1 10 0 0)
    SF-ADJUSTMENT       "Lightness"                  '(100 -100 100 1 10 0 0)
    SF-ADJUSTMENT       "Saturation"                 '(-70 -100 100 1 10 0 0)
)

;The technique may not be effective when used on images with large areas of high saturation.\
;    The default values adjust for that. If the image is too dark, you may want to adjust brightness/contrast values only slightly before starting the script.\
;    \nTip: If you do not have a good Gimpressionist preset, simply hit cancel at the Gimpressionist dialog window, and you will still get a good greyscale line-art.\
;    \n file:lalas_line-art.scm"