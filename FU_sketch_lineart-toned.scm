; FU_sketch_lineart-toned.scm  
; version 4.3 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
; 23/07/2020 on GIMP-2.10.20
;
; Modified to work with GIMP-2.4+ by Paul Sherman 11/22/2007
; 02/15/2014 - accommodate indexed images, flatten option on bottom
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows 10
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.10\scripts
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.10\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================
; Original information 
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
;     is too light. This is particularly useful for the light, creamy Latte coffee, but all coffee   
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
;==============================================================


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Set register common information
(define SCRIPT-FU-LINEART-CHOICE1   (list "Automatic" "Manual"))
(define SCRIPT-FU-LINEART-CHOICE2   (list "Java" "Capuccino" "Expresso" "Mocha" "Latte"))
(define SCRIPT-FU-LINEART-CHOICE3     (list "80 deg C" "85 deg C" "90 deg C" "95 deg C"))
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

(define (FU-lineart-toned 
        img 
        inLayer 
        tempmode 
        ppdecaf? 
        texture? 
        polychrome? 
        art-color 
        stmode 
        opmode 
        brightness 
        contrast 
        lightness 
        saturation
        flatten?
    )
    
    (gimp-image-undo-group-start img)   
    (if (not (= RGB (car (gimp-image-base-type img))))
            (gimp-image-convert-rgb img)
    )
            
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to create names of Coffee Beans and blending options
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation)
        (let* (
                
                (brt "x")
                (crt "x")
                (ltn "x")
                (stn "x")
                (bcls "x")
                (r "x")
                (g "x")
                (b "x")
                (rgb "x")
              )
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
                    rgb
                )
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
        (define new (car(gimp-layer-copy layer 1))) ; Add an alpha channel
        (gimp-item-set-name new layername)
        new
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ;  Helper function to return matrix array
    ;  with thanks to Iccii script
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (get-matrix matrix-list)
        (define (list-ref l n) (nth n l))
        (let* ( (count 0)
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
                (green 0)
                (blue 0)
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
                (set! blue  (if (eqv? blue?  TRUE) 1 0))
            )
            (begin
                (set! red   0)
                (set! green 0)
                (set! blue  0)
            )
        )
        (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
            (set! alpha   (if (eqv? alpha? TRUE) 1 0))
            (set! alpha   0)
        )
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
    
    (define matrix-list                                               ; prepare pre-processing water
           (list   0   0   0   0   0
                   0   1   2   1   0
                   0   2   4   2   0
                   0   1   2   1   0
                   0   0   0   0   0)) 	      
    (define channels                                                 ; prepare smooth boiling water
           (list   0   1   1   1   0))
        
  (let*
      (
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
        (index 1)
        (capuccino-layer 0)
        (sobel-layer1 0)
        (sobel-layer2 0)
        (sobel-layer3 0)
        (sobel-layer4 0)
        (latte-layer 0)
        (latte-layer1 0)
        (latte-layer2 0)
        (coffee-layer 0)
        (coffee-layer1 0)
        (coffee-layer2 0)
       )
        
        
        ;; Real work goes in here
        
        (gimp-image-insert-layer img bg-layer 0 -1)
        (gimp-image-set-active-layer img bg-layer)
        (gimp-edit-fill bg-layer FILL-WHITE)
        (if  (= opmode  0) 
            (begin
                (gimp-image-insert-layer img grey-layer 0 -1)
                (gimp-context-set-background '(128 128 128))
                (gimp-edit-fill grey-layer 1)                      ;  Grey Layer fill
                (gimp-layer-set-mode grey-layer LAYER-MODE-ADDITION)     ;   Addition Layer Mode
                (gimp-image-insert-layer img sobel-layer 0 -1)
                (if  (> tempmode 0) 
                    (begin
                        (let* ( (matrix (get-matrix matrix-list))
                                (channels (get-channels sobel-layer FALSE TRUE TRUE TRUE FALSE))
                              )
                            (set! index 0)
                            (while (<  index tempmode)
                                (plug-in-convmatrix 1 img sobel-layer 25 matrix TRUE      ;;  thanks to Iccii script
                                    16 0 5 channels 0)
                                (set! index (+ index 1))
                            )
                        ) 
                    )
                )
                (gimp-brightness-contrast sobel-layer 10 -9)
                (gimp-image-set-active-layer img grey-layer)
                (gimp-image-merge-down img grey-layer  EXPAND-AS-NECESSARY)
            )
        )
        (if  (= opmode  1) 
            (begin
                (gimp-image-insert-layer img sobel-layer 0 -1)
                (if  (> tempmode 0) 
                    (begin
                        (let* ( (matrix (get-matrix matrix-list))
                                (channels (get-channels sobel-layer FALSE TRUE TRUE TRUE FALSE))
                              )
                            (set! index 0)
                            (while (<  index tempmode)
                                (plug-in-convmatrix 1 img sobel-layer 25 matrix TRUE      ;;  thanks to Iccii script
                                    16 0 5 channels 0)
                                (set! index (+ index 1))
                            )
                        ) 
                    )
                )
                (gimp-brightness-contrast sobel-layer brightness contrast)
                (gimp-drawable-hue-saturation sobel-layer 0 0 lightness saturation 0)
            )
        )
        (gimp-image-set-active-layer img sobel-layer)
        (plug-in-sobel 1 img sobel-layer TRUE TRUE TRUE)
        
        (if  (= stmode 1)                                      ;  Capuccino - 5 spoons of steamed milk please
            (begin
                (set! capuccino-layer (car (gimp-image-merge-down img sobel-layer  EXPAND-AS-NECESSARY)))
                (set! sobel-layer1 (copylayer capuccino-layer "Sobel Layer 1"))
                (gimp-image-insert-layer img sobel-layer1 0 -1)
                (plug-in-gauss-rle 1 img sobel-layer1 5 TRUE TRUE)
                (gimp-layer-set-mode sobel-layer1 LAYER-MODE-SCREEN)      ; Screen Layer Mode
                (set! sobel-layer2 (copylayer sobel-layer1 "Sobel Layer 2"))
                (gimp-image-insert-layer img sobel-layer2 0 -1)
                (set! sobel-layer3 (copylayer sobel-layer2 "Sobel Layer 3"))
                (gimp-image-insert-layer img sobel-layer3 0 -1)
                (set! sobel-layer4 (copylayer sobel-layer3 "Sobel Layer 4"))
                (gimp-image-insert-layer img sobel-layer4 0 -1)
            )
        )
        
        (if  (and  (<= stmode 1)     
                (if  (eqv? texture? TRUE)                                                        ; slow art brewing, go for a long Java coffee break
                    (begin
                        (gimp-image-insert-layer img impress-layer 0 -1)
                        (gimp-image-set-active-layer img impress-layer)
                        (gimp-brightness-contrast impress-layer brightness contrast)
                        ;;(plug-in-gimpressionist 0 img impress-layer "Crosshatch") ; impressionist cannot run in non-interactive mode yet
                        (plug-in-gimpressionist 1 img impress-layer "Crosshatch") ; 
                        (gimp-layer-set-mode impress-layer LAYER-MODE-HSV-SATURATION-LEGACY)    ; Saturation Layer Mode
                    )
                ) 
            )          ; end of  (and
        ) 
        
        (if  (> stmode 1)                                                                           ; expresso coffee - instant art gratification
            (begin
                (gimp-image-set-active-layer img sobel-layer)
                (gimp-layer-set-mode sobel-layer LAYER-MODE-DIFFERENCE)     ; Difference Layer Mode
                (set! sobel-layer1 (copylayer sobel-layer "Sobel Layer 1"))
                (gimp-image-insert-layer img sobel-layer1 0 -1)
                (gimp-layer-set-mode sobel-layer1 LAYER-MODE-DIFFERENCE)   ; Difference Layer Mode
                (set! sobel-layer2 (copylayer sobel-layer1 "Sobel Layer 2"))
                (gimp-image-insert-layer img sobel-layer2 0 -1)
                (gimp-layer-set-mode sobel-layer2 LAYER-MODE-DIFFERENCE)   ; Difference Layer Mode
                (gimp-item-set-visible inLayer FALSE)
                
                (if  (or (= stmode 3) (= stmode 4))                                           ; mocha or latte
                    (begin
                        (set! latte-layer (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))) 
                        (gimp-drawable-desaturate latte-layer DESATURATE-LIGHTNESS)
                        (gimp-drawable-levels latte-layer HISTOGRAM-VALUE 0.5 1.0 TRUE 1 0.0 1.0 TRUE) ; was 128 255 1 0 255 
                    )
                )
                
                (if  (= stmode 4)                                                                       ; latte coffee - with a little skimmed milk
                    (begin
                        (set! latte-layer1 (copylayer latte-layer "Latte Layer 1"))
                        (gimp-image-insert-layer img latte-layer1 0 -1)
                        (plug-in-gauss-rle 1 img latte-layer1 5 TRUE TRUE)
                        (gimp-layer-set-mode latte-layer1 LAYER-MODE-MULTIPLY)      ; Multiply Layer Mode
                        (set! latte-layer2 (copylayer latte-layer1 "Latte Layer 2"))
                        (gimp-image-insert-layer img latte-layer2 0 -1)
                        (plug-in-gauss-rle 1 img latte-layer2 10 TRUE TRUE)
                        (gimp-layer-set-mode latte-layer2 LAYER-MODE-SCREEN)
                    )
                )    ; Screen Layer Mode
                
                (if  (eqv? texture? TRUE) 
                    (begin 
                        (gimp-image-insert-layer img impress-layer 0 -1)
                        (gimp-layer-set-mode impress-layer LAYER-MODE-HSV-SATURATION-LEGACY)    ; Saturation Layer Mode
                    )
                )
                (gimp-item-set-visible inLayer TRUE)
            )
        )  ; end of  expresso coffee (if  (> stmode 1)
        
        (gimp-image-insert-layer img black-layer 0 -1)
        (gimp-image-set-active-layer img black-layer)
        (gimp-context-set-background '(0 0 0))
        (gimp-edit-fill black-layer FILL-BACKGROUND)                          ; Black Line Art
        (gimp-layer-set-mode black-layer LAYER-MODE-HSL-COLOR-LEGACY)         ; Color Layer Mode
        
        (gimp-image-insert-layer img color-layer 0 -1)
        (gimp-image-set-active-layer img color-layer)
        (gimp-context-set-foreground art-color)
        (gimp-edit-fill color-layer FILL-FOREGROUND)                           ; Color Line Art
        (gimp-layer-set-mode color-layer LAYER-MODE-HSL-COLOR-LEGACY)          ; Color Layer Mode
        
        (if  (eqv? polychrome? TRUE) 
            (begin
                (gimp-layer-add-alpha inLayer)
                (gimp-image-raise-item-to-top img inLayer)
                (gimp-layer-set-mode inLayer LAYER-MODE-HSL-COLOR-LEGACY)
            )
        )               ; Color Layer Mode
        
        (if  (or (eqv? flatten? TRUE)
            (eqv? ppdecaf? TRUE))                      ; Post processing coffee decaffeinator requires flatten mode
            (begin
                (set! coffee-layer (car (gimp-image-flatten img)))
                (gimp-item-set-name coffee-layer (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation))
                (if  (eqv? ppdecaf? TRUE) 
                    (begin
                        (set! coffee-layer1 (copylayer coffee-layer "Coffee Layer 1"))
                        (gimp-image-insert-layer img coffee-layer1 0 -1)
                        (gimp-layer-set-mode coffee-layer1 LAYER-MODE-MULTIPLY-LEGACY)     ; Multiply Layer Mode
                        (set! coffee-layer2 (copylayer coffee-layer1 "Coffee Layer 1"))
                        (gimp-image-insert-layer img coffee-layer2 0 -1)
                        (set! coffee-layer (car (gimp-image-flatten img)))
                        (gimp-item-set-name coffee-layer (coffee-name stmode tempmode texture? polychrome? art-color ppdecaf? opmode brightness contrast lightness saturation))
                        ;;(plug-in-dilate 1 img coffee-layer 1 HISTOGRAM-VALUE 1.0 7 0 255)
                        (plug-in-dilate 1 img coffee-layer 1 0 1.0 7 0 255)
                    )
                )
            )
        )
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
  )
)

(script-fu-register "FU-lineart-toned"
    "<Image>/Script-Fu/Artist/Toned Line Art"
    "Lasm's famous Line Art effect for photographs. (Also works with indexed...)\n\nThe technique may not  be effective when used on images with 
 large areas of high saturation.\n\nThe default values adjust for that. If the image is too dark, you may want to adjust brightness/contrast values 
 only slightly before starting the script.\n\nTip: If you do not have a good Gimpressionist preset, simply hit cancel at the Gimpressionist dialog
 window, and you will still get a good greyscale line-art. If you get a black screen you may still have a result - Check using Levels and Curves.\n file:FU_sketch_lienart-toned.scm"
    "lasm"
    "Copyright 2001-2005, lasm"
    "March 12, 2001"
    "*"
    SF-IMAGE            "The Image"             0
    SF-DRAWABLE         "The Layer"             0
    SF-OPTION           "Water Temperature"     '("80 deg C" "85 deg C" "90 deg C" "95 deg C")
    SF-TOGGLE           "Post Processing Decaffeinator"    FALSE
    SF-TOGGLE           "Texture"               TRUE
    SF-TOGGLE           "Polychromatic"         FALSE
    SF-COLOR            "Line Art Color"        '(220 219 219)  ;220 219 219
    SF-OPTION           "Art Style"             '("Java" "Capuccino" "Expresso" "Mocha" "Latte")
    SF-OPTION           "Adjustment"            '("Automatic" "Manual")
    SF-ADJUSTMENT       "Brightness"            '(10 -127 127 1 10 0 0)
    SF-ADJUSTMENT       "Contrast"              '(-9 -127 127 1 10 0 0)
    SF-ADJUSTMENT       "Lightness"             '(100 -100 100 1 10 0 0)
    SF-ADJUSTMENT       "Saturation"            '(-70 -100 100 1 10 0 0)
    SF-TOGGLE           "Flatten Image"         TRUE
)

; end of script