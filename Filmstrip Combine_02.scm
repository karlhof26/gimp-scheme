; FilmStrip Combine rel 0.01 
; Created by Graechan 
; You will need to install GMIC to run this Scipt
; GMIC can be downloaded from http://sourceforge.net/projects/gmic/files/
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
;
;
;find layer by name proceedure
(define (find-layer-by-name image layerName)
  (let* (
            (layerList (vector->list (cadr (gimp-image-get-layers image))))    
            (wantedLayerId -1)
            (layerId 0)
            (layerText "")
        )
        
        (while (not (null? layerList))
            (set! layerId (car layerList))
            (set! layerText (cond ((defined? 'gimp-image-get-item-position) (car (gimp-item-get-name layerId)))
                        (else (car (gimp-drawable-get-name layerId)))))
            (if (string=? layerText layerName) (set! wantedLayerId layerId))          
            (set! layerList (cdr layerList))
        ) ;endwhile        
        (if (= -1 wantedLayerId) (error (string-append "Could not find a layer with name:- " layerName)))
        (list wantedLayerId)
  ) ;end variables
) ;end find layer by name proceedure


; include layer Procedure
(define (include-layer image newlayer oldlayer stack)	;stack 0=above 1=below
    (let*()
      (cond ((defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
            (gimp-image-insert-layer image newlayer (car (gimp-item-get-parent oldlayer)) 
                (+ (car (gimp-image-get-layer-position image oldlayer)) stack)
            )                                     ;For GIMP 2.8 
            )
            (else
                (gimp-image-add-layer image newlayer (+ (car (gimp-image-get-layer-position image oldlayer)) stack)) ;For GIMP 2.6 
         )
      ) ;end cond
  )
) ;end add layer procedure

(define (script-fu-filmstrip-combine inDir)
    
 (let* (
        (image (car (gimp-image-new 256 256 RGB)))
        (layer 0)
        (pasted 0)
        (width 0)
        (height 0)
        (drawable 0)
        (file-list 0)
        (loadfile 0)
        (img 0)
        (drawable-width 0)
        (drawable-height 0)
        (cnt 4)
        (n 0)
        (layer0 0)
        (layer1 0)
        (layer2 0)
        (layer3 0)
       )
        
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        
        ;  (set! file-list (cadr (file-glob (string-append inDir DIR-SEPARATOR "*.jpg") 1)))
        (set! file-list
            (let loop ((patterns '("*.[Jj][Pp][Gg]" "*.[Pp][Nn][Gg]")) ;If you want to be case insensitive, use '("*.[Jj][Pp][Gg]" "*.[Pp][Nn][Gg]") for the patterns.
                (names '()) )
                (if (null? patterns)
                    names
                    (loop (cdr patterns)
                        (append names (cadr (file-glob (string-append inDir DIR-SEPARATOR (car patterns)) 1))) 
                    )
                )
            )
        )
        
        
    (while (not (null? file-list)) 
        
        (set! loadfile (car file-list))
        (set! img (car (gimp-file-load RUN-NONINTERACTIVE loadfile loadfile)))
        (set! width (car (gimp-image-width img)))
        (set! height (car (gimp-image-height img)))
          
        (set! layer (car (gimp-layer-new image (car (gimp-image-width img)) (car (gimp-image-height img)) RGBA-IMAGE "Frame" 100 LAYER-MODE-NORMAL)))
        (gimp-image-add-layer image layer 0)
        (gimp-edit-copy-visible img)
        (set! pasted (car (gimp-edit-paste layer FALSE)))
        (gimp-floating-sel-anchor pasted)
        
        ;; removed by Karlhof26 (gimp-image-delete img)
        ;	(gimp-display-new image)
        (set! file-list (cdr file-list))
    )
    (gimp-image-resize-to-layers image)
    (set! drawable (car (gimp-image-get-active-layer image)))
    
    (stitch-layers image drawable
                        0 ;overlap 
                        FALSE ;top-on-right - was False
                        FALSE) ;use-mask
        
    (set! drawable (car (gimp-image-get-active-layer image)))
    (set! drawable-width (car (gimp-drawable-width drawable)))
    (set! drawable-height (car (gimp-drawable-height drawable)))
    (gimp-image-resize image drawable-width (* drawable-height 3.5) 0 0)
    (set! width (car (gimp-image-width image)))
    (set! height (car (gimp-image-height image)))
    
    (while (> cnt 0)
        (set! layer0 (car (gimp-layer-copy drawable TRUE)))
        (gimp-image-add-layer image layer0 1)
        (gimp-drawable-set-name layer0 (string-append "layer" (number->string n)))
        (set! n (+ n 1))
        (set! cnt (- cnt 1))
    )
    (set! layer0 (car (find-layer-by-name image "layer0")))
    (set! layer1 (car (find-layer-by-name image "layer1")))
    (set! layer2 (car (find-layer-by-name image "layer2")))
    (set! layer3 (car (find-layer-by-name image "layer3")))
        
    (gimp-layer-set-offsets layer0 0 drawable-height)
    (gimp-drawable-desaturate layer0 DESATURATE-LIGHTNESS) ;{ DESATURATE-LIGHTNESS (0), DESATURATE-LUMINOSITY (1), DESATURATE-AVERAGE (2) }
    
    (gimp-layer-set-offsets layer1 0 (* drawable-height 1.75))
    (gimp-drawable-desaturate layer1 DESATURATE-LUMINANCE) ;{ DESATURATE-LIGHTNESS (0), DESATURATE-LUMINOSITY (1), DESATURATE-AVERAGE (2) }
    
    (gimp-layer-set-offsets layer2 0 (* drawable-height 2.25))
    (gimp-drawable-desaturate layer2 DESATURATE-AVERAGE) ;{ DESATURATE-LIGHTNESS (0), DESATURATE-LUMINOSITY (1), DESATURATE-AVERAGE (2) }
    
    (gimp-layer-set-offsets layer3 0 (- height drawable-height))
    
    (plug-in-colorify 0 image drawable '(255 255 129))
    
    (set! drawable (car (gimp-image-merge-visible-layers image  EXPAND-AS-NECESSARY))) 
    
    (gimp-display-new image)
    (gimp-context-pop)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    
 )
) 

(script-fu-register "script-fu-filmstrip-combine"        
    "FilmStrip Combine"
    "Select a directory with approx 5 equal sized JPG or PNG images. e.g. 1024x768. Makes a set of image strips out of the images. Will request a color for shading. \nfile:Filmstrip Combine_02.scm"
    "Graechan for Mackenzieh"
    "Graechan - http://gimpchat.com"
    "June 2014"
    ""
    SF-DIRNAME        "Load pictures from" "(none)"
)

(script-fu-menu-register "script-fu-filmstrip-combine" "<Image>/Script-Fu2/Effects/")

(define (stitch-layers img drawable
                overlap top-on-right use-mask)
        ;; Calculate the size for the new image
    (let* (
            (layers (gimp-image-get-layers img))
            (num-layers (car layers))
            (layer-array (cadr layers))
            (bottomlayer (aref layer-array (- num-layers 1)))
            ; Pandora assumes that all layers are the same size as the first:
            ; XXX change this eventually.
            (layer-w (car (gimp-drawable-width bottomlayer)))
            (layer-h (car (gimp-drawable-height bottomlayer)))
            (width layer-w)
            (height (/ layer-h (car (gimp-image-get-layers img))))
            (area (* width height))
            (overlap-frac (/ overlap 100))
            (extra-frac (- 1.0 overlap-frac))
            (hslop (/ layer-w 4))
            (vslop (/ (* hslop 3) 2))
            (pan-img-w (* layer-w (+ 1 (* (- num-layers .3) (- 1 overlap-frac)))))
            (pan-img-h (+ layer-h vslop))
            (newy (/ vslop 2))
            (i (- num-layers 1))    ; start from the bottom layer - removed extra 1
            (thislayer)
            (thislayer-w)
            (newx)
            (masklayer)
            (grad-w)
            (grad-start)
            (grad-end)
        )
        ;;(gimp-message "started stitch")
        ;; (gimp-message (number->string pan-img-w))
        
        (gimp-image-resize img pan-img-w pan-img-h 0 0)
        
        ;; Loop over the layers starting with the second, moving each one.
        ;; Layers are numbered starting with 0 as the top layer in the stack.
        ;(gimp-layer-translate bottomlayer 0 newy)
        
      (while (>= i 0)
        ;;(gimp-message (number->string i))
                
                (set! thislayer (aref layer-array i))
                (set! thislayer-w (car (gimp-drawable-width thislayer)))
                (set! newx (if (= top-on-right TRUE)
                                (* (- (- num-layers i) 1)
                                    (* thislayer-w extra-frac))
                                (* i (* thislayer-w extra-frac))
                            )
                )
                
             (if (= (car (gimp-layer-is-floating-sel thislayer)) FALSE)
                (begin
                ;;(prog1 
                  (gimp-layer-translate thislayer newx newy)
                  (if (and (= use-mask TRUE)
                           (= (car (gimp-layer-get-mask thislayer)) -1)
                           ;;(not (= i (- num-layers 1))) ;; Karl asks can and take more than 2 items?
                       )
                      (begin
                      ;;(let* 
                           ;; (
                            (set! masklayer (car (gimp-layer-create-mask
                                              thislayer ADD-MASK-BLACK)))
                             (set! grad-w (* (* layer-w overlap-frac) .5)) ; was0.5
                             (set! grad-start (if (= top-on-right TRUE)
                                             grad-w
                                             (- thislayer-w grad-w)))
                             (set! grad-end (if (= top-on-right TRUE)
                                           0 thislayer-w))
                           ;;  )
                        (gimp-message "mask T;ue and")
                        (gimp-layer-add-alpha thislayer)
                        (gimp-layer-add-mask thislayer masklayer)
                        (gimp-context-set-foreground '(255 255 255))
                        (gimp-context-set-background '(0 0 0))
                        (gimp-edit-blend masklayer BLEND-FG-BG-RGB LAYER-MODE-NORMAL
                                         GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE
                                         FALSE 0 0 TRUE
                                         grad-start 0 grad-end 0)
                        (gimp-layer-set-edit-mask thislayer FALSE)
                      )
                  )
                )
                ;;)
            )
        (set! i (- i 1))
      )
        
        
        
        (set! drawable (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))
        (gimp-image-resize-to-layers img)
        ;;;;scale image to given area if required	
        (gimp-image-scale-full img 
            (max 1 (min 262144 (round (* width (sqrt (/ area (* width height)))))))
            (max 1 (min 262144 (round (* height (sqrt (/ area (* width height))))))) 3)
        
        (gimp-displays-flush)
    )
    
    ; end of script
)

;end of file  