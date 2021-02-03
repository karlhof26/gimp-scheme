;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Pencil sketch script  for GIMP 2.10.22
; Original author: Jeff Trefftzs <trefftzs@tcsn.net>
;
; Tags: artistic
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; 200121 - Version 2     Substantially modified by Karl Hofmeyr (karlhof26) for GIMP-2.10.22
; 020113 - Version 1.1.  Added multiple choice for edge detection
; 020113 - Version 1.01. Fixed mask texture bug.
; 020113 - Version 1.0.  Initial Release.
;
; Changed on March 5, 2004 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 2.0pre3
; Updated for GIMP 2.4 by Paul Sherman
;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define the function:

(define (script-fu-pencil-sketch-trefftzs-b inImage inLayer
                 inWeight
                 inEdges
                 inMaskPat
                 inTextToggle
                 inTexture)
  (let*
      (
        (WhiteLayer (car (gimp-layer-copy inLayer TRUE)))
        (MaskedLayer (car (gimp-layer-copy inLayer TRUE)))
        (EdgeLayer (car (gimp-layer-copy inLayer TRUE)))
        (LayerMask (car (gimp-layer-create-mask MaskedLayer 0)))
        
        (tmplayer 0)
       
       )
        
        (gimp-image-undo-group-start inImage)
        
        (gimp-image-insert-layer inImage WhiteLayer 0 -1)
        (gimp-image-insert-layer inImage MaskedLayer 0 -1)
        (gimp-image-insert-layer inImage EdgeLayer 0 -1)
        
        
        (gimp-drawable-set-name WhiteLayer "Paper Layer")
        (gimp-drawable-set-name MaskedLayer "Copy with layer mask")
        (gimp-drawable-set-name EdgeLayer "Edges from original image")
        
        ;; Real work goes in here
        
        (gimp-drawable-fill WhiteLayer FILL-WHITE) ; Fill the white layer
        (if (> inTextToggle 0)
            (begin
                (gimp-patterns-set-pattern inTexture)
                (gimp-edit-bucket-fill WhiteLayer BUCKET-FILL-PATTERN LAYER-MODE-NORMAL
                    100 0 FALSE 0 0)
            )
        )
        
        ;;  Create the layer mask and put the paper pattern in it.
        
        (gimp-layer-add-mask MaskedLayer LayerMask)
        ;The following doesn't appear to be needed under 2.0 - kcozens
        ;    (gimp-image-set-active-channel inImage LayerMask)
        (gimp-context-set-pattern inMaskPat)
        (gimp-edit-bucket-fill LayerMask BUCKET-FILL-PATTERN LAYER-MODE-NORMAL
              100        ; opacity
              0            ; threshold
              FALSE        ; no sample-merged
              0 0)        ; X, Y coords
        
        (gimp-displays-flush)
        
        
        (gimp-image-unset-active-channel inImage) ; finished with it
        
        ;;  Now find the edges
        
        (gimp-image-set-active-layer inImage EdgeLayer)
        
        (cond ((= inEdges 0)        ; Laplacian edges
                ;(gimp-message "edges0")
                ;(plug-in-gauss-iir TRUE inImage EdgeLayer inWeight TRUE TRUE)
                ;(plug-in-laplace TRUE inImage EdgeLayer)
                (plug-in-edge TRUE inImage EdgeLayer 7 1 5)
                (plug-in-vinvert TRUE inImage EdgeLayer)
                (gimp-drawable-desaturate EdgeLayer DESATURATE-LUMINANCE)
                
                (plug-in-colortoalpha 1 inImage EdgeLayer '(255 255 255))
              )
            ((= inEdges 1)        ; Sobel Edges
                
                
                ;was plug-in-sobel T T T)
                (plug-in-edge TRUE inImage EdgeLayer 6 1 0)
                (gimp-dodgeburn EdgeLayer 10 0 0 2 #(0.0 4.0))
                (gimp-drawable-threshold EdgeLayer HISTOGRAM-VALUE 0.0 0.3)
                
                ;(gimp-drawable-invert EdgeLayer TRUE)
                (plug-in-colortoalpha 1 inImage EdgeLayer '(255 255 255))
                (gimp-displays-flush)
                
                
            )
            ((= inEdges 2)        ; Synthetic Edges
                
                (set! tmplayer (car (gimp-layer-copy EdgeLayer TRUE)))
                (gimp-image-insert-layer inImage tmplayer 0 -1)
                (plug-in-neon 1 inImage tmplayer 10.0 (/ inWeight 40))
                
                (gimp-item-set-name tmplayer "tmplayer")
                (gimp-drawable-invert tmplayer TRUE)
                (plug-in-colortoalpha 1 inImage tmplayer '(255 255 255))
                
                (gimp-item-set-visible EdgeLayer FALSE)
                
                
                (gimp-displays-flush)
                
            )
        )                ; end cond
        
        (gimp-layer-set-mode EdgeLayer LAYER-MODE-DARKEN-ONLY) ; in case white bg
        
        (gimp-image-set-active-layer inImage inLayer)
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush)
    )
)

(script-fu-register
    "script-fu-pencil-sketch-trefftzs-b"
    "<Toolbox>/Script-Fu/Artistic/Pencil Sketch..."
    "Creates an interesting simulation of a pencil sketch by placing a textured layer mask on the original image, above a white layer, and below a layer containing edges from the original image.
    This script does most of the work, but you may want to adjust the levels of thelayer mask to vary the amount of image texture.  If you have additional edge detector plugins (such as the ipx suite), you may want to use one of them for the edges instead of the built-in laplacian.
    I did not include this option, since the ipx tools are still very much beta at this time.
    Additional interesting effects can be obtained by varying the levels of the Paper Layer. \nfile:trefftzs-pencil-sketch.scm"
    "Jeff Trefftzs"
    "Copyright 2002, Jeff Trefftzs"
    "January 12, 2002"
    "RGB* GRAY*"
    SF-IMAGE "The Image" 0
    SF-DRAWABLE "The Layer" 0
    SF-ADJUSTMENT "Line Weight (Fine) 1 <----> 128 (Thick)"     '(10 1 128 1 8 0 1)
    SF-OPTION "Edge Detector" '("Laplace" "Sobel" "Synthetic")
    SF-PATTERN "Image Texture" "Paper"
    SF-TOGGLE  "Textured Paper" FALSE
    SF-PATTERN "Paper Texture" "Crinkled Paper"
    
)

; end of script