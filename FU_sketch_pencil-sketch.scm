; FU_sketch_pencil-sketch.scm   
; version 2.8 [gimphelp.org]
; last modified/tested by Karl Hofmeyr
; 02/17/2014 on GIMP-2.10.10
;
; Modified 10/01/2008 to remove deprecated procedures
; 02/15/2014 - accommodated indexed images, option (default) to merge layers when complete
; 02/17/2014 - corrected undefined error for variable (tmplayer) used with texture->synthetic
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;   Linux
;   /home/yourname/.gimp-2.8/scripts  
;   or
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
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
; pencil-sketch.scm
; Jeff Trefftzs <trefftzs@tcsn.net>
;
; Changelog:
; 020113 - Version 1.1.  Added multiple choice for edge detection
; 020113 - Version 1.01. Fixed mask texture bug.
; 020113 - Version 1.0.  Initial Release.
;
; Changed on March 5, 2004 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 2.0pre3
; Updated for GIMP 2.10.18
;==============================================================


(define (FU-pencil-sketch 
        inImage 
        inLayer
        inWeight
        inEdges
        inMaskPat
        inTextToggle
        inTexture
        inMerge
    )
    
    (gimp-image-undo-group-start inImage)
    (define indexed (car (gimp-drawable-is-indexed inLayer)))
    (if (= indexed TRUE)(gimp-image-convert-rgb inImage))
    
  (let*
      (
        (WhiteLayer (car (gimp-layer-copy inLayer TRUE)))
        (MaskedLayer (car (gimp-layer-copy inLayer TRUE)))
        (EdgeLayer (car (gimp-layer-copy inLayer TRUE)))
        (LayerMask (car (gimp-layer-create-mask MaskedLayer ADD-MASK-WHITE)))
      )
    
    (gimp-image-insert-layer inImage WhiteLayer 0 -1)
    (gimp-image-insert-layer inImage MaskedLayer 0 -1)
    (gimp-image-insert-layer inImage EdgeLayer 0 -1)
    
    (gimp-item-set-name WhiteLayer "Paper Layer")
    (gimp-item-set-name MaskedLayer "Copy with layer mask")
    (gimp-item-set-name EdgeLayer "Edges from original image")
    
    ; Real work goes in here
    (gimp-drawable-fill WhiteLayer FILL-WHITE) ; Fill the white layer
    (if (= inTextToggle TRUE)
        (begin
            (gimp-message "texture the paper")
            (gimp-context-set-pattern inTexture)
            (gimp-edit-bucket-fill WhiteLayer BUCKET-FILL-PATTERN LAYER-MODE-NORMAL
                100 0 FALSE 0 0)
            (gimp-layer-set-mode WhiteLayer LAYER-MODE-LINEAR-LIGHT)
        )
        (begin
            (gimp-message "Don't texture the paper")
            ;(gimp-context-set-pattern "Marble #3")
            ;(gimp-edit-bucket-fill WhiteLayer BUCKET-FILL-PATTERN LAYER-MODE-NORMAL
            ;    100 0 FALSE 0 0)
        )
    )
    
    ; Create the layer mask and put the paper pattern in it.
    (gimp-layer-add-mask MaskedLayer LayerMask)
    (gimp-context-set-pattern inMaskPat)
    (gimp-edit-bucket-fill LayerMask BUCKET-FILL-PATTERN LAYER-MODE-NORMAL-LEGACY
            100         ; opacity
            0           ; threshold
            FALSE       ; no sample-merged
            0 0)        ; X, Y coords
    (gimp-image-unset-active-channel inImage) ; finished with it
    
    ; Now find the edges
    (gimp-image-set-active-layer inImage EdgeLayer)
    
    (cond
        ((= inEdges 0)      ; Laplacian edges
            (gimp-message "Laplacian edges")
            (plug-in-gauss-iir RUN-NONINTERACTIVE inImage EdgeLayer (* inWeight 2) TRUE TRUE) ; this is the blur
            (plug-in-edge RUN-NONINTERACTIVE inImage EdgeLayer inWeight 0 2) ; this is the edge (5=laplace)
            (gimp-drawable-invert EdgeLayer FALSE) ; trying for a bteer invert with the FALSE
            (gimp-layer-set-mode EdgeLayer LAYER-MODE-HSV-VALUE) ; try HSV VALUE ; was MULTIPLY (but ugly)
            (gimp-drawable-levels EdgeLayer HISTOGRAM-VALUE
                0.65 ; low input ; was 0.25
                1.0     ; high input
                TRUE ; clamp
                1.0     ; gamma
                0.0     ;low
                1.0     ;high
                TRUE    ) ; clamp
            (gimp-displays-flush)
            ;(gimp-message "debug out")
            ;(quit)
            
        )
        ((= inEdges 1)      ; Sobel Edges
            (gimp-message "Sobel edges")
            (plug-in-sobel TRUE inImage EdgeLayer TRUE TRUE FALSE)
            (gimp-drawable-curves-spline EdgeLayer HISTOGRAM-VALUE 8 #(0.0 0.20 0.1 0.60 0.70 0.90 1.0 1.0)) ;; was 0.0 0.0 0.57 0.05 0.591 0.992 1.0 1.0)
            (gimp-layer-set-mode EdgeLayer LAYER-MODE-MULTIPLY)
            ;(gimp-displays-flush)
            ;(quit) 
        )
        ((= inEdges 2)      ; Synthetic Edges
            (define tmplayer (car (gimp-layer-copy EdgeLayer TRUE))) 
            (gimp-image-insert-layer inImage tmplayer 0 -1)
            (gimp-layer-set-mode tmplayer LAYER-MODE-DIVIDE)
            (plug-in-gauss-iir TRUE inImage tmplayer (* inWeight 5) TRUE TRUE)
            (set! EdgeLayer 
                (car (gimp-image-merge-down inImage tmplayer
                            EXPAND-AS-NECESSARY)))
            ;(gimp-levels EdgeLayer HISTOGRAM-VALUE
            ;    (- 255 inWeight) ; low input
            ;    255     ; high input
            ;    1.0     ; gamma
            ;    0 255)
            (gimp-drawable-levels EdgeLayer HISTOGRAM-VALUE
                (/ (- 255 (* inWeight 12)) 255) ; low input
                1.0     ; high input
                TRUE ; clamp
                1.0     ; gamma
                0.0     ;low
                1.0     ;high
                TRUE    ) ; clamp
        )
        ((= inEdges 3)      ; Laplacian edges with No Invert
            ;(gimp-message "Laplacian Edge Trace")
            (gimp-progress-pulse)
            (plug-in-gauss-iir RUN-NONINTERACTIVE inImage EdgeLayer (* inWeight 2) TRUE TRUE) ; this is the blur
            (plug-in-edge RUN-NONINTERACTIVE inImage EdgeLayer inWeight 2 4) ; this is the edge (5=laplace)
            ;(gimp-drawable-invert EdgeLayer FALSE)
            (gimp-layer-set-mode EdgeLayer LAYER-MODE-HARDLIGHT)
            (gimp-layer-set-opacity EdgeLayer 93)
            (gimp-drawable-levels EdgeLayer HISTOGRAM-VALUE
                0.0 ; low input
                0.41     ; high input 
                TRUE ; clamp
                1.0     ; gamma 
                0.0     ;low
                1.0     ;high
                TRUE    ) ; clamp
            (gimp-displays-flush)
        )    
    )
     ; in case white bg
    (gimp-image-set-active-layer inImage inLayer)
    
    
    ;(if (= inMerge TRUE)(gimp-image-merge-visible-layers inImage EXPAND-AS-NECESSARY))
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
  ); end let
)

(script-fu-register "FU-pencil-sketch"
    "<Toolbox>/Script-Fu/Artist/Pencil Sketch A"
    "Creates an interesting simulation of a pencil sketch by placing a textured layer mask on the original image, above a white layer, 
    and below a layer containing edges from the original image.\n\nThis script does most of the work, but you may want to adjust the levels
    of thelayer mask to vary the amount of image texture.  If you have additional edge detector plugins (such as the ipx suite), you may want
    to use one of them for the edges instead of the built-in laplacian.  I did not include this option, since the ipx tools are 
    still very much beta at this time.\n\nAdditional interesting effects can be obtained by varying the levels of the Paper Layer.\n file:FU_sketch_pencil-sketch.scm"
    "Jeff Trefftzs"
    "Copyright 2002, Jeff Trefftzs"
    "January 12, 2002"
    "*"
    SF-IMAGE        "The Image"                                    0
    SF-DRAWABLE     "The Layer"                                    0
    SF-ADJUSTMENT   "Line Weight (Fine) 1.0 <----> 10.0 (Thick)"   '(3.0 1.0 10.0 0.1 0.5 1 1)
    SF-OPTION       "Edge Detector"                             '("Laplace" "Sobel" "Synthetic" "LaplaceTrace")
    SF-PATTERN      "Image Texture"                             "Paper"
    SF-TOGGLE       "Textured Paper"                            TRUE
    SF-PATTERN      "Paper Texture"                             "paper2" ; was crinkled papaer
    SF-TOGGLE       "Merge layers when complete?"               FALSE
)

; end of script 