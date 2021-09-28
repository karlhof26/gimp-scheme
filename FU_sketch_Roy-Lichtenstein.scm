; FU_sketch_Roy-Lichtenstein.scm
; version 3.0
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10 
; 09/27/2021 on Gimp-2.10.24

; 02/15/2014 - accommodate indexed images
;==============================================================
;
; Installation: 
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8/10)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.10\scripts
;   or
;   ..AppData\Roaming\GIMP\2.10\scripts
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
; $Log: Roy-Lichtenstein.scm,v $
; Revision 1.2  2008-04-07 14:05:16+05:30  Cprogrammer
; combined to if statements into one if-else
;   
; Revision 1.1  2008-04-06 15:31:32+05:30  Cprogrammer
; Initial revision
;   
; photo-Roy-Lichtenstein.scm
; by $Author: Cprogrammer $
; $Revision: 1.2 $
; Description
;   
; A script-fu script that adds the "Roy Lichtenstein" effect to an image
; Adapted from tutorial by Funadium at http://www.flickr.com/photos/funadium/2354849007/
;==============================================================


(define (FU-RoyLichtenstein
        theImage
        drawable
        baseOpacity
        BackGroundColour
        contrast
        edgeMethod
        edgeAmount
        erodeImage
        newsPrint
        pixelSize
        spotFunc
        blackAng
        cyanAng
        magentaAng
        yellowAng
        posterizeLevel
        NewsPrintOpacity
        DeSpeckle
        inFlatten
    )
    
    ; Initiate some variables
    (let*
        (
            (base 0)
            (NewsPrintLayer 0)
            (BorderLayer 0)
            (width 0)
            (height 0)
            (bottomlayer 0)
            ;(drawable 0)
            (old-fg 0)
            (old-bg 0)
        )
        
        (gimp-image-undo-group-start theImage)
        (if (not (= RGB (car (gimp-image-base-type theImage))))
            (gimp-image-convert-rgb theImage)
        )
        ;(set! drawable (car (gimp-image-get-active-drawable theImage)))
        ; Read the image height and width so that we can create a new layer of the same
        ; dimensions of the current image
        (set! old-fg (car (gimp-context-get-foreground)))
        (set! old-bg (car (gimp-context-get-background)))
        (set! width  (car (gimp-image-width  theImage)))
        (set! height (car (gimp-image-height theImage)))
        
        ; Add a coloured layer to bottom. This I felt gives some punch to the image
        ; You can play with different colours to get different effects.
        (set! bottomlayer (car (gimp-layer-new theImage width height RGB-IMAGE "Bottom" 100 LAYER-MODE-NORMAL-LEGACY)))
        (gimp-image-insert-layer theImage bottomlayer 0 -1)
        (gimp-context-set-foreground BackGroundColour)
        (gimp-edit-bucket-fill bottomlayer BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 255 0 1 1)
        (gimp-image-lower-item-to-bottom theImage bottomlayer)
        
        ; Add the NewsPrint layer to the image 
        (if (= newsPrint TRUE)
            (begin
                (set! NewsPrintLayer (car (gimp-layer-copy drawable FALSE)))
                (gimp-image-insert-layer theImage NewsPrintLayer 0 -1)
                ; Rename the layer to NewsPrint
                (gimp-drawable-set-name NewsPrintLayer "NewsPrint")
                (if (= DeSpeckle TRUE)
                    (begin
                        (gimp-drawable-posterize NewsPrintLayer posterizeLevel)
                        (plug-in-gauss     RUN-NONINTERACTIVE theImage NewsPrintLayer 6 6 0)
                        (plug-in-despeckle RUN-NONINTERACTIVE theImage NewsPrintLayer 5 2 25 254)
                    )
                )
                (plug-in-newsprint RUN-NONINTERACTIVE theImage NewsPrintLayer pixelSize 
                    2 60 blackAng spotFunc cyanAng spotFunc magentaAng spotFunc yellowAng spotFunc 3) ; last was 15; was RBG and 100% blacktakeout
                ; Change the NewsPrint Layer's opacity
                (gimp-layer-set-opacity NewsPrintLayer NewsPrintOpacity)
            )
        )
        
        ; Add Black Edge Border layer to the image
        (set! BorderLayer (car (gimp-layer-copy drawable TRUE)))
        (gimp-image-insert-layer theImage BorderLayer 0 1)
        (gimp-drawable-set-name BorderLayer "BorderLayer")
        
        (plug-in-gauss RUN-NONINTERACTIVE theImage BorderLayer 3 3 0)
        (plug-in-edge  RUN-NONINTERACTIVE theImage BorderLayer edgeAmount 0 edgeMethod)
        (gimp-drawable-invert BorderLayer TRUE)
        (gimp-drawable-desaturate BorderLayer DESATURATE-LUMINANCE)
        (gimp-drawable-brightness-contrast BorderLayer 0 contrast)
        
        (if (= erodeImage TRUE)
            (begin
                (plug-in-erode RUN-NONINTERACTIVE theImage BorderLayer 1 0 1 0 0 254)
                (plug-in-gauss RUN-NONINTERACTIVE theImage BorderLayer 3 3 0)
            )
        )
        ; This makes only the edge visible and rest of the image becomes transparent
        (plug-in-colortoalpha RUN-NONINTERACTIVE theImage BorderLayer '(255 255 255))
        
        (gimp-layer-set-opacity drawable baseOpacity)
        
        (if (= inFlatten TRUE)
            (begin
                (gimp-image-flatten theImage)
            )
        )
        
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-background old-bg)
    )
)

(script-fu-register "FU-RoyLichtenstein" 
    "<Image>/Script-Fu/Sketch/Roy Lichtenstein"
    "Add Roy Lichtenstein effect to an image. 
    \nOversaturate image before running if needed.
    \nfile: FU_sketch_Roy-Lichtenstein.scm"
    "Author: Cprogrammer "
    "Author: Cprogrammer "
    "Date: 2008-04-07 12:27"
    "*"
    SF-IMAGE        "Image"                   0
    SF-DRAWABLE     "drawable"                0
    SF-ADJUSTMENT   "Base Layer Opacity"      '(15 0 100 5 10 1 0)
    SF-COLOR        "Background Colour"       '(255 255 255)
    SF-ADJUSTMENT   "Contrast"                '(0.35 -0.50 0.50 0.05 0.10 2 1)
    SF-OPTION       "Edge Detect Algorithm"   '("Sobel" "Prewitt Compass" "Gradient" "Roberts" "Differntial" "Laplace")
    SF-ADJUSTMENT   "Edge Amount"             '(6 1 10 1 5 0 0)
    SF-TOGGLE       "Erode image"             FALSE
    SF-TOGGLE       "News Print Effect"       TRUE
    SF-ADJUSTMENT   "Newsprint Pixel Size"    '(3 1 20 1 10 1 1)
    SF-OPTION       "Spot Function"           '("Dot" "Line" "Diamond" "Euclid Dot" "PS Diamond")
    SF-ADJUSTMENT   "Black Angle"             '(45 -90 90 1 10 1 1)
    SF-ADJUSTMENT   "Cyan Angle"              '(15 -90 90 1 10 1 1)
    SF-ADJUSTMENT   "Magenta  Angle"          '(75 -90 90 1 10 1 1)
    SF-ADJUSTMENT   "Yellow Angle"            '(0 -90 90 1 10 1 1)
    SF-ADJUSTMENT   "Posterize Level"         '(5 2 255 1 10 1 1)
    SF-ADJUSTMENT   "Newsprint Layer Opacity" '(90 0 100 5 10 1 1)
    SF-TOGGLE       "Despeckle"               TRUE
    SF-TOGGLE       "Flatten image"           FALSE
)

; end of script