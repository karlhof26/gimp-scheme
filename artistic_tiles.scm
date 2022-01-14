; GPL v3
; version 2 of tiles
; 
; originally  based on random.scm
; by Charles Cave <charlesweb@optusnet.com.au>
; http://members.optusnet.com.au/~charles57/GIMP

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html
;
;


(define (script-fu-tiles inWidth inHeight inTileWidth inTileHeight inColour1 inColour2 inColour3 inColour4 inColour5 inColour6 inColour7 inColour8 inColour9 inColour10 inNumColours inGroutWidth inGroutColour inGroutOpacity)
    (let* (
            ; define our local variables
            (theImageWidth  inWidth)
            (theImageHeight inHeight)
            (theTileWidth  inTileWidth)
            (theTileHeight inTileHeight)
            (theColour1 inColour1)
            (theColour2 inColour2)
            (theColour3 inColour3)
            (theColour4 inColour4)
            (theColour5 inColour5)
            (theColour6 inColour6)
            (theColour7 inColour7)
            (theColour8 inColour8)
            (theColour9 inColour9)
            (theColour10 inColour10)
            (theColours 
                (list theColour1 
                    theColour2 
                    theColour3 
                    theColour4 
                    theColour5 
                    theColour6 
                    theColour7 
                    theColour8 
                    theColour9 
                    theColour10 
                )   
            ) 
            (theNumColours inNumColours) 
            (theGroutWidth inGroutWidth) 
            (theGroutColour inGroutColour) 
            (theGroutOpacity inGroutOpacity) 
            (theColour '(0 0 0))
            (theX 0) 
            (theY 0) 
            (theImage)
            (theImage
                (car
                    (gimp-image-new
                        theImageWidth
                        theImageHeight
                        RGB
                    )
                )
            )
            
            ;create a new layer for the tiles
            (theTilesLayer
                (car
                    (gimp-layer-new
                        theImage
                        theImageWidth
                        theImageHeight
                        RGB-IMAGE
                        "Tiles"
                        100
                        LAYER-MODE-NORMAL
                    )
                )
            )
            ;create a new layer for the grout
            (theGroutLayer
                (car
                    (gimp-layer-new
                    theImage
                    theImageWidth
                    theImageHeight
                    RGB-IMAGE
                    "Grout"
                    100
                    LAYER-MODE-NORMAL
                    )
                )
            )
            
        )
        ;end of our local variables
        
        (gimp-image-insert-layer theImage theTilesLayer 0 1)
        (gimp-context-set-foreground '(0 0 0) )
        (gimp-drawable-fill theTilesLayer FILL-FOREGROUND)
        
        ; draw the tiles
        (while (< theX theImageWidth) 
            (while (< theY theImageHeight) 
                
                (gimp-rect-select 
                    theImage 
                    theX 
                    theY 
                    theTileWidth 
                    theTileHeight 
                    CHANNEL-OP-REPLACE 
                    FALSE 
                    0 
                ) 
                (set! theColour (list-ref theColours (random theNumColours)))
                (gimp-context-set-foreground theColour) 
                (gimp-edit-bucket-fill
                    theTilesLayer
                    BUCKET-FILL-FG
                    LAYER-MODE-NORMAL
                    100
                    0
                    FALSE
                    0
                    0
                )
                (set! theY (+ theY theTileHeight)) 
            ) 
            (set! theY 0) 
            (set! theX (+ theX theTileWidth)) 
        )
        
        ; draw the grout lines
        (gimp-image-insert-layer theImage theGroutLayer 0 0) 
        (gimp-layer-add-alpha theGroutLayer) 
        (gimp-selection-all theImage) 
        (gimp-edit-clear theGroutLayer) 
        
        (plug-in-grid 1 theImage theGroutLayer theGroutWidth theTileWidth 0 theGroutColour theGroutOpacity theGroutWidth theTileHeight 0 theGroutColour theGroutOpacity theGroutWidth 0 0 theGroutColour theGroutOpacity) 
        
        ; display the image
        (gimp-display-new theImage)
        (list theImage theTilesLayer theGroutLayer)
    )
)

(script-fu-register 
    "script-fu-tiles"                        										;func name
    "Artistic Tiles"                                                    ;menu label
    "Creates a tiled image, using random colours set by the user. \nfile:artistic_tiles.scm"      ;description
    "John Irving"                                                       ;author
    "copyright 2009, John Irving"                                       ;copyright notice
    "2009"                                                              ;date created
    "New Image"                                                         ;image type that the script works on
    SF-VALUE        "Image Width"                   "800"                           ;image width
    SF-VALUE        "Image Height"                  "600"                           ;image height
    SF-VALUE        "Tile Width"                    "7"                             ;tile width
    SF-VALUE        "Tile Height"                   "7"                             ;tile height
    SF-COLOR        "Colour 1"                      '(40 40 40)                     ;colour variable
    SF-COLOR        "Colour 2"                      '(45 45 45)                     ;colour variable
    SF-COLOR        "Colour 3"                      '(50 50 50)                     ;colour variable
    SF-COLOR        "Colour 4"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 5"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 6"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 7"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 8"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 9"                      '(0 0 0)                        ;colour variable
    SF-COLOR        "Colour 10"                     '(0 0 0)                        ;colour variable
    SF-VALUE        "Num of Tile Colours"           "3"                             ;num of colours to use
    SF-VALUE        "Grout Width"                   "1"                             ;grout width
    SF-COLOR        "Grout Colour"                  '(35 35 35)                     ;grout colour
    SF-VALUE        "Grout Opacity - 0 > 255"       "255"                           ;grout opacity
)

(script-fu-menu-register "script-fu-tiles" "<Image>/Script-Fu/Render")

; end of script