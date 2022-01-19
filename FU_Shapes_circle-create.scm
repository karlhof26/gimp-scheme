; FU_Shapes_circle-creator.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/04/2014 on GIMP-2.8.10
; 23/11/2020 on GIMP-2.10.22 
;
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	
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
; (c)2010 by Paul Sherman
; uploaded to gimphelp.org
;
; initial idea based upon:
; Plugin  : draw-circle.scm
; Author  : Arch. Giuseppe Conte 
;==============================================================


(define (FU-circle-creator image layer Radius FeatherRadius isSolid circlethick sfcolor)
    (let* (
            (sx 0)
            (sy 0)
            (diameter 0)
            (dx 0)
            (dy 0)
            (image-width (car (gimp-drawable-width layer)))
            (image-height (car (gimp-drawable-height layer)))
            (center-x (/ image-width 2))
            (center-y (/ image-height 2))
            (layer1 '())
        )
        
        
        ; make sure thickness of circle outline is not larger than the radius
        ; just knock it down rather than bother the user...
        
        (while (> circlethick Radius)
            (set! circlethick (- circlethick 1))
        )
        
        (if (> FeatherRadius circlethick)
            (set! FeatherRadius circlethick)
        )
        ; make sure radius fits inside image (width then height)
        
        (while (>= Radius (- center-x 2))
            (set! Radius (- Radius 1))
        )
        
        (while (>= Radius (- center-y 2))
            (set! Radius (- Radius 1))
        )
        
        ; since size is now asured to fit in the image,
        ; we can now center it in the image, since circle will be on it' own layer,
        ; dragging it where wanted is easier than setting x and y coordinates.
        
        (set! sx (- center-x Radius))
        (set! sy (- center-y Radius))
        (set! diameter (* Radius 2))
        (set! dx diameter)
        (set! dy diameter)
        
        (set! layer1 (car (gimp-layer-copy layer 1)))
        (gimp-image-undo-group-start image)
        
        (gimp-image-insert-layer image layer1 0 -1)
        (gimp-image-set-active-layer image layer1)
        (gimp-edit-clear layer1)
        (gimp-image-select-ellipse image CHANNEL-OP-REPLACE sx sy dx dy)
        (gimp-context-set-foreground sfcolor)
        (gimp-edit-fill layer1 0)
        
        (gimp-selection-feather image FeatherRadius)
        
        ; punch appropriate-size hole in circle, if not solid
        (if (not (= isSolid 1)) 
            (begin
                (gimp-selection-shrink image circlethick)
                (gimp-edit-cut layer1)
                
            )
        )
        
        (gimp-selection-none image)
        (plug-in-autocrop-layer 1 image layer1)
        
        ; tidy up
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        
    ) ;;let
) ;;def

(script-fu-register "FU-circle-creator"
    "<Toolbox>/Script-Fu/Decor/Create New/Circle Draw"
    "Draw a circle. Can be filled. \nfile:FU_Shapes_circle-create.scm"
    "Paul Sherman"
    "Paul Sherman"
    "15 October 2010"
    "*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Layer"             0
    SF-ADJUSTMENT   "Radius"            '(16 0 9999 1 10 0 1)
    SF-ADJUSTMENT   "Feather Edge"      '(1 0 9999 1 10 0 1)
    SF-TOGGLE       "Solid Circle?"     FALSE
    SF-ADJUSTMENT   "Circle Thickness (used if not solid)"  '(1 0 200 1 10 0 1)
    SF-COLOR        "Circle Fill COLOR"          '(0 0 0)
)

;end of script