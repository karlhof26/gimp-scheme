; tech_2_02.scm
; version 2.0  
; last modified/tested by Karl Hofmeyr
; 22/04/2020 on GIMP-2.10.18 
;
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8/10)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   or
;   C:\Users\YOUR-NAME\AppData\Roaming\Gimp\2.10\scripts
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
;Created by Wyatt Arent
;Thanks to Adrian Likins for pieces of the 'Predator' script
;
; Modified by karlhof26 (Karl Hofmeyr) to allow the full color option to remain as a toggle
; Other changes made to ensure Gimp 2.10.18 compatibility
; EXtra posterization step added
;==============================================================

(define (script-fu-tech-two 
        image
        drawable
        color
        edge
        pixamm
        brightness
        fullcolor
        )
    (let* (
            (width (car(gimp-image-width image)))
            (height (car(gimp-image-height image)))
            (normcolor (car(gimp-context-get-background)))
            (layer (car(gimp-layer-new image width height RGB-IMAGE "techrender" 100 LAYER-MODE-NORMAL)))
            (colorit TRUE)
          )
        (gimp-image-undo-group-start image)
        (gimp-image-insert-layer image layer 0 -1)
        (gimp-context-set-background color)
        (gimp-drawable-fill layer FILL-BACKGROUND)
        (gimp-layer-add-alpha layer)
        (gimp-context-set-background normcolor)
        
        ;(gimp-message "line 80")
        ;;(plug-in-scatter-hsv 1 image layer 2 0 30 60)
        (plug-in-hsv-noise 1 image layer 2 0 30 60)
        (plug-in-gauss 1 image layer 5 5 0)
        (plug-in-sharpen 1 image layer 70)
        (plug-in-bump-map 1 image layer layer 150 40.70 40 0 0 0 0 1 0 1)
        (plug-in-pixelize 1 image layer pixamm)
        (plug-in-max-rgb 1 image layer 0)
        ;;(plug-in-edge 1 image layer edge 1 0)
        (plug-in-edge 1 image layer edge 1 0)
        (plug-in-sharpen 1 image layer 65)
        
        ;(gimp-brightness-contrast layer 50 75)
        (gimp-drawable-brightness-contrast layer 0.196 0.294)
        
        (plug-in-colorify 1 image layer color)
        (plug-in-sharpen 1 image layer 70)
        (plug-in-neon 1 image layer 1.02 brightness)
        (gimp-selection-none image)
        
        ;;(plug-in-scatter-hsv 1 image drawable 2 0 30 60)
        (plug-in-hsv-noise 1 image drawable 6 0 20 50)
        (plug-in-gauss 1 image drawable 5 5 0)
        (plug-in-sharpen 1 image drawable 70)
        (plug-in-bump-map 1 image drawable drawable 150 40.70 40 0 0 0 0 1 0 1)
        (plug-in-pixelize 1 image drawable pixamm)
        
        (gimp-drawable-posterize drawable 3) ; was 3
        
        (plug-in-max-rgb 1 image drawable 1) ; was 0=minimize
        (plug-in-edge 1 image drawable edge 1 0)
        (plug-in-sharpen 1 image drawable 65)
        ;(gimp-brightness-contrast drawable 50 75)
        (gimp-drawable-brightness-contrast drawable 0.196 0.294)
        
        (if (= fullcolor FALSE)
            (begin
                (plug-in-colorify 1 image drawable color)
                (gimp-drawable-brightness-contrast layer -0.11 0.38)
            )
        )
        (plug-in-sharpen 1 image drawable 70)
        (plug-in-neon 1 image drawable 1.05 brightness ) ; was 1.05 brightness
        (gimp-selection-none image)
        
        
        
        (gimp-image-set-active-layer image layer)
        ; added by karlhof26
        (gimp-displays-flush)
        (gimp-layer-set-mode layer LAYER-MODE-ADDITION-LEGACY)
        
        (gimp-displays-flush)
        
        (gimp-image-undo-group-end image)
        (gc) ; garbage collector  
    )
)

(script-fu-register "script-fu-tech-two"
            "<Image>/Script-Fu2/Render/Tech Variation 2..."
            "Create a cool technological-like image. Uses the base image. \n file:tech_2_02.scm"
            "Wyatt Arent"
            "Wyatt Arent"
            "November 2009"
            ""
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0
            SF-COLOR      "Color"           '(12 248 20) ; was 0 255 20
            SF-ADJUSTMENT "Edge"            '(8 1 10 1 1 0 0)
            SF-ADJUSTMENT "Pixel Ammount"   '(4 1 10 1 1 0 0)
            SF-ADJUSTMENT "Brightness"      '(.9 .1 1 .1 1 1 0) ; was .01 .01 1 0 
            SF-TOGGLE     "Keep full Color"      FALSE
)

; end of script