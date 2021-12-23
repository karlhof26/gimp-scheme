; tech_3_karlhof26_02.scm
; version 2.0  
; last modified/tested by Karl Hofmeyr
; 21/12/2021 on GIMP-2.10.24
;
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8/10)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	or
;   C:\Users\YOUR-NAME\App Data\Roaming\Gimp\2.10\scripts
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
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
;Created by Wyatt Arent
;Thanks to Adrian Likins for pieces of the 'Predator' script
; Substantially modified by karlhof26 Karl Hofmeyr
;==============================================================

(define (script-fu-tech-three 
        image
        drawable
        color
        edge
        pixamm
        brightness)
    (let* (
            (width (car(gimp-image-width image)))
            (height (car(gimp-image-height image)))
            (normcolor (car(gimp-context-get-background)))
            (layer (car(gimp-layer-new image width height RGB-IMAGE "techrender" 100 LAYER-MODE-NORMAL-LEGACY)))
            (copyedge)
          )
        (gimp-image-undo-group-start image)
        (gimp-image-insert-layer image layer 0 -1)
        (gimp-context-set-background color)
        (gimp-drawable-fill layer FILL-BACKGROUND)
        (gimp-layer-add-alpha layer)
        (gimp-context-set-background normcolor)
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
        (plug-in-hsv-noise 1 image drawable 6 0 20 40) ; was 6 0 30 60
        (plug-in-gauss 1 image drawable 7 7 0)
        (plug-in-sharpen 1 image drawable 70)
        ;(plug-in-bump-map 1 image drawable drawable 150 40.70 40 0 0 0 0 1 0 1)
        (plug-in-pixelize 1 image drawable pixamm)
        (gimp-drawable-posterize drawable 4) ; was drawable 4
        (plug-in-max-rgb 1 image drawable 0) ; was 0=minimize
        
        (set! copyedge (car(gimp-layer-copy drawable TRUE)))
        (gimp-image-insert-layer image copyedge 0 1)
        (set! edge (- edge 2))
        (if (< edge 1)
            (begin
                (set! edge 1)
            )
        )
        (plug-in-edge 1 image copyedge edge 1 0) ; was edge 1 0
        (plug-in-colorify 1 image copyedge color)
        (plug-in-colortoalpha 1 image copyedge '(0 0 0))
        
        
        ;(plug-in-sharpen 1 image drawable 45) ; was 65
        
        
        
        ;(gimp-brightness-contrast drawable 50 75)
        (gimp-drawable-brightness-contrast drawable 0.196 0.345)
        
        
        
        (plug-in-colorify 1 image drawable color)
        
     ;   (gimp-displays-flush)
     ;   (quit)
        
        (plug-in-sharpen 1 image drawable 70)
      ;  (plug-in-neon 1 image drawable 1.05 (* brightness 7) )
        (gimp-selection-none image)
        
        (gimp-image-set-active-layer image layer)
        ; added by karlhof26
        (gimp-layer-set-mode layer LAYER-MODE-ADDITION-LEGACY)
        (gimp-displays-flush)
        
        (gimp-image-undo-group-end image)
        
        (gc) ; garbage collector 
    )
)

(script-fu-register "script-fu-tech-three"
            "<Image>/Script-Fu2/Render/Tech Variation 3..."
            "Create a cool technological-like image. Uses the base image. Similar to Hercules graphics. \nfile:tech_3_karlhof26_02.scm"
            "karlhof26"
            "karlhof26"
            "April 2020"
            ""
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0
            SF-COLOR      "Color"           '(0 255 20)
            SF-ADJUSTMENT "Edge"            '(8 1 10 1 1 0 0)
            SF-ADJUSTMENT "Pixel Ammount"   '(6 1 20 1 1 0 0)
            SF-ADJUSTMENT "Brightness"      '(.9 .1 1 .1 1 1 0) ; was .01 .01 1 0
)

; end of script