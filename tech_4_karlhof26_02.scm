; Tech Variation 4
;Created by Wyatt Arent
;Thanks to Adrian Likins for pieces of the 'Predator' script
;
; author: Wyat Arent
; date: 2014
;
;
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8/10)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.10\scripts
;	or
;   C:\Users\YOUR-NAME\App Data\Roaming\Gimp\2.10\scripts
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.10/scripts  
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
;
;==============================================================



(define (script-fu-tech-four 
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
            (layer (car(gimp-layer-new image width height RGB-IMAGE "techrender" 100 LAYER-MODE-OVERLAY)))
            (noiselayer (car(gimp-layer-new image width height RGB-IMAGE "noiselayer" 100 LAYER-MODE-OVERLAY)))
            (linelayer (car (gimp-layer-copy drawable TRUE)))
            (lineedgelayer)
            (pixwidth (* pixamm (/ 4 3)))
          )
        (gimp-image-undo-group-start image)
        (gimp-image-insert-layer image layer 0 -1)
        (gimp-image-insert-layer image noiselayer 0 -1)
        (gimp-image-insert-layer image linelayer 0 -1)
        (gimp-context-set-background color)
        (gimp-drawable-fill layer 1)
        (gimp-layer-add-alpha layer)
        ;(gimp-context-set-background normcolor)
        (gimp-displays-flush)
        
        ;;(plug-in-scatter-hsv 1 image layer 2 0 30 60)
        (plug-in-solid-noise 1 image noiselayer TRUE TRUE 31145916963 3 4 4)
        (plug-in-pixelize2 1 image noiselayer pixamm (/ width 150))
        (gimp-drawable-posterize noiselayer 6)
        
        ; TECHRender Layer layer
        (gimp-displays-flush)
        (plug-in-randomize-hurl 1 image layer 15 1 1 1)
        ;(plug-in-hsv-noise 1 image layer 2 20 20 20)
        ;(plug-in-gauss 1 image layer 5 5 0)
        (gimp-displays-flush)
        
        
        
        (plug-in-sharpen 1 image layer 70)
         ;(plug-in-bump-map 1 image layer layer 150 40.70 40 0 0 0 0 1 0 1)
         ;(plug-in-pixelize2 1 image layer pixamm pixwidth)
         ;(plug-in-max-rgb 1 image layer 0)
        ;(plug-in-edge 1 image layer edge 1 0)
        (plug-in-colorify 1 image layer color)
        (gimp-displays-flush)
        ;(quit)
        
        
        ; LINE layer section
        (gimp-item-set-name linelayer "linelayer")
        (gimp-drawable-posterize linelayer 5)
        (gimp-drawable-desaturate linelayer DESATURATE-LUMINANCE)
        (plug-in-colorify 1 image linelayer color)
        
        (set! lineedgelayer (car (gimp-layer-copy linelayer TRUE)))
        (gimp-image-insert-layer image lineedgelayer 0 -1)
        
        (plug-in-pixelize 1 image linelayer pixamm)
        
        (plug-in-pixelize 1 image lineedgelayer (round (* (/ width 400) pixamm)))
        (plug-in-edge 1 image lineedgelayer edge 1 0)
        (plug-in-dilate 1 image lineedgelayer 0 CHANNEL-GRAY 0.333 2 250 250)
        (gimp-layer-set-mode lineedgelayer LAYER-MODE-DODGE)
        ;(plug-in-colorify 1 image linelayer color)
        
        (gimp-brightness-contrast layer 50 75)
         
        (gimp-displays-flush)
         
        
        (gimp-image-raise-item image noiselayer)
        (gimp-layer-set-opacity noiselayer 50.0)
        
        (gimp-image-raise-item image layer)
        (gimp-layer-set-mode layer LAYER-MODE-LINEAR-BURN)
        
        (gimp-image-set-active-layer image drawable)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-tech-four"
            "<Image>/Script-Fu2/Render/Tech Variation 4..."
            "Create a cool technological-like image \nfile:tech_4_karlhof26_02.scm"
            "Wyatt Arent"
            "Wyatt Arent"
            "November 2009"
            ""
            SF-IMAGE      "SF-IMAGE" 0
            SF-DRAWABLE   "SF-DRAWABLE" 0
            SF-COLOR      "Color"           '(0 255 20)
            SF-ADJUSTMENT "Edge"            '(8 1 10 1 1 0 0)
            SF-ADJUSTMENT "Pixel Ammount"   '(4 1 20 1 1 0 0)
            SF-ADJUSTMENT "Brightness"      '(.9 .1 1 .1 1 1 0) ; was .01 .01 1 0
)

; end of script