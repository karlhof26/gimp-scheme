; karlhof26-3Dlandscape_02.scm 
; 
; last modified/tested by karlhof26
; 20/02/2021 on GIMP-2.10.22
;
; --------------------------------------------------------------------
; Created by B Knowles 
; 
; 19/02/2021 - fixed to work Gimp-2.10.22 (removed deprecated function calls)
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
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Original script by B Knowles, but now modifed by karlhof26

(define (script-fu-karlhof26landscape W H thumb seed1 seed2 crack detail depth blur lean edge interpole remgrid flatten)
    (let* (
                (leanW 1)
                (leanH 1)
                (leanE 1)
                (crack2 1)
                
                (img (car (gimp-image-new W H RGB)))
                (cloud (car (gimp-layer-new img W H RGBA-IMAGE "Cloud" 100 LAYER-MODE-NORMAL)))
                (mapA (car (gimp-layer-new img W H RGBA-IMAGE "Map" 100 LAYER-MODE-NORMAL)))
                (noise (car (gimp-layer-new img W H 1 "Noise Layer" 100 0)))
                (blurred)
           )
           
        ;varible set up
        (if (= thumb TRUE) (set! W 400))
        (if (= thumb TRUE) (set! H 300))
        (if (= thumb TRUE) (set! lean 62))
        (if (= thumb TRUE) (set! edge 3))
        (set! leanW (- W lean))
        (set! leanH (- H lean))
        (set! leanE (- W edge))
        (set! crack2 (- 0 crack))
        ;(gimp-message "line80")
        
        ;make new image, add layers, fill white, invert
        (gimp-image-insert-layer img cloud 0 -1)
        (gimp-image-insert-layer img mapA 0 -1)
        (gimp-display-new img)
        (gimp-drawable-fill cloud 2)
        (gimp-drawable-fill mapA 2)
        (gimp-drawable-invert cloud FALSE)
        (gimp-drawable-invert mapA FALSE)
        
        ;add noise, 0.1 x, 1.0 y
        (plug-in-solid-noise 1 img mapA 0 0 seed1 1 0.1 1.0)
        
        ;brightness contrast, -crack/crack 
        ;(gimp-brightness-contrast mapA crack2 crack)
        (gimp-drawable-brightness-contrast mapA (/ crack2 255) (/ crack 255))
        
        ;make, add, and fill noise layer
        (gimp-image-insert-layer img noise 0 -1)
        (gimp-drawable-fill noise 2)
        (gimp-drawable-invert noise TRUE)
        (gimp-layer-set-mode noise 6)
        (plug-in-solid-noise 1 img noise 0 0 seed1 detail 4 4)
        
        ;add noise to cloud
        (plug-in-solid-noise 1 img cloud 0 0 seed2 detail 4 4)	
        
        ;make stone layer
        (set! mapA (car (gimp-image-merge-down img noise 0)))
        (plug-in-bump-map 1 img cloud mapA 135 45 depth 0 0 0 0 TRUE TRUE 0)
        (gimp-image-lower-layer-to-bottom img mapA)
        
        ;copy, blur
        (set! blurred (car (gimp-layer-copy cloud 1)))
        (gimp-image-insert-layer img blurred 0 -1)
        (plug-in-gauss 1 img blurred blur blur 0)
        
        ;make grid layer
        (define grid (car (gimp-layer-new img W H 1 "Grid Layer" 100 0)))
        (gimp-image-insert-layer img grid 0 -1)
        (gimp-edit-clear grid)
        (plug-in-grid 1 img grid 1 16 8 '(0 0 0) 255 1 16 8 '(0 0 0) 255 0 2 6 '(0 0 0) 255)
        
        ;displace
        (plug-in-displace 1 img grid 20 20 1 1 blurred blurred 1)
        
        ;(gimp-display-new img)
        
        ;blur, mode, merge
        (plug-in-gauss 1 img grid 1 1 0)
        (gimp-layer-set-mode grid LAYER-MODE-SOFTLIGHT) ; was 19
        (if (= TRUE remgrid) (gimp-layer-set-opacity grid 0))
        (define guide (car (gimp-image-merge-down img grid 0)))
        
        ;set to darken only
        (gimp-layer-set-mode cloud LAYER-MODE-DARKEN-ONLY)
        
        ;perspective
        (if (= interpole TRUE)
            (define anchor (car (gimp-item-transform-perspective guide lean lean leanW lean edge leanH leanE leanH)))
        )
        
        ;fill, flatten, flush
        (gimp-drawable-fill mapA 0)
        (if (= TRUE flatten) (gimp-image-flatten img))
        (gimp-displays-flush)
        (gimp-image-clean-all img)
        (gc)
        
    )
);end define

(script-fu-register "script-fu-karlhof26landscape"
    "<Image>/File/Create/Landscape Map"
    "Creates a Fake 3D Landscape Map. Uses the FG color. \nfile:karlhof26-3landscape_02.scm"
    "karlhof26"
    "karlhof26"
    "July 24 2008"
    ""
    SF-ADJUSTMENT "Width" '(800 200 1600 100 1 0 1)
    SF-ADJUSTMENT "Height" '(600 150 1200 100 1 0 1)
    SF-TOGGLE "Use Thumbnail Sizes" FALSE
    SF-ADJUSTMENT "Seed 1" '(26999 0 99999 1 1 0 1)
    SF-ADJUSTMENT "Seed 2" '(74999 0 99999 1 1 0 1)
    SF-ADJUSTMENT "Balance" '(80 -127 127 1 1 0 1)
    SF-ADJUSTMENT "Detail" '(15 1 15 1 1 0 1)
    SF-ADJUSTMENT "Depth" '(55 5 65 1 1 0 1)
    SF-ADJUSTMENT "Blur" '(40 1 100 1 1 0 1)
    SF-ADJUSTMENT "Lean" '(125 5 500 25 1 0 1)
    SF-ADJUSTMENT "Edge" '(5 0 100 5 1 0 1)
    SF-TOGGLE "Transform" TRUE
    SF-TOGGLE "Remove Grid" FALSE
    SF-TOGGLE "Flatten" TRUE
)

;end of script