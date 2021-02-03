; FU_shapes_cd-mask.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/04/2014 on GIMP-2.8.10
;
; tweaked for GIMP-2.4.x by Paul Sherman 10/24/2007
;
; Oct 1,2008 removed deprecated foreground function
;
; 09/27/2010 - added adjustable inner diameter (by %)
; 14/11/2020 - updated for Gimp-2.10.22 
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
; Make a CD label shape.
; Copyright (C) 2002 by Akkana Peck, akkana@shallowsky.com.
;==============================================================

; Utility to calculate the inner radius
(define (inner-diam diameter mini adj)
    (let* (
            (j (/ adj 100))
          )
        (if (= mini TRUE)
            (* (/ diameter 2.2) j) 
            (* (/ diameter 3.1) j)
        )
    )
)

;; Select the CD shape.  Then you can cut, or whatever.
(define (CD-select img diameter mini adj)
    (gimp-image-select-ellipse img CHANNEL-OP-REPLACE 0 0 diameter diameter)
    (let* (
            (inner (inner-diam diameter mini adj))
            (offset (/ (- diameter inner) 2))
        )
        (gimp-image-select-ellipse img CHANNEL-OP-SUBTRACT offset offset inner inner)
    )
)

;; Actually make a CD shape, in a solid color.
(define (FU-CD-label diameter color mini adj)
    (let* (
            (old-fg-color (car (gimp-context-get-foreground)))
            (img (car (gimp-image-new diameter diameter RGB)))
            (cdlayer (car (gimp-layer-new img diameter diameter
                        RGBA-IMAGE "CD" 100 LAYER-MODE-NORMAL)))
          )
        (gimp-image-undo-disable img)
        (gimp-image-insert-layer img cdlayer 0 -1)
        (gimp-selection-all img)
        (gimp-edit-clear cdlayer)
        
        (gimp-context-set-foreground color)
        (CD-select img diameter mini adj)
        (gimp-edit-bucket-fill cdlayer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        ;; Clean up
        (gimp-selection-none img)
        (gimp-image-set-active-layer img cdlayer)
        (gimp-context-set-foreground old-fg-color)
        (gimp-image-undo-enable img)
        (gimp-display-new img)
    )
)

;; Cut out a CD shape from the current image.
(define (FU-CD-mask img drawable mini adj)
    (gimp-image-undo-group-start img)
    (CD-select img
        (min (car (gimp-image-width img)) (car (gimp-image-height img)))
        mini adj)
    (gimp-selection-invert img)
    (gimp-edit-clear drawable)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
)

(script-fu-register "FU-CD-label"
        "<Toolbox>/Script-Fu/Decor/Create New/CD label"
        "CD label shape. \nfile:FU_shapes_cd-mask.scm"
        "Akkana Peck"
        "Akkana Peck"
        "December 2002"
        ""
        SF-ADJUSTMENT   "Diameter"       '(800 1 2000 10 50 0 1)
        SF-COLOR        "Color"          '(174 201 255)
        SF-TOGGLE       "Mini CD"        FALSE
        SF-ADJUSTMENT   "Adjust Size of Center %age"    '(100 10 300 1 10 0 0)
)


(script-fu-register "FU-CD-mask"
    "<Toolbox>/Script-Fu/Decor/Shapes/CD mask"
    "Select a CD label shape out of the current layer. \nfile:FU_shapes_cd-mask.scm"
    "Akkana Peck"
    "Akkana Peck"
    "December 2002"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-TOGGLE       "Mini CD"       FALSE
    SF-ADJUSTMENT   "Adjust Size of Center %"   '(100 10 300 1 10 0 0)
)

;end of script