; FU_photo_pastel-portrait.scm 
; version 1.0 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 20/10/2020 on GIMP-2.10.20
;
; 02/14/2014 - convert to RGB if needed 
;=============================================================
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
; by Paul Sherman
; based upon iccii-pastel-image.scm
; which was....
; based on pastel-windows100.scm
; --------------------------------------------------------------------
;  Reference Book
;  Windows100% Magazine October, 2001
;  Tamagorou's Photograph touching up class No.29
;  theme 1 -- Create the Pastel image
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  by Iccii 2001/10/19 <iccii@hotmail.com>
;     - Initial relase
; version 0.2 by Tim Jacobs 2004/04/15
;     - Modified to work for GIMP 2.x
; version 0.3 by Karl Hofmeyr 2022/12/20
;     - Modified for Gimp 2.10.32
;==============================================================



(define (FU-pastel-portrait
            img
            drawable
            detail
            sharpenoption
            lengthA
            amount
            anglemblur
            flatten
        )
    
    (gimp-image-undo-group-start img)
    (if (not (= RGB (car (gimp-image-base-type img))))
                (gimp-image-convert-rgb img))
    (let* (
            (ImageW (car (gimp-drawable-width drawable))) 
            (ImageH (car (gimp-drawable-height drawable))) 
            
            (layer-orig (car (gimp-layer-copy drawable TRUE)))
            (old-selection (car (gimp-selection-save img)))
            (layer-copy0 (car (gimp-layer-copy drawable TRUE)))
            (dummy (gimp-image-add-layer img layer-copy0 -1))
            (dummy2 (if (< 0 (car (gimp-layer-get-mask layer-copy0)))
                  (gimp-layer-remove-mask layer-copy0 MASK-DISCARD)))
            (layer-copy1 (car (gimp-layer-copy layer-copy0 TRUE)))
            (lengthA (if (= lengthA 1) 0 lengthA))
            (dummy3 (begin
                (plug-in-mblur TRUE img layer-copy0 0 lengthA anglemblur 0 0 )
                (plug-in-mblur TRUE img layer-copy0 0 lengthA (+ anglemblur 180) 0 0 ) 
                )
            )
            (layer-copy2 (car (gimp-layer-copy layer-copy0 TRUE)))
            (marged-layer 0)
            (final-layer 0)
            (new-layer 0)
            (working-copy)
            (revisedlengthA 0)
            (finalbackground)
        )
        
        (set! working-copy (car (gimp-layer-copy drawable TRUE)))
        (gimp-image-insert-layer img working-copy 0 -1)
        (if (> lengthA 2)
            (begin
                (set! revisedlengthA (+ lengthA 2))
            )
            (begin
                (set! revisedlengthA 2)
            )
        )
        (plug-in-mblur TRUE img working-copy 0 revisedlengthA anglemblur (/ ImageW 2) (/ ImageH 2))
        (plug-in-mblur TRUE img working-copy 0 revisedlengthA (+ anglemblur 120) 500  500 )
        (gimp-item-set-name working-copy "working-copy")
        
        
        (gimp-image-insert-layer img layer-copy2 0 -1)
        (gimp-image-insert-layer img layer-copy1 0 -1)
        (gimp-image-insert-layer img layer-orig 0 -1)
        (gimp-item-set-name layer-copy2 "layer copy2")
        (gimp-item-set-name layer-copy1 "layer copy1")
        (gimp-item-set-name layer-orig "layer orig")
        
        (plug-in-gauss-iir TRUE img layer-copy1 (- 20 detail) TRUE TRUE) ;
        
        (plug-in-edge TRUE img layer-copy1 10.0 1 1) ; was 10.0 1 0)
        
        (gimp-layer-set-mode layer-copy1 LAYER-MODE-DIVIDE)
        
        ;(gimp-message "line136")
        ;(gimp-displays-flush)
        ;(quit)
        
        (set! marged-layer (car (gimp-image-merge-down img layer-copy1 EXPAND-AS-NECESSARY)))
        (gimp-layer-set-mode marged-layer LAYER-MODE-HSV-VALUE)
        (gimp-item-set-name marged-layer "marged layer")
        ;(gimp-message "line143")
        ;(gimp-displays-flush)
        ;(quit)
        
        (if (= sharpenoption TRUE)
            (begin
                (plug-in-unsharp-mask TRUE img layer-copy0 (+ 1.5 (* lengthA 0.8)) amount 128) ; was length/5 and threshold 0
            )
        )
        
        ;(gimp-message "line152")
        ;(gimp-displays-flush)
        ;(quit)
        
        (set! final-layer (car (gimp-image-merge-down img marged-layer EXPAND-AS-NECESSARY)))
        (gimp-item-set-name final-layer "final layer")
        (gimp-layer-set-opacity final-layer 95.5)
        
        
        (set! finalbackground (car (gimp-layer-new img ImageW ImageH RGBA-IMAGE "final background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer img finalbackground 0 2)
        (gimp-drawable-fill finalbackground FILL-WHITE)
        (gimp-item-set-name finalbackground "final background")
        
        ;(gimp-message "line165")
        ;(gimp-displays-flush)
        ;(quit)
        
        ;(gimp-selection-load old-selection)
        ;(gimp-edit-copy final-layer)
        ;(gimp-image-remove-layer img final-layer)
        ;(gimp-floating-sel-anchor (car (gimp-edit-paste drawable 0)))
        ;(gimp-selection-load old-selection)
        ;(gimp-image-remove-channel img old-selection)
        
        ;(set! new-layer (car(gimp-image-get-active-layer img)))
        ;(gimp-layer-set-opacity new-layer 30)
        ;(gimp-item-set-name new-layer "Pastel Layer")
        ;
        (gimp-image-lower-layer img layer-orig)
        (gimp-image-lower-layer img layer-orig)
        (gimp-item-set-name layer-orig "Original Image")
        
        (if (= flatten TRUE)
            (gimp-image-flatten img)
        )
        
        ;; Clean up 
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gc) ; garbage cleanup; memory cleanup
    )
)

(script-fu-register
    "FU-pastel-portrait"
    "Pastel Portrait"
    "Creates a slightly Painterly version of photo. \nfile:FU_photo_pastel-portrait.scm"
    "Paul Sherman <psherman2001@gmail.com>"
    "Paul Sherman"
    "Feb 2014"
    "*"
    SF-IMAGE      "Image"         0
    SF-DRAWABLE   "Drawable"      0
    SF-ADJUSTMENT "Detail Level (higher is sharper)"  '(4.0 0 20.0 0.1 0.5 1 1)
    SF-TOGGLE     "Sharpen sketch"                  TRUE
    SF-ADJUSTMENT "Sharpen radius length"           '(12 0 32 1 1 0 1)
    SF-ADJUSTMENT "Shapen sharpness Amount"         '(2.2 0 5.0 0.1 0.5 1 1)
    SF-ADJUSTMENT "Color blur Angle"                '(45 0 180 1 15 0 0)
    SF-TOGGLE     "Flatten image"                   FALSE
 )
 
(script-fu-menu-register "FU-pastel-portrait" "<Toolbox>/Script-Fu/Photo/Effects")

;end of script