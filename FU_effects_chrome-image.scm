; FU_effects_chrome-image.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 03/09/2020 on GIMP-2.10.20 by karlhof26
;
; 12/14/2008 took out color layer - made use confusing for me
;
; edited - 11/27/2008 by Paul Sherman
; removing deprecated functions
;
; 10-/15/2010 - tweaked default settings, emboss was to much...
; 02/14/2014 - convert to RGB if needed
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
; Chrome image script  for GIMP 1.2
; Copyright (C) 2001-2002 Iccii <iccii@hotmail.com>
;==============================================================


(define (FU-chrome-image
            img
            drawable
            contrast
            deform
            random_b
            emboss?
            bright
            stopearly
    )
    
    (gimp-image-undo-group-start img)
    (if (not (= RGB (car (gimp-image-base-type img))))
            (gimp-image-convert-rgb img)
    )
            
    (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-context-get-foreground)))
            (image-type (if (eqv? (car (gimp-drawable-is-gray drawable)) TRUE)
                         GRAYA-IMAGE
                         RGBA-IMAGE))
            (point-num (round (+ 2 (* random_b 2))))
            (step (round (/ 255 (+ (* random_b 2) 1))))
            (control_pts (cons-array (* point-num 2) 'byte))
            (count 0)
        )
        
        
        (if (eqv? (car (gimp-drawable-is-gray drawable)) FALSE)
            (gimp-drawable-desaturate drawable DESATURATE-LUMINANCE)
        )
        (plug-in-gauss-iir2 1 img drawable deform deform)
        (gimp-displays-flush)
        
        
        (if (= emboss? TRUE)
            (plug-in-emboss 1 img drawable 30 45.0 35 1)
            ;(gimp-drawable-levels drawable HISTOGRAM-VALUE 0.3 0.9 TRUE 1.15 0.0 1.0 TRUE)
            
        )
        (gimp-displays-flush)
        (if (= stopearly TRUE)
            (begin
            )
            (begin
                (aset control_pts 0 0)
                (aset control_pts 1 0); was 1 0
                
                (while (< count random_b)
                    
                    (aset control_pts (+ (* count 4) 2) (round (* step (+ (* count 2) 1))))
                    (aset control_pts (+ (* count 4) 3) (+ 128 contrast))
                    (aset control_pts (+ (* count 4) 4) (round (* step (+ (* count 2) 2))))
                    (aset control_pts (+ (* count 4) 5) (- 128 contrast))
                    (set! count (+ count 1))
                )
                
                (aset control_pts (- (* point-num 2) 2) 255) ; was 255
                (aset control_pts (- (* point-num 2) 1) 255) ; was 255
                
                (gimp-curves-spline drawable HISTOGRAM-VALUE (* point-num 2) control_pts)
            )
        )
        (gimp-displays-flush)
        ;(quit)
        
        (if (= bright TRUE)
                (begin
                    
                    (gimp-drawable-brightness-contrast drawable 0.32 0.39)
                    (gimp-drawable-levels drawable HISTOGRAM-VALUE 0.40 0.99 TRUE 1.88 0.0 1.0 TRUE)
                    (gimp-drawable-brightness-contrast drawable 0.0 0.48)
                )
            
        )
        (gimp-context-set-foreground old-fg)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gc) ; garbage collect; an array was used 
    )
)

(script-fu-register
    "FU-chrome-image"
    "<Toolbox>/Script-Fu/Effects/Chrome Image"
    "Create chrome image.  Useful when you want to create metallic surfaces. Works best on transparent layer with thick lines that are not black. \nfile:FU_effects_chrome-image.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2002, Feb"
    "*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-ADJUSTMENT "Contrast variation"  '(40 0 127 1 1 0 0)
    SF-ADJUSTMENT "Deformation"         '(23 1 50 1 10 0 0)
    SF-ADJUSTMENT "Ramdomeness"         '(4 1 7 1 10 0 1)
    SF-TOGGLE     "Enable Emboss"   TRUE
    SF-TOGGLE     "Bright Chrome"   FALSE
    SF-TOGGLE     "Stop after emboss" FALSE
)

; end of script