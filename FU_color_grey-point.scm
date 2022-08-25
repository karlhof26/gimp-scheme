; FU_color_grey-point.scm 
; version 3.2 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
;
; 12/14/2008 - added ability to set the foreground color
; from the script dialog
;
; tweaked for GIMP-2.4.x (updated deprecated functions) 
; by Paul Sherman 10/24/2007, later moved menu location
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
; Grey point is a script for The GIMP
; Description: flexible white balance correction tool
; Version 2.0
; Last changed: 15.02.2007
; Copyright (C) 2004 Dr. Martin Rogge <marogge@onlinehome.de>
;
; based on:
; White/Black balance script  for GIMP 1.2
; Copyright (C) 2002 Iccii <iccii@hotmail.com>
;
; Version 3.0
; updated on 26/05/2020 to work with Gimp 2.10.18
;==============================================================


(define (FU-grey-point image drawable mode changeFG fgcolor)
    
    (define (round x) (trunc (+ x 0.5)))
    (define (interpolate source target i)
        (if (= source 0) target (max (min (round (* (/ target source) i))  255) 0)))
    
    (if (= changeFG TRUE)
        (gimp-context-set-foreground fgcolor)
    )
    
    
    
  (let* (
            (fg             (car (gimp-context-get-foreground)))
            (bg             (car (gimp-context-get-background)))
            
            (source_red   (car   fg))
            (source_green (cadr  fg))
            (source_blue  (caddr fg))
            
            (average      (round (/ (+ source_red source_green source_blue) 3)))
            
            (target_red   (cond ((= mode 0) average)
                            ((= mode 1) 255)
                            ((= mode 2) (car bg))
                          )
            )
            (target_green (cond ((= mode 0) average)
                                ((= mode 1) 255)
                                ((= mode 2) (cadr bg))
                           )
            )
            (target_blue  (cond ((= mode 0) average)
                                ((= mode 1) 255)
                                ((= mode 2) (caddr bg))
                          )
            )
            
            (i             0)
            (num_bytes     256)
            
            (red-curve    (cons-array num_bytes 'byte))
            (green-curve  (cons-array num_bytes 'byte))
            (blue-curve   (cons-array num_bytes 'byte))
        )
    
    (gimp-image-undo-group-start image)
    (if (not (= RGB (car (gimp-image-base-type image))))
            (gimp-image-convert-rgb image)
    )
    
    (while (< i num_bytes)
        (aset red-curve   i (interpolate source_red   target_red   i))
        (aset green-curve i (interpolate source_green target_green i))
        (aset blue-curve  i (interpolate source_blue  target_blue  i))
        (set! i (+ i 1))
    )
    
    (gimp-curves-explicit drawable HISTOGRAM-RED   num_bytes red-curve  )
    (gimp-curves-explicit drawable HISTOGRAM-GREEN num_bytes green-curve)
    (gimp-curves-explicit drawable HISTOGRAM-BLUE  num_bytes blue-curve )
    
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    (gc) ; an array was used so garbage collect
  )
)

(script-fu-register
    "FU-grey-point"
    "<Toolbox>/Script-Fu/Colors/Color Move Grey Point"
    "Linear conversion of RGB colour space. The purpose of this script is to transform the foreground colour (as chosen with the 
 colour pick tool) to a chosen target colour. The entire RGB colour space is linearly converted to achieve the desired mapping
 of the foreground colour. There are three possible choices for the target colour:\n(a) a grey colour of the same density as the
 foreground colour (desaturation)\n(b) white\n(c) the background colour (as chosen with the colour pick tool)\nThe main
 application area of this script should be the correction of the colour temperature of digital images. Your feedback is welcome.)
 \nfile:FU_color_grey-point.scm"
    "Dr. Martin Rogge <marogge@onlinehome.de>"
    "Dr. Martin Rogge"
    "29/09/2004 to 15/02/2007"
    "*"
    SF-IMAGE    "Image"         0
    SF-DRAWABLE "Drawable"      0
    SF-OPTION   "Foreground transformation"  '("Desaturation" "White" "Background")
    SF-TOGGLE   "Change Foreground Color"    FALSE
    SF-COLOR    "New Foreground Color" '(0 0 0)
)

; end of script