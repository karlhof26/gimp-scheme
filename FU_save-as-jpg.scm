; FU_save-as-jpg.scm 
; version 3.0 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/03/2014 on GIMP-2.8.10
; 27/10/2020 on GIMP-2.10.22
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
; Original script by Saul Goode
; script-fu-quick-jpeg-save.scm
; http://chiselapp.com/user/saulgoode/repository/script-fu/home
;==============================================================


(define (FU_save-as-jpg image) 
  (let* (
            (nameparts (strbreakup (car (gimp-image-get-filename image)) "."))
            (filename "")
        )
    (unless (and (> (length nameparts) 1)
                 (member (car (last nameparts)) '("jpg" "jpeg" "JPG" "JPEG"))
                 )
        (set! nameparts (append (butlast nameparts) '("jpg")))
    )
    (set! filename (unbreakupstr nameparts "."))
    (let* ( (buffer (car (gimp-edit-named-copy-visible image "jpg buffer")))
            (new-image (car (gimp-edit-named-paste-as-new buffer)))
            (layer (car (gimp-image-flatten new-image)))
          )
        (unless (zero? (car (gimp-image-base-type new-image)))
            (gimp-image-convert-rgb new-image)
        )
         
        ;(file-jpeg-save RUN-NONINTERACTIVE 
        ;    new-image 
        ;    layer
        ;    filename 
        ;    filename 
        ;    0.92 ; JPEG compression level
        ;    0.05 ; smoothing was0
        ;    1 ; optimize 
        ;    0 ; progressive 
        ;    "" ; comment 
        ;    2 ; subsmp (0-4) ; was0
        ;    1 ; baseline 
        ;    0 ; restart 
        ;    1 ;dct was 0
        ;)
        
        (gimp-file-save RUN-NONINTERACTIVE 
            new-image 
            layer
            filename 
            filename 
        )
        (gimp-image-delete new-image)
        
        (gimp-buffer-delete "jpg buffer")
        (gimp-image-clean-all image)
        (gimp-displays-flush)
        (gc) ; garbage cleanup
    )
  )
)

(script-fu-register "FU_save-as-jpg"
    "Fast Save as JPG"
    "Fast Save the image as a JPG file\nWarning: Removes EXIF data. Only 1 . is allowed. Save Time in (windows) folders does not update but can be found in Gimp File>Open dialog. For more options and a proper file overwrite protected dialog, use the FILE > EXPORT AS menu item when saving as a JPG.\nfile:FU_save-as-jpg.scm"
    "Paul Sherman"
    "Paul Sherman"
    "February 2014"
    "*"
    SF-IMAGE    "Image"       0
)
  
(script-fu-menu-register "FU_save-as-jpg"
            "<Image>/File/Save/"
)

;end of script