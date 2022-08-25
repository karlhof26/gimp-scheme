; Mirror Image
; Created by Fencepost
 
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License 
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
; Rel 0.02 - karlhof26 update to enable Gimp 2.10.18 operation
;
;
;

(define (mirror-image image drw horv side)
  (let* (
            (originalLayer (car (gimp-image-get-active-layer image)))
            (copyLayer 0)
            (width (car (gimp-drawable-width originalLayer)))
            (height (car (gimp-drawable-height originalLayer)))
            
            (cX (/ width 2))
            (cY (/ height 2))
            
            
       )
        ; Allow for the GIMP settings to be restored prior to running the script
        (gimp-context-push)
        
        (gimp-image-undo-group-start image)
        (gimp-layer-add-alpha originalLayer)
        (set! copyLayer (car (gimp-layer-copy originalLayer TRUE)))
        (gimp-image-insert-layer image copyLayer 0 -1)
        
        (if (= horv 0)
            (begin
                (gimp-image-select-rectangle image CHANNEL-OP-ADD 0 0 cX height)
                (if (= side 0)
                    (begin
                        (gimp-selection-invert image)      
                        (gimp-edit-clear copyLayer)
                        (gimp-selection-none image)
                        (gimp-item-transform-flip-simple copyLayer ORIENTATION-HORIZONTAL TRUE cX)
                        
                        
                    )
                    (begin
                        (gimp-edit-clear copyLayer)
                        (gimp-selection-none image)
                        (gimp-item-transform-flip-simple copyLayer ORIENTATION-HORIZONTAL TRUE cX)
                    )
                
                
                )
            )
        )
        
        (if (= horv 1)
            (begin
                (gimp-image-select-rectangle image CHANNEL-OP-ADD 0 0 width cY)
                (if (= side 0)
                    (begin
                        (gimp-selection-invert image)      
                        (gimp-edit-clear copyLayer)
                        (gimp-selection-none image)
                        (gimp-item-transform-flip-simple copyLayer ORIENTATION-VERTICAL TRUE cY)
                        
                        
                    )
                    (begin
                        (gimp-edit-clear copyLayer)
                        (gimp-selection-none image)
                        (gimp-item-transform-flip-simple copyLayer ORIENTATION-VERTICAL TRUE cY)
                    )
                
                
                )
            )
        )
        
        
        
        
        
        (gimp-image-merge-down image copyLayer CLIP-TO-IMAGE)
        (set! originalLayer (car (gimp-image-get-active-layer image)))
        
        
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        
        ; Return GIMP settings back to those used prior to script
        (gimp-context-pop)
        
  )
)

(script-fu-register "mirror-image"
    "<Toolbox>/Script-Fu/Render/Mirror Image..."
    "Mirror an image around horizontal or vertical axis. \nfile:fp-mirror-image.scm"
    "Art Wade"
    "Art Wade"
    "September 17, 2016"
    "RGB*"
    SF-IMAGE       "Image"               0
    SF-DRAWABLE    "Drawable"            0
    SF-OPTION       "Horizontal or Vertical?"       '("Horizontal"
                     "Vertical")
    SF-OPTION       "Left or Right (Top or Bottom)?"   '("Left (Top if Vertical)"
                     "Right (Bottom if Vertical)")
)
               
; end of script
