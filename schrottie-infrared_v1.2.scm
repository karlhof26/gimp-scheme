;
; Infrared Effect, v1.1 
;
; Maik Bischoff (schrottie@gmail.com)
; (C) 2015, berlin, Germany
;
; Find out more at https://blog.dafb-o.de/howto-infraroteffekte-mit-gimp-erzeugen/
; 
; This script was tested with Gimp 2.10.22
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
;

(define (script-fu-infrared-effect-schr inImage inLayer inColor inRadius inDelta useGrad mapGrad pixelizeSize)
    
    (gimp-image-undo-group-start inImage)
    
    (let* (
            (CopyLayer (car (gimp-layer-copy inLayer TRUE)))
          )
        (gimp-image-insert-layer inImage CopyLayer 0 -1)
        (gimp-item-set-name CopyLayer "ColorLayer")
        (gimp-item-set-name inLayer "Infrared-Image")
        (gimp-layer-set-opacity CopyLayer inColor)
        (gimp-layer-set-mode CopyLayer LAYER-MODE-GRAIN-MERGE) ; was 21
        (plug-in-sel-gauss TRUE inImage inLayer inRadius inDelta)
        (plug-in-colors-channel-mixer 1 inImage inLayer TRUE 1.5 1.2 -1.3 0 0 0 0 0 0) ; was TRUE 1.0 1.0 -1.0 0 0 0 0 0 0
        
        (gimp-displays-flush)
        (plug-in-pixelize2 1 inImage inLayer pixelizeSize pixelizeSize)
        
        (if (= useGrad TRUE)
            (begin
                (gimp-context-set-gradient mapGrad)
                (plug-in-gradmap 1 inImage inLayer)
            )
        )
        
        (gimp-displays-flush)
        (gimp-image-merge-visible-layers inImage 2)
    )
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
)

(script-fu-register
    "script-fu-infrared-effect-schr"
    "Infrared effect"
    "Infrared effect. 21 is a good value for pixelize. \nfile:schrottie-infrared_v1.2.scm"
    "Maik Bischoff"
    "copyright 2015, Maik Bischoff"
    "August 18, 2015" 
    "RGB*"
    SF-IMAGE        "The Image"             0
    SF-DRAWABLE     "The Layer"             0
    SF-ADJUSTMENT   "Intensity Amount (Farbintensität festlegen)" '(30.0 15.0 40.0 1.0 0 2 0)
    SF-ADJUSTMENT   "Radius (Weichzeichner)"            '(5.0 1.0 20.0 1.0 0 2 0)
    SF-ADJUSTMENT   "Delta (Weichzeichner)"             '(25.0 10.0 60.0 1.0 0 2 0)
    SF-TOGGLE       "Use Gradient" TRUE
    SF-GRADIENT     "Gradient"  "Full saturation spectrum CW"
    SF-ADJUSTMENT   "Pixelise size"             '(21 1 60 1 5 0 0)
)

(script-fu-menu-register "script-fu-infrared-effect-schr" "<Toolbox>/Script-Fu/Effects/")

; Changelog:
;
; v1.0 - 20150818
; -- initial version
;
; v1.1 - 20150820
; -- plug-in-sel-gauss: delta from 50.0 to 10.0 for less blur 
;
; v1.2 - 20150828
; -- plug-in-sel-gauss: adjustments added 
