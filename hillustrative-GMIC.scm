; This script contains the command syntax for G'MIC git post v1.79 
; refer to the original post for scripts for v1.79 & older
; https://discuss.pixls.us/t/highlight-bloom-and-photoillustration-look/2509/65
; hiillustrative-GMIC.scm   
;
; last modified/tested by Karl Hofmeyr
; 05/23/2020 on GIMP-2.10.18
;
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;   Linux
;   /home/yourname/.gimp-2.8/scripts  
;   or
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
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
;
; Updated for GIMP 2.10.18
;==============================================================

(define (script-fu-hillustrative
            theImage
            baseLayer
            with_fast_tone_mapping
            desaturate
            vintage
        )
        
    (let* (
              (blurLayer 0)
              (detailLayer 0)
              (graphicnovelLayer 0)
              (tempvar 1)
              (templayer2 0)
              (templayer3 0)
              (templayer4 0)
        )
        ; Initialize an undo, so the process can be undone with a single undo
        (gimp-image-undo-group-start theImage)
        
        
        (set! blurLayer (car (gimp-layer-copy baseLayer TRUE)))
        
        (gimp-image-insert-layer theImage blurLayer 0 -1) ; duplicate base layer
        (gimp-item-set-name blurLayer "Blur layer") ; rename the dupe layer 
        
        (set! templayer2 (car (gimp-layer-copy baseLayer TRUE)))
        (gimp-image-insert-layer theImage templayer2 0 -1)
        (gimp-item-set-name templayer2 "templayer2")
        (set! templayer3 (car (gimp-layer-copy baseLayer TRUE)))
        (gimp-image-insert-layer theImage templayer3 0 -1)
         (gimp-item-set-name templayer3 "templayer3")
        (set! templayer4 (car (gimp-layer-copy baseLayer TRUE)))
        (gimp-image-insert-layer theImage templayer4 0 -1)
        (gimp-item-set-name templayer4 "templayer4")
        
        ;(plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -fx_crystal 50,0.2,20")
        ;(gimp-displays-flush)
        ;(gimp-message "ntoast line 72")
        ;(gimp-image-set-active-layer theImage blurLayer)
        ;(plug-in-gmic-qt 1 theImage blurLayer 1 0 "cartoon")
        
        (gimp-displays-flush)
        (gimp-image-set-active-layer theImage templayer2)
        (plug-in-gmic-qt 1 theImage templayer2 1 0 "-v - -fx_dreamsmooth 4,1,2,0.8,0,0.8,1,24,1")
        (gimp-displays-flush)
        
        
        (gimp-image-set-active-layer theImage templayer3)
        (plug-in-gmic-qt 1 theImage templayer3 1 0 "-v - -cartoon")
        
        
        (gimp-image-set-active-layer theImage templayer4)
        (plug-in-gmic-qt 1 theImage templayer4 1 0 "-v - -fx_crystal 50,0.2,20")
        (gimp-item-set-visible templayer2 FALSE)
        (gimp-item-set-visible templayer3 FALSE)
        (gimp-item-set-visible templayer4 FALSE)
        
        (gimp-image-set-active-layer theImage blurLayer)
        ;(plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -fx_smooth_bilateral 45.2,7,6,1")
        ;(gimp-displays-flush)
        ;(gimp-image-set-active-layer theImage blurLayer)
        ;(plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - fx_smooth_bilateral")
        ;(gimp-displays-flush)
        (plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -fx_smooth_anisotropic 103,0.13,0.63,0.6,2.35,0.8,30,2,1,1,1,1,1,24,1") ; apply anisotropic smoothing 20,0.16,0.63,0.6,2.35,0.8,30,2,0,1,1,0,1,24"
        (gimp-layer-set-mode blurLayer LAYER-MODE-GRAIN-EXTRACT-LEGACY) ; set mode to grain extract
        
        (gimp-displays-flush)
        
        
        ;(set! detailLayer (car (gimp-layer-new-from-visible theImage theImage "Detail layer")))
        (set! detailLayer (car (gimp-layer-copy blurLayer TRUE)))
        (gimp-item-set-name detailLayer "DetailLayer")
        
        (if (= desaturate TRUE)
            (begin
                ;(gimp-message "lower saturation")
                (gimp-image-set-active-layer theImage blurLayer)
                (gimp-drawable-hue-saturation blurLayer HUE-RANGE-ALL 0 0 -40 0) ; decrease saturation
            )
        )
        (gimp-displays-flush)
        
        (if (= vintage TRUE)
            (begin
                ;(gimp-message "vintage")
                (gimp-image-set-active-layer theImage blurLayer)
                
                (plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -fx_mix_rgb 1.1,9.9,0,1,0,0,0.7,-10.0,0.2,0,9.9,1") ; vintage
                (gimp-layer-set-mode blurLayer LAYER-MODE-GRAIN-EXTRACT-LEGACY)
            )
        )
        (gimp-displays-flush)
        
        
        (if (= with_fast_tone_mapping TRUE)
            (begin
                ; code to do if selected
                ;gimp-message "fast tone map")
                (gimp-image-set-active-layer theImage blurLayer)
                ;(gimp-layer-set-mode blurLayer LAYER-MODE-GRAIN-EXTRACT-LEGACY)
                (plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -fx_map_tones_fast 20,0.2,11,1")
                
            )
        )
        (gimp-displays-flush)
        
        
        (gimp-image-insert-layer theImage detailLayer 0 -1)
        (gimp-layer-set-mode detailLayer LAYER-MODE-GRAIN-MERGE) ; set layer mode to grain merge
        
        (gimp-layer-set-mode blurLayer LAYER-MODE-NORMAL) ; set blur layer mode back to to normal
        (gimp-item-set-name blurLayer "Simple Local Contrast") ; rename the dupe layer 
        (gimp-displays-flush)
        
        
        (gimp-image-set-active-layer theImage blurLayer)     
        ;;(plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -simplelocalcontrast_p 25,1,50,1,1,1,1,1,1,1,1,1,1") ; apply simple local contrast
        (plug-in-gmic-qt 1 theImage blurLayer 1 0 "-v - -afre_localcontrast 1,40,1,1")
        ;(gimp-message "auto contrast")
        ;(FU-auto-contrast 1 theImage blurLayer TRUE)
        (gimp-displays-flush)
        
        
        (set! graphicnovelLayer (car (gimp-layer-copy blurLayer TRUE)))
        (gimp-image-insert-layer theImage graphicnovelLayer 0 -1) ; duplicate base layer
        (gimp-item-set-name graphicnovelLayer "Graphic Novel") ; rename the dupe layer
        (plug-in-gmic-qt 1 theImage graphicnovelLayer 1 0 "-v - -fx_graphic_novelfxl 1,5.3,15.08,5.9,20.32,0,1.028,190.1,0,1,0.146,0.787,0.44,0,0,2,1,1,1,1.257,0.371,1.747,1") ; apply Graphic Novel 1,2,6,5,20,0,1.02857,190.1,0,1,0.0761905,0.857143,0,0,0,2,1,1,1,1.25714,0.371429,1.04762,1"
        (gimp-layer-set-opacity graphicnovelLayer 50)
        
        (gimp-image-raise-item-to-top theImage detailLayer)
        
        ;Ensure the updated image is displayed now
        (gimp-displays-flush)
        
        (gimp-image-undo-group-end theImage)
    )
) ;end define

(script-fu-register "script-fu-hillustrative"
    "<Image>/Script-Fu/Effects/Hillustrative..."
            "This script tries emulating a photo-illustrative look à la Dave Hill, 
using G'MIC filters for local contrast and highlight bloom, and grain extract/merge for recovering 
details. After the script has finished its job, it will leave 4 layers: the original base layer, 
the Simple Local Contrast layer, the Graphic Novel, and the Detail layer (from bottom to top). \n file:hiillustrative-GMIC.scm"
            "Sébastien Guyader"
            "Sébastien Guyader"
            "December 2016"
            "*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Drawable"          0
    SF-TOGGLE       "With tone mapping" TRUE
    SF-TOGGLE       "Desaturate"        FALSE
    SF-TOGGLE       "Vintage tone"      FALSE
)

;end of script