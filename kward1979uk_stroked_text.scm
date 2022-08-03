;Stroked Text script
;Karl Ward
;21:53 20/11/2007
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 

(define (kward1979uk-stoked-text inimage indraw text font font-size stroke text-colour stroke-colour)
    (let* (
            (theImage inimage)
            (theDraw indraw)
          )
        (gimp-image-undo-group-start theImage)
        (gimp-context-set-foreground text-colour)
        (let* (
                (text-layer (gimp-text-fontname theImage -1 0 0 text 0 TRUE font-size PIXELS font))
                (textheight (car (gimp-drawable-height (car text-layer))))
                (textwidth (car (gimp-drawable-width (car text-layer))))
            )
            (gimp-layer-resize (car text-layer) (+ textwidth (* 2 stroke)) (+ textheight (* 2 stroke))
                    stroke stroke)
            (gimp-layer-translate (car text-layer) stroke stroke)
            (gimp-selection-layer-alpha (car text-layer))
            (gimp-selection-grow theImage stroke)
            (gimp-context-set-foreground stroke-colour)
            (let* (
                    (textheight2 (car (gimp-drawable-height (car text-layer))))
                    (textwidth2 (car (gimp-drawable-width (car text-layer))))           
                    
                    (stroke-layer (gimp-layer-new theImage textwidth2 textheight2 1 "inset" 100 0) )
                  )
                (gimp-drawable-fill (car stroke-layer) 3)
                (gimp-image-add-layer theImage (car stroke-layer) -1)
                (gimp-bucket-fill (car stroke-layer) 0 LAYER-MODE-NORMAL 100 0 0 0 0 )
                (gimp-image-lower-layer theImage (car stroke-layer))
                (gimp-selection-none theImage)
                (gimp-image-merge-down theImage (car text-layer) 0 )
                
                (gimp-image-undo-group-end theImage)
                (gimp-displays-flush)
                
            )
        )
        
    )
)

(script-fu-register "kward1979uk-stoked-text"
        "<Toolbox>/Script-Fu/Text/Stroked-text..."
        "Creates text that has been stroked around. Text with a coloured border. \nfile:kward1979uk_stroked_text.scm"
        "Karl Ward"
        "Karl Ward"
        "Oct 2007"
        ""
        SF-IMAGE        "SF-IMAGE"          0
        SF-DRAWABLE     "SF-DRAWABLE"       0
        SF-STRING       "Text"              "Any Text"
        SF-FONT	        "Font"              "Arial Bold"
        SF-ADJUSTMENT   "Font-size"         '(120 1 300 1 10 0 1)
        SF-ADJUSTMENT   "Stroke"            '(5 1 20 1 1 1 0)
        SF-COLOR        "Text colour"       '(255 255 255)
        SF-COLOR        "Stroke colour"     '(0 0 0)
)
