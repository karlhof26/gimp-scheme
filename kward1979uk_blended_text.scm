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

(define (blend-text inimage inlayer text font font-size)
    (set! inimage inimage)
    (gimp-image-undo-group-start inimage)
    (let* (
            (theHeight (car (gimp-image-height inimage)))
            (theWidth (car (gimp-image-width inimage)))
        )
        (gimp-context-set-foreground '(255 255 255))
        (let* (
                (text-layer (car (gimp-text-fontname inimage -1 0 0 text 0 TRUE font-size PIXELS font)))
                (textheight (car (gimp-drawable-height text-layer)))
                (textwidth (car (gimp-drawable-width text-layer)))
            )
            (gimp-selection-layer-alpha text-layer)
            (gimp-selection-invert inimage)
            (let* (
                    (blackshadow (car (script-fu-drop-shadow inimage text-layer 2 2 5 '(0 0 0) 80 0)))
                    (whiteshadow (car (script-fu-drop-shadow inimage text-layer 1 1 0 '(250 250 250) 80 0)))
                )
                (gimp-selection-none inimage)
                (let* (
                        (stroke (car (gimp-layer-new inimage textwidth textheight RGBA-IMAGE "new layer" 100 LAYER-MODE-NORMAL)))
                    )
                    (gimp-image-insert-layer inimage stroke 0 0)
                    
                    (gimp-drawable-fill stroke 3)
                    (gimp-image-lower-layer inimage stroke)
                    (gimp-image-lower-layer inimage stroke)
                    (gimp-image-lower-layer inimage stroke)
                    (gimp-selection-layer-alpha text-layer)
                    (gimp-selection-grow inimage 1)
                    (gimp-context-set-foreground '(0 0 0))
                    (gimp-edit-bucket-fill stroke 0 0 100 0 0 0 0)
                    
                    (let* (
                            (blank (car (gimp-layer-new inimage 256 256
                                RGBA-IMAGE "blank" 100 LAYER-MODE-NORMAL)))
                        )
                        (gimp-image-insert-layer inimage blank 0 0)
                        
                        (gimp-drawable-fill blank 3)
                        (gimp-image-set-active-layer inimage blank)
                        (let* (
                                (merge1 (car (gimp-image-merge-down inimage blank 0)))
                                (merge2 (car (gimp-image-merge-down inimage merge1 0)))
                                (merge3 (car (gimp-image-merge-down inimage merge2 0)))
                                (merge4 (car (gimp-image-merge-down inimage merge3 0)))
                            )
                            (script-fu-add-bevel  inimage merge4 5 0 0)
                            
                            (let* (
                                    (blank2 (car (gimp-layer-new inimage 256 256
                                        RGBA-IMAGE "blank2" 100 LAYER-MODE-NORMAL)))
                                )
                                (gimp-image-insert-layer inimage blank2 0 0)
                                
                                (gimp-drawable-fill blank2 3)
                                (let* (
                                        (bevelled (car (gimp-image-merge-down inimage blank2 0)))
                                    )
                                    (gimp-selection-layer-alpha bevelled)
                                    (let* (
                                            (outshadow (car (script-fu-drop-shadow inimage bevelled 2 2 5 '(0 0 0) 80 0)))
                                            (blank3 (car (gimp-layer-new inimage 256 256
                                                RGBA-IMAGE "blank2" 100 LAYER-MODE-NORMAL)))
                                        )
                                        (gimp-image-insert-layer inimage blank3 0 0)
                                        
                                        (gimp-drawable-fill blank3 3)
                                        (let* (
                                                (final (car (gimp-image-merge-down inimage blank3 0)))
                                                (merge5 (car (gimp-image-merge-down inimage final 0)))
                                            )
                                            (gimp-selection-none inimage)
                                            (gimp-image-crop inimage theWidth  theHeight 0 0)
                                            (let* (
                                                    (end-layer (car (gimp-layer-set-mode merge5 LAYER-MODE-OVERLAY)))
                                                )
                                                (gimp-image-undo-group-end inimage)
                                                (gimp-displays-flush)
                                                
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (gc) ; garbage collect
    )
)

(script-fu-register     "blend-text"
        "<Image>/Script-Fu/Text/Blended Text..."
        "Create Text that blends with the image. \nfile:kward1979uk_blended_text.scm"
        "Karl Ward"
        "Karl Ward"
        "October 2005"
        ""
        SF-IMAGE        "SF-IMAGE"          0
        SF-DRAWABLE     "SF-DRAWABLE"       0
        SF-STRING       "Text"              "Insert text here"
        SF-FONT         "Font"              "Arial Bold"
        SF-ADJUSTMENT   "Font-size"         '(14 1 300 1 10 0 1)
)
            
;end of script