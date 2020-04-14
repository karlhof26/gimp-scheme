;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; bumpmylogo.scm
; Version 0.4 (For Gimp  2.10,18) 
; A Script-Fu that create a bumpmapped Text or Shape
;
; Copyright (C) 2005-2008 Denis Bodor <lefinnois@lefinnois.net>
; All rights reserved.
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * Neither the name of the University of California, Berkeley nor the
;       names of its contributors may be used to endorse or promote products
;       derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-bumpmy-logo-effect    img
                    basetext
                    text-color
                    boolrippleh
                    boolripplev
                    bnoise
                    bplasma
        )
    
  (let* (
            (width (car (gimp-drawable-width basetext)))
            (height (car (gimp-drawable-height basetext)))
            
            (fond (car (gimp-layer-new   img
                        width height RGBA-IMAGE
                        "Background" 100 LAYER-MODE-NORMAL)))
            (damap (car (gimp-layer-new  img
                        width height RGB-IMAGE
                        "Map" 100 LAYER-MODE-NORMAL)))
            (innermap (car (gimp-layer-new  img
                        width height RGB-IMAGE
                        "iMap" 100 LAYER-MODE-NORMAL)))
            (chantext "")
            (masktext "")
        )
        
        
        (gimp-context-push)
        
        ; filling back with background
        (gimp-context-set-background '(255 255 255))
        (gimp-selection-none img)
        (script-fu-util-image-resize-from-layer img basetext)
        (gimp-image-add-layer img fond 1)
        (gimp-edit-clear fond)
        
        ; correcting resizing effect on background
        (gimp-context-set-foreground '(255 255 255))
        (gimp-layer-resize-to-image-size fond)
        (gimp-edit-fill fond FILL-FOREGROUND)
        
        ;(gimp-message (number->string width))
        ;(gimp-message (number->string height))
        
        ; waving/rippling the text
        (if (= boolrippleh TRUE)
            ;Horizontal ripple
            (plug-in-ripple 1 img basetext 26 2 0 0 0 TRUE FALSE)
        )
        (if (= boolripplev TRUE)
            ;Vertical ripple
            (plug-in-ripple 1 img basetext 26 2 1 0 0 TRUE FALSE)
        )
        (plug-in-gauss-rle2 1 img basetext 1 1)
        
        ; save the selection
        (gimp-selection-layer-alpha basetext)
        (set! chantext (car (gimp-selection-save img)))
        (gimp-selection-none img)
        
        ; creating map
        (gimp-image-insert-layer img damap 0 1)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-fill damap FILL-FOREGROUND)
        
        (gimp-selection-load chantext)
        (gimp-selection-grow img 15)
        (gimp-selection-invert img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill damap FILL-FOREGROUND)
        (gimp-selection-none img)
        (plug-in-gauss-rle2 1 img damap 27 27)
        (gimp-selection-load chantext)
        (gimp-edit-fill damap FILL-FOREGROUND)
        (gimp-selection-none img)
        (plug-in-gauss-rle2 1 img damap 2 2)
        
        (gimp-context-set-foreground '(128 128 128))
        (gimp-selection-all img)
        (gimp-edit-fill fond FILL-FOREGROUND)
        (gimp-selection-none img)
        
        (if (= bplasma TRUE)
            ;plasma
            (plug-in-plasma 1 img fond 0 1.0)
            ; else?
            ;(gimp-desaturate fond)
        )
        (if (= bnoise TRUE)
            (plug-in-noisify 1 img fond 1 0.2 0.2 0.2 0)
        )
        
        (gimp-drawable-desaturate fond DESATURATE-LUMINOSITY)
        
        ; apply bumpmap
        (plug-in-bump-map 1
            img
            fond
            damap
            135
            42 ; elevation
            33
            0
            0
            0
            0
            1
            0
            0) ; was LINEAR
            
        ; creating second map (inner shape)
        (gimp-image-insert-layer img innermap 0 1)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-fill innermap FILL-FOREGROUND)
        (gimp-selection-load chantext)
        (gimp-selection-shrink img 3)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill innermap FILL-FOREGROUND)
        (gimp-selection-none img)
        (plug-in-gauss-rle2 1 img innermap 6 6)
        
        (gimp-context-set-foreground text-color)
        (gimp-edit-fill basetext FILL-FOREGROUND)
        (plug-in-bump-map 1
            img
            basetext
            innermap
            135
            32
            5
            0
            0
            0
            0
            1
            1
            0) ; was LINEAR
        (gimp-selection-load chantext)
        (gimp-selection-shrink img 2)
        (set! masktext (car (gimp-layer-create-mask basetext ADD-MASK-SELECTION)))
        (gimp-layer-add-mask basetext masktext)
        (gimp-selection-none img)
        (plug-in-gauss-rle2 1 img masktext 1 1)
        
        (gimp-image-raise-layer img fond)
        (gimp-image-raise-layer img fond)
        
        (gimp-context-pop)
    )
)



(define (script-fu-bumpmy-logo-alpha img
                    text-layer
                    text-color
                    boolrippleh
                    boolripplev
                    bnoise
                    bplasma
                        )
    
    (begin
        (gimp-image-undo-group-start img)
        (apply-bumpmy-logo-effect img text-layer text-color boolrippleh boolripplev bnoise bplasma)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(gimp-message-set-handler 1)

(script-fu-register  "script-fu-bumpmy-logo-alpha"
            "Bump my..."
            "Create a bumpmapped shape or logo \n file:bumpmy.scm"
            "karlhof26"
            "karlhof26"
            "06/03/2020"
            ""
            SF-IMAGE    "Image"               0
            SF-DRAWABLE "Drawable"            0
            SF-COLOR    "Shape Color"         '(96 128 59)
            SF-TOGGLE   "Ripple Horiz."       TRUE
            SF-TOGGLE   "Ripple Vert."        TRUE
            SF-TOGGLE   "Background Noise"    TRUE
            SF-TOGGLE   "Background Plasma"   TRUE
            )

(script-fu-menu-register "script-fu-bumpmy-logo-alpha"
            "<Image>/Script-Fu2/Alpha to Logo")

(define (script-fu-bumpmy-logo   font
                text
                text-color
                boolrippleh
                boolripplev
                bnoise
                bplasma
                size
        )
    
  (let* (
            (img (car (gimp-image-new 256 256 RGB)))
            (border (/ size 4))
            (text-layer (car (gimp-text-fontname img
                            -1 0 0 text border TRUE 
                            size PIXELS font)))
            (width (car (gimp-drawable-width text-layer)))
            (height (car (gimp-drawable-height text-layer)))
       )
        
        (gimp-image-undo-disable img)
        (gimp-drawable-set-name text-layer text)
        (apply-bumpmy-logo-effect img text-layer text-color boolrippleh boolripplev bnoise bplasma)
        (gimp-image-undo-enable img)
        (gimp-display-new img)    
  )
)


(script-fu-register     "script-fu-bumpmy-logo"
            "Bump my logo"
            "Create a bumpmapped logo. \n file:bumpmy.scm"
            "karlhof26"
            "karlhof26"
            "06/03/2020"
            ""
            SF-FONT     "Font Name"             "Serif Bold"
            SF-STRING   "Enter your text"       "BUMP MY LOGO"
            SF-COLOR    "Text Color"            '(200 150 40)
            SF-TOGGLE   "Ripple Horiz."         TRUE
            SF-TOGGLE   "Ripple Vert."          TRUE
            SF-TOGGLE   "Background Noise"      TRUE
            SF-TOGGLE   "Background Plasma"     TRUE
            SF-ADJUSTMENT "Font size (pixels)"  '(150 2 1000 1 10 0 1)
)

(script-fu-menu-register "script-fu-bumpmy-logo"
            "<Image>/Script-Fu2/Logos")
