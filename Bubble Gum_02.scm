; Bubble Gum rel 0.03 
; Created by Graechan from tutorial by Conbagui at http://www.gimpchat.com/viewtopic.php?f=23&t=9849
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
; Rel 0.02 - Bugfix
; Rel 0.03 - Bugfix for foreign languages
; Rel 0.04 - Chnaged to work on Gimp 2.10.18
;
(define list-blend-dir '("Left to Right" "Top to Bottom" "Diagonal"))

; include layer Procedure
(define (include-layer image newlayer oldlayer stack)	;stack 0=above 1=below
    (cond ((defined? 'gimp-image-get-item-position) ;test for 2.8 compatability
            (gimp-image-insert-layer image newlayer (car (gimp-item-get-parent oldlayer)) 
            (+ (car (gimp-image-get-layer-position image oldlayer)) stack))                                     ;For GIMP 2.8 
          )
         (else
           (gimp-image-add-layer image newlayer (+ (car (gimp-image-get-layer-position image oldlayer)) stack)) ;For GIMP 2.6 
          )
    ) ;end cond
) ;end add layer procedure



(define (get-blending-mode mode)
    (let* (
            (modenumbers #(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14))
          )
        (vector-ref modenumbers mode)
  )
)

(define (math-round input)
    (floor (+ input 0.5))
)

(define (math-ceil input)
    (if (= input (floor input))
        input
        (+ (floor input) 1)
    )
)

(define (draw-blurshape img drawable size initgrowth sel invert)
  (let* ((k initgrowth)
        (currshade 0)
        (i 0))
        (while (< i size)
            (if (> k 0)
                (gimp-selection-grow img k)
                (if (< k 0)
                    (gimp-selection-shrink img (abs k))
                )
            )
            (if (= invert 1)
                (set! currshade (math-round (* (/ (- size (+ i 1)) size) 255)))
                (set! currshade (math-round (* (/ (+ i 1) size) 255)))
            )
            (gimp-context-set-foreground (list currshade currshade currshade))
            (if (= (car (gimp-selection-is-empty img)) 0)
                (gimp-edit-fill drawable 0)
            )
            (gimp-selection-load sel)
            (set! k (- k 1))
            (set! i (+ i 1))
        )
    )
)

(define (apply-contour drawable channel contour)
  (let* ((contourtypes #(0 0 0 0 0 0 0 0 0 1 1))
  (contourlengths #(6 6 10 14 18 10 18 18 10 256 256))
  (contours #(#(0 0 127 255 255 0)
#(0 255 127 0 255 255)
#(0 64 94 74 150 115 179 179 191 255)
#(0 0 5 125 6 125 48 148 79 179 107 217 130 255)
#(0 0 33 8 64 38 97 102 128 166 158 209 191 235 222 247 255 255)
#(0 0 28 71 87 166 194 240 255 255)
#(0 0 33 110 64 237 97 240 128 138 158 33 191 5 222 99 255 255)
#(0 0 33 74 64 219 97 186 128 0 158 176 191 201 222 3 255 255)
#(3 255 54 99 97 107 179 153 252 0)
#(0 5 9 13 16 19 22 25 27 29 30 32 33 34 35 36 38 39 40 41 43 44 46 47 48 49 50 51 52 53 54 55 55 56 56 57 57 58 58 59 59 59 60 60 60 61 61 61 61 62 62 62 62 62 63 63 63 63 63 63 64 64 64 64 64 71 75 78 81 84 86 89 91 93 95 96 98 99 101 102 103 104 105 107 107 108 110 111 112 113 114 115 116 117 118 119 119 120 121 121 122 123 123 123 124 124 124 125 125 125 125 125 125 125 126 126 126 126 126 126 126 125 125 125 125 125 125 125 125 130 134 137 141 145 148 151 153 156 158 160 162 163 165 166 167 168 170 171 171 172 173 174 175 176 177 178 178 179 180 181 181 182 183 183 184 184 185 185 186 186 187 187 188 188 189 189 189 189 190 190 190 190 191 191 191 191 191 191 191 191 191 191 193 194 196 197 198 200 201 203 204 205 207 208 209 211 212 213 214 215 217 218 219 220 220 221 222 222 223 223 224 224 224 224 224 223 223 222 222 221 221 220 219 218 217 216 215 214 213 212 211 210 209 208 206 205 204 203 202 200 199 198 197 196 194 194)
#(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 127 125 123 121 119 117 115 113 111 109 107 105 103 101 99 97 95 93 91 89 87 85 83 81 79 77 75 73 71 69 67 65 63 61 59 57 55 53 51 49 47 45 43 41 39 37 35 33 31 29 27 25 23 21 19 17 15 13 11 9 7 5 3 1 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99 101 103 105 107 109 111 113 115 117 119 121 123 125 127 128 126 124 122 120 118 116 114 112 110 108 106 104 102 100 98 96 94 92 90 88 86 84 82 80 78 76 74 72 70 68 66 64 62 60 58 56 54 52 50 48 46 44 42 40 38 36 34 32 30 28 26 24 22 20 18 16 14 12 10 8 6 4 2))))
    
    (gimp-message "line 104 inside contour")
    (if (= (vector-ref contourtypes (- contour 1)) 0)
      (gimp-curves-spline drawable channel (vector-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
      (gimp-curves-explicit drawable channel (vector-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
    )
  )
)

;THIS part is BROKEN
;(define (get-layer-pos img layer)
;    (let* (
;            (layerdata (gimp-image-get-layers img))
;            (numlayers (car layerdata))
;            (layerarray (cadr layerdata))
;            (i 0)
;            (pos -1)
;    )
;    (while (< i numlayers)
;        (if (= layer (vector-ref layerarray i))
;            (begin
;                (set! pos i)
;                (set! i numlayers)
;            )
;            (set! i (+ i 1))
;        )
;    )
;    pos
;  )
;)

(define (add-under-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (+ (get-layer-pos img oldlayer) 1))
)

(define (add-over-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (get-layer-pos img oldlayer))
)

(define (bubble-gum-drop-shadow img
                        drawable
                        color
                        opacity
                        contour
                        noise
                        mode
                        spread
                        size
                        offsetangle
                        offsetdist
                        knockout
                        merge)
  (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (layername (car (gimp-drawable-get-name drawable)))
            (growamt (math-ceil (/ size 2)))
            (steps (math-round (- size (* (/ spread 100) size))))
            (lyrgrowamt (math-round (* growamt 1.2)))
            (shadowlayer (car (gimp-layer-new img (+ drwwidth (* lyrgrowamt 2)) (+ drwheight (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-dropshadow") opacity (get-blending-mode mode))))
            (shadowmask 0)
            (alphaSel 0)
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (math-round (* offsetdist (cos ang))))
            (offsetY (math-round (* offsetdist (sin ang))))
            (origmask 0)
    )
    (gimp-message "line 170")
    (add-under-layer img shadowlayer drawable)
    (gimp-layer-set-offsets shadowlayer (- (+ (car drwoffsets) offsetX) lyrgrowamt) (- (+ (cadr drwoffsets) offsetY) lyrgrowamt))
    (gimp-selection-all img)
    (gimp-context-set-foreground color)
    (gimp-edit-fill shadowlayer FILL-FOREGROUND)
    (gimp-selection-none img)
    (set! shadowmask (car (gimp-layer-create-mask shadowlayer 1)))
    (gimp-layer-add-mask shadowlayer shadowmask)
    (gimp-selection-layer-alpha drawable)
    (gimp-message "line 180")
    
    (if (> (car (gimp-layer-get-mask drawable)) -1)
      (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (gimp-selection-translate img offsetX offsetY)
    (set! alphaSel (car (gimp-selection-save img)))
    (draw-blurshape img shadowmask steps growamt alphaSel 0)
    (gimp-selection-none img)
    (if (> contour 0)
      (begin
            (gimp-message "line 191")
            (apply-contour shadowmask 0 contour)
            (gimp-selection-load alphaSel)
            
            (gimp-selection-grow img growamt)
            (gimp-selection-invert img)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill shadowmask FILL-FOREGROUND)
            (gimp-selection-none img)
      )
    )
    (if (> noise 0)
        (begin
            (gimp-message "line 208")
            (apply-noise img drawable shadowlayer noise)
        )
    )
    (if (= knockout 1)
      (begin
            (gimp-context-set-foreground '(0 0 0))
            (gimp-selection-layer-alpha drawable)
            (gimp-edit-fill shadowmask 0)
      )
    )
    (gimp-layer-remove-mask shadowlayer 0)
    (gimp-selection-none img)
    (if (= merge 1)
      (begin
            (set! origmask (car (gimp-layer-get-mask drawable)))
            (if (> origmask -1)
                (gimp-layer-remove-mask drawable 0)
            )
            (set! shadowlayer (car (gimp-image-merge-down img drawable 0)))
            (gimp-drawable-set-name shadowlayer layername)
      )
    )
    (gimp-context-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(define (bubble-gum-inner-shadow img
                    drawable
                    color
                    opacity
                    contour
                    noise
                    mode
                    source
                    choke
                    size
                    offsetangle
                    offsetdist
                    merge)
  (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-palette-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (shadowlayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-innershadow") opacity (get-blending-mode mode))))
            (shadowmask 0)
            (alphaSel 0)
            (growamt (math-ceil (/ size 2)))
            (chokeamt (* (/ choke 100) size))
            (steps (math-round (- size chokeamt)))
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (math-round (* offsetdist (cos ang))))
            (offsetY (math-round (* offsetdist (sin ang))))
            (origmask 0)
            (alphamask 0)
        )
        
    (gimp-message "line 270")
    (add-over-layer img shadowlayer drawable)
    (gimp-layer-set-offsets shadowlayer (car drwoffsets) (cadr drwoffsets))
    (gimp-selection-all img)
    (gimp-context-set-foreground color)
    (gimp-edit-fill shadowlayer FILL-FOREGROUND)
    (gimp-selection-none img)
    (set! shadowmask (car (gimp-layer-create-mask shadowlayer 1)))
    (gimp-layer-add-mask shadowlayer shadowmask)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
      (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (gimp-message "line 283")
    (gimp-selection-translate img offsetX offsetY)
    (set! alphaSel (car (gimp-selection-save img)))
    (if (= source 0)
      (begin
            (gimp-message "line 288")
            (gimp-selection-all img)
            (gimp-context-set-foreground '(255 255 255))
            (gimp-edit-fill shadowmask FILL-FOREGROUND)
            (gimp-selection-load alphaSel)
            (draw-blurshape img shadowmask steps (- growamt chokeamt) alphaSel 1)
      )
      (begin
        (gimp-message "line 294")
        (draw-blurshape img shadowmask steps (- growamt chokeamt) alphaSel 0)
      )
    )
    (gimp-selection-none img)
    (if (> contour 0)
        (apply-contour shadowmask 0 contour)
    )
    (gimp-message "line 302")
    (if (= merge 0)
      (begin
            (gimp-selection-layer-alpha drawable)
            (gimp-selection-invert img)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill shadowmask FILL-FOREGROUND)
      )
    )
    (if (> noise 0)
        (apply-noise img drawable shadowlayer noise)
    )
    (gimp-layer-remove-mask shadowlayer 0)
    (gimp-message "line 315")
    (if (= merge 1)
        (if (= source 0)
            (begin
                (gimp-message "line 319")
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! alphamask (car (gimp-layer-create-mask drawable 3)))
                (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                (gimp-drawable-set-name shadowlayer layername)
                (gimp-layer-add-mask shadowlayer alphamask)
                (gimp-layer-remove-mask shadowlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask shadowlayer origmask)
                )
            )
            (begin
                (gimp-message "line 337")
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                (gimp-drawable-set-name shadowlayer layername)
                (gimp-message "line 347")
                (if (> origmask -1)
                    (gimp-layer-add-mask shadowlayer origmask)
                )
            )
        )
    )
    (gimp-message "line 354")
    (gimp-selection-none img)
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-message "line 360")
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(define (bubble-gum-inner-glow img
                    drawable
                    color
                    opacity
                    contour
                    noise
                    mode
                    source
                    choke
                    size
                    merge)
  (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-palette-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (glowlayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-innerglow") opacity (get-blending-mode mode))))
            (glowmask 0)
            (alphaSel 0)
            (shrinkamt (* (/ choke 100) size))
            (steps (- size shrinkamt))
            (i 0)
            (currshade 0)
            (origmask 0)
            (alphamask 0)
        )
        (gimp-message "line 401")
    (add-over-layer img glowlayer drawable)
    (gimp-layer-set-offsets glowlayer (car drwoffsets) (cadr drwoffsets))
    (gimp-selection-all img)
    (gimp-context-set-foreground color)
    (gimp-edit-fill glowlayer FILL-FOREGROUND)
    (gimp-selection-none img)
    (gimp-message "line 408")
    (set! glowmask (car (gimp-layer-create-mask glowlayer 1)))
    (gimp-layer-add-mask glowlayer glowmask)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
      (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (set! alphaSel (car (gimp-selection-save img)))
    (gimp-message "line 416")
    (if (= source 0)
        (begin
            (gimp-selection-all img)
            (gimp-palette-set-foreground '(255 255 255))
            (gimp-edit-fill glowmask 0)
            (gimp-selection-load alphaSel)
            (gimp-message "line 423")
            (draw-blurshape img glowmask steps (- (* shrinkamt -1) 1) alphaSel 1)
        )
        (begin
            (gimp-message "line 427")
            (draw-blurshape img glowmask steps (* shrinkamt -1) alphaSel 0)
        )
    )
    (gimp-message "line 431")
    (gimp-selection-none img)
    (if (> contour 0)
        (apply-contour glowmask 0 contour)
    )
    (if (and (= source 0) (= merge 0))
        (begin
            (gimp-selection-load alphaSel)
            (gimp-selection-invert img)
            (gimp-palette-set-foreground '(0 0 0))
            (gimp-edit-fill glowmask 0)
        )
    )
    (if (> noise 0)
        (apply-noise img drawable glowlayer noise)
    )
    (gimp-layer-remove-mask glowlayer 0)
    (if (= merge 1)
        (if (= source 0)
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! alphamask (car (gimp-layer-create-mask drawable 3)))
                (set! glowlayer (car (gimp-image-merge-down img glowlayer 0)))
                (gimp-drawable-set-name glowlayer layername)
                (gimp-layer-add-mask glowlayer alphamask)
                (gimp-layer-remove-mask glowlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask glowlayer origmask)
                )
            )
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! glowlayer (car (gimp-image-merge-down img glowlayer 0)))
                (gimp-drawable-set-name glowlayer layername)
                (if (> origmask -1)
                    (gimp-layer-add-mask glowlayer origmask)
                )
            )
        )
    )
    (gimp-message "line 477")
    (gimp-selection-none img)
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-message "line 483")
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(define (bubble-gum-bevel-emboss img
                    drawable
                    style
                    depth
                    direction
                    size
                    soften
                    angle
                    altitude
                    glosscontour
                    highlightcolor
                    highlightmode
                    highlightopacity
                    shadowcolor
                    shadowmode
                    shadowopacity
                    surfacecontour
                    invert
                    merge)
  (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-palette-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (layername (car (gimp-drawable-get-name drawable)))
            (imgtype (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)))
            (lyrgrowamt (math-round (* size 1.2)))
            (bumpmaplayer 0)
            (highlightlayer 0)
            (highlightmask 0)
            (shadowlayer 0)
            (shadowmask 0)
            (layersize 0)
            (alphaSel 0)
            (halfsizef 0)
            (halfsizec 0)
            (origmask 0)
            (alphamask 0)
        )
    
    (gimp-message "line 537")
    
    (cond
        ((= style 0)
            (begin
                (set! layersize (list
                    (+ drwwidth (* lyrgrowamt 2))
                    (+ drwheight (* lyrgrowamt 2))
                    (- (car drwoffsets) lyrgrowamt)
                    (- (cadr drwoffsets) lyrgrowamt)
                    )
                )
            )
        )
        ((= style 1)
            (begin
                (set! layersize (list
                    drwwidth
                    drwheight
                    (car drwoffsets)
                    (cadr drwoffsets)
                    )
                )
            )
        )
        ((= style 2)
            (begin
                (set! layersize (list
                    (+ drwwidth lyrgrowamt)
                    (+ drwheight lyrgrowamt)
                    (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
                    (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
                    )
                )
            )
        )
        (  ;elseif
            (begin
                (set! layersize (list
                    (+ drwwidth lyrgrowamt)
                    (+ drwheight lyrgrowamt)
                    (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
                    (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
                    )
                )
            )
        )
    )
    (gimp-message "line 585")
    (set! bumpmaplayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-bumpmap") 100 0)))
    (set! highlightlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-highlight") highlightopacity (get-blending-mode highlightmode))))
    (set! shadowlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-shadow") shadowopacity (get-blending-mode shadowmode))))
    (add-over-layer img bumpmaplayer drawable)
    (add-over-layer img shadowlayer bumpmaplayer)
    (add-over-layer img highlightlayer shadowlayer)
    (gimp-layer-set-offsets bumpmaplayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets shadowlayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets highlightlayer (caddr layersize) (cadddr layersize))
    (gimp-selection-all img)
    (gimp-message "line 596")
    (gimp-context-set-foreground highlightcolor)
    (gimp-edit-fill highlightlayer 0)
    (gimp-palette-set-foreground shadowcolor)
    (gimp-edit-fill shadowlayer 0)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-message "line 602")
    (gimp-edit-fill bumpmaplayer 0)
    (set! highlightmask (car (gimp-layer-create-mask highlightlayer 1)))
    (set! shadowmask (car (gimp-layer-create-mask shadowlayer 1)))
    (gimp-layer-add-mask highlightlayer highlightmask)
    (gimp-layer-add-mask shadowlayer shadowmask)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
        (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (set! alphaSel (car (gimp-selection-save img)))
    (gimp-message "line 613")
    (cond
        ((= style 0)
            (draw-blurshape img bumpmaplayer size size alphaSel 0)
        )
        ((= style 1)
            (draw-blurshape img bumpmaplayer size 0 alphaSel 0)
        )
        ((= style 2)
            (begin
                (set! halfsizec (math-ceil (/ size 2)))
                (draw-blurshape img bumpmaplayer size halfsizec alphaSel 0)
           )
        )
        (
            (begin
                (gimp-message "line 629")
                (set! halfsizef (floor (/ size 2)))
                (set! halfsizec (- size halfsizef))
                (gimp-selection-all img)
                (gimp-palette-set-foreground '(255 255 255))
                (gimp-edit-fill bumpmaplayer 0)
                (draw-blurshape img bumpmaplayer halfsizec halfsizec alphaSel 1)
                (draw-blurshape img bumpmaplayer halfsizef 0 alphaSel 0)
            )
        )
    )
    (gimp-selection-all img)
    (gimp-context-set-foreground '(127 127 127)) ; thats a grey
    (gimp-edit-fill highlightmask 0)
    (gimp-selection-none img)
    (gimp-message "line 644")
    (if (> surfacecontour 0)
        (apply-contour bumpmaplayer 0 surfacecontour)
    )
    (if (< angle 0)
        (set! angle (+ angle 360))
    )
    (plug-in-bump-map 1 img highlightmask bumpmaplayer angle altitude depth 0 0 0 0 1 direction 0)
    
    (if (> glosscontour 0)
        (begin
            (gimp-message "line 655 gloss contour")
            (apply-contour highlightmask 0 glosscontour)
        )
    )
    (gimp-message "line 659")
    
    (if (> soften 0)
        (begin
            (gimp-message "soften")
            (plug-in-gauss-rle 1 img highlightmask soften 1 1)
        )
    )
    (if (> invert 0)
        (gimp-invert highlightmask)
    )
    (gimp-channel-combine-masks shadowmask highlightmask 2 0 0)
    (gimp-levels highlightmask 0 127 255 1.0 0 255)
    (gimp-levels shadowmask 0 0 127 1.0 255 0)
    (gimp-selection-load alphaSel)
    (gimp-message "line 674")
    
    (if (= style 0)
        (gimp-selection-grow img size)
        (if (or (= style 2) (= style 3))
            (gimp-selection-grow img halfsizec)
        )
    )
    (gimp-selection-invert img)
    (gimp-palette-set-foreground '(0 0 0))
    (gimp-edit-fill shadowmask 0)
    (gimp-selection-none img)
    (gimp-image-remove-layer img bumpmaplayer)
    (gimp-message "line 687")
    
    (if (= merge 1)
        (if (= style 1)
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! alphamask (car (gimp-layer-create-mask drawable 3)))
                (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
                (gimp-drawable-set-name highlightlayer layername)
                (gimp-layer-add-mask highlightlayer alphamask)
                (gimp-layer-remove-mask highlightlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask highlightlayer origmask)
                )
            )
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (gimp-layer-remove-mask drawable 0)
                )
                (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
                (gimp-drawable-set-name highlightlayer layername)
            )
        )
    )
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-message "line 724")
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(define (bubble-gum-satin img
                drawable
                color
                opacity
                mode
                offsetangle
                offsetdist
                size
                contour
                invert
                merge)
    (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-palette-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (layername (car (gimp-drawable-get-name drawable)))
            (growamt (math-ceil (/ size 2)))
            (lyrgrowamt (math-round (* growamt 1.2)))
            (satinlayer (car (gimp-layer-new img (+ (car (gimp-drawable-width drawable)) (* lyrgrowamt 2)) (+ (car (gimp-drawable-height drawable)) (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-satin") 100 0)))
            (satinmask 0)
            (blacklayer 0)
            (drwoffsets (gimp-drawable-offsets drawable))
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (math-round (* offsetdist (cos ang))))
            (offsetY (math-round (* offsetdist (sin ang))))
            (alphaSel 0)
            (layeraoffsets 0)
            (layerboffsets 0)
            (dx 0)
            (dy 0)
            (origmask 0)
            (alphamask 0)
        )
    (gimp-message "line 763")
    (add-over-layer img satinlayer drawable)
    (gimp-layer-set-offsets satinlayer (- (car drwoffsets) lyrgrowamt) (- (cadr drwoffsets) lyrgrowamt))
    (gimp-selection-all img)
    (gimp-palette-set-foreground '(0 0 0))
    (gimp-edit-fill satinlayer 0)
    (gimp-selection-none img)
    (gimp-selection-layer-alpha drawable)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
        (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
    )
    (set! alphaSel (car (gimp-selection-save img)))
    (draw-blurshape img satinlayer size growamt alphaSel 0)
    (gimp-message "line 776")
    (plug-in-autocrop-layer 1 img satinlayer)
    (set! satinmask (car (gimp-layer-copy satinlayer 0)))
    (add-over-layer img satinmask satinlayer)
    (gimp-layer-translate satinlayer offsetX offsetY)
    (gimp-layer-translate satinmask (* offsetX -1) (* offsetY -1))
    (gimp-message "line 782")
    (set! layeraoffsets (gimp-drawable-offsets satinlayer))
    (set! layerboffsets (gimp-drawable-offsets satinmask))
    (set! dx (- (max (car layeraoffsets) (car layerboffsets)) (min (car layeraoffsets) (car layerboffsets))))
    (set! dy (- (max (cadr layeraoffsets) (cadr layerboffsets)) (min (cadr layeraoffsets) (cadr layerboffsets))))
    (set! blacklayer (car (gimp-layer-new img (+ (car (gimp-drawable-width satinlayer)) dx) (+ (car (gimp-drawable-height satinlayer)) dy) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-satinblank") 100 0)))
    (add-under-layer img blacklayer satinlayer)
    (gimp-layer-set-offsets blacklayer (min (car layeraoffsets) (car layerboffsets)) (min (cadr layeraoffsets) (cadr layerboffsets)))
    (gimp-selection-all img)
    (gimp-message "line 791")
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill blacklayer 0)
    (gimp-selection-none img)
    (gimp-layer-set-mode satinmask 6)
    (set! satinlayer (car (gimp-image-merge-down img satinlayer 0)))
    (set! satinlayer (car (gimp-image-merge-down img satinmask 0)))
    (gimp-drawable-set-name satinlayer (string-append layername "-satin"))
    (if (> contour 0)
        (begin
            (gimp-message "line 801 contour")
            (apply-contour satinlayer 0 contour)
            (gimp-selection-load alphaSel)
            (gimp-selection-grow img size)
            (gimp-selection-invert img)
            (gimp-context-set-foreground '(0 0 0))
            (gimp-edit-fill satinlayer 0)
            (gimp-selection-none img)
        )
    )
    (if (= invert 1)
        (begin
            (gimp-message "line 813")
            (gimp-drawable-invert satinlayer TRUE)
        )
    )
    (set! satinmask (car (gimp-layer-create-mask satinlayer 5)))
    (gimp-layer-add-mask satinlayer satinmask)
    (gimp-selection-all img)
    (gimp-palette-set-foreground color)
    (gimp-edit-fill satinlayer 0)
    (gimp-selection-none img)
    (gimp-layer-set-opacity satinlayer opacity)
    (gimp-layer-set-mode satinlayer (get-blending-mode mode))
    (gimp-message "line 825")
    (gimp-layer-resize satinlayer (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable)) (- (car (gimp-drawable-offsets satinlayer)) (car drwoffsets)) (- (cadr (gimp-drawable-offsets satinlayer)) (cadr drwoffsets)))
    (if (= merge 1)
        (begin
            (set! origmask (car (gimp-layer-get-mask drawable)))
            (if (> origmask -1)
                (begin
                    (set! origmask (car (gimp-channel-copy origmask)))
                    (gimp-layer-remove-mask drawable 1)
                )
            )
            (set! alphamask (car (gimp-layer-create-mask drawable 3)))
            (set! satinlayer (car (gimp-image-merge-down img satinlayer 0)))
            (gimp-drawable-set-name satinlayer layername)
            (gimp-layer-add-mask satinlayer alphamask)
            (gimp-layer-remove-mask satinlayer 0)
            (if (> origmask -1)
                (gimp-layer-add-mask satinlayer origmask)
            )
        )
        (begin
            (gimp-message "line 846")
            (gimp-selection-load alphaSel)
            (gimp-selection-invert img)
            (gimp-palette-set-foreground '(0 0 0))
            (gimp-edit-fill satinmask 0)
        )
    )
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-message "line 857")
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(define (bubble-gum-pattern-overlay img
                      drawable
                        pattern
                        opacity
                        mode
                        merge)
  (gimp-image-undo-group-start img)
  (let* (
            (origpattern (car (gimp-context-get-pattern)))
           (origselection (car (gimp-selection-save img)))
             (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (patternlayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-pattern") opacity (get-blending-mode mode))))
            (origmask 0)
            (alphamask 0)
    )
    (gimp-message "line 881")
    (add-over-layer img patternlayer drawable)
    (gimp-layer-set-offsets patternlayer (car drwoffsets) (cadr drwoffsets))
    (gimp-selection-all img)
    (gimp-context-set-pattern pattern)
    (gimp-edit-fill patternlayer 4)
    (gimp-selection-none img)
    (gimp-message "line 888")
    (if (= merge 1)
        (begin
            (set! origmask (car (gimp-layer-get-mask drawable)))
            (if (> origmask -1)
                (begin
                    (set! origmask (car (gimp-channel-copy origmask)))
                    (gimp-layer-remove-mask drawable 1)
                )
            )
            (set! alphamask (car (gimp-layer-create-mask drawable 3)))
            (set! patternlayer (car (gimp-image-merge-down img patternlayer 0)))
            (gimp-drawable-set-name patternlayer layername)
            (gimp-layer-add-mask patternlayer alphamask)
            (gimp-layer-remove-mask patternlayer 0)
            (if (> origmask -1)
                (gimp-layer-add-mask patternlayer origmask)
            )
        )
        (begin
            (gimp-selection-layer-alpha drawable)
            (if (> (car (gimp-layer-get-mask drawable)) -1)
                (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
            )
            (set! alphamask (car (gimp-layer-create-mask patternlayer 4)))
            (gimp-layer-add-mask patternlayer alphamask)
            (gimp-layer-remove-mask patternlayer 0)
        )
    )
    (gimp-context-set-pattern origpattern)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)
    (gimp-message "line 921")
 )
 (gimp-image-undo-group-end img)
)

(define (script-fu-bubble-gum 
                            text
                            letter-spacing
                            line-spacing
                            font-in 
                            font-size
                            main-color
                            highlight-color
                            bkg-type 
                            pattern
                            bkg-color
                            gradient
                            gradient-type
                            reverse
                            blendir
                            conserve)
                            
  (let* (
         (image (car (gimp-image-new 256 256 RGB)))         
         (border (/ font-size 4))
         (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
         (size-layer (car (gimp-text-fontname image -1 0 0 text border TRUE font-size PIXELS font)))
         (final-width (car (gimp-drawable-width size-layer)))
         (final-height (car (gimp-drawable-height size-layer)))
         (text-layer 0)
         (text-layer-copy 0)
         (width 0)
         (height 0)
         (bkg-layer 0)
         (ver 2.8)
         (selection-channel 0)
         (aspect 0)
         
         
         (drop-shadow 0)
         (x1 0)
         (y1 0)
         (x2 0)
         (y2 0)
        )
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (if (= ver 2.8) (gimp-context-set-dynamics "Dynamics Off"))
    (gimp-context-set-foreground '(123 123 123)) ;---------------------------------karlhof26 vanilla color during build
    (gimp-context-set-background '(255 255 255))
    (gimp-message "line 973")
    ;;;;adjust the size-layer
    (gimp-text-layer-set-justification size-layer 2)
    (gimp-text-layer-set-letter-spacing size-layer letter-spacing)
    (gimp-text-layer-set-line-spacing size-layer line-spacing)
    (set! final-width (car (gimp-drawable-width size-layer)))
    (set! final-height (car (gimp-drawable-height size-layer)))
    
    ;;;;Add the text layer for a temporary larger Image size
    (set! text-layer (car (gimp-text-fontname image -1 0 0 text (round (/ 325 4)) TRUE 325 PIXELS font)))
    (gimp-drawable-set-name text-layer "Text")
    ;;;;adjust text
    (gimp-text-layer-set-justification text-layer 2)
    (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
    (gimp-message "line 987")
    (gimp-text-layer-set-line-spacing text-layer line-spacing)
    ;;;;set the new width and height
    (set! width (car (gimp-drawable-width text-layer)))
    (set! height (car (gimp-drawable-height text-layer)))    
    (gimp-image-remove-layer image size-layer)
    (gimp-image-resize-to-layers image)
    (gimp-message "line 994")
    
    ;;;;begin the script
    ;;;;copy the text-layer
    (set! text-layer-copy (car (gimp-layer-copy text-layer TRUE)))
    (include-layer image text-layer-copy text-layer 0)	;stack 0=above 1=below
    (gimp-drawable-set-visible text-layer-copy FALSE)
    (gimp-message "line 1001")
    
    ;;;;add a drop shadow
    (bubble-gum-drop-shadow image
                       text-layer ;drawable
                       '(0 0 0) ;color was Black
                       75 ;opacity
                        0 ;contour
                        0 ;noise
                        2 ;mode
                        0 ;spread
                        10 ;size
                        108 ;offsetangle
                        7 ;offsetdist
                        TRUE ;knockout
                        FALSE) ;merge
    (gimp-message "line 1017")
    (gimp-image-set-active-layer image text-layer)
    
    (bubble-gum-inner-shadow image
                    text-layer ;drawable
                    "Black" ;color
                    100 ;opacity
                    0 ;contour
                    0 ;noise
                    2 ;mode
                    0 ;source
                    0 ;choke
                    14 ;size
                    -65 ;offsetangle
                    7 ;offsetdist
                    TRUE) ;merge
    (set! text-layer (car (gimp-image-get-active-layer image)))
    (gimp-message "line 1034")
    
    (bubble-gum-inner-glow image
                      text-layer ;drawable
                      "White" ;color
                      100 ;opacity
                      0 ;contour
                      0 ;noise
                      6 ;mode
                      0 ;source
                      0 ;choke
                      21 ;size
                      TRUE) ;merge
    (set! text-layer (car (gimp-image-get-active-layer image)))
    (gimp-message "line 1048")
   
    (bubble-gum-satin image
                    text-layer ;drawable
                    "Black" ;color
                    10 ;opacity
                    2 ;mode
                    19 ;offsetangle
                    10 ;offsetdist
                    15 ;size
                    5 ;contour
                    TRUE ;invert
                    TRUE) ;merge
    (gimp-message "line 1061")
    (set! text-layer (car (gimp-image-get-active-layer image)))
    
    (bubble-gum-bevel-emboss image
                    text-layer ;drawable
                    1 ;style
                    65 ;depth
                    0 ;direction
                    40 ;size
                    14 ;soften
                    108 ;angle
                    30 ;altitude
                    0 ;glosscontour
                    "White" ;highlightcolor was "White"
                    0 ;highlightmode
                    18 ;highlightopacity
                    highlight-color ;shadowcolor   ;--------------------------------------> highlight colour here (242 1 164)
                    0 ;shadowmode
                    42 ;shadowopacity
                    4 ;surfacecontour
                    FALSE ;invert
                    FALSE) ;merge
    (gimp-image-set-active-layer image text-layer)
    (gimp-message "line 1084")
    
    (let* (
            (img-width width)
            (img-height height)
            (img (car (gimp-image-new img-width img-height RGB)))
            (img-layer (car (gimp-layer-new img img-width img-height RGBA-IMAGE "pattern" 100 LAYER-MODE-NORMAL-LEGACY)))
            (pattern-layer (car (gimp-layer-new img img-width img-height RGBA-IMAGE "Pool-pattern" 22 LAYER-MODE-SCREEN-LEGACY)))
        )
        
        (gimp-image-insert-layer img img-layer 0 0)
        (gimp-context-set-background main-color) ; -------------------> main colour-2 here 255 202 212
        (gimp-drawable-fill img-layer FILL-BACKGROUND)
        (gimp-message "line 1097")
        (python-fu-foggify 1 img img-layer "Clouds" '(152 151 177) 1 50)
        (gimp-image-add-layer img pattern-layer -1)
        (gimp-item-set-name pattern-layer "pattern layer") 
        (gimp-context-set-pattern "Pool Bottom")
        (gimp-drawable-fill pattern-layer FILL-PATTERN)
        (gimp-edit-copy-visible img)
        
        ;(gimp-image-delete img)
        (gimp-display-new img)
    )
    
    (bubble-gum-pattern-overlay image
                        text-layer ;drawable
                        (caadr (gimp-patterns-list "")) ;pattern
                        100 ;opacity
                        8 ;mode
                        TRUE) ;merge
    (set! text-layer (car (gimp-image-get-active-layer image)))
    (gimp-message "line 1116")
    
    (gimp-image-set-active-layer image text-layer-copy)
    (gimp-drawable-set-visible text-layer-copy TRUE)
    (gimp-layer-set-mode text-layer-copy LAYER-MODE-BURN-LEGACY)
    (gimp-message "line 1121")
    
    (bubble-gum-bevel-emboss image
                    text-layer-copy ;drawable
                    1 ;style
                    65 ;depth
                    0 ;direction
                    20 ;size
                    10 ;soften
                    121 ;angle
                    30 ;altitude
                    10 ;glosscontour
                    "White" ;highlightcolor
                    4 ;highlightmode
                    59 ;highlightopacity
                    "Black" ;shadowcolor
                    2 ;shadowmode
                    10 ;shadowopacity
                    0 ;surfacecontour
                    FALSE ;invert
                    FALSE) ;merge
        
    ;end
    (gimp-message "line 1144")
    
    ;;;;Scale Image to it's original size;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (set! aspect (/ final-width (car (gimp-image-width image)))) 
    (gimp-image-scale image final-width (* (car (gimp-image-height image)) aspect))
    (set! width (car (gimp-image-width image)))
    (set! height (car (gimp-image-height image)))
    (gimp-displays-flush)    
        
    ;;;;create the background layer
    (gimp-image-set-active-layer image text-layer)    
    (cond ((not (= bkg-type 3))
            (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
            (include-layer image bkg-layer text-layer 2)	;stack 0=above 1=below
        )
    ) ;endcond
    (gimp-message "line 1160")
    (gimp-context-set-pattern pattern)
    (gimp-context-set-background bkg-color)
    (gimp-context-set-gradient gradient)
    (if (= bkg-type 1) (gimp-drawable-fill bkg-layer FILL-PATTERN))
    (if (= bkg-type 0)
        (begin
            (gimp-drawable-fill bkg-layer FILL-BACKGROUND) 
            (plug-in-hsv-noise 1 image bkg-layer 1 3 10 10)
        )
    )
    (gimp-message "line 1171")
    (if (= bkg-type 2) 
        (begin
            (gimp-selection-none image)
            (gimp-drawable-fill bkg-layer FILL-BACKGROUND)
            (if (= blendir 0) (set! x2 width))
            (if (= blendir 1) (set! y2 height))
            (if (= blendir 2)
                (begin
                    (set! x2 (/ width 2))
                    (set! y2 (/ height 2))
                )
            )
            (gimp-edit-blend bkg-layer BLEND-CUSTOM LAYER-MODE-NORMAL gradient-type 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE x1 y1 x2 y2)
        )
    )
    
    (if (= conserve FALSE)
        (begin
            (set! text-layer (car (gimp-image-merge-visible-layers image EXPAND-AS-NECESSARY)))
            (gimp-drawable-set-name text-layer "BUBBLE\n  GUM")
            (gimp-layer-resize-to-image-size text-layer)
        )
    ) ;endif
    (gimp-message "good finish OK")
    (gimp-context-pop)
    
    (gimp-display-new image)
    
    )
)
  
(script-fu-register "script-fu-bubble-gum"
    "Bubble Gum"
    "Can create text with the appearance of Bubble Gum"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "June 2011"
    ""
    SF-TEXT       "Text"    "BUBBLE\nGUM"
    SF-ADJUSTMENT "Letter Spacing"            '(0 -100 100 1 5 0 0)
    SF-ADJUSTMENT "Line Spacing"              '(0 -100 100 1 5 0 0)
    SF-FONT       "Font"                      "Cooper Black,"
    SF-ADJUSTMENT "Font size (pixels)"        '(200 6 500 1 1 0 1)
    SF-COLOR      "Main color"              '(255 202 212)
    SF-COLOR      "Highlights color"              '(242 1 164)
    SF-OPTION     "Background Type"           '("Color" "Pattern" "Gradient" "None")
    SF-PATTERN    "Pattern"                   "Pink Marble"
    SF-COLOR      "Background color"          '(54 0 40)
    SF-GRADIENT   "Background Gradient"       "Abstract 3"
    SF-ENUM       "Gradient Fill Mode"        '("GradientType" "gradient-linear")
    SF-TOGGLE     "Reverse the Gradient"      FALSE
    SF-OPTION     "Blend Direction"           '("Left to Right" "Top to Bottom" "Diagonal")
    SF-TOGGLE     "Keep the Layers"           TRUE
)

(script-fu-menu-register "script-fu-bubble-gum" "<Image>/Script-Fu/Logos")

;end of script
