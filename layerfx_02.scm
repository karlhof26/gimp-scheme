; GIMP Layer Effects
; Copyright (c) 2008 Jonathan Stipe
; JonStipe@prodigy.net 

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (get-blending-mode modesk)
    
    (let* (
            
            ; (v (list->vector '(1 2 3)))
            (result 0)
            ;dropped 'leading the #
            ; must allocate memory for the vectors - or they will be limited to 10
            
            (modenumbersk '(0 1 3 6 4 5 16 17 18 19 20 37 38 39 8 9 10 11 12 13 14 33 26 31 63 63))
            
            (k '(1 2 3 45))
            (res 0)
          )
          (gimp-message "in get blending mode")
          ;; (modenumbers '#(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14; extras added by karlhof26 just a numerical sequence; 28 = item 21 from drop down
          ;; (modenumbers (list->vector '(0 1 3 6 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14
          ;;   0 1  3  6  4  5  16  17  18  19  20  21  6  7  8  9  10  11  12  13  14
        ;(gimp-message "crashing here maybe ")
        (set! result (list-ref modenumbersk modesk))
        ;(set! result (vector-ref modenumbers mode))
        ;(gimp-message (number->string result))
        ;(gimp-message "crashing here maybe not...returning")
        ;(set! res (+ res 24))
        
        (result)
        
    )
)

(define (get-grad-blending-mode modep)
    (let* (
            (modenumberss '#(3 0 1 2 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3))
            (v (list->vector '(3 0 1 2 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3))) ; was 333 at end so added the last 4 3's
            (result 0)
            
          )
          ;(gimp-message "in grad blending mode")
          (set! result (vector-ref modenumberss modep))
          result
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

(define (get-layer-pos img layer)
    (let* (
            (layerdata (gimp-image-get-layers img))
            (numlayers (car layerdata))
            (layerarray (cadr layerdata))
            (i 0)
            (pos -1)
        )
        
        (while (< i numlayers)
            (if (= layer (vector-ref layerarray i))
                (begin
                    (set! pos i)
                    (set! i numlayers)
                )
            )
            (set! i (+ i 1))
        )
        pos
    )
)

(define (add-under-layer img newlayer oldlayer)
    (let* (
            (oldposit2 -1) ; was 0
          )
        (set! oldposit2 (get-layer-pos img oldlayer))
        ;(gimp-message "attempting to add add the layer")
        ;; was (gimp-image-insert-layer img newlayer (+ oldposit2 1) oldposit2)
        (gimp-image-insert-layer img newlayer 0 oldposit2)
    )
)

(define (add-over-layer img newlayer oldlayer)
    (let* ((oldposit 0)
            (finalposit 0)
            )
        (set! oldposit (get-layer-pos img oldlayer) )
        (if (or (= oldposit 0) (= oldposit -1))
            (set! finalposit -1)
            (set! finalposit (+ oldposit 1))
        )
        (gimp-image-insert-layer img newlayer oldposit finalposit)
        ;(gimp-message "insert done")
    )
)

(define (draw-blurshape img drawable size initgrowth sel invert)
    (let* (
            (k initgrowth)
            (currshade 0)
            (i 0)
          )
          
        (while (< i size)
            (if (> k 0)
                (gimp-selection-grow img k)
            )
            (if (< k 0)
                (gimp-selection-shrink img (abs k))
            )
            
            (if (= invert 1)
                (set! currshade (round (* (/ (- size (+ i 1)) size) 255)))
                (set! currshade (round (* (/ (+ i 1) size) 255)))
            )
            (gimp-context-set-foreground (list currshade currshade currshade))
            (if (= (car (gimp-selection-is-empty img)) 0)
                (gimp-edit-fill drawable FILL-FOREGROUND)
            )
            (gimp-selection-load sel)
            (set! k (- k 1))
            (set! i (+ i 1))
        )
    )
)

(define (apply-contour-kh drawable channel contour)
    (let* (
            (dummy-var 0)
             ; (0 0 127 255 255 0)
            ;dropped leading' before the #
            (contourtypes '(0 0 0 0 0 0 0 0 0 1 1))
            (contourlengths '(6 6 10 14 18 10 18 18 10 256 256))
            (contours #(#(0.0 0.0 0.5 1.0 1.0 0.0) ;is a space needed
                        #(0.0 1.0 0.5 0.0 0.99 1.0) ; was 0.0 1.0 1.0 at end
                        #(0.0 0.25 0.368 0.29 0.588 0.45 0.701 0.701 0.749 1.00) 
                        #(0.0 0.0 0.196 0.4901 0.0235 0.04901 0.188 0.5803 0.3098 0.7019 0.4196 0.8509 0.509 1.000) ;0 0 5 125 6 125 48 148 79 179 107 217 130 255
                        #(0.0 0.0 0.129 0.031 0.250 0.149 0.380 0.400 0.501 0.650 0.619 0.819 0.749 0.921 0.870 0.968 1.000 1.000) ;;(0 0 33 8 64 38 97 102 128 166 158 209 191 235 222 247 255 255)
                        #(0.0 0.0 0.109 0.278 0.341 0.650 0.760 0.941 1.000 1.000) ;; 0 0 28 71 87 166 194 240 255 255)
                        #(0.0 0.0 0.129 0.431 0.250 0.929 0.380 0.941 0.501 0.541 0.619 0.129 0.749 0.019 0.870 0.388 1.000 1.000) ;; 0 0 33 110 64 237 97 240 128 138 158 33 191 5 222 99 255 255)
                        #(0.0 0.0 0.129 0.290 0.250 0.858 0.380 0.729 0.501 0.000 0.619 0.690 0.749 0.788 0.870 0.011 1.000 1.000) ;;(0 0 33 74 64 219 97 186 128 0 158 176 191 201 222 3 255 255)
                        #(0.011 1.000 0.211 0.388 0.380 0.419 0.701 0.600 0.988 1.000) ;; 3 255 54 99 97 107 179 153 252 0); was 0 at end
             #(0.000 0.019 0.035 0.050 0.062 0.074 0.086 0.098 0.105 0.112
               0.117 0.125 0.129 0.133 0.137 0.141 0.149 0.152 0.156 0.160
               0.168 0.172 0.180 0.184 0.188 0.192 0.196 0.200 0.203 0.207
               0.211 0.215 0.215 0.219 0.219 0.223 0.223 0.227 0.227 0.231
               0.231 0.231 0.235 0.235 0.235 0.239 0.239 0.239 0.239 0.243
               0.243 0.243 0.243 0.243 0.247 0.247 0.247 0.247 0.247 0.247
               0.250 0.250 0.250 0.250 0.250 0.278 0.294 0.305 0.317 0.329
               0.337 0.349 0.356 0.364 0.372 0.376 0.384 0.388 0.396 0.400
               0.403 0.407 0.411 0.419 0.419 0.423 0.431 0.435 0.439 0.443
               0.447 0.450 0.454 0.458 0.462 0.466 0.466 0.470 0.474 0.474
               0.478 0.482 0.482 0.482 0.486 0.486 0.486 0.490 0.490 0.490
               0.490 0.490 0.490 0.490 0.494 0.494 0.494 0.494 0.494 0.494
               0.494 0.490 0.500 0.500 0.500 0.500 0.500 0.500 0.500 0.501
               0.502 0.503 0.504 0.505 0.506 0.507 0.508 0.509 0.510 0.511
               0.512 0.513 0.514 0.514 0.515 0.516 0.517 0.500 0.500 0.500
               0.500 0.500 0.500 0.500 0.501 0.502 0.503 0.504 0.505 0.506
               0.507 0.508 0.509 0.510 0.511 0.512 0.513 0.514 0.514 0.515
               0.516 0.517 0.500 0.500 0.500 0.500 0.500 0.500 0.500 0.501
               0.502 0.503 0.504 0.505 0.506 0.507 0.508 0.509 0.510 0.511
               0.512 0.513 0.514 0.514 0.515 0.516 0.517 0.500 0.500 0.500
               0.500 0.500 0.500 0.500 0.501 0.502 0.503 0.504 0.505 0.506
               0.507 0.508 0.509 0.510 0.511 0.512 0.513 0.514 0.514 0.515
               0.816 0.817 0.817 0.817 0.817 0.818 0.818 0.819 0.820 0.821
               0.902 0.903 0.904 0.905 0.906 0.907 0.908 0.909 0.910 0.911
               0.912 0.913 0.914 0.914 0.915 0.916 0.917 0.997 0.997 0.997
               0.997 0.997 0.997 0.997 0.998 1.000) 
            
            ; 125 125 125 125 125 125 125 130 134 137 141 145 148 151 153 156 158 160 162 163 165 166 167 168 170
            ; 171 171 172 173 174 175 176 177 178 178 179 180 181 181 182 183 183 184 184 185 185 186 186 187 187
            ; 188 188 189 189 189 189 190 190 190 190 191 191 191 191 191 191 191 191 191 191 193 194 196 197 198
            ; 200 201 203 204 205 207 208 209 211 212 213 214 215 217 218 219 220 220 221 222 222 223 223 224 224
            ; 224 224 224 223 223 222 222 221 221 220 219 218 217 216 215 214 213 212 211 210 209 208 206 205 204
            ; 203 202 200 199 198 197 196 194 194)
            ; ;;(0 5 9 13 16 19 22 25 27 29 30 32 33 34 35 36 38 39 40 41 43 44 46 47 48 49 50 51 52 53 54 55 55 56 56 57 57 58 58 59 59 59 60 60 60 61 61 61 61 62 62 62 62 62 63 63 63 63 63 63 64 64 64 64 64 71 75 78 81 84 86 89 91 93 95 96 98 99 101 102 103 104 105 107 107 108 110 111 112 113 114 115 116 117 118 119 119 120 121 121 122 123 123 123 124 124 124 125 125 125 125 125 125 125 126 126 126 126 126 126 126 125 125 125 125 125 125 125 125 130 134 137 141 145 148 151 153 156 158 160 162 163 165 166 167 168 170 171 171 172 173 174 175 176 177 178 178 179 180 181 181 182 183 183 184 184 185 185 186 186 187 187 188 188 189 189 189 189 190 190 190 190 191 191 191 191 191 191 191 191 191 191 193 194 196 197 198 200 201 203 204 205 207 208 209 211 212 213 214 215 217 218 219 220 220 221 222 222 223 223 224 224 224 224 224 223 223 222 222 221 221 220 219 218 217 216 215 214 213 212 211 210 209 208 206 205 204 203 202 200 199 198 197 196 194 194)
            ;#(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 127 125 123 121 119 117 115 113 111 109 107 105 103 101 99 97 95 93 91 89 87 85 83 81 79 77 75 73 71 69 67 65 63 61 59 57 55 53 51 49 47 45 43 41 39 37 35 33 31 29 27 25 23 21 19 17 15 13 11 9 7 5 3 1 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99 101 103 105 107 109 111 113 115 117 119 121 123 125 127 128 126 124 122 120 118 116 114 112 110 108 106 104 102 100 98 96 94 92 90 88 86 84 82 80 78 76 74 72 70 68 66 64 62 60 58 56 54 52 50 48 46 44 42 40 38 36 34 32 30 28 26 24 22 20 18 16 14 12 10 8 6 4 2)
                      #(0.000 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009
                      0.010 0.011 0.012 0.013 0.014 0.015 0.016 0.017 0.018 0.019
                      0.020 0.021 0.022 0.023 0.024 0.025 0.026 0.027 0.028 0.029
                      0.030 0.031 0.032 0.033 0.034 0.035 0.036 0.037 0.038 0.039
                      0.040 0.041 0.042 0.043 0.044 0.045 0.046 0.047 0.048 0.049
                      0.050 0.051 0.052 0.053 0.054 0.055 0.056 0.057 0.058 0.059
                      0.060 0.061 0.062 0.063 0.064 0.065 0.066 0.067 0.068 0.069
                      0.070 0.071 0.072 0.073 0.074 0.075 0.076 0.077 0.078 0.079
                      0.080 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009
                      0.090 0.011 0.012 0.013 0.014 0.015 0.016 0.017 0.018 0.019
                      0.100 0.021 0.022 0.023 0.024 0.025 0.026 0.027 0.028 0.029
                      0.110 0.031 0.032 0.033 0.034 0.035 0.036 0.037 0.038 0.039
                      0.120 0.041 0.042 0.043 0.044 0.045 0.046 0.047 0.048 0.049
                      0.130 0.051 0.052 0.053 0.054 0.055 0.056 0.057 0.058 0.059
                      0.140 0.061 0.062 0.063 0.064 0.065 0.066 0.067 0.068 0.069
                      0.150 0.071 0.072 0.073 0.074 0.075 0.076 0.077 0.078 0.079
                      0.160 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009
                      0.170 0.011 0.012 0.013 0.014 0.015 0.016 0.017 0.018 0.019
                      0.180 0.021 0.022 0.023 0.024 0.025 0.026 0.027 0.028 0.029
                      0.190 0.031 0.032 0.033 0.034 0.035 0.036 0.037 0.038 0.039
                      0.200 0.041 0.042 0.043 0.044 0.045 0.046 0.047 0.048 0.049
                      0.210 0.051 0.052 0.053 0.054 0.055 0.056 0.057 0.058 0.059
                      0.220 0.061 0.062 0.063 0.064 0.065 0.066 0.067 0.068 0.069
                      0.230 0.071 0.072 0.073 0.074 0.075 0.076 0.077 0.078 0.079
                      0.240 0.241 0.072 0.073 0.074 0.075 0.076 0.077 0.078 0.079
                      0.250 0.251 0.252 0.253 0.254 1.000
                      )
                      ))
          )
         
         
         (gimp-message "in contours")
         (gimp-message (number->string contour))
         (set! dummy-var 3)
         (if (< (- contour 1) 0)
            (begin
                (gimp-message "contour error")
                (quit)
            )
          )
         
        (if (= (list-ref contourtypes (- contour 1)) 0)
            (begin
                (gimp-message "contour type0")
                ;;(gimp-drawable-curves-spline drawable channel (vector-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
                (gimp-drawable-curves-spline drawable channel (list-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
            )
            (begin
                (gimp-message "contour type1")
                (gimp-drawable-curves-explicit drawable channel (list-ref contourlengths (- contour 1)) (vector-ref contours (- contour 1)))
            )
        )
        
        ;(gimp-message "leaving countours")
        
        
    )
)

(define (apply-noise img drawable srclayer noise)
    (let* (
            (drwwidth (car (gimp-drawable-width srclayer)))
            (drwheight (car (gimp-drawable-height srclayer)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets srclayer))
            (srcmask (car (gimp-layer-get-mask srclayer)))
            (noiselayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-noise") 100 0)))
            (blanklayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-noise") 100 0)))
          )
        ;(gimp-message "apply noise start row 232")
        (add-over-layer img noiselayer srclayer)
        (add-over-layer img blanklayer noiselayer)
        (gimp-layer-set-offsets noiselayer (car drwoffsets) (cadr drwoffsets))
        (gimp-layer-set-offsets blanklayer (car drwoffsets) (cadr drwoffsets))
        (gimp-selection-all img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill noiselayer FILL-FOREGROUND)
        (gimp-edit-fill blanklayer FILL-FOREGROUND)
        (gimp-context-set-foreground '(255 255 255))
        ;(gimp-message "apply noise row 203")
        
        (gimp-selection-load srcmask)
        (gimp-selection-invert img)
        (plug-in-colortoalpha 1 img noiselayer '(0 0 0))
        (gimp-selection-invert img)
        
        (gimp-edit-fill blanklayer FILL-FOREGROUND) ;; was drawable-edit-fill
        (plug-in-hsv-noise 1 img noiselayer 2 0 0 254) ;; was 1 0 0 255
        (gimp-layer-set-mode blanklayer LAYER-MODE-OVERLAY-LEGACY)
        (gimp-layer-set-opacity blanklayer noise)
        
        ;added by karlhof26 
        (plug-in-autocrop-layer 1 img noiselayer)
        (plug-in-autocrop-layer 1 img blanklayer)
        
        (set! noiselayer (car (gimp-image-merge-down img blanklayer 0)))
        (gimp-layer-create-mask noiselayer ADD-MASK-COPY)
        
        (set! blanklayer (car (gimp-layer-create-mask noiselayer ADD-MASK-COPY))); was add-mask-copy (5);
        (gimp-layer-add-mask noiselayer blanklayer)
        
         
        (gimp-channel-combine-masks srcmask blanklayer CHANNEL-OP-SUBTRACT 0 0) ; was 2 0 0
        
        
        (gimp-layer-set-mode noiselayer LAYER-MODE-OVERLAY-LEGACY)
                 
        (gimp-image-remove-layer img noiselayer)
        
        (gimp-displays-flush)
        (gc)
        ;(gimp-message "noise layer finish row 271")
    )
)

(define (script-fu-layerfx-drop-shadow img
                        drawable
                        colorkh
                        opacitykh
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
            (growamt (math-ceil (/ size 2))) ; was math-ceil
            (steps (math-round (- size (* (/ spread 100) size))))
            (lyrgrowamt (math-round (* growamt 1.2))) ; was math-round 
            ;(khopacity (* opacity 1.0))
            (shadowlayer (car (gimp-layer-new img (round (+ drwwidth (* lyrgrowamt 2))) (round (+ drwheight (* lyrgrowamt 2)))
                (cond 
                    ((= (car (gimp-image-base-type img)) 0) 1)
                    ((= (car (gimp-image-base-type img)) 1) 3)
                )
                (string-append layername "-dropshadow")
                100
                (get-blending-mode mode)
                )))
                ; opacity (get-blending-mode mode)
            ; (shadowlayer (car (gimp-layer-new img (+ drwwidth (* lyrgrowamt 2)) (+ drwheight (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-dropshadow") opacity (gimp-image-get-default-new-layer-mode img))))
            (shadowmask 0)
            (alphaSel 0)
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (* offsetdist (cos ang)))
            (offsetY (* offsetdist (sin ang)))
            (origmask 0)
            (opacity 0)
            (tempcolor '(0 0 0))
        )
        ;(gimp-message "drop shadow row 320")
        ; there is a bug passing opacity through; don't use opacitykh anywhere as a result.
        (set! opacity 100.0)
        
        ;(gimp-message (number->string (cdr (colorkh))))
        ;(gimp-message (number->string opacity))
        ;(gimp-message (number->string offsetdist))
        ;(gimp-message (number->string offsetX))
        ;(gimp-message (number->string lyrgrowamt))
        
        (add-under-layer img shadowlayer drawable)
        (gimp-layer-set-offsets shadowlayer (- (+ (car drwoffsets) offsetX) lyrgrowamt) (- (+ (cadr drwoffsets) offsetY) lyrgrowamt))
        ;(gimp-message "drop shadow row 332")
        (gimp-layer-set-opacity shadowlayer opacity) ; was opacitykh
        
        (gimp-selection-all img)
        ;(gimp-message "drop shadow row 336")
        (gimp-context-set-foreground '(0 0 50))
        ;(gimp-message "drop shadow row 338")
        (set! tempcolor '(0 0 10))
        (gimp-context-set-foreground colorkh)
        ;(gimp-message "drop shadow row 341")
        (gimp-drawable-edit-fill shadowlayer FILL-FOREGROUND)
        ;(gimp-message "drop shadow row 343")
        (gimp-selection-none img)
        (set! shadowmask (car (gimp-layer-create-mask shadowlayer ADD-MASK-BLACK))) ; was 1
        (gimp-layer-add-mask shadowlayer shadowmask)
        ;(gimp-message "drop shadow row 347")
        ; deprecated(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        
        ;(gimp-message "drop shadow row 351")
        (if (> (car (gimp-layer-get-mask drawable)) -1) ; was drawable
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3) ; was drawable
        )
        
        (gimp-selection-translate img offsetX offsetY)
        (set! alphaSel (car (gimp-selection-save img)))
        ;(gimp-message "drop shadow row 358")
        (draw-blurshape img shadowmask steps growamt alphaSel 0)
        (gimp-selection-none img)
        ;(gimp-message "drop shadow row 361")
        (if (> contour 0)
            (begin
                ;(gimp-displays-flush)
                ;(gimp-message "drop shadow row 365")
                ;(apply-contour shadowmask HISTOGRAM-VALUE 0)
                ;(apply-contour-kh shadowmask 0 4)
                ;
                ;(gimp-message "drop shadow row 369")
                ;(apply-contour-kh shadowmask 0 10)
                ;
                ;
                ;(gimp-message "drop shadow row 373")
                ;(apply-contour-kh shadowmask 0 2)
                
                (apply-contour-kh shadowmask 0 contour)
                ;(gimp-displays-flush)
                (gimp-selection-load alphaSel)
                ;(gimp-message "drop shadow row 379")
                (gimp-selection-grow img growamt)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-drawable-edit-fill shadowmask FILL-FOREGROUND)
                (gimp-selection-none img)
                ;(gimp-message "drop shadow row 374")
            )
        )
        ;(gimp-message "drop shadow row 387")
        (if (> noise 0)
            (apply-noise img drawable shadowlayer noise)
        )
        
        (if (= knockout TRUE)
            (begin
                (gimp-context-set-foreground '(0 0 0))
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (gimp-drawable-edit-fill shadowmask FILL-FOREGROUND)
            )
        )
        ;(gimp-message "drop shadow row 400")
        (gimp-layer-remove-mask shadowlayer 0)
        (gimp-selection-none img)
        (gimp-displays-flush)
        (if (= merge TRUE)
            (begin
                ;(gimp-message "drop shadow row 407")
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (gimp-layer-remove-mask drawable 0)
                )
                (gimp-layer-set-visible drawable TRUE)
                (set! shadowlayer (car (gimp-image-merge-down img drawable 0)))
                (gimp-drawable-set-name shadowlayer layername)
            )
        )
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-image-undo-group-end img)
        (gc) ; garbage collection
        ;(gimp-message "drop shadow good end")
    )
    
)

(define (script-fu-layerfx-inner-shadow img
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
            (origfgcolor (car (gimp-context-get-foreground)))
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
            (steps (round (- size chokeamt)))
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (round (* offsetdist (cos ang))))
            (offsetY (round (* offsetdist (sin ang))))
            (origmask 0)
            (alphamask 0)
        )
        (add-over-layer img shadowlayer drawable)
        (gimp-layer-set-offsets shadowlayer (car drwoffsets) (cadr drwoffsets))
        (gimp-selection-all img)
        (gimp-context-set-foreground color)
        (gimp-edit-fill shadowlayer 0)
        (gimp-selection-none img)
        (set! shadowmask (car (gimp-layer-create-mask shadowlayer ADD-MASK-BLACK)))
        (gimp-layer-add-mask shadowlayer shadowmask)
        ;(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        (if (> (car (gimp-layer-get-mask drawable)) -1)
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
        )
        (gimp-selection-translate img offsetX offsetY)
        (set! alphaSel (car (gimp-selection-save img)))
        (if (= source 0)
            (begin
                (gimp-selection-all img)
                (gimp-context-set-foreground '(255 255 255))
                (gimp-edit-fill shadowmask 0)
                (gimp-selection-load alphaSel)
                (draw-blurshape img shadowmask steps (- growamt chokeamt) alphaSel 1)
            )
            (begin
                (draw-blurshape img shadowmask steps (- growamt chokeamt) alphaSel 0)
            )
        )
        (gimp-selection-none img)
        (if (> contour 0)
            (begin
                (gimp-message "line534 apply contour")
                (apply-contour-kh shadowmask 0 contour)
            )
        )
        (if (= merge 0)
            (begin
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-edit-fill shadowmask 0)
            )
        )
        (if (> noise 0)
            (begin
                (apply-noise img drawable shadowlayer noise)
            )
        )
        
        (gimp-displays-flush)
        ;(gimp-message "line554")
        
        
        (gimp-layer-remove-mask shadowlayer 0)
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
                    (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                    (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                    (gimp-drawable-set-name shadowlayer layername)
                    (gimp-layer-add-mask shadowlayer alphamask)
                    (gimp-layer-remove-mask shadowlayer 0)
                    (if (> origmask -1)
                        (gimp-layer-add-mask shadowlayer origmask)
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
                    (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                    (gimp-drawable-set-name shadowlayer layername)
                    (if (> origmask -1)
                        (gimp-layer-add-mask shadowlayer origmask)
                    )
                )
            )
        )
        (gimp-selection-none img)
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-image-undo-group-end img)
        (gc)
    )
    
)
 

(define (script-fu-layerfx-outer-glow img
                        drawable
                        color
                        opacity
                        contour
                        noise
                        mode
                        spread
                        size
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
            (lyrgrowamt (round (* size 1.2)))
            (glowlayer (car (gimp-layer-new img (+ drwwidth (* lyrgrowamt 2)) (+ drwheight (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-outerglow") opacity (get-blending-mode mode))))
            (glowmask 0)
            (alphaSel 0)
            (growamt (* (/ spread 100) size))
            (steps (- size growamt))
            (origmask 0)
        )
        
        (add-under-layer img glowlayer drawable)
        (gimp-layer-set-offsets glowlayer (- (car drwoffsets) lyrgrowamt) (- (cadr drwoffsets) lyrgrowamt))
        (gimp-selection-all img)
        (gimp-context-set-foreground color)
        (gimp-edit-fill glowlayer 0)
        (gimp-selection-none img)
        (set! glowmask (car (gimp-layer-create-mask glowlayer ADD-MASK-BLACK))) ; was 1
        (gimp-layer-add-mask glowlayer glowmask)
        ;(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        (if (> (car (gimp-layer-get-mask drawable)) -1)
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
        )
        (set! alphaSel (car (gimp-selection-save img)))
        (draw-blurshape img glowmask steps size alphaSel 0)
        (gimp-selection-none img)
        (if (> contour 0)
            (begin
                (apply-contour-kh glowmask 0 contour)
                (gimp-selection-load alphaSel)
                (gimp-selection-grow img size)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-edit-fill glowmask 0)
                (gimp-selection-none img)
            )
        )
        (if (> noise 0)
            (apply-noise img drawable glowlayer noise)
        )
        (if (= knockout 1)
            (begin
                (gimp-context-set-foreground '(0 0 0))
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (gimp-edit-fill glowmask 0)
            )
        )
        (gimp-layer-remove-mask glowlayer 0)
        (gimp-selection-none img)
        (if (= merge 1)
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (gimp-layer-remove-mask drawable 0)
                )
                (set! glowlayer (car (gimp-image-merge-down img drawable 0)))
                (gimp-drawable-set-name glowlayer layername)
            )
        )
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-image-undo-group-end img)
        (gc)
    )
    
)

(define (script-fu-layerfx-inner-glow img
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
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (glowlayer (car (gimp-layer-new img drwwidth drwheight 
                        (cond ((= (car (gimp-image-base-type img)) 0) 1)
                         ((= (car (gimp-image-base-type img)) 1) 3))
                        ;RGBA-IMAGE
                        (string-append layername "-innerglow") opacity 
                        (get-blending-mode mode)
                        )))
            
            (glowmask 0)
            (alphaSel 0)
            (shrinkamt (* (/ choke 100) size))
            (steps (- size shrinkamt))
            (i 0)
            (currshade 0)
            (origmask 0)
            (alphamask 0)
        )
        
        (gimp-context-push)
        (gimp-message "inner glow - line732")
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
        (gimp-message (number->string (get-blending-mode mode)))
        (gimp-message (number->string mode))
        
        (add-over-layer img glowlayer drawable)
        (gimp-layer-set-offsets glowlayer (car drwoffsets) (cadr drwoffsets))
        (gimp-selection-all img)
        (gimp-context-set-foreground color)
        (gimp-edit-fill glowlayer FILL-FOREGROUND)
        (gimp-selection-none img)
        (gimp-message "inner glow - line750")
        (set! glowmask (car (gimp-layer-create-mask glowlayer ADD-MASK-BLACK))) ; was 1
        (gimp-layer-add-mask glowlayer glowmask)
        ;(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        (if (> (car (gimp-layer-get-mask drawable)) -1)
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
        )
        (set! alphaSel (car (gimp-selection-save img)))
        (gimp-message "inner glow - line759")
        (if (= source 0)
            (begin
                (gimp-message "inner glow - line762 - inside source=0")
                (gimp-selection-all img)
                (gimp-context-set-foreground '(255 255 255))
                (gimp-edit-fill glowmask FILL-FOREGROUND)
                (gimp-selection-load alphaSel)
                (draw-blurshape img glowmask steps (- (* shrinkamt -1) 1) alphaSel 1)
            )
            (draw-blurshape img glowmask steps (* shrinkamt -1) alphaSel 0)
        )
        (gimp-message "inner glow - line771")
        (gimp-selection-none img)
        (if (> contour 0)
            (apply-contour-kh glowmask 0 contour)
        )
        (if (and (= source 0) (= merge 0))
            (begin
                (gimp-selection-load alphaSel)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-edit-fill glowmask 0)
            )
        )
        (gimp-message "inner glow - line784")
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
                    (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
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
        (gimp-message "inner glow - line824")
        (gimp-selection-none img)
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-message "inner glow - line759")
        (gimp-message "inner glow - ending")
        (gimp-image-undo-group-end img)
        (gimp-context-pop)
        (gimp-displays-flush)
        (gc) ; memory cleanup; garbage cleanup
    )
    
)

(define (script-fu-layerfx-bevel-emboss img
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
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (layername (car (gimp-drawable-get-name drawable)))
            (imgtype (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)))
            (lyrgrowamt (round (* size 1.2)))
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
            
            (expandedsel)
            (floatingselshad)
            (bumpcopyresult)
        )
        (gimp-context-push)
        (gimp-message "emboss line 885")
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
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
            (
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
        (set! bumpmaplayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-bumpmap") 100 0)))
        (set! highlightlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-highlight") highlightopacity (get-blending-mode highlightmode))))
        (set! shadowlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-shadow") shadowopacity (get-blending-mode shadowmode))))
        (add-over-layer img bumpmaplayer drawable)
        (add-over-layer img shadowlayer bumpmaplayer)
        (add-over-layer img highlightlayer shadowlayer)
        (gimp-layer-set-offsets bumpmaplayer (caddr layersize) (cadddr layersize))
        (gimp-layer-set-offsets shadowlayer (caddr layersize) (cadddr layersize))
        (gimp-layer-set-offsets highlightlayer (caddr layersize) (cadddr layersize))
        
        (gimp-message "emboss line 950")
        ;(gimp-displays-flush)
        
        
        ; was (gimp-selection-all img)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        
        
        ;added by karlhof26 to enable colours to expand correctly
        (set! halfsizec (floor (/ size 2)))
        ;(gimp-message (number->string halfsizec))
        (if (= style 0)
            (begin
                (gimp-selection-grow img size)
            )
            (begin
                (if (or (= style 2) (= style 3))
                    (begin
                        (gimp-selection-grow img halfsizec)
                    )
                )
            )
        )
        
        (gimp-context-set-foreground highlightcolor)
        (gimp-edit-fill highlightlayer 0)
        (gimp-context-set-foreground shadowcolor)
        (gimp-edit-fill shadowlayer 0)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-displays-flush)
        (gimp-edit-fill bumpmaplayer FILL-FOREGROUND)
        ;(gimp-message "emboss line 901")
        ;(gimp-displays-flush)
        
        ;added by kh to enable colours to expand correctly
        (if (= style 0)
            (gimp-selection-shrink img size)
            (if (or (= style 2) (= style 3))
                (gimp-selection-shrink img halfsizec)
            )
        )
        
        
        (set! highlightmask (car (gimp-layer-create-mask highlightlayer ADD-MASK-SELECTION)))
        (set! shadowmask (car (gimp-layer-create-mask shadowlayer ADD-MASK-SELECTION))) ; was 1
        (gimp-layer-add-mask highlightlayer highlightmask)
        (gimp-layer-add-mask shadowlayer shadowmask)
        
        ;(gimp-displays-flush)
        
        
        ;(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        (gimp-message "emboss line 915")
        
        
        
        (if (> (car (gimp-layer-get-mask drawable)) -1)
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
        )
        (set! alphaSel (car (gimp-selection-save img)))
        ;(gimp-displays-flush)
        ;(gimp-message "emboss line 941")
        
        
        (cond
            ((= style 0)
                (begin
                    (gimp-message "style 0")
                    (draw-blurshape img bumpmaplayer size size alphaSel 0)
                )
            )
            ((= style 1)
                (begin
                    (draw-blurshape img bumpmaplayer size 0 alphaSel 0)
                )
            )
            ((= style 2)
                (begin
                    (set! halfsizec (math-ceil (/ size 2)))
                    (draw-blurshape img bumpmaplayer size halfsizec alphaSel 0)
                )
            )
            (else
                (begin
                    (gimp-message "emboss line 1013")
                    (set! halfsizef (floor (/ size 2)))
                    (set! halfsizec (- size halfsizef))
                    (gimp-selection-all img)
                    (gimp-context-set-foreground '(255 255 255))
                    (gimp-edit-fill bumpmaplayer 0)
                    (draw-blurshape img bumpmaplayer halfsizec halfsizec alphaSel 1)
                    (draw-blurshape img bumpmaplayer halfsizef 0 alphaSel 0)
                )
            )
        )
        (gimp-message "emboss line 1024")
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-image-select-item img CHANNEL-OP-ADD bumpmaplayer)
        (gimp-message "emboss line 1029")
        ;(gimp-selection-all img)
        
        (gimp-context-set-foreground '(127 127 127))
        (gimp-edit-fill highlightmask 0)
        
        ;(gimp-selection-all img)
        
        
        (if (> surfacecontour 0)
            (begin
                (gimp-message "line1040")
                (apply-contour-kh bumpmaplayer 0 surfacecontour)
            )
        )
        (gimp-message "emboss line 1044")
        (gimp-message (number->string style))
        (gimp-message (number->string surfacecontour))
        
        (gimp-message (number->string angle))
        (gimp-message (number->string altitude))
        (gimp-message (number->string depth))
        (gimp-message (number->string direction))
        (gimp-message "emboss line 1051")
        (gimp-displays-flush)
        
        
        (gimp-selection-none img)
        ;(gimp-image-select-item img CHANNEL-OP-ADD bumpmaplayer)
        
        (if (< angle 0)
            (begin
                (gimp-message "angle change")
                (set! angle (+ angle 360))
            )
        )
        
        (gimp-message "emboss line 1066")
        ;(plug-in-bump-map 1 img highlightmask bumpmaplayer angle altitude depth 0 0 0 0 1 direction 0)
        (plug-in-bump-map 1 img highlightmask bumpmaplayer angle altitude depth 0 0 0 0 1 direction 1)
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-image-select-item img CHANNEL-OP-ADD bumpmaplayer)
        (gimp-message "emboss line 1073")
        (if (> glosscontour 0)
            (begin
                (gimp-message "gloss contour")
                (apply-contour-kh highlightmask 0 glosscontour)
                (apply-contour-kh highlightlayer 0 glosscontour)
            )
            (begin
                (gimp-message "No gloss")
            )
        )
        (if (> soften 0)
            (begin
                (gimp-message "soften blur")
                (plug-in-gauss-rle 1 img highlightmask soften 1 1)
            )
        )
        (if (= invert TRUE) ;;(> invert 0) 
            (begin
                (gimp-message "invert")
                (gimp-drawable-invert highlightmask FALSE)
            )
            (begin
                (gimp-message "invert FALSE")
            )
        )
        (gimp-message "emboss line 1099")
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-channel-combine-masks shadowmask highlightmask CHANNEL-OP-SUBTRACT 0 0)
        ;(if (>= style 2)
        ;    (gimp-channel-combine-masks shadowmask bumpmaplayer CHANNEL-OP-ADD 0 0)
        ;)
        
        (gimp-message "emboss line 1106")
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-drawable-levels highlightmask HISTOGRAM-VALUE 0.4 1.0 TRUE 1.0 0.0 1.0 TRUE)  ; was 0 127 255 1.0 0 255 ; 0.5 1.0 TRUE 1.0...
        (gimp-drawable-levels shadowmask HISTOGRAM-VALUE 0.0 0.55 TRUE 1.3 0.0 1.0 TRUE) ; was 0 0 127 1.0 255 0; 0.0 0.49 TRUE 1.0...
        (gimp-drawable-levels highlightmask HISTOGRAM-VALUE 0.0 1.0 TRUE 1.9 0.0 1.0 TRUE)
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-selection-load alphaSel)
        (gimp-displays-flush)
        ;(quit)
        
        (if (= style 0)
            (begin
                (gimp-message "emboss line 1128 style0 grow")
                (gimp-selection-grow img size)
                
                (gimp-context-set-foreground '(255 255 255))
                (gimp-edit-fill shadowmask FILL-FOREGROUND)
                
            )
            (begin
                (if (or (= style 2) (= style 3))
                    (begin
                        (gimp-message "style 2 or 3")
                        
                                
                        
                        ;(gimp-selection-grow img halfsizec)
                        
                        ; karlhof26 addition
                        ;(gimp-selection-grow img size)
                        ;(gimp-context-set-foreground '(0 0 0))
                        ;(gimp-edit-fill shadowlayer FILL-FOREGROUND)
                        
                        (gimp-selection-grow img halfsizec)
                        (gimp-context-set-foreground '(255 255 255))
                        (gimp-edit-fill shadowmask FILL-FOREGROUND)
                        
                        
                        ;(gimp-selection-grow img halfsizec)
                        
                        
                        
                    )
                )
            )
            (begin
                (gimp-message "emboss line 1162 style1 no grow needed")
                ;(gimp-selection-grow img size)
                
                (gimp-context-set-foreground '(255 255 255))
                (gimp-edit-fill shadowmask FILL-FOREGROUND)
            )
        )
        (gimp-message "line1169 emboss")
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-message "emboss line 1127")
        (gimp-selection-invert img)
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill shadowmask FILL-FOREGROUND)
        
        ; new addition
        ; now create a selection to get the needed from bummaplayer
        ;invert back
        
        (gimp-selection-invert img)
        ; make the inner selection
        (gimp-selection-shrink img size)
        (set! expandedsel (car (gimp-selection-save img)))
        
        ; make outer selection and subtract
        (gimp-selection-grow img size)
        (gimp-image-select-item img CHANNEL-OP-SUBTRACT expandedsel)
        
        ; coppy from bummaplayer to shadowlayer
        (set! bumpcopyresult (car (gimp-edit-copy bumpmaplayer)))
        (set! floatingselshad (car (gimp-edit-paste shadowlayer FALSE)))
        
        (gimp-floating-sel-anchor floatingselshad)
        
        (gimp-image-select-item img CHANNEL-OP-REPLACE expandedsel)
         
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill shadowmask FILL-FOREGROUND)
        
                        
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-message "emboss 1205")
        (gimp-selection-none img)
        
        ; can't remove shadowlayer - need it an masks for follow-on scripts and actions
        (gimp-image-remove-layer img bumpmaplayer)
        
        
        (gimp-displays-flush)
        ;(quit)
        
        (gimp-message "emboss 1215")
        (if (= merge TRUE)
          (begin
            (gimp-message "emboss 1218")
            (if (= style 1)
                (begin
                    ;(gimp-message "emboss line 1081 style 1")
                    (set! origmask (car (gimp-layer-get-mask drawable)))
                    (if (> origmask -1)
                        (begin
                            (set! origmask (car (gimp-channel-copy origmask)))
                            (gimp-layer-remove-mask drawable 1)
                        )
                    )
                    (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                    (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                    (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
                    (gimp-drawable-set-name highlightlayer layername)
                    (gimp-layer-add-mask highlightlayer alphamask)
                    (gimp-message "emboss line 1234")
                    (gimp-layer-remove-mask highlightlayer 0)
                    
                    (if (> origmask -1)
                        (begin
                            (gimp-layer-add-mask highlightlayer origmask)
                        )
                    )
                )
                (begin
                    (gimp-message "emboss line 1244 merge style 0 or >=2")
                    (set! origmask (car (gimp-layer-get-mask drawable)))
                    (if (> origmask -1)
                        (gimp-layer-remove-mask drawable 0)
                    )
                    (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
                    (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
                    (gimp-drawable-set-name highlightlayer layername)
                    
                    ;(gimp-drawable-brightness-contrast img highlightlayer 1.5 1.5)
                )
            )
          )
          (begin
                (gimp-message "emboss 1258 No merge")
          )
        )
        
        (gimp-message "emboss 1262")
        (gimp-drawable-brightness-contrast highlightlayer 0.47 0.45)
        
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
    )
    (gimp-message "emboss - Good finish OK")
    (gimp-context-pop)
    (gimp-image-undo-group-end img)
    (gc) ; garbage cleanup; memory cleanup ; array was used.
)

(define (script-fu-layerfx-satin img
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
    
  (let* (
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (layername (car (gimp-drawable-get-name drawable)))
            (growamt (math-ceil (/ size 2)))
            (lyrgrowamt (round (* growamt 1.2)))
            (satinlayer (car (gimp-layer-new img (+ (car (gimp-drawable-width drawable)) (* lyrgrowamt 2)) (+ (car (gimp-drawable-height drawable)) (* lyrgrowamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-satin") 100 0)))
            (satinmask 0)
            (blacklayer 0)
            (drwoffsets (gimp-drawable-offsets drawable))
            (ang (* (* (+ offsetangle 180) -1) (/ (* 4 (atan 1.0)) 180)))
            (offsetX (round (* offsetdist (cos ang))))
            (offsetY (round (* offsetdist (sin ang))))
            (alphaSel 0)
            (layeraoffsets 0)
            (layerboffsets 0)
            (dx 0)
            (dy 0)
            (origmask 0)
            (alphamask 0)
        )
        (gimp-image-undo-group-start img)
        (gimp-context-push)
        (gimp-message "satin line 1332")
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
        
        (add-over-layer img satinlayer drawable)
        (gimp-layer-set-offsets satinlayer (- (car drwoffsets) lyrgrowamt) (- (cadr drwoffsets) lyrgrowamt))
        (gimp-selection-all img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill satinlayer 0)
        (gimp-selection-none img)
        ;(gimp-selection-layer-alpha drawable)
        (gimp-image-select-item img CHANNEL-OP-ADD drawable)
        (if (> (car (gimp-layer-get-mask drawable)) -1)
            (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
        )
        (set! alphaSel (car (gimp-selection-save img)))
        (draw-blurshape img satinlayer size growamt alphaSel 0)
        (plug-in-autocrop-layer 1 img satinlayer)
        (set! satinmask (car (gimp-layer-copy satinlayer 0)))
        (add-over-layer img satinmask satinlayer)
        (gimp-layer-translate satinlayer offsetX offsetY)
        (gimp-layer-translate satinmask (* offsetX -1) (* offsetY -1))
        (set! layeraoffsets (gimp-drawable-offsets satinlayer))
        (set! layerboffsets (gimp-drawable-offsets satinmask))
        (set! dx (- (max (car layeraoffsets) (car layerboffsets)) (min (car layeraoffsets) (car layerboffsets))))
        (set! dy (- (max (cadr layeraoffsets) (cadr layerboffsets)) (min (cadr layeraoffsets) (cadr layerboffsets))))
        (set! blacklayer (car (gimp-layer-new img (+ (car (gimp-drawable-width satinlayer)) dx) (+ (car (gimp-drawable-height satinlayer)) dy) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-satinblank") 100 0)))
        (add-under-layer img blacklayer satinlayer)
        (gimp-layer-set-offsets blacklayer (min (car layeraoffsets) (car layerboffsets)) (min (cadr layeraoffsets) (cadr layerboffsets)))
        (gimp-selection-all img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill blacklayer 0)
        (gimp-selection-none img)
        (gimp-layer-set-mode satinmask LAYER-MODE-DIFFERENCE-LEGACY)
        (set! satinlayer (car (gimp-image-merge-down img satinlayer 0)))
        (set! satinlayer (car (gimp-image-merge-down img satinmask 0)))
        (gimp-drawable-set-name satinlayer (string-append layername "-satin"))
        (if (> contour 0)
            (begin
                (apply-contour-kh satinlayer 0 contour)
                (gimp-selection-load alphaSel)
                (gimp-selection-grow img size)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-edit-fill satinlayer 0)
                (gimp-selection-none img)
            )
        )
        (if (= invert 1)
            (gimp-drawable-invert satinlayer TRUE)
        )
        (set! satinmask (car (gimp-layer-create-mask satinlayer ADD-MASK-COPY))) ; was 5
        (gimp-layer-add-mask satinlayer satinmask)
        (gimp-selection-all img)
        (gimp-context-set-foreground color)
        (gimp-edit-fill satinlayer 0)
        (gimp-selection-none img)
        (gimp-layer-set-opacity satinlayer opacity)
        (gimp-layer-set-mode satinlayer (get-blending-mode mode))
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
                (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                (set! satinlayer (car (gimp-image-merge-down img satinlayer 0)))
                (gimp-drawable-set-name satinlayer layername)
                (gimp-layer-add-mask satinlayer alphamask)
                (gimp-layer-remove-mask satinlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask satinlayer origmask)
                )
            )
            ; else
            (begin
                (gimp-selection-load alphaSel)
                (gimp-selection-invert img)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-edit-fill satinmask 0)
            )
        )
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaSel)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-image-undo-group-end img)
        (gimp-context-pop)
        ;(gimp-message "OK line 1092")
        (gc)
  )
)

(define (script-fu-layerfx-stroke img
                    drawable
                    color
                    opacity
                    mode
                    size
                    position
                    merge)
    (gimp-image-undo-group-start img)
  (let* (
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (layername (car (gimp-drawable-get-name drawable)))
            (strokelayer 0)
            (drwoffsets (gimp-drawable-offsets drawable))
            (alphaselection 0)
            (outerselection 0)
            (innerselection 0)
            (origmask 0)
            (alphamask 0)
            (outerwidth 0)
            (innerwidth 0)
            (growamt 0)
        )
        (gimp-context-push)
        (gimp-message "stroke line 1464")
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
        
        (if (= position 0)
            (begin
                (set! strokelayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
                (add-over-layer img strokelayer drawable)
                (gimp-layer-set-offsets strokelayer (car drwoffsets) (cadr drwoffsets))
                (gimp-selection-all img)
                (gimp-edit-clear strokelayer)
                (gimp-selection-none img)
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                )
                (set! alphaselection (car (gimp-selection-save img)))
                (gimp-selection-shrink img size)
                (set! innerselection (car (gimp-selection-save img)))
                (if (= merge 1)
                    (begin
                        (set! origmask (car (gimp-layer-get-mask drawable)))
                        (if (> origmask -1)
                            (begin
                                (set! origmask (car (gimp-channel-copy origmask)))
                                (gimp-layer-remove-mask drawable 1)
                            )
                        )
                        (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                        (gimp-selection-none img)
                        (gimp-threshold alphaselection 1 255)
                        (gimp-selection-load alphaselection)
                        (gimp-selection-combine innerselection 1)
                        (gimp-context-set-foreground color)
                        (gimp-edit-fill strokelayer 0)
                        (set! strokelayer (car (gimp-image-merge-down img strokelayer 0)))
                        (gimp-drawable-set-name strokelayer layername)
                        (gimp-layer-add-mask strokelayer alphamask)
                        (gimp-layer-remove-mask strokelayer 0)
                        (if (> origmask -1)
                            (gimp-layer-add-mask strokelayer origmask)
                        )
                    )
                    (begin
                        (gimp-selection-load alphaselection)
                        (gimp-selection-combine innerselection 1)
                        (gimp-context-set-foreground color)
                        (gimp-edit-fill strokelayer 0)
                    )
                )
            )
        )
        (if (= position 100)
            (begin
                (set! growamt (round (* size 1.2)))
                (set! strokelayer (car (gimp-layer-new img (+ drwwidth (* growamt 2)) (+ drwheight (* growamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
                (add-under-layer img strokelayer drawable)
                (gimp-layer-set-offsets strokelayer (- (car drwoffsets) growamt) (- (cadr drwoffsets) growamt))
                (gimp-selection-all img)
                (gimp-edit-clear strokelayer)
                (gimp-selection-none img)
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                )
                (set! alphaselection (car (gimp-selection-save img)))
                (set! innerselection (car (gimp-selection-save img)))
                (gimp-selection-none img)
                (gimp-threshold innerselection 255 255)
                (gimp-selection-load alphaselection)
                (gimp-selection-grow img size)
                (gimp-selection-combine innerselection 1)
                (gimp-context-set-foreground color)
                (gimp-edit-fill strokelayer 0)
                (if (= merge 1)
                    (begin
                        (set! origmask (car (gimp-layer-get-mask drawable)))
                        (if (> origmask -1)
                            (gimp-layer-remove-mask drawable 0)
                        )
                        (set! strokelayer (car (gimp-image-merge-down img drawable 0)))
                        (gimp-drawable-set-name strokelayer layername)
                    )
                )
            )
        )
        (if (and (< position 100) (>= position 1))
            (begin
                (gimp-message "stroke line1516")
                (set! outerwidth (round (* (/ position 100) size)))
                (set! innerwidth (- size outerwidth))
                (set! growamt (round (* outerwidth 1.2)))
                (set! strokelayer (car (gimp-layer-new img (+ drwwidth (* growamt 2)) (+ drwheight (* growamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
                (add-over-layer img strokelayer drawable)
                (gimp-layer-set-offsets strokelayer (- (car drwoffsets) growamt) (- (cadr drwoffsets) growamt))
                (gimp-selection-all img)
                (gimp-edit-clear strokelayer)
                (gimp-selection-none img)
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                )
                (set! alphaselection (car (gimp-selection-save img)))
                (gimp-selection-shrink img innerwidth)
                (set! innerselection (car (gimp-selection-save img)))
                (gimp-selection-load alphaselection)
                (gimp-selection-grow img outerwidth)
                (gimp-selection-combine innerselection 1)
                (gimp-context-set-foreground color)
                (gimp-edit-fill strokelayer 0)
                (if (= merge 1)
                    (begin
                        (set! origmask (car (gimp-layer-get-mask drawable)))
                        (if (> origmask -1)
                            (gimp-layer-remove-mask drawable 0)
                        )
                        (set! strokelayer (car (gimp-image-merge-down img strokelayer 0)))
                        (gimp-drawable-set-name strokelayer layername)
                    )
                )
            )
        )
        (gimp-message "stroke line1551")
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img alphaselection)
        (gimp-image-remove-channel img innerselection)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-image-undo-group-end img)
        (gimp-context-pop)
        (gc)
        (gimp-message "Ok stroke line 1560")
  )
)


(define (script-fu-layerfx-color-overlay img
                        drawable
                        color
                        opacity
                        mode
                        merge)
        (gimp-image-undo-group-start img)
    (let* (
            (origfgcolor (car (gimp-context-get-foreground)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (colorlayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-color") opacity (get-blending-mode mode))))
            (origmask 0)
            (alphamask 0)
          )
        (gimp-context-push)
        (gimp-message "color overlay line 1629")
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
        
        (add-over-layer img colorlayer drawable)
        (gimp-layer-set-offsets colorlayer (car drwoffsets) (cadr drwoffsets))
        (gimp-selection-all img)
        (gimp-context-set-foreground color)
        (gimp-edit-fill colorlayer 0)
        (gimp-selection-none img)
        (if (= merge 1)
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER)))
                (set! colorlayer (car (gimp-image-merge-down img colorlayer 0)))
                (gimp-drawable-set-name colorlayer layername)
                (gimp-layer-add-mask colorlayer alphamask)
                (gimp-layer-remove-mask colorlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask colorlayer origmask)
                )
            )
            (begin
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                )
                (set! alphamask (car (gimp-layer-create-mask colorlayer ADD-MASK-SELECTION))) ; was 4
                (gimp-layer-add-mask colorlayer alphamask)
                (gimp-layer-remove-mask colorlayer 0)
            )
        )
        (gimp-context-set-foreground origfgcolor)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-context-pop)
        (gimp-image-undo-group-end img)
        (gc)
        ;;(gimp-message "Ok line 1358")
    )
)


(define (script-fu-layerfx-gradient-overlay img 
                        drawable
                        gradblendmode
                        paintmode
                        grad
                        gradtype
                        repeattype
                        reversetrue
                        opacityd
                        mode
                        cx
                        cy
                        gradangle
                        gradsize
                        merge)
       
    (let* (
            (origgradient (car (gimp-context-get-gradient)))
            (origselection (car (gimp-selection-save img)))
            (drwwidth (car (gimp-drawable-width drawable)))
            (drwheight (car (gimp-drawable-height drawable)))
            (layername (car (gimp-drawable-get-name drawable)))
            (drwoffsets (gimp-drawable-offsets drawable))
            (newopacity opacityd)
            (basetypecalc 1)
            (gradientlayer 0) 
            (origmask 0)
            (alphamask 0)
            (ang   (* (* (* gradangle -1) (/ (* 4 (atan 1.0)) 180)) 1))
            
            (angkh (* (* (* gradangle  1) (/ (* 4 (atan 1.0)) 180)) 1))
            (angkh2 gradangle)
            (stdunitkh 0) ; the 01 is to cater for numbers close to zero
            
            (offsetX (+ (* (cos angkh) (/ gradsize 2)) stdunitkh))
            (offsetY (+ (* (sin angkh) (/ gradsize 2)) stdunitkh)) 
            
            (x1 (+ (- cx offsetX) (car drwoffsets)))
            (y1 (+ (- cy offsetY) (cadr drwoffsets)))
            (x2 (+ (+ cx offsetX) (car drwoffsets)))
            (y2 (+ (+ cy offsetY) (cadr drwoffsets)))
            
            
            (dxkh (- x1 x2))
            (dykh (- y1 y2))
            (counterkh 0)
            
            (finalname "x")
            (finblendmode 0)
            (fingrad 1)
            (mappaintmode 22)
            (gradslopeoffsetpercent 0)
            
            (listk '(1 2 5 11 21 34 12 18 2 4 11 12 13 14 13 133 1333 12 11 8 4))
            (modekk '(0 1 3 6 4 5 16 17 18 19 20 37 38 39 40 32 42 43 44 45 46 47 57 58 62 63 28 27))
            
            
          )
          (gc) ; garbage clean
        (gimp-image-undo-group-start img)
        (gimp-message "layerfx gradover start")
        
        (gimp-context-push)
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
        (gimp-message "list-ref test")
        (gimp-message "in= 15")
        (gimp-message (number->string (list-ref listk 15)))
        (gimp-message (number->string (list-ref modekk 15)))
        (gimp-message (number->string (list-ref modekk 22)))
        
        (if (> newopacity 100)
            (set! newopacity 100.0)
        )
        (if (< newopacity 0)
            (begin
                (gimp-message "opacity passed is somehow negative")
                (set! newopacity 50.0)
            )
        )            
        
        ;(gimp-message "layerfx gradover start2")
        (set! basetypecalc
           (cond 
               ((= (car (gimp-image-base-type img)) 0) 1)
               ((= (car (gimp-image-base-type img)) 1) 3))
        )
        
        ;(gimp-message (number->string drwwidth))
        ;(gimp-message (number->string drwheight))
        ;(gimp-message (number->string cx))
        ;(gimp-message (number->string cy))
        ;(gimp-message (number->string gradangle))
        ;(gimp-message (number->string ang))
        ;(gimp-message (number->string angkh))
        ;(gimp-message "G x calcs")
        ;(gimp-message (number->string offsetX))
        (if (< (abs offsetX) (/ gradsize 15))
            (begin
                ;(gimp-message "G offset v close to zero so add a bit=1") ; this messes with the angle a bit but enables a difference
                (set! offsetX (+ offsetX 1))
                (set! x1 (+ (- cx offsetX) (car drwoffsets)))
                (set! x2 (+ (+ cx offsetX) (car drwoffsets)))
            )
        )
        ;(gimp-message (number->string dxkh))
        ;(gimp-message (number->string dykh))
       ; (while (and (< (abs dxkh) gradsize) (< (abs dykh) (/ gradsize 1)) (< counterkh 5) (> x1 (/ gradsize 2)) (> y1 (/ gradsize 2)))
       ;     ;(begin
       ;         (gimp-message "G expand the difference but may not work")
       ;        (set! x1 (- x1 dxkh))
       ;         (set! y1 (- y1 dykh))
       ;         (set! dxkh (- x1 x2))
       ;         (set! dykh (- y1 y2))
       ;         (set! counterkh (+ counterkh 1))
       ;     ;)
       ; )
        
        ;(gimp-message (number->string (car drwoffsets)))
        ;(gimp-message (number->string x1))
        ;(gimp-message (number->string x2))
        ;(gimp-message "G Y calcs")
        ;(gimp-message (number->string offsetY))
        ;(gimp-message (number->string (cadr drwoffsets)))
        ;(gimp-message (number->string y1))
        ;(gimp-message (number->string y2))
        ;(gimp-message (number->string (* 4 (atan 1.0))))
        ;(gimp-message "break 1")
        
        ; given an angle in radians
        ;x = Cos(angle) * radius + CenterX;
        ;y = Sin(angle) * radius + CenterY;
        ; radians = degress x pi / 180
        ;(gimp-message "HH Hay-ch H")
        ;(gimp-message (number->string (* gradangle (/ 3.141593 180))))
        ;(gimp-message (number->string (+ (* (cos 1.5) 100) 500)))
    ;    (gimp-message (number->string (* ang (/ 3.141593 180))))
    ;    (gimp-message (number->string (cos (* ang (/ 3.141593 180)))))
    ;    (gimp-message (number->string (* gradsize (cos (* ang (/ 3.141593 180)))))) 
    ;    ;
        ;(gimp-message "II eye eye")
        ;(gimp-message (number->string gradsize))
        ;(gimp-message (number->string opacityd))
        ;(gimp-message (number->string repeattype))
        ;(gimp-message (number->string basetypecalc))
        ;(gimp-message (number->string newopacity))
        
        ;(gimp-message "JJ jay jay")
        (gimp-message "toets")
        ;(gimp-message "gradtype check")
        ;(if (= gradtype 1)
        ;    (gimp-message "Ok")
        ;    (gimp-message "ok bad")
        ;)
        ;(gimp-message (number->string gradtype))
        
        ;(gimp-message (number->string gradblendmode))
    
        (set! fingrad (get-grad-blending-mode gradblendmode))
        
        (gimp-message (number->string gradblendmode))
        (gimp-message (number->string fingrad))
        (gimp-message "paintmode=")
        ;(gimp-message (number->string paintmode))
    
        ;(set! mappaintmode (get-blending-mode paintmode))
        (set! mappaintmode (list-ref modekk paintmode))
        (gimp-message (number->string paintmode))
        (gimp-message "mappedpaintmode=")
        (gimp-message (number->string mappaintmode))
        (gimp-message "about to gimp context set") 
        
        (gimp-context-set-gradient grad)
        
        ;(gimp-message "gimp context set OK")
        ;(gimp-message (number->string repeattype))
        ;(gimp-message "JJpt2")
        
        (set! finalname (string-append layername "-gradient"))
        
        ;(gimp-message (number->string mode))
        ;(gimp-message "JJpt3")
        
        (set! finblendmode (get-blending-mode mode))
        ;(set! finblendmode (list-ref modekk mode))
        (gimp-message "JJpt4")
        (gimp-message (number->string finblendmode))
        ;(set! finblendmode (get-blending-mode mode))
        ;(gimp-message "JJ2")
        ; (gimp-message (number->string finblendmode))
        ;(gimp-message "KK")
        (set! gradientlayer (car (gimp-layer-new img drwwidth drwheight basetypecalc finalname newopacity LAYER-MODE-NORMAL)))
        (gimp-layer-set-mode gradientlayer mappaintmode)
        ;(gimp-message " two")
        
        ;(add-over-layer img gradientlayer drawable)
        (gimp-image-insert-layer img gradientlayer 0 -1)
        
        (gimp-message " three")
        (gimp-message (number->string (car drwoffsets)))
        (gimp-message (number->string (cadr drwoffsets)))
        (gimp-layer-set-offsets gradientlayer (car drwoffsets) (cadr drwoffsets))
        
        ;(gimp-message " three a")
        (gimp-selection-none img)
        ;(gimp-message " three b")
        (gimp-edit-clear gradientlayer)
        ;(gimp-message " three c")
        ;(gimp-context-set-gradient "Fruit15Apple")
        ;(gimp-message " three c1")
        ;(gimp-context-set-gradient "2 ton Gold 03")
        ;(gimp-message " three d")
        ;(gimp-message grad)
        ;(gimp-message " three d1")
        (gimp-context-set-gradient grad)
        ;(gimp-message " three d2")
        (if (and (>= gradtype 6) (<= gradtype 8))
            (begin
                ;(gimp-message "type 6 , 7 or 8 ie Symmetrical")
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
            )
            (begin
                ;(gimp-message "type 1 to 5 or 9 plus (ie not symmetrical)")
            )
        )
        ;(gimp-message " four")
        (if (or (= gradtype 2) (= gradtype 3))
            (begin
                ;(gimp-message "grad type 2 or 3")
                (set! x1 (+ (+ cx 0) (car drwoffsets)))
                (set! y1 (+ (- cy (/ gradsize 5)) (cadr drwoffsets)))
                (set! x2 (+ (+ cx (* gradsize 0.8)) (car drwoffsets)))
                (set! y2 (+ (+ cy (* gradsize 0.75)) (cadr drwoffsets)))
                (if (< y1 1)
                    (set! y1 1)
                )
                (if (< y2 1)
                    (set! y2 1)
                )
                (if (< x1 1)
                    (set! x1 1)
                )
                (if (< x2 1)
                    (set! x2 1)
                )
                
            )
            (begin
                ;(gimp-message "grad type 0,1 or more than 3")
            )
        )
        ;(gimp-message " maths done")
        
        ;(gimp-message (number->string cx))
        ;(gimp-message (number->string cy))
        ;(gimp-message (number->string x1))
        ;(gimp-message (number->string y1))
        ;(gimp-message (number->string x2))
        ;(gimp-message (number->string y2))
        ;(gimp-message (number->string offsetX))
        ;(gimp-message (number->string offsetY))
        
        (gimp-image-set-active-layer img gradientlayer)
        
        ;;(gimp-edit-blend gradientlayer fingrad paintmode gradtype newopacity gradslopeoffsetpercent repeattype reversetrue FALSE 1 0 FALSE x1 y1 x2 y2) ; was 100 for opacity
        
        (gimp-edit-blend gradientlayer fingrad finblendmode gradtype newopacity gradslopeoffsetpercent repeattype reversetrue FALSE 1 0 FALSE x1 y1 x2 y2) ; was 100 for opacity
        (gimp-displays-flush)
        (gimp-message "five")
        
        
        
        (gimp-selection-none img)
        
        ;(gimp-message "layerfx gradover ready merge")
        
        
        (if (= merge TRUE)
            (begin
                    (gimp-message " six merge true")
                    (set! origmask (car (gimp-layer-get-mask drawable)))
                    (if (> origmask -1)
                        (begin
                            ;(gimp-message " six a")
                            (set! origmask (car (gimp-channel-copy origmask)))
                            (gimp-layer-remove-mask drawable 1)
                        )
                    )
                    ;(gimp-message " six b")
                    (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                    (set! gradientlayer (car (gimp-image-merge-down img gradientlayer 0)))
                    (gimp-drawable-set-name gradientlayer layername)
                    (gimp-layer-add-mask gradientlayer alphamask)
                    (gimp-layer-remove-mask gradientlayer MASK-APPLY) ; was 0
                    (if (> origmask -1)
                        (begin
                            ;(gimp-message " six c")
                            (gimp-layer-add-mask gradientlayer origmask)
                        )
                    )
                    ;(gimp-message " six d")
            )
            (begin
                (gimp-message " six e- no merge")
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (begin
                        ;(gimp-message " six f")
                        ;(gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                        (gimp-image-select-item img CHANNEL-OP-INTERSECT (car (gimp-layer-get-mask drawable)) )
                    )
                    (begin
                        ;(gimp-message "six f no mask")
                    )
                )
                ;(gimp-message "six g")
                (set! alphamask (car (gimp-layer-create-mask gradientlayer ADD-MASK-SELECTION)))
                (gimp-layer-add-mask gradientlayer alphamask)
                
                
                (gimp-layer-remove-mask gradientlayer MASK-APPLY) ; was 0
                (gimp-message " six h")
                
                ;(gimp-layer-set-mode gradientlayer finblendmode)
                (gimp-displays-flush)
                
            )
        )
        
        ;(gimp-message "seven")
        (gimp-displays-flush)
        (gimp-context-set-gradient origgradient)
        (gimp-selection-load origselection)
        ;(gimp-message "eight")
        (gimp-image-remove-channel img origselection)
        (gimp-context-pop)
        (gimp-image-undo-group-end img)
        
        (gimp-displays-flush)
        (gc)
        ;(gimp-message "OKk")
    )   
)


(define (script-fu-layerfx-pattern-overlay img
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
        (gimp-context-push)
        
        (gimp-context-set-sample-threshold-int 1)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-transparent FALSE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 1 1)
        (gimp-context-set-antialias FALSE)
        
       
        (add-over-layer img patternlayer drawable)
        (gimp-layer-set-offsets patternlayer (car drwoffsets) (cadr drwoffsets))
        (gimp-selection-all img)
        (gimp-context-set-pattern pattern) 
        (gimp-edit-fill patternlayer 4)
        (gimp-selection-none img)
        (if (= merge 1)
            (begin
                (set! origmask (car (gimp-layer-get-mask drawable)))
                (if (> origmask -1)
                    (begin
                        (set! origmask (car (gimp-channel-copy origmask)))
                        (gimp-layer-remove-mask drawable 1)
                    )
                )
                (set! alphamask (car (gimp-layer-create-mask drawable ADD-MASK-ALPHA-TRANSFER))) ; was 3
                (set! patternlayer (car (gimp-image-merge-down img patternlayer 0)))
                (gimp-drawable-set-name patternlayer layername)
                (gimp-layer-add-mask patternlayer alphamask)
                (gimp-layer-remove-mask patternlayer 0)
                (if (> origmask -1)
                    (gimp-layer-add-mask patternlayer origmask)
                )
            )
            (begin
                ;(gimp-selection-layer-alpha drawable)
                (gimp-image-select-item img CHANNEL-OP-ADD drawable)
                (if (> (car (gimp-layer-get-mask drawable)) -1)
                    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
                )
                (set! alphamask (car (gimp-layer-create-mask patternlayer ADD-MASK-SELECTION))) ;was 4
                (gimp-layer-add-mask patternlayer alphamask)
                (gimp-layer-remove-mask patternlayer 0)
            )
        )
        (gimp-context-set-pattern origpattern)
        (gimp-selection-load origselection)
        (gimp-image-remove-channel img origselection)
        (gimp-displays-flush)
        (gimp-context-pop)
        (gimp-image-undo-group-end img)
        ;(gimp-message "Ok")
  )
  
)


(script-fu-register "script-fu-layerfx-drop-shadow"
            "<Toolbox>/Script-Fu/Layer Effects/Drop Shadow..."
            "Adds a drop shadow to a layer. Does not add a shadow to an unfilled or drawn selection. Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"             0
            SF-DRAWABLE     "Drawable"          0
            SF-COLOR        "colorkh"           '(0 0 1)
            SF-ADJUSTMENT   "Opacity"           '(75 0 100 1 10 0 0)
            SF-OPTION       "Contour"           '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-ADJUSTMENT   "Noise - noise opacity"             '(0 0 100 1 10 0 0)
            SF-OPTION       "Blending Mode"     '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Spread"            '(3 0 100 1 10 1 0)
            SF-ADJUSTMENT   "Size"              '(15 0 250 1 10 1 1)
            SF-ADJUSTMENT   "Offset Angle"          '(120 -180 180 1 10 0 0)
            SF-ADJUSTMENT   "Offset Distance"       '(25 0 30000 1 10 1 1)
            SF-TOGGLE       "Layer knocks out Drop Shadow"  FALSE
            SF-TOGGLE       "Merge with layer"              FALSE
)

(script-fu-register "script-fu-layerfx-inner-shadow"
            "<Toolbox>/Script-Fu/Layer Effects/Inner Shadow..."
            "Adds an inner shadow to a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"         0
            SF-DRAWABLE     "Drawable"      0
            SF-COLOR        "Color"         '(0 0 0)
            SF-ADJUSTMENT   "Opacity"       '(75 0 100 1 10 1 0)
            SF-OPTION       "Contour"       '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-ADJUSTMENT   "Noise"         '(0 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"     '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-OPTION       "Source"        '("Edge" "Center")
            SF-ADJUSTMENT   "Choke"         '(0 0 100 1 10 1 0)
            SF-ADJUSTMENT   "Size"          '(5 0 250 1 10 1 1)
            SF-ADJUSTMENT   "Offset Angle"      '(120 -180 180 1 10 1 0)
            SF-ADJUSTMENT   "Offset Distance"   '(5 0 30000 1 10 1 1)
            SF-TOGGLE       "Merge with layer"      FALSE
)

(script-fu-register "script-fu-layerfx-outer-glow"
            "<Toolbox>/Script-Fu/Layer Effects/_Outer Glow..."
            "Creates an outer glow effect around a layer. Requires a layer with transparency and a filled in area. \nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"             0
            SF-DRAWABLE     "Drawable"          0
            SF-COLOR        "Color"             '(255 255 190)
            SF-ADJUSTMENT   "Opacity"           '(75 0 100 1 10 1 0)
            SF-OPTION       "Contour"           '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-ADJUSTMENT   "Noise"             '(0 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"     '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Spread"            '(0 0 100 1 10 1 0)
            SF-ADJUSTMENT   "Size"              '(5 0 999 1 10 1 1)
            SF-TOGGLE       "Layer knocks out Outer Glow"   FALSE
            SF-TOGGLE       "Merge with layer"              FALSE
)

(script-fu-register "script-fu-layerfx-inner-glow"
            "<Toolbox>/Script-Fu/Layer Effects/_Inner Glow..."
            "Creates an inner glow effect around a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"         0
            SF-DRAWABLE     "Drawable"      0
            SF-COLOR        "Color"         '(255 255 190)
            SF-ADJUSTMENT   "Opacity"       '(75 0 100 1 10 1 0)
            SF-OPTION       "Contour"       '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-ADJUSTMENT   "Noise"         '(0 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"     '("Normal" "Dissolve" "Multiply" "Difference" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-OPTION       "Source"        '("Edge" "Center")
            SF-ADJUSTMENT   "Choke"         '(0 0 100 1 10 1 0)
            SF-ADJUSTMENT   "Size"          '(5 0 999 1 10 1 1)
            SF-TOGGLE       "Merge with layer"    FALSE
)

(script-fu-register "script-fu-layerfx-bevel-emboss"
            "<Toolbox>/Script-Fu/Layer Effects/Bevel and Emboss..."
            "Creates beveling and embossing effects over a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"         0
            SF-DRAWABLE     "Drawable"      0
            SF-OPTION       "Style"         '("Outer Bevel" "Inner Bevel" "Emboss" "Pillow Emboss")
            SF-ADJUSTMENT   "Depth"         '(3 1 65 1 10 0 0)
            SF-OPTION       "Direction"     '("Up" "Down")
            SF-ADJUSTMENT   "Size"          '(5 0 250 1 10 0 0) 
            SF-ADJUSTMENT   "Soften"        '(0 0 16 1 2 0 0)
            SF-ADJUSTMENT   "Angle"         '(120 -180 180 1 10 1 0)
            SF-ADJUSTMENT   "Altitude"      '(30 0 90 1 10 1 0)
            SF-OPTION       "Gloss Contour"         '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-COLOR        "Highlight Color"       '(255 255 255)
            SF-OPTION       "Highlight Mode"        '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Highlight Opacity"     '(50 0 100 1 10 1 0)
            SF-COLOR        "Shadow Color"          '(0 0 0)
            SF-OPTION       "Shadow Mode"           '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Shadow Opacity"        '(89 0 100 1 10 1 0)
            SF-OPTION       "Surface Contour"       '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-TOGGLE       "Invert"                FALSE
            SF-TOGGLE       "Merge with layer"      FALSE
)

(script-fu-register "script-fu-layerfx-satin"
            "<Toolbox>/Script-Fu/Layer Effects/_Satin..."
            "Creates a satin effect over a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"         0
            SF-DRAWABLE     "Drawable"      0
            SF-COLOR        "Color"         '(0 0 0)
            SF-ADJUSTMENT   "Opacity"       '(75 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"     '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Offset Angle"      '(19 -180 180 1 10 1 0)
            SF-ADJUSTMENT   "Offset Distance"   '(11 0 30000 1 10 1 1)
            SF-ADJUSTMENT   "Size"              '(14 0 250 1 10 0 0)
            SF-OPTION       "Contour"           '("Linear" "Cone" "Cone - Inverted" "Cove - Deep" "Cove-Shallow" "Gaussian" "Half Round" "Ring" "Ring - Double" "Rolling Slope - Descending" "Rounded Steps" "Sawtooth 1")
            SF-TOGGLE       "Invert"            TRUE 
            SF-TOGGLE       "Merge with layer"  FALSE
)

(script-fu-register "script-fu-layerfx-stroke"
            "<Toolbox>/Script-Fu/Layer Effects/Stroke..."
            "Creates a coloured border around a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"                 0
            SF-DRAWABLE     "Drawable"              0
            SF-COLOR        "Color"                 '(255 255 255) 
            SF-ADJUSTMENT   "Opacity"               '(100 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"         '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Size"                  '(16 1 999 1 10 0 1)
            SF-ADJUSTMENT   "Position 0 = inside, 100 = outside"      '(48 0 100 1 10 0 0)
            SF-TOGGLE       "Merge with layer"      FALSE
)

(script-fu-register "script-fu-layerfx-color-overlay"
            "<Toolbox>/Script-Fu/Layer Effects/Color Overlay..."
            "Overlays a color over a layer.  Requires a layer with transparency and a filled in area. \nfile:layerfx_02.scm"
            "Jonathan Stipe <JonStipe@prodigy.net>"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"                 0
            SF-DRAWABLE     "Drawable"              0
            SF-COLOR        "Color"                 '(255 255 255)
            SF-ADJUSTMENT   "Opacity"               '(100 0 100 1 10 1 0)
            SF-OPTION       "Blending Mode"         '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-TOGGLE       "Merge with layer"      FALSE
)

(script-fu-register "script-fu-layerfx-gradient-overlay"
            "<Toolbox>/Script-Fu/Layer Effects/Gradient Overlay"
            "Overlays a gradient over a layer.  Paint Layer Modes sets mode of the resulting layer. Blend mode only works if Paint Layer mode is normal. Requires a layer with transparency and a filled in area.\n file:layerfx_02.scm"
            "Jonathan Stipe JonStipe@prodigy.net"
            "Jonathan Stipe"
            "January 2008"
            "RGBA, GRAYA"
            SF-IMAGE        "Image"             0
            SF-DRAWABLE     "Drawable"          0
            SF-OPTION       "Grad Blend Mode"   '("Custom" "FG BG RGB" "FG BG HSV" "FG Trans")
            SF-OPTION       "Paint Layer Mode (Layer mode is set to this) "        '("Normal" "Dissolve" "Multiply" "Difference" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Hue2" "Saturation2" "Color2" "Value2" "Divide2" "Dodge2" "Burn2" "HardLight2" "Soft Light2" "Grain extract2" "Grain merge2" "Color Erase2" "Erase mode2" "Replace mode2" "anti erase mode2")
            SF-GRADIENT     "Gradient"          "Default"
            SF-OPTION       "Gradient Type"     '("Linear" "Bi-linear" "Radial" "Square" "Conical (sym)" "Conical (asym)" "Shaped (angular)" "Shaped (spherical)" "Shaped (dimpled)" "Spiral (cw)" "Spiral (ccw)")
            SF-OPTION       "Repeat"            '("None" "Sawtooth Wave" "Triangular Wave")
            SF-TOGGLE       "Reverse"            FALSE
            SF-ADJUSTMENT   "Opacity"           '(100 1 100 1 1 0 1)
            SF-OPTION       "Blending Mode - only works if Paint Layer mode=Normal"     '("Normal" "Dissolve" "Multiply" "Difference" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
            SF-ADJUSTMENT   "Center X"          '(500 1 262144 1 10 0 1)
            SF-ADJUSTMENT   "Center Y"          '(400 1 262144 1 10 0 1)
            SF-ADJUSTMENT   "Gradient Angle"    '(90 -180 180 1 10 0 1)
            SF-ADJUSTMENT   "Gradient Width"    '(120 0.1 262144 1 10 1 1)
            SF-TOGGLE       "Merge with layer"    FALSE
)
;;   0       1           3          6           4         5        16      17     18            19             20              21           6            7         8            9            10          11      12          13      14

(script-fu-register "script-fu-layerfx-pattern-overlay"
    "<Toolbox>/Script-Fu/Layer Effects/Pattern Overlay..."
    "Overlays a pattern over a layer.  Requires a layer with transparency and a filled in area.\nfile:layerfx_02.scm"
    "Jonathan Stipe <JonStipe@prodigy.net>"
    "Jonathan Stipe"
    "January 2008"
    "RGBA, GRAYA"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-PATTERN      "Pattern"       "Wood"
    SF-ADJUSTMENT   "Opacity"               '(100 1 100 1 10 1 0)
    SF-OPTION       "Blending Mode"         '("Normal" "Dissolve" "Multiply" "Divide" "Screen" "Overlay" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only" "Hue" "Saturation" "Color" "Value")
    SF-TOGGLE       "Merge with layer"      FALSE
)

;