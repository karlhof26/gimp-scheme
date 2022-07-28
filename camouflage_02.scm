;; Copyright Copyright 2010, Jeffrey Aylesworth jeffrey@aylesworth.ca  
;; 2022 Karl Hofmeyr
;; Gimp Camo
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED AS IS AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 
;
;
; Rare case found: random numbers may be equal resulting in only 1 color visible - have included logic to remove.
;
;

(define (do-camo image col layerA inrand intile)
    (let* (
            (variabnl 0)
            (nextrand 1)
          )
        ;(set! layer (car (gimp-layer-new image wdth height type "a" 100 0)))
        ;;(gimp-image-add-layer image layerA -1)
        (set! nextrand (+ 5 (rand 200)))
        (while (= nextrand inrand)
            (set! nextrand (+ 5 (rand 9999)))
            (gimp-message "resetting random number")
        )
        
        (plug-in-solid-noise
                1
                image
                layerA   
                intile
                0
                nextrand ; was (rand)
                1
                4
                4
                )
                (gimp-drawable-posterize layerA 2)
                (gimp-drawable-invert layerA TRUE)
                (plug-in-colortoalpha 1 image layerA '(0 0 0))
                (plug-in-colorify 1 image layerA col)
                ; layer
    nextrand
            
    )
)


(define (script-fu-create-camo image drawable colA colB randseed tileable)
    (let* (
            (wdth (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (type (car (gimp-drawable-type-with-alpha drawable)))
            (layer1 (car (gimp-layer-new image wdth height RGBA-IMAGE "a" 100 0)))
            (layer2 (car (gimp-layer-new image wdth height RGBA-IMAGE "b" 100 0)))
            
            (seed 20223)
            (firstrand 0)
            (secondrand 0)
        )
        
        ; set random seed so that pattern is repeatable
        (if (= randseed 0)
            (begin
                (set! *seed* (car (gettimeofday))) ; Random Number Seed From Clock (*seed* is global) 
                (random-next)                      ; Next Random Number Using Seed
                (set! randseed (rand 255555))
            )
            (begin
                
            )
        )
        
        
        (srand randseed)
        (set! firstrand (+ 2 (rand 8))) ; was (rand 7)
        (gimp-image-undo-group-start image)
        ;(set! layer (car (gimp-layer-new image wdth height type "a" 100 0)))
        (gimp-image-insert-layer image layer1 0 -1)
            (plug-in-solid-noise
                    1
                    image
                    layer1   
                    tileable
                    0
                    firstrand
                    1
                    4
                    4
                   )
            (gimp-drawable-posterize layer1 2)
    ;        (gimp-invert layer1)
    ;        (plug-in-colortoalpha 1 image layer1 '(0 0 0))
    ;        (plug-in-colorify 1 image layer1 colA)
        (do-camo image colA layer1 firstrand tileable)
        ;;(layerA (do-camo colA))
        (gimp-image-insert-layer image layer2 0 -1)
        (set! secondrand (do-camo image colB layer2 firstrand tileable))
        
        (gimp-image-merge-down image layer2 2)
        ;(gimp-message (number->string randseed))
        ;(gimp-message (number->string firstrand))
        ;(gimp-message (number->string secondrand))
        
        (gimp-item-set-name (car (gimp-image-get-layer-by-name image "a")) "camo")
        
        (gimp-item-set-name (car (gimp-image-get-layer-by-name image "camo"))
            (string-append "Camo seed:" (number->string randseed) (if (= tileable TRUE) "/Tileable" "/NotTileable"))) 
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gc) ; garbage collect
        
    )
)

(script-fu-register "script-fu-create-camo"
    "Camouflage"
    "Creates a camouflage pattern on an image. \nfile:camouflage_02.scm"
    "Jeffrey Aylesworth jeffrey at aylesworth"
    "Copyright 2009 Jeffrey Aylesworth"
    "2010/01/10"
    ""
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Drawable"          0
    SF-COLOR        "Colour 1"          '(150 0 0)
    SF-COLOR        "Colour 2"          '(0 150 0)
    SF-ADJUSTMENT   "Random seed (0=randomise)"       '(0 0 255555 1 10 0 1)
    SF-TOGGLE       "Tileable?"         FALSE
)

(script-fu-menu-register "script-fu-create-camo" "<Image>/Script-Fu/Render")
;;

