;   Licence GPL 
;       Ce fichier est le Script-Fu "3dscript.scm" pour Gimp. 
;       Ce Script-Fu "3dscript.scm" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;       telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;       Ce Script-Fu "3dscript.scm" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;       pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;       Consultez la GNU General Public License pour plus de détails.
;       Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;        
;    Ce fichier 3dscript.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;   
;   
;   Version 20200327 by karlhof26
;    
;   
;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(define (script-fu-make-3d beeld laag diepte hrichting vrichting half brichting bhoogte bprojdiep)
        (gimp-image-undo-group-start beeld)
    (let* (
            (orglaag (car (gimp-image-get-active-layer beeld)))
          )
        (let* (
                (werklaag (car (gimp-layer-copy laag TRUE )))
                (pos (car (gimp-image-get-layer-position beeld laag)))
                (nwpos (+  pos 1))
              )
            (gimp-image-insert-layer beeld werklaag 0 nwpos)
        )
        (let* (
;                (nlaag 0)
                (baslaag (car (gimp-image-get-active-layer beeld)))
;                (baspos (car (gimp-image-get-layer-position beeld baslaag)))
                (lagenteller diepte)
                (focuslaag baslaag)
                (hwissel 0)
                (vwissel 0)
              )
            (set! hrichting (- 1 hrichting))
            (set! vrichting( - 1 vrichting))
            (set! hwissel hrichting)
            (set! vwissel vrichting)
            (plug-in-bump-map 1 beeld baslaag baslaag brichting bhoogte bprojdiep 0 0 0 0 TRUE FALSE 0) 
            (while (> lagenteller 0)
                (let* (
                        (focuslaag (car (gimp-image-get-active-layer beeld)))
                        (werklaag (car (gimp-layer-copy focuslaag TRUE )))
                        (laagpos (car (gimp-image-get-layer-position beeld focuslaag)))
                        (nwpos (+ laagpos 1))
                      )
                    (if (= half 2)
                        (if (= vwissel 0)
                            (set! vwissel vrichting)
                            (set! vwissel 0)
                        )
                    )
                    (if (= half 1)
                        (if (= hwissel 0)
                            (set! hwissel hrichting)
                            (set! hwissel 0)
                        )
                    )
                    (gimp-image-insert-layer beeld werklaag 0 nwpos)
                    (gimp-item-transform-2d werklaag 0 0 1 1 0 hwissel vwissel)
                    (set! lagenteller (- lagenteller 1))
                )
            )
            (gimp-image-set-active-layer beeld baslaag)
            (set! lagenteller diepte)
            (while (> lagenteller 0)
                (gimp-image-merge-down beeld baslaag 0)
                (set! baslaag (car (gimp-image-get-active-layer beeld)))
                (set! lagenteller (- lagenteller 1))
            )
            (gimp-image-set-active-layer beeld orglaag)
            (gimp-displays-flush)
            (gimp-image-undo-group-end beeld)
            (gc) ; memory cleanup
        )  
    )
)

(script-fu-register "script-fu-make-3d"     ;func name
    "Make-3D Text"                               ;menu label   
    "Makes a 3D representation of the current (text) layer\
        \n file:3dscript.scm"        ;description
    "Frans Rijven"                          ;author
    "copyright 2009, Frans Rijven"          ;copyright notice
    "Aug 11 , 2009"                         ;date created
    "*"                                     ;image type that the script works on
    SF-IMAGE      "Image"           0
    SF-DRAWABLE   "Drawable"        0
    SF-VALUE      "Depth"           "12"
    SF-OPTION     "Horizontal direction"    '("Right" "Neutral" "Left")
    SF-OPTION     "Vertical direction"      '("Bottom" "Neutral" "Top")
    SF-OPTION     "Half depth"              '("none" "horizontal" "vertical")
    SF-VALUE      "Bump-direction:"         "90"
    SF-VALUE      "Bump-height:"            "45"
    SF-VALUE      "Bump-projection depth:"  "2"
)

(script-fu-menu-register "script-fu-make-3d" "<Image>/Script-Fu/Text")
        