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
    
(define (script-fu-make-3d beeld laag diepte scalingopt hrichting vrichting half brichting bhoogte bprojdiep)
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
                (bumpmaplayer (car (gimp-layer-copy baslaag TRUE)))
                (lagenteller diepte)
                (focuslaag baslaag)
                (hwissel 0)
                (vwissel 0)
                
                (scalefactor 1.01)
              )
            (set! hrichting (- 1 hrichting))
            (set! vrichting( - 1 vrichting))
            (set! hwissel hrichting)
            (set! vwissel vrichting)
            
            ;(gimp-message (number->string hrichting)) ; horiz direction
            ;(gimp-message (number->string hwissel))
            ;(gimp-message (number->string vrichting)) ; vertical direction
            ;(gimp-message (number->string vwissel))
            
            (if (and (= hwissel 0) (= vwissel 0))
                (begin
                    (gimp-message "All neutral - NO 3D effect. Setting to 0.10")
                    (set! vwissel 0.10)
                )
            )
            
            (cond ((= scalingopt 0)
                    (set! scalefactor 1.0)
                )
                ((= scalingopt 1)
                    (set! scalefactor 1.01)
                )
                ((= scalingopt 2)
                    (set! scalefactor 1.1)
                )
                ((= scalingopt 3)
                    (set! scalefactor 1.33)
                )
                ((= scalingopt 4)
                    (set! scalefactor 2.0)
                )
            )
            (gimp-image-insert-layer beeld bumpmaplayer 0 1)
            (gimp-drawable-desaturate bumpmaplayer 0)
            (gimp-item-set-name baslaag "base layer baslaag")
            
            (plug-in-bump-map 1 beeld 
                            baslaag  ;layer
                            bumpmaplayer  ; bump map layer
                            brichting   ; degrees
                            bhoogte     ; elevation
                            bprojdiep   ; depth
                            0 0.5 0 0 TRUE FALSE 0) 
            
            ;(plug-in-bump-map 1 beeld 
            ;                baslaag  ;layer
            ;                baslaag  ; bump map layer
            ;                brichting   ; degrees
            ;                bhoogte     ; elevation
            ;                bprojdiep   ; depth
            ;                0 0 0 0 TRUE FALSE 0) 
            
            (gimp-image-set-active-layer beeld baslaag)
            (gimp-layer-set-visible bumpmaplayer FALSE)
            
            ;(gimp-displays-flush)
            ;(quit)
            
            (while (> lagenteller 0)
                (let* (
                        (focuslaag (car (gimp-image-get-active-layer beeld)))
                        (werklaag (car (gimp-layer-copy focuslaag TRUE )))
                        (laagpos (car (gimp-image-get-layer-position beeld focuslaag)))
                        (nwpos (+ laagpos 1))
                      )
                    (if (= half 2)
                        (begin
                            (if (= vwissel 0)
                                (begin
                                    (set! vwissel vrichting)
                                )
                                (begin
                                    (set! vwissel 0)
                                )
                            )
                        )
                    )
                    (if (= half 1)
                        (begin
                            (gimp-message "half=1")
                            (if (= hwissel 0)
                                (begin
                                    (gimp-message "=0")
                                    (set! hwissel hrichting)
                                )
                                (begin
                                    (set! hwissel 0)
                                )
                            )
                            (gimp-message (number->string hwissel))
                        )
                    )
                    (gimp-image-insert-layer beeld werklaag 0 nwpos)
                    ;(gimp-item-transform-2d werklaag 0 0 1 1 0 hwissel vwissel)
                    ;
                    (gimp-item-transform-2d werklaag 0 0 1.00 1.00 0 (* hwissel scalefactor) (* vwissel scalefactor))
                    ;(gimp-item-transform-2d werklaag 0 0 hrichting vrichting 0 hwissel vwissel)
                    (set! lagenteller (- lagenteller 1))
                )
            )
            (gimp-displays-flush)
            ;(quit)
            
            
            (gimp-image-set-active-layer beeld baslaag)
            (set! lagenteller diepte)
            (while (> lagenteller 0)
                (gimp-image-merge-down beeld baslaag 0)
                (set! baslaag (car (gimp-image-get-active-layer beeld)))
                (set! lagenteller (- lagenteller 1))
            )
            
            (gimp-image-set-active-layer beeld orglaag)
            
            (gimp-layer-set-visible laag FALSE)
            
            (gimp-displays-flush)
            (gimp-image-undo-group-end beeld)
            (gc) ; memory cleanup
        )  
    )
)

(script-fu-register "script-fu-make-3d"     ;func name
    "Make-3D Text"                               ;menu label   
    "Makes a 3D representation of the current text layer. Half determines strength of direction. Leave Bump settings at defaults at first.
        \n file:3dscript.scm"        ;description
    "Frans Rijven"                          ;author
    "copyright 2009, Frans Rijven"          ;copyright notice
    "Aug 11 , 2009"                         ;date created
    "*"                                     ;image type that the script works on
    SF-IMAGE      "Image"                   0
    SF-DRAWABLE   "Drawable"                0
    SF-ADJUSTMENT      "Depth"              '(32 1 100 1 5 0 0)
    SF-OPTION     "3D Type"                '("Black" "Color" "Bigger" "Bigger2" "Deeper3")
    SF-OPTION     "Horizontal direction"    '("Right" "Neutral" "Left")
    SF-OPTION     "Vertical direction"      '("Bottom" "Neutral" "Top")
    SF-OPTION     "Half strength direction"          '("none" "Less horizontal" "Less vertical")
    SF-VALUE      "Bump-direction:"         "135"
    SF-VALUE      "Bump-height:"            "45"
    SF-ADJUSTMENT      "Bump-projection depth"      '(11 1 60 1 5 0 0)
)

(script-fu-menu-register "script-fu-make-3d" "<Image>/Script-Fu/Text")
        