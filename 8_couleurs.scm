; Script-Fu 8_couleurs.scm
; Download :  
;
;   Copyright (C) 2013 samj ( http://aljacom.com/~gmic/ )
;   
;   Licence GPL
;       Ce fichier est le Script-Fu "8_couleurs.scm" pour Gimp. 
;       Ce Script-Fu "8_couleurs.scm" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;       telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;       Ce Script-Fu "8_couleurs.scm" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;       pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;       Consultez la GNU General Public License pour plus de détails.
;       Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;       
;    Ce fichier 8_couleurs.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;   
;   
;   Version 20130404
;   
;    Version 20200424
;    Updated by karlhof26 for Gimp 2.10.18
;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;	Accès sur l'image par :
;
;	Couleurs > 8 Couleurs ...
;	Colors > 8 Couleurs ...
;
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;


(define (huit_couleurs_samj
            image
            drawable
            Soften
            Color_1
            Color_2
            Color_3
            Color_4
            Color_5
            Color_6
            Color_7
            Color_8
            Back_Ground_Color
            Use_Back_Ground_Color
            Noise
            flatten
            
            
        )
    (let* (
            
            (message (car (gimp-message-get-handler)))
            (old_context_transform_direction (car (gimp-context-get-transform-direction)))
            (old_context_interpolation (car (gimp-context-get-interpolation)))
            (old_context_recursion (car (gimp-context-get-transform-recursion)))
            (old_context_transform_resize (car (gimp-context-get-transform-resize)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            (old_context-antialias (car (gimp-context-get-antialias)))
            (old_context-feather (car (gimp-context-get-feather)))
            (old_context-feather-radiusx (car (gimp-context-get-feather-radius)))
            (old_context-feather-radiusy (cadr (gimp-context-get-feather-radius)))
            (old_context-sample-merged (car (gimp-context-get-sample-merged)))
            (old_ontext-sample-criterion (car (gimp-context-get-sample-criterion)))
            (old_context-sample-threshold (car (gimp-context-get-sample-threshold)))
            (old_context-sample-transparent (car (gimp-context-get-sample-transparent)))
            
            (old-brush-size (car (gimp-context-get-brush-size)))
            (old-brush-aspect-ratio (car (gimp-context-get-brush-aspect-ratio)))
            (old-brush-angle (car (gimp-context-get-brush-angle)))
            (old-brush-name (car (gimp-context-get-brush)))
            
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            
            (transparence (car (gimp-drawable-has-alpha drawable)))
            
            (selection_en_cours)
            (nouvelle_image)
            (calque_copie)
            (copier_visible)
            (buffer-name "copier_visible")
            (rvb 0)
            (selection_bounds 0)
            (select_exist 0)
            (select_offset_x 0)
            (select_offset_y 0)
            (select_width 0)
            (select_height 0)
            
            (sortie_boucle 0)
            
            (calque_fond)
            
            (calque_8_couleurs)
            
            (Nb_couleurs_trouvees 0)
            
          )
        
        
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 0 0)
        (gimp-context-set-sample-merged FALSE)
        (gimp-context-set-sample-criterion 0)
        (gimp-context-set-sample-threshold 0)
        (gimp-context-set-sample-transparent FALSE)
        
        (gimp-context-set-transform-direction TRANSFORM-FORWARD)
        (gimp-context-set-interpolation 2)
        ;;(gimp-context-set-transform-recursion 3)
        (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
        
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL) ; was 0legacy
        
        
        (gimp-image-undo-group-start image)
        
        (gimp-selection-none image)
        
        
        
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        (set! copier_visible (car (gimp-edit-named-copy-visible image buffer-name)))
        
        ; créer une nouvelle image rgb
        (set! nouvelle_image (car (gimp-image-new width height 0)))
        
        (gimp-image-undo-group-start nouvelle_image)
        
        ; créer le premier calque
        (set! calque_copie (car (gimp-layer-new nouvelle_image width height 0 "calque_gris" 100 0)))
        
        (gimp-image-insert-layer nouvelle_image calque_copie 0 -1) ; was -1 0
        
        (gimp-image-select-rectangle nouvelle_image CHANNEL-OP-ADD  width height 0 0 )
        
        (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
        
        (gimp-floating-sel-anchor selection_en_cours)
        
        (gimp-selection-none nouvelle_image)
        
        (gimp-layer-add-alpha calque_copie)
        (gimp-drawable-levels-stretch calque_copie)
        
        (gimp-drawable-equalize calque_copie TRUE)
        
        ; ajuster taille image à la taille des calques
        (gimp-image-resize-to-layers nouvelle_image)
        
        (set! calque_copie (car (gimp-image-get-active-layer nouvelle_image)))
        
        (gimp-drawable-desaturate calque_copie 1)
        
        (gimp-drawable-posterize calque_copie 6)
        
        (plug-in-gauss 1 nouvelle_image calque_copie 4 4 1)
        
        (if (= Use_Back_Ground_Color 0)
            (gimp-image-convert-indexed nouvelle_image 0 0 8 1 1 "")
            ;else
            (begin
                (gimp-image-convert-indexed nouvelle_image 0 0 9 1 1 "")
                (set! Nb_couleurs_trouvees -1)
            )
        )
        
        (gimp-image-convert-grayscale nouvelle_image)
        
        (gimp-image-convert-rgb nouvelle_image)
        
        (gimp-context-set-background Back_Ground_Color)
        
        ; ajouter un calque à la couleur du fond
        (set! calque_fond (car (gimp-layer-new nouvelle_image width height 0 "calque_fond" 100 LAYER-MODE-NORMAL)))
        
        (gimp-image-insert-layer nouvelle_image calque_fond -1 0)
        
        (gimp-drawable-fill calque_fond 1)
        
        (gimp-item-set-visible calque_fond TRUE)
        
        ; ajouter le calque 8 couleurs
        (set! calque_8_couleurs (car (gimp-layer-new nouvelle_image width height 0 "calque_8_couleurs" 100 LAYER-MODE-NORMAL)))
        
        (gimp-image-insert-layer nouvelle_image calque_8_couleurs 0 -1 ) ; -1 0
        
        (gimp-item-set-visible calque_8_couleurs TRUE)
        
        (gimp-layer-add-alpha calque_8_couleurs)
        
        (gimp-drawable-fill calque_8_couleurs 3)
        
        
        
        
        ; boucle pour trouver les couleurs et faire une sélection par couleur , un chemin
        (while
            (= sortie_boucle 0)
                (begin
                    
                    (gimp-image-select-color nouvelle_image 0 calque_copie (list rvb rvb rvb))
                    (gimp-selection-feather nouvelle_image 0)
                    
                    (set! selection_bounds (gimp-selection-bounds nouvelle_image))
                    (set! select_exist (car selection_bounds))
                    ;(set! select_offset_x (cadr selection_bounds))
                    ;(set! select_offset_y (caddr selection_bounds))
                    ;(set! select_width (- (cadr (cddr selection_bounds)) select_offset_x))
                    ;(set! select_height (- (caddr (cddr selection_bounds)) select_offset_y))
                    ;(gimp-message (number->string select_exist))
                    
                    (if (= select_exist 1)
                        (begin
                            
                            (if (> Soften 0)
                                (begin
                                    (gimp-selection-grow nouvelle_image Soften)
                                    (gimp-selection-shrink nouvelle_image Soften)
                                )
                            )
                            
                            (set! Nb_couleurs_trouvees (+ Nb_couleurs_trouvees 1) )
                            ;(gimp-message (number->string Nb_couleurs_trouvees))
                            
                            (if (= Nb_couleurs_trouvees 1)
                                (begin
                                    (gimp-context-set-foreground Color_1)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 2)
                                (begin
                                    (gimp-context-set-foreground Color_2)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 3)
                                (begin
                                    (gimp-context-set-foreground Color_3)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 4)
                                (begin
                                    (gimp-context-set-foreground Color_4)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 5)
                                (begin
                                    (gimp-context-set-foreground Color_5)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 6)
                                (begin
                                    (gimp-context-set-foreground Color_6)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 7)
                                (begin
                                    (gimp-context-set-foreground Color_7)
                                )
                            )
                            
                            (if (= Nb_couleurs_trouvees 8)
                                (begin
                                    (gimp-context-set-foreground Color_8)
                                )
                            )
                            
                            
                            (if (> Nb_couleurs_trouvees 0)
                                (begin
                                    ; (gimp-edit-bucket-fill drawable fill-mode paint-mode opacity threshold sample-merged x y)
                                    (gimp-edit-bucket-fill calque_8_couleurs 0 0 100 0 FALSE 0 0)
                                    
                                    (if (= Noise 1)
                                        (plug-in-hsv-noise 1 nouvelle_image calque_8_couleurs (- 9 Nb_couleurs_trouvees) 0 170 0)
                                    )
                                )
                            )
                            
                            
                            
                        )
                    )
                    
                    (gimp-selection-none nouvelle_image)
                    
                    (set! rvb (+ rvb 1) )
                    (if (or (= rvb 256) (= Nb_couleurs_trouvees 8) )
                        (begin
                            
                            (set! sortie_boucle 1 )							
                        )
                    )
                    
                    
                    
                    
                    
                )
        )
        
        
        
        (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
        
        (gimp-floating-sel-anchor selection_en_cours)
        
        (gimp-selection-none nouvelle_image)
        
        
        
        (gimp-image-undo-group-end nouvelle_image)
        (gimp-display-new nouvelle_image)
        
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        ; aplatir l'image
        (if (= flatten TRUE)
                (gimp-image-flatten nouvelle_image)
                ;else
                (gimp-image-set-active-layer nouvelle_image calque_8_couleurs)
        )
        
        
        ;******************************************************************************************************
        
        ;; rétablir le mode message
        (gimp-message-set-handler message)
        
        (gimp-context-set-transform-direction old_context_transform_direction )
        (gimp-context-set-interpolation old_context_interpolation)
        (gimp-context-set-transform-recursion old_context_recursion)
        (gimp-context-set-transform-resize old_context_transform_resize)
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
        
        (gimp-context-set-antialias old_context-antialias)
        (gimp-context-set-feather old_context-feather)
        (gimp-context-set-feather-radius old_context-feather-radiusx old_context-feather-radiusy)
        (gimp-context-set-sample-merged old_context-sample-merged)
        (gimp-context-set-sample-criterion old_ontext-sample-criterion)
        (gimp-context-set-sample-threshold old_context-sample-threshold)
        (gimp-context-set-sample-transparent old_context-sample-transparent)
        
        (gimp-context-set-brush-default-size)
        (gimp-context-set-brush-size old-brush-size)
        (gimp-context-set-brush-aspect-ratio old-brush-aspect-ratio)
        (gimp-context-set-brush-angle old-brush-angle)
        (gimp-context-set-brush old-brush-name)
        
        
        
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        
        
    )
)


(script-fu-register "huit_couleurs_samj"
                    "<Toolbox>/Script-Fu/Toolbox/Color/8 Couleurs ..."
                    "Colorize an image using 8 Colours. Changes the tone of the image to a set of 8 colours. \n file:8_couleurs.scm"
                    "samj"
                    "karlhof26"
                    "20130404"
                    "RGB*"
                    SF-IMAGE    "Image"     0
                    SF-DRAWABLE "Drawable"  0
                    SF-ADJUSTMENT "Soften " '(2 0 60 1 5 0 1)
                    SF-COLOR "Color 1 "     '(69 49 8)
                    SF-COLOR "Color 2 "     '(88 62 10)
                    SF-COLOR "Color 3 "     '(115 81 13)
                    SF-COLOR "Color 4 "     '(138 98 16)
                    SF-COLOR "Color 5 "     '(163 116 19)
                    SF-COLOR "Color 6 "     '(189 134 22)
                    SF-COLOR "Color 7 "      '(214 152 25)
                    SF-COLOR "Color 8 "     '(240 170 28)
                    SF-COLOR "Background Color "        '(0 0 0)
                    SF-TOGGLE "Use Background Color"    FALSE
                    SF-TOGGLE "Noise"                   FALSE
                    SF-TOGGLE "Flatten"                 FALSE
                    
)

;; FIN