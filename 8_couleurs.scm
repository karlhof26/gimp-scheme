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
;    Version 20200424 and Version 20220821
;    Updated by karlhof26 for Gimp 2.10.18 and Gimp 2.10.32 (21/08/2022)
;   Refactored to work more predictably
;   Options added for Gradient and Palette as source of colors
;   Options for number fo colors to be selected
;   Undo errors corrected - no memory leaks
;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;	Accès sur l'image par :
;
;   Script-Fu/Couleurs > 8 Couleurs ...
;   Script-Fu/Colors > 8 Couleurs ...
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
            NumColors
            Soften
            ColorSource
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
            paletteName
            gradientName
            newImageFlag
            flatten
            
            
        )
        
    (define (checkcolor innumber multiplier)
        (let (
                (outnumber 0)
             )
            (set! outnumber (* innumber multiplier))
            (set! outnumber (round outnumber))
            (set! outnumber (min outnumber multiplier))
            (set! outnumber (max outnumber 0))
            
            (if (> outnumber 255)
                (begin
                    (gimp-message (number->string outnumber))
                    (gimp-message "outexceeded")
                    (gimp-displays-flush)
                    (quit)
                )
                (begin
                    ;(gimp-message (number->string outnumber))
                    ;(gimp-message "less")
                )
            )
            outnumber
        )
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
            
            (color_count 0)
            
            (dummy1)
            (color_list)
            (color_value)
            
            (checkNumColors 1)
            (holdness 2)
            
          )
        
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 0 0) ; was 0 0 
        (gimp-context-set-sample-merged FALSE)
        (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
        (gimp-context-set-sample-threshold-int 5) ; was 0
        (gimp-context-set-sample-transparent FALSE)
        
        (gimp-context-set-transform-direction TRANSFORM-FORWARD)
        (gimp-context-set-interpolation 2)
        ;;(gimp-context-set-transform-recursion 3)
        (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
        
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL) ; was 0legacy
        
        
        
        
        (gimp-selection-none image)
        
        (if (= ColorSource 1)
            (begin
                ; check palette has at least 8 entries
                (gimp-context-set-palette paletteName)
                (set! color_count (car (gimp-palette-get-colors paletteName)))
                ;(gimp-message (number->string color_count))
                (if (< color_count NumColors)
                    (begin
                        (gimp-message "Palette has fewer colors than requested. Reducing Number of Colors")
                        (set! NumColors color_count)
                    )
                )
            )
        )
        (if (= ColorSource 2)
            (begin
                
                (gimp-context-set-gradient gradientName)
                (if (< NumColors 2)
                    (begin
                        (gimp-message "Gradient must have 2 colors sampled")
                        (set! NumColors 2)
                    )
                )
                
                (set! color_count (gimp-gradient-get-uniform-samples gradientName NumColors FALSE))
                
                
                (set! dummy1 (car color_count))
                (set! color_list (cadr color_count))
                ; debugging messages
                ;(gimp-message (number->string dummy1))
                ;(gimp-message (number->string (list-ref color_count 0)))
                ;(gimp-message "Two")
                ;(gimp-message (number->string (aref color_list 2)))
            )
        )
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        (set! copier_visible (car (gimp-edit-named-copy-visible image buffer-name)))
        
        (if (= newImageFlag TRUE)
            (begin
                ; créer une nouvelle image rgb
                (set! nouvelle_image (car (gimp-image-new width height 0)))
                
                (gimp-image-undo-disable nouvelle_image)
                
                ; créer le premier calque
                (set! calque_copie (car (gimp-layer-new nouvelle_image width height 0 "calque_gris" 100 0)))
                
                (gimp-image-insert-layer nouvelle_image calque_copie 0 -1) ; was -1 0
                
                ;(gimp-image-select-rectangle nouvelle_image CHANNEL-OP-ADD  width height 0 0 )
                (gimp-image-select-rectangle nouvelle_image CHANNEL-OP-REPLACE  0 0 width height)
                
                (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
                
                (gimp-floating-sel-anchor selection_en_cours)
                
                (gimp-selection-none nouvelle_image)
            )
            (begin
                ; NO NEW IMAGE USE EXISTNG IMAGE
                
                ; 
                (set! calque_copie (car (gimp-layer-new image width height 0 "calque_gris" 100 0)))
                (gimp-image-insert-layer image calque_copie 0 -1) ; was -1 0
                ;(gimp-image-select-rectangle nouvelle_image CHANNEL-OP-ADD  width height 0 0 )
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE  0 0 width height)
                (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
                (gimp-floating-sel-anchor selection_en_cours)
                (gimp-selection-none image)
                (gimp-displays-flush)
            )
        )
        
        (gimp-layer-add-alpha calque_copie)
        (gimp-drawable-levels-stretch calque_copie)
        
        ;(gimp-drawable-equalize calque_copie TRUE)
        
        (if (= newImageFlag TRUE)
            (begin
                ; ajuster taille image à la taille des calques
                (gimp-image-resize-to-layers nouvelle_image)
                
                (set! calque_copie (car (gimp-image-get-active-layer nouvelle_image)))
                (gimp-drawable-desaturate calque_copie 1)
                (if (= Use_Back_Ground_Color FALSE)
                    (begin
                    )
                    (begin
                         (set! NumColors (+ NumColors 1))
                    )
                )
                (gimp-drawable-posterize calque_copie (+ NumColors 0)); was 6
                
                (plug-in-gauss 1 nouvelle_image calque_copie 4 4 1)
            )
            (begin
                ; resize the image
                (gimp-image-resize-to-layers image)
                (set! calque_copie (car (gimp-image-get-active-layer image)))
                
                (if (= Use_Back_Ground_Color FALSE)
                    (begin
                        ;(gimp-drawable-desaturate calque_copie DESATURATE-LUMINANCE) ; was 1
                        ;(gimp-drawable-posterize calque_copie (+ NumColors 0)); was 6
                    )
                    (begin
                        ;(gimp-drawable-desaturate calque_copie DESATURATE-LUMINANCE)
                        ;(gimp-drawable-posterize calque_copie (+ NumColors 1))
                        (set! NumColors (+ NumColors 1))
                    )
                )
                
                (gimp-drawable-desaturate calque_copie DESATURATE-LUMINANCE) ; was 1
                (gimp-drawable-posterize calque_copie (+ NumColors 0)); was 6
                
                ;;(gimp-drawable-posterize calque_copie (+ NumColors 3)); was 6
                ;(plug-in-gauss 1 image calque_copie 4 4 1) ; was 4 4 1
                (gimp-layer-set-name calque_copie "calque_copie")
                
            )
        )
        
        
        (if (= newImageFlag TRUE)
            (begin
                ; conversion to indexed not needed
                ;(if (= Use_Back_Ground_Color FALSE)
                ;    (begin
                ;        (gimp-image-convert-indexed nouvelle_image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE NumColors TRUE TRUE "xkh") ; was 8
                ;    )
                ;    ;else
                ;    (begin
                ;        (gimp-image-convert-indexed nouvelle_image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE (+ NumColors 1) TRUE TRUE "xkh") ; was 9
                ;            (set! Nb_couleurs_trouvees -1)
                ;    )
                ;)
                ;(gimp-image-convert-grayscale nouvelle_image)
                ; 
                ;(gimp-image-convert-rgb nouvelle_image)
                
                (gimp-context-set-background Back_Ground_Color)
                
                ; ajouter un calque à la couleur du fond
                (set! calque_fond (car (gimp-layer-new nouvelle_image width height 0 "calque_fond" 100 LAYER-MODE-NORMAL)))
                
                (gimp-image-insert-layer nouvelle_image calque_fond -1 0)
                
                (gimp-drawable-fill calque_fond FILL-BACKGROUND)
                
                (gimp-item-set-visible calque_fond TRUE)
                
                ; ajouter le calque 8 couleurs
                (set! calque_8_couleurs (car (gimp-layer-new nouvelle_image width height 0 "calque_8_couleurs" 100 LAYER-MODE-NORMAL)))
                
                (gimp-image-insert-layer nouvelle_image calque_8_couleurs 0 -1 ) ; -1 0
                
                (gimp-item-set-visible calque_8_couleurs TRUE)
                
                (gimp-layer-add-alpha calque_8_couleurs)
                
                (gimp-drawable-fill calque_8_couleurs 3)
                
                
            )
            (begin
                ;; dither the existing image
                ;(if (= Use_Back_Ground_Color FALSE)
                ;    (begin
                ;        (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE (+ NumColors 0) FALSE TRUE "xkh") ; was 8 ; was T T "")
                ;    )
                ;    ;else
                ;    (begin
                ;        (gimp-image-convert-indexed image CONVERT-DITHER-NONE CONVERT-PALETTE-GENERATE (+ NumColors 1) TRUE TRUE "xkh") ; was 9
                ;            (set! Nb_couleurs_trouvees -1)
                ;    )
                ;)
                ;
                ;
                ;
                ;(gimp-image-convert-grayscale image)
                ;(gimp-image-convert-rgb image)
                
                
                
                (gimp-displays-flush)
                
                
                (gimp-context-set-background Back_Ground_Color)
                
                
                
                ; 
                (set! calque_fond (car (gimp-layer-new image width height 0 "calque_fond" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image calque_fond -1 0)
                (gimp-drawable-fill calque_fond FILL-BACKGROUND)
                (gimp-item-set-visible calque_fond FALSE)
                ;
                (set! calque_8_couleurs (car (gimp-layer-new image width height 0 "calque_8_couleurs" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image calque_8_couleurs 0 -1 ) ; -1 0
                (gimp-item-set-visible calque_8_couleurs TRUE)
                (gimp-layer-add-alpha calque_8_couleurs)
                (gimp-drawable-fill calque_8_couleurs FILL-TRANSPARENT) ; was 3
                (gimp-displays-flush)
                
                (if (= Use_Back_Ground_Color FALSE)
                    (begin
                    )
                    (begin
                         (gimp-item-set-visible calque_fond TRUE)
                    )
                )
            )
        )
        
        (set! rvb 1)
        (set! color_value (/ 255 (- NumColors 1)))
        
        (set! checkNumColors (if (= Use_Back_Ground_Color FALSE) NumColors (- NumColors 1)))
        
        ; boucle pour trouver les couleurs et faire une sélection par couleur , un chemin
        (while
            (= sortie_boucle 0)
                (begin
                    
                    (if (= newImageFlag TRUE)
                        (begin
                            (gimp-image-select-color nouvelle_image 0 calque_copie (list rvb rvb rvb))
                            (gimp-selection-feather nouvelle_image 0)
                            (if (= Use_Back_Ground_Color TRUE)
                                (begin
                                    ;(gimp-selection-shrink image 0)
                                    (gimp-selection-feather image 2)
                                )
                            )
                            
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
                                    
                                    (if (= ColorSource 0)
                                        (begin
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
                                            (if (and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                (begin
                                                    ;(gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                )
                                            )
                                        )
                                    )
                                    
                                    
                                    (if (= ColorSource 1)
                                        (begin
                                            
                                            (cond
                                                ((= Nb_couleurs_trouvees 1)
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 0)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 2) (> checkNumColors 1))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 1)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 3)  (> checkNumColors 2))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 2)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 4) (> checkNumColors 3))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 3)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 5) (> checkNumColors 4))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 4)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 6) (> checkNumColors 5))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 5)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 7) (> checkNumColors 6))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 6)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 8) (> checkNumColors 7))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 7)))
                                                )
                                                ((and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                   ; (gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                
                                                )
                                                (else 
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                )
                                                ;((= Nb_couleurs_trouvees 13)
                                                ;    (gimp-context-set-foreground Color_1)
                                                ;)
                                            )
                                        )
                                    )
                                    
                                    (if (= ColorSource 2)
                                        (begin
                                            
                                            (cond
                                                ((= Nb_couleurs_trouvees 1)
                                                    
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 0) 255)
                                                                                   (checkcolor (aref color_list 1) 255)
                                                                                   (checkcolor (aref color_list 2) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 2)  (> checkNumColors 1))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 4) 255)
                                                                                   (checkcolor (aref color_list 5) 255)
                                                                                   (checkcolor (aref color_list 6) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 3) (> checkNumColors 2))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 8) 255)
                                                                                   (checkcolor (aref color_list 9) 255)
                                                                                   (checkcolor (aref color_list 10) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 4) (> checkNumColors 3))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 12) 255)
                                                                                   (checkcolor (aref color_list 13) 255)
                                                                                   (checkcolor (aref color_list 14) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 5) (> checkNumColors 4))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 16) 255)
                                                                                   (checkcolor (aref color_list 17) 255)
                                                                                   (checkcolor (aref color_list 18) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 6) (> checkNumColors 5))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 20) 255)
                                                                                   (checkcolor (aref color_list 21) 255)
                                                                                   (checkcolor (aref color_list 22) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 7) (> checkNumColors 6))
                                                    (gimp-context-set-foreground (list ((car checkcolor (aref color_list 24) 255))
                                                                                       (checkcolor (aref color_list 25) 255)
                                                                                       (checkcolor (aref color_list 26) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 8) (> checkNumColors 8))
                                                    ;(gimp-message "in color 8")
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 28) 255)
                                                                                       (checkcolor (aref color_list 29) 255)
                                                                                       (checkcolor (aref color_list 30) 255)))
                                                )
                                                ((and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                    ;(gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                
                                                )
                                                (else
                                                    ;(gimp-message "inside else")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                )
                                                ;((= Nb_couleurs_trouvees 13)
                                                ;    (gimp-context-set-foreground Color_1)
                                                ;)
                                            )
                                        )
                                    )
                                    
                                    (if (> Nb_couleurs_trouvees 0)
                                        (begin
                                            ; (gimp-edit-bucket-fill drawable fill-mode paint-mode opacity threshold sample-merged x y)
                                            (gimp-edit-bucket-fill calque_8_couleurs 0 0 100 0 FALSE 0 0)
                                            
                                            (if (= Noise TRUE)
                                                (begin
                                                    ;;(plug-in-hsv-noise 1 nouvelle_image calque_8_couleurs (- 9 Nb_couleurs_trouvees) 5 170 (* Nb_couleurs_trouvees 2))
                                                    (plug-in-hsv-noise 1 image calque_8_couleurs 4 7 170 12)
                                                )
                                            )
                                        )
                                    )
                                    
                                    
                                    
                                )
                            )
                            
                            (gimp-selection-none nouvelle_image)
                        )
                        
                        ;-------------------------------------------------------------------------------------------------
                        (begin
                            ; BLOCK FOR EXISTING IMAGE
                            (gimp-image-select-color image 0 calque_copie (list rvb rvb rvb))
                            (gimp-selection-feather image 0)
                            (if (= Use_Back_Ground_Color TRUE)
                                (begin
                                    ;(gimp-selection-shrink image 0)
                                    (gimp-selection-feather image 2)
                                )
                            )
                            (set! selection_bounds (gimp-selection-bounds image))
                            (set! select_exist (car selection_bounds))
                            (gimp-displays-flush)
                            
                            (if (= select_exist 1)
                                (begin
                                    (if (> Soften 0)
                                        (begin
                                            (gimp-selection-grow image Soften)
                                            (gimp-selection-shrink image Soften)
                                        )
                                    )
                                    (set! Nb_couleurs_trouvees (+ Nb_couleurs_trouvees 1) )
                                    ;(gimp-message (number->string Nb_couleurs_trouvees))
                                    
                                    (if (= ColorSource 0)
                                        (begin
                                            (cond
                                                ((= Nb_couleurs_trouvees 1)
                                                    (gimp-context-set-foreground Color_1)
                                                )
                                                ((= Nb_couleurs_trouvees 2)
                                                    (gimp-context-set-foreground Color_2)
                                                )
                                                ((= Nb_couleurs_trouvees 3)
                                                    (gimp-context-set-foreground Color_3)
                                                )
                                                ((= Nb_couleurs_trouvees 4)
                                                    (gimp-context-set-foreground Color_4)
                                                )
                                                ((= Nb_couleurs_trouvees 5)
                                                    (gimp-context-set-foreground Color_5)
                                                )
                                                ((= Nb_couleurs_trouvees 6)
                                                    (gimp-context-set-foreground Color_6)
                                                )
                                                ((= Nb_couleurs_trouvees 7)
                                                    (gimp-context-set-foreground Color_7)
                                                )
                                                ((= Nb_couleurs_trouvees 8)
                                                    (gimp-context-set-foreground Color_8)
                                                )
                                                ((and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                    ;(gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                
                                                )
                                                ;((= Nb_couleurs_trouvees 13)
                                                ;    (gimp-context-set-foreground Color_1)
                                                ;)
                                            )
                                        )
                                    )
                                    
                                    (if (= ColorSource 1)
                                        (begin
                                            
                                            (cond
                                                ((= Nb_couleurs_trouvees 1)
                                                    ;(gimp-message "color1")
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 0)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 2) (> checkNumColors 1))
                                                    ;(gimp-message "color2")
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 1)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 3) (> checkNumColors 2))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 2)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 4)  (> checkNumColors 3))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 3)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 5)  (> checkNumColors 4))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 4)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 6) (> checkNumColors 5))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 5)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 7) (> checkNumColors 6))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 6)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 8) (> checkNumColors 7))
                                                    (gimp-context-set-foreground (car (gimp-palette-entry-get-color paletteName 7)))
                                                )
                                                ((and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                    ;(gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                
                                                )
                                                ;((= Nb_couleurs_trouvees 13)
                                                ;    (gimp-context-set-foreground Color_1)
                                                ;)
                                            )
                                        )
                                    )
                                    
                                    (if (= ColorSource 2)
                                        (begin
                                            
                                            (cond
                                                ((= Nb_couleurs_trouvees 1)
                                                    
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 0) 254)
                                                                                       (checkcolor (aref color_list 1) 254)
                                                                                       (checkcolor (aref color_list 2) 254)))
                                                    
                                                )
                                                ((and (= Nb_couleurs_trouvees 2) (> checkNumColors 1))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 4) 254)
                                                                                       (checkcolor (aref color_list 5) 254)
                                                                                       (checkcolor (aref color_list 6) 254)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 3)  (> checkNumColors 2))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 8) 255)
                                                                                       (checkcolor (aref color_list 9) 255)
                                                                                       (checkcolor (aref color_list 10) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 4)  (> checkNumColors 3))
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 12) 255)
                                                                                       (checkcolor (aref color_list 13) 255)
                                                                                       (checkcolor (aref color_list 14) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 5)  (> checkNumColors 4))
                                                    (gimp-context-set-foreground (list  (checkcolor (aref color_list 16) 255)
                                                                                        (checkcolor (aref color_list 17) 255)
                                                                                        (checkcolor (aref color_list 18) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 6)  (> checkNumColors 5))
                                                    ;(gimp-message "6 colors")
                                                    (gimp-context-set-foreground (list  (checkcolor (aref color_list 20) 255)
                                                                                        (checkcolor (aref color_list 21) 255)
                                                                                        (checkcolor (aref color_list 22) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 7)  (> checkNumColors 6))
                                                    ;(gimp-message "7 colors")
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 24) 255)
                                                                                       (checkcolor (aref color_list 25) 255)
                                                                                       (checkcolor (aref color_list 26) 255)))
                                                )
                                                ((and (= Nb_couleurs_trouvees 8)  (> checkNumColors 7))
                                                    ;(gimp-message "8 colors")
                                                    (gimp-context-set-foreground (list (checkcolor (aref color_list 28) 255)
                                                                                       (checkcolor (aref color_list 29) 255)
                                                                                       (checkcolor (aref color_list 30) 255)))
                                                )
                                                ((and (> Nb_couleurs_trouvees checkNumColors) (= Use_Back_Ground_Color TRUE))
                                                    ;(gimp-message "its the background color")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                
                                                )
                                                (else
                                                    ;(gimp-message "else")
                                                    (gimp-context-set-foreground Back_Ground_Color)
                                                )
                                                ;((= Nb_couleurs_trouvees 13)
                                                ;    (gimp-context-set-foreground Color_1)
                                                ;)
                                            )
                                        )
                                    )
                                    (if (> Nb_couleurs_trouvees 0)
                                        (begin
                                            ; (gimp-edit-bucket-fill drawable fill-mode paint-mode opacity threshold sample-merged x y)
                                            (gimp-edit-bucket-fill calque_8_couleurs 0 0 100 0 FALSE 0 0)
                                            (if (= Noise TRUE)
                                                (begin
                                                    (set! holdness (- 9 Nb_couleurs_trouvees))
                                                    (if (or (< holdness 1) (> holdness 8))
                                                        (set! holdness 4) 
                                                    )
                                                    ;;(plug-in-hsv-noise 1 image calque_8_couleurs (- 9 Nb_couleurs_trouvees) 7 170 (* Nb_couleurs_trouvees 2))
                                                    (plug-in-hsv-noise 1 image calque_8_couleurs 4 7 170 12)
                                                )
                                            )
                                        )
                                    )
                                )
                                (begin
                                    ;(gimp-message "beep no selection")
                                )
                            )
                            (gimp-selection-none image)
                        )
                        ;-------------------------------------------------------------------------------------------------
                    )
                    (gimp-progress-update (/ rvb 256))
                    ;(set! rvb (+ rvb 1) )
                    (set! rvb (* color_value Nb_couleurs_trouvees))
                    ;(gimp-message (number->string rvb))
                     
                    (if (or (or (>= rvb 256) (= Nb_couleurs_trouvees 8)) (= Nb_couleurs_trouvees NumColors))
                        (begin
                            
                            (set! sortie_boucle 1 )			
                        )
                    )
                    
                    
                    
                    
                )
        )
        
        
        
        (if (= newImageFlag TRUE)
            (begin
                ;******************************************************************************************************
                ; 
                ;******************************************************************************************************
                (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
        
                (gimp-floating-sel-anchor selection_en_cours)
                (gimp-selection-none nouvelle_image)
                
                
                (gimp-image-undo-enable nouvelle_image)
                (gimp-display-new nouvelle_image)
                
                
                ; aplatir l'image
                (if (= flatten TRUE)
                        (gimp-image-flatten nouvelle_image)
                        ;else
                        (gimp-image-set-active-layer nouvelle_image calque_8_couleurs)
                )
            )
            ;******************************************************************************************************
            (begin
                (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
                (gimp-floating-sel-anchor selection_en_cours)
                (gimp-selection-none image)
                (gimp-displays-flush)
                ; Flatten if needed
                (if (= flatten TRUE)
                        (gimp-image-flatten image)
                        ;else
                        (gimp-image-set-active-layer image calque_8_couleurs)
                )
            )
        )
        
        ;******************************************************************************************************
        ;(gimp-message "Finishing")
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
        
        (gimp-context-pop)
        
        
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gc) ; garbage cleanup; memory cleanup
        ;(gimp-message "Good finish OK")
        
    )
)


(script-fu-register "huit_couleurs_samj"
                    "<Toolbox>/Script-Fu/Colors/8 Couleurs ..."
                    "Colorize an image using 8 Colours. Changes the tone of the image to a set of 8 colours. New image has older options and produces different outcome. \nfile:8_couleurs.scm"
                    "samj"
                    "karlhof26"
                    "20130404"
                    "RGB*"
                    SF-IMAGE    "Image"     0
                    SF-DRAWABLE "Drawable"  0
                    SF-ADJUSTMENT "Number of Colors to use"     '(8 1 8 1 1 0 0)
                    SF-ADJUSTMENT "Soften "                     '(2 0 60 1 5 0 1)
                    SF-OPTION "Color source"                 '("Custom" "Palette" "Gradient")
                    SF-COLOR "Color 1 "                     '(69 49 8)
                    SF-COLOR "Color 2 "                     '(88 62 10)
                    SF-COLOR "Color 3 "                     '(115 81 13)
                    SF-COLOR "Color 4 "                     '(138 98 16)
                    SF-COLOR "Color 5 "                     '(163 116 19)
                    SF-COLOR "Color 6 "                     '(189 134 22)
                    SF-COLOR "Color 7 "                     '(214 152 25)
                    SF-COLOR "Color 8 "                     '(240 170 28)
                    SF-COLOR "Background Color "            '(0 0 0)
                    SF-TOGGLE "Use Background Color"        FALSE
                    SF-TOGGLE "Noise"                       FALSE
                    SF-PALETTE "Palette to Use"             "Default"
                    SF-GRADIENT "Gradient"                  "Full saturation spectrum CW"
                    SF-TOGGLE "Create new image"            FALSE
                    SF-TOGGLE "Flatten"                     FALSE
                    
)

;; FIN 