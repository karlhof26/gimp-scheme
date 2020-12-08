; Script-Fu script_fu_masques.scm 
; Download : 
;
;   Copyright (C) 2013 samj ( http://aljacom.com/ ) 
;
;	Licence GPL
;		Ce fichier est le Script-Fu "script_fu_masques.scm" pour Gimp. 
;		Ce Script-Fu "script_fu_masques.scm" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;		telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;		Ce Script-Fu "script_fu_masques.scm" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;		pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;		Consultez la GNU General Public License pour plus de détails.
;		Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;
;	Ce fichier script_fu_masques.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;
;
;	Version 20130613 (origine)
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;	Accès sur l'image par :
;
;	Filtres > Script-Fu Masques ...
;	Filters > Script-Fu Masques ...
;
;
;
;
;	Origine :
;				- Ce script-fu est une adaptation d'un autre script utilisé pour créer des images à crayonner pour les enfants.
;
;
;	Principes :
;				- L'utilisation des images à couleurs indexées permet d'obtenir des zones uniformes de couleurs.
;				- La transformation en niveaux de gris permet de sélectionner rapidement ces zones uniformes, de les transformer en masques pour produire d'autres effets.
;
;	Utilisation
;				- Utiliser des images bien contrastées, éventuellement augmenter les contrastes avec Gimp.
;				- L'option "Rounded Lines (blur)" permet d'avoir des contours arrondis.
;				- L'option "Soften" permet d'arrondir les pointes.
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;


(define (Script-Fu_Masques_samj
        image
        drawable
        Pretreatments
        Rounded_Lines
        Soften
        Nb_Masques
        Choix_des_couleurs_des_masques
        Invert_Colors
        
    )
    (let* (
            
            (message (car (gimp-message-get-handler)))
            
            
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
            
            
            (matrix 0)
            (matrix-list 0)
            (channels 0)
            
            (masque_1)
            (masque_2)
            (masque_3)
            (masque_4)
            (masque_5)
            (masque_6)
            (masque_7)
            (masque_8)
            (masque_9)
            (masque_10)
            (masque_11)
            (masque_12)
            (masque_13)
            (masque_14)
            (masque_15)
            (masque_16)
            (Nb_masque 0)
            (Couleur_des_masques 0)
            
            
            
        )
        
        
        ; pour image existante
        (gimp-image-undo-group-start image) 
        
        ;; Start undo. Recommandé par Saulgoode pour nouvelle image http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        ; (gimp-image-undo-disable image)
        
        ; sauvegardé le contexte.  Recommandé par Saulgoode http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        (gimp-context-push)
        
        ;; sélectionner la console erreurs pour envoyer un message
        (gimp-message-set-handler 2)
        
        (gimp-selection-none image)
        
        
        
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 0 0)
        (gimp-context-set-sample-merged FALSE)
        (gimp-context-set-sample-criterion 0)
        (gimp-context-set-sample-threshold 0)
        (gimp-context-set-sample-transparent FALSE)
        
        (gimp-context-set-transform-direction TRANSFORM-FORWARD)
        (gimp-context-set-interpolation 2)
        (gimp-context-set-transform-recursion 3)
        (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
        
        (gimp-context-set-paint-mode 0) ;
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        (set! copier_visible (car (gimp-edit-named-copy-visible image buffer-name)))
        
        ; créer une nouvelle image rgb
        (set! nouvelle_image (car (gimp-image-new width height 0)))
        
        ;; Start undo. Recommandé par Saulgoode pour nouvelle image http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        (gimp-image-undo-disable nouvelle_image)
        
        
        ; créer le premier calque
        (set! calque_copie (car (gimp-layer-new nouvelle_image width height 0 "fond FirstLayer" 100 0)))
        
        (gimp-image-insert-layer nouvelle_image calque_copie 0 -1) ; was -1 0
        
        (gimp-image-select-rectangle nouvelle_image 0  width height 0 0 )
        
        (set! selection_en_cours (car (gimp-edit-named-paste calque_copie copier_visible 0)))
        
        (gimp-floating-sel-anchor selection_en_cours)
        
        (gimp-selection-none nouvelle_image)
        
        (gimp-layer-add-alpha calque_copie)
        
        (gimp-levels-stretch calque_copie)
        (gimp-equalize calque_copie TRUE)
        
        ; ajuster taille image à la taille des calques
        (gimp-image-resize-to-layers nouvelle_image)
        
        (set! calque_copie (car (gimp-image-get-active-layer nouvelle_image)))
        
        
        (if (or (= Pretreatments 1) (= Pretreatments 3) ) ; "B For dark images"
            (begin
                
                (set! matrix-list
                    '(
                        0       0       0       0       0
                        0       1       0       0       0
                        0       0       1       0       0
                        0       0       0       0       0
                        0       0       0       0       0
                     )
                )
                
                (set! channels (make-vector 5 'long ))
                
                (vector-set! channels 0 1 )
                (vector-set! channels 1 1 )
                (vector-set! channels 2 1 )
                (vector-set! channels 3 1 )
                (vector-set! channels 4 1 )
                
                ; Convert maxtrix list (25) into matrix array (5x5)
                (define (get-matrix matrix-list)
                    (let* (
                            (n 0)
                        )
                        (set! n 25 )
                        (define (list-ref l n) (nth n l))
                        (let* ( (count 0)
                                (matrix (cons-array 25 'double))
                              )
                            (while (< count 25 )
                                (aset matrix count (list-ref matrix-list count))
                                (set! count (+ count 1))
                            )
                            matrix ; Return the matrix array
                        )
                    )
                )
                
                ; (plug-in-convmatrix run-mode image drawable argc-matrix matrix alpha-alg divisor offset argc-channels channels bmode)
                (plug-in-convmatrix 1 nouvelle_image calque_copie 25 (set! matrix (get-matrix matrix-list)) 1 1 0 5 channels 0)
                
                
            )
        )
        
        
        (if (or (= Pretreatments 2) (= Pretreatments 3) ) ; "C Low contrast picture"
            (begin
                
                (set! matrix-list
                    '(
                        -1      0       0       0       0
                        0       1       0       0       0
                        0       0       1       0       0
                        0       0       0       0       0
                        0       0       0       0       0
                     )
                )
                
                (set! channels (make-vector 5 'long ))
                
                (vector-set! channels 0 1 )
                (vector-set! channels 1 1 )
                (vector-set! channels 2 1 )
                (vector-set! channels 3 1 )
                (vector-set! channels 4 1 )
                
                ; Convert maxtrix list (25) into matrix array (5x5)
                (define (get-matrix matrix-list)
                  (let* (
                            (n 0)
                        )
                    (set! n 25 )
                    (define (list-ref l n) (nth n l))
                        (let* ( (count 0)
                                (matrix (cons-array 25 'double))
                              )
                            (while (< count 25 )
                                (aset matrix count (list-ref matrix-list count))
                                (set! count (+ count 1))
                            )
                            matrix ; Return the matrix array
                        )
                  )
                )
                
                ; (plug-in-convmatrix run-mode image drawable argc-matrix matrix alpha-alg divisor offset argc-channels channels bmode)
                (plug-in-convmatrix 1 nouvelle_image calque_copie 25 (set! matrix (get-matrix matrix-list)) 1 1 0 5 channels 0)
                
                
                
            )
        )
        
        (gimp-desaturate-full calque_copie 1)
        
        (gimp-posterize calque_copie Nb_Masques)
        
        (if (> Rounded_Lines 0) 
            ;;;;;(plug-in-gauss 1 nouvelle_image calque_copie (round (+ 1 (/ width 40)))  (round (+ 1 (/ height 40)))   1)
            (plug-in-gauss 1 nouvelle_image calque_copie Rounded_Lines  Rounded_Lines   1)
        )
        
        (gimp-convert-indexed nouvelle_image 0 0 Nb_Masques 1 1 "")
        
        (gimp-image-convert-grayscale nouvelle_image)
        
        (gimp-image-convert-rgb nouvelle_image)
        
        
        
        
        
        ;******************************************************************************************************
        ; créer les calques utilisés comme masques
        ;******************************************************************************************************
        
        (gimp-context-set-background '(0 0 0))
        (gimp-context-set-foreground '(255 255 255))
        
        (set! sortie_boucle 0)
        (set! rvb -1)
        (if (= Choix_des_couleurs_des_masques 0) (set! Couleur_des_masques 3) )
        (if (= Choix_des_couleurs_des_masques 1) (set! Couleur_des_masques 2) )
        
        
        
        ; boucle pour trouver les couleurs, faire une sélection par couleur , créer les masques
        (while (= sortie_boucle 0)
            (begin
                
                (set! rvb (+ rvb 1) )
                (if (= rvb 256)
                    (begin
                        (set! sortie_boucle 1 )
                    )
                )
                
                (gimp-selection-none nouvelle_image)
                
                (if (= sortie_boucle 0)
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
                                
                                (set! Nb_masque (+ Nb_masque 1) )
                                
                                (if (> Nb_masque Nb_Masques)
                                    (begin
                                        (set! sortie_boucle 1 )
                                    )
                                    ;else
                                    (begin
                                        
                                        ; (gimp-message (number->string Nb_masque) ) 
                                        
                                        (if (= Nb_masque 1)
                                            (begin
                                                (set! masque_1 (car (gimp-layer-new nouvelle_image width height 1 "masque_1" 100 0)))
                                                (gimp-image-insert-layer nouvelle_image masque_1 -1 0)
                                                (gimp-drawable-fill masque_1 Couleur_des_masques) ; blanc ou transparent
                                                (gimp-edit-fill masque_1 1) ; background
                                                (gimp-item-set-visible masque_1 TRUE)
                                                (gimp-selection-none nouvelle_image)
                                                (if (= Invert_Colors 1) (gimp-invert masque_1) )
                                                ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        (if (= Nb_masque 2)
                                            (begin
                                                    (set! masque_2 (car (gimp-layer-new nouvelle_image width height 1 "masque_2" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_2 -1 0)
                                                    (gimp-drawable-fill masque_2 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_2 1) ; background
                                                    (gimp-item-set-visible masque_2 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_2) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 3)
                                            (begin
                                                    (set! masque_3 (car (gimp-layer-new nouvelle_image width height 1 "masque_3" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_3 -1 0)
                                                    (gimp-drawable-fill masque_3 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_3 1) ; background
                                                    (gimp-item-set-visible masque_3 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_3) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 4)
                                            (begin
                                                    (set! masque_4 (car (gimp-layer-new nouvelle_image width height 1 "masque_4" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_4 -1 0)
                                                    (gimp-drawable-fill masque_4 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_4 1) ; background
                                                    (gimp-item-set-visible masque_4 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_4) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 5)
                                            (begin
                                                    (set! masque_5 (car (gimp-layer-new nouvelle_image width height 1 "masque_5" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_5 -1 0)
                                                    (gimp-drawable-fill masque_5 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_5 1) ; background
                                                    (gimp-item-set-visible masque_5 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_5) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 6)
                                            (begin
                                                    (set! masque_6 (car (gimp-layer-new nouvelle_image width height 1 "masque_6" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_6 -1 0)
                                                    (gimp-drawable-fill masque_6 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_6 1) ; background
                                                    (gimp-item-set-visible masque_6 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_6) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 7)
                                            (begin
                                                    (set! masque_7 (car (gimp-layer-new nouvelle_image width height 1 "masque_7" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_7 -1 0)
                                                    (gimp-drawable-fill masque_7 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_7 1) ; background
                                                    (gimp-item-set-visible masque_7 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_7) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 8)
                                            (begin
                                                    (set! masque_8 (car (gimp-layer-new nouvelle_image width height 1 "masque_8" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_8 -1 0)
                                                    (gimp-drawable-fill masque_8 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_8 1) ; background
                                                    (gimp-item-set-visible masque_8 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_8) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 9)
                                            (begin
                                                    (set! masque_9 (car (gimp-layer-new nouvelle_image width height 1 "masque_9" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_9 -1 0)
                                                    (gimp-drawable-fill masque_9 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_9 1) ; background
                                                    (gimp-item-set-visible masque_9 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_9) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 10)
                                            (begin
                                                    (set! masque_10 (car (gimp-layer-new nouvelle_image width height 1 "masque_10" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_10 -1 0)
                                                    (gimp-drawable-fill masque_10 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_10 1) ; background
                                                    (gimp-item-set-visible masque_10 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_10) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 11)
                                            (begin
                                                    (set! masque_11 (car (gimp-layer-new nouvelle_image width height 1 "masque_11" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_11 -1 0)
                                                    (gimp-drawable-fill masque_11 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_11 1) ; background
                                                    (gimp-item-set-visible masque_11 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_11) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 12)
                                            (begin
                                                    (set! masque_12 (car (gimp-layer-new nouvelle_image width height 1 "masque_12" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_12 -1 0)
                                                    (gimp-drawable-fill masque_12 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_12 1) ; background
                                                    (gimp-item-set-visible masque_12 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_12) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 13)
                                            (begin
                                                    (set! masque_13 (car (gimp-layer-new nouvelle_image width height 1 "masque_13" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_13 -1 0)
                                                    (gimp-drawable-fill masque_13 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_13 1) ; background
                                                    (gimp-item-set-visible masque_13 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_13) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 14)
                                            (begin
                                                    (set! masque_14 (car (gimp-layer-new nouvelle_image width height 1 "masque_14" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_14 -1 0)
                                                    (gimp-drawable-fill masque_14 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_14 1) ; background
                                                    (gimp-item-set-visible masque_14 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_14) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 15)
                                            (begin
                                                    (set! masque_15 (car (gimp-layer-new nouvelle_image width height 1 "masque_15" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_15 -1 0)
                                                    (gimp-drawable-fill masque_15 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_15 1) ; background
                                                    (gimp-item-set-visible masque_15 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1) (gimp-invert masque_15) )
                                                    ; (gimp-message (number->string rvb) )
                                            )
                                        )
                                        
                                        (if (= Nb_masque 16)
                                            (begin
                                                    (set! masque_16 (car (gimp-layer-new nouvelle_image width height 1 "masque_16" 100 0)))
                                                    (gimp-image-insert-layer nouvelle_image masque_16 -1 0)
                                                    (gimp-drawable-fill masque_16 Couleur_des_masques) ; blanc ou transparent
                                                    (gimp-edit-fill masque_16 1) ; background
                                                    (gimp-item-set-visible masque_16 TRUE)
                                                    (gimp-selection-none nouvelle_image)
                                                    (if (= Invert_Colors 1)
                                                        (gimp-invert masque_16) )
                                                    ; (gimp-message (number->string rvb) )
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
        
        
        (gimp-selection-none nouvelle_image)
        ; was not removed
        ; (gimp-image-remove-layer nouvelle_image calque_copie)
        
        
        
        
        ;******************************************************************************************************
        ;    
        ;******************************************************************************************************
        
        
        
        (gimp-display-new nouvelle_image)
        
        ; End undo. Recommandé par Saulgoode pour nouvelle image http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        (gimp-image-undo-enable nouvelle_image)
        
        
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        ;; rétablir le mode message
        (gimp-message-set-handler message)
        
        ; restituer l'ancien contexte.  Recommandé par Saulgoode http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        (gimp-context-pop)
        
        ; End undo. Recommandé par Saulgoode pour nouvelle image http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
        ;(gimp-image-undo-enable image)
        
        ; pour image existante
        (gimp-image-undo-group-end image)
        
        (gimp-displays-flush)
        
        
    )
)



; Nouvelle méthode recommandé par Saulgoode http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
(script-fu-register "Script-Fu_Masques_samj"
                    "Script-Fu Masques ..."
                    "Script-Fu Masques. Creates Masks in upto 16 variations. Hide top-most few layers to reveal image.\nfile:script_fu_masques.scm"
                    "samj"
                    "samj"
                    "20130613"
                    "RGB*"
                    SF-IMAGE    "Image"     0
                    SF-DRAWABLE "Drawable"  0
                    SF-OPTION "Pretreatments " '("A  Without" "B  For dark images" "C  Low contrast picture" "D Combination of B + C  ")
                    SF-ADJUSTMENT "Rounded Lines (blur) " '(0 0 60 1 5 0 1)
                    SF-ADJUSTMENT "Soften " '(0 0 60 1 5 0 1)
                    SF-ADJUSTMENT "Nb Masks " '(8 2 16 1 5 0 1)
                    SF-OPTION "Colors Mask " '("Black Transparent" "Black White")
                    SF-TOGGLE "Invert Colors" FALSE
                    
)

; Nouvelle méthode recommandé par Saulgoode http://gimpchat.com/viewtopic.php?f=9&t=7342#p93780
(script-fu-menu-register "Script-Fu_Masques_samj"
                    "<Toolbox>/Script-Fu2/Masks/"
)



;; FIN