; Début du script-fu Bokeh_Effect_By_Starlight86_V2_Gimp_2_8.scm
;
; Accès par :   Fichier > Créer > Motifs > Bokeh Effect Version 2.3 ...
;               File > Create > Patterns > Bokeh Effect Version 2.3 ...
;
;                __________________________________________________________
;
;
; Le didacticiel d'origine pour PhotoShop est disponible sur le site http://abduzeedo.com/digital-bokeh-photoshop-screencast-photo-guides  (Photoshopping Digital Bokeh from Ash Davies on Vimeo.)
; Merci à Ash Davies.
;
; script-fu adapté du didacticiel vidéo    Awesome Bokeh Effect in Gimp
;                                          http://www.puteraaladin.blogspot.ca/2008/10/gimp-tutorial-awesome-bokeh-effect-in.html
;				   
; Merci à Starlight86, l'auteur.
;
;
;
; Licence GNU/GPL
;
; --------------------------------------------------------------------
; édité avec Notepad++    http://notepad-plus-plus.org/
;
; version 1.0 par samj (  http://www.aljacom.com/~gimp       http://samjcreations.blogspot.com  ) 24 juin 2012
; version 1.0.1 par samj mise à niveau pour 100% compatible Gimp 2.8 (gimp-ellipse-select remplacé par gimp-image-select-ellipse , gimp-context-set-feather , gimp-context-set-feather-radius , gimp-context-set-antialias)  25 juin 2012
; version 1.0.2 par samj mise à niveau pour Fedora 17 64 bits de seed dans plug-in-solid-noise où le max = (2^31 - 1) soit 2147483647
; version 2.0 par samj ajout formes : ovale - Création d'une fonction Creer_Brosse_Pour_Bokeh_Effect
; version 2.1 Ajout Remplissage_des_bulles par des motifs le 4 juillet 2012
; version 2.2 20130330 Ajout couleurs aléatoires dans Remplissage des bulles / Filling bubbles + la fonction SF-GRADIENT ne fonctionne pas avec Gimp 2.8.4
; version 2.3 20150330 Modification contexte par jontait2 pour Gimp 2.8.14 : http://www.gimpchat.com/viewtopic.php?f=9&t=4592&start=50#p165081
; 
;
; --------------------------------------------------------------------
;
; Divers : Nombre de bulles = largeur*hauteur/1600 , bulles réparties sur 3 calques , diametre mini = petite dimension/30 , diametre maxi = petite dimension/5
;          dimension mini=256
;
;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FONCTION Creer_Brosse_Pour_Bokeh_Effect  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (Creer_Brosse_Pour_Bokeh_Effect
                diametre_maxi_bulle_fonction
                diametre_mini_bulle_fonction
                Options_rendu_fonction
                diametre_exterieur_bulle_a_creer_fonction
                diametre_interieur_bulle_a_creer_fonction
                Couleur_contour_des_bulles_fonction
                Couleur_interieur_des_bulles_fonction
                Opacite_des_bulles_fonction
                Forme_des_bulles_fonction
                Coeff_X_fonction				; <= 1
                Coeff_Y_fonction				; <= 1
                img_brosse_fonction
                calque_brosse_fonction
                Choix_Melange_fonction
                Nombre_total_de_motifs_fonction
                Remplissage_des_bulles_fonction
    )
    
    
    
    
                ;; Créer l'image de la brosse ***************************************************************************
                    
                    ; valeurs pour créer les bulles et l'image de la brosse
                    (set! diametre_exterieur_bulle_a_creer_fonction (round (random diametre_maxi_bulle_fonction)))
                    
                    ; vérifier que diametre_exterieur_bulle_a_creer_fonction est inférieur à 512 - 4 (512 semble être le max)
                    (if (> diametre_exterieur_bulle_a_creer_fonction 508)
                        (set! diametre_exterieur_bulle_a_creer_fonction 508)
                    )					
                    
					; vérifier que le minimum est respecté
					(if
						(< diametre_exterieur_bulle_a_creer_fonction diametre_mini_bulle_fonction)
							(set! diametre_exterieur_bulle_a_creer_fonction diametre_mini_bulle_fonction)
					)					

					; valeur du diamètre intérieur plus foncé
					(set! diametre_interieur_bulle_a_creer_fonction (round (* diametre_exterieur_bulle_a_creer_fonction 0.916666)))				

					; créer une image pour la brosse
					(set! img_brosse_fonction (car (gimp-image-new (+ 4 diametre_exterieur_bulle_a_creer_fonction) (+ 4 diametre_exterieur_bulle_a_creer_fonction) 0)))

					; modifier couleur de premier plan
					(if
						(or (= Options_rendu_fonction 1) (= Remplissage_des_bulles_fonction 2) )
							(begin
								; mode bulles multicolores
								(gimp-context-set-foreground (list (round (random 255)) (round (random 255)) (round (random 255))))
							)
							;else
							(begin
								(gimp-context-set-foreground Couleur_contour_des_bulles_fonction)
							)
					)
                    
                    ; modifier couleur d'arrière plan
                    (if
                        (= Remplissage_des_bulles_fonction 2) ; couleurs aléatoires
                            (begin
                                (gimp-context-set-background (list (round (random 255)) (round (random 255)) (round (random 255))))
                            )
                            ;else
                            (begin
                                (gimp-context-set-background '(0 0 0))
                            )
                    )
                    
                    ; créer le calque calque_brosse_fonction
                    (set! calque_brosse_fonction (car (gimp-layer-new img_brosse_fonction (+ 4 diametre_exterieur_bulle_a_creer_fonction) (+ 4 diametre_exterieur_bulle_a_creer_fonction) 1 "calque_brosse_fonction" 100 0)))
                    
                    ; ajouter le calque calque_brosse_fonction
                    (gimp-image-insert-layer img_brosse_fonction calque_brosse_fonction 0 -1)
                    
                    ; ne rien sélectionner
                    (gimp-selection-none img_brosse_fonction)	
                    
                    ; selection circulaire partie + claire
;					(gimp-ellipse-select 
;						img_brosse_fonction 				; image 
;						2									; x 
;						2									; y 
;						diametre_exterieur_bulle_a_creer_fonction	; width 
;						diametre_exterieur_bulle_a_creer_fonction	; height 
;						0									; operation 
;						TRUE								; antialias 
;						TRUE								; feather 
;						4									; feather-radius
;					)
                    
                    (gimp-context-set-feather TRUE)
                    
                    (gimp-context-set-feather-radius 
                        4									; feather-radius-x 
                        4									; feather-radius-y
                    )
                    
                    (gimp-context-set-antialias TRUE)
                    
                    ; forme ronde
                    (if
                        (= Forme_des_bulles_fonction 0)
                            
                            ; sélection elliptique
                            (gimp-image-select-ellipse 
                                img_brosse_fonction					; image 
                                0									; operation 
                                2									; x 
                                2									; y 
                                diametre_exterieur_bulle_a_creer_fonction	; width 
                                diametre_exterieur_bulle_a_creer_fonction	; height
                            )
                    )
                    
                    
                    ; forme elliptique
                    (if (= Forme_des_bulles_fonction 1)
                        (begin
                                
                                ; ne rien sélectionner
                                (gimp-selection-none img_brosse_fonction)
                                
                                ; créer des valeurs aléatoires entre 0.5 et 1
                                ;(gimp-message "line 177")
                                (set! Coeff_X_fonction  (+ 0.5 ( / (random 5) 10)  ) ) ; was (+ 0.5 ( / (random 6) 10)  ) )
                                (set! Coeff_Y_fonction  (+ 0.5 ( / (random 5) 10)  ) ) ; was (+ 0.5 ( / (random 6) 10)  ) )
                                
                                ; sélection elliptique
                                (gimp-image-select-ellipse 
                                    img_brosse_fonction					; image 
                                    0									; operation 
                                    2									; x 
                                    2									; y 
                                    (round (* diametre_exterieur_bulle_a_creer_fonction Coeff_X_fonction))	; width 
                                    (round (* diametre_exterieur_bulle_a_creer_fonction Coeff_Y_fonction))	; height
                                )
                        )   
                    )
                    
                    
                    ; forme carrée
                    (if (= Forme_des_bulles_fonction 2)
                        
                        ; sélectionner tout
                        (gimp-selection-all img_brosse_fonction)
                    )
                    
                    
                    ; mélange de formes
                    (if (= Forme_des_bulles_fonction 3)
                        (begin
                                    ; ne rien sélectionner
                                    (gimp-selection-none img_brosse_fonction)
                                    
                                    (set! Choix_Melange_fonction (random Choix_Melange_fonction) )
                                    
                                    (if
                                        (or (= Choix_Melange_fonction 1) (= Choix_Melange_fonction 1)) ; renvoie 0 ou 1 ; was (= Choix_Melange_fonction 0)
                                            (begin
                                                ; forme carrée
                                                ; sélectionner tout
                                                (gimp-selection-all img_brosse_fonction)
                                                
                                            )
                                            ; Else
                                            (begin
                                                ; forme elliptique
                                                ; créer des valeurs aléatoires entre 0.5 et 1
                                                ;(gimp-message "line 222")
                                                (set! Coeff_X_fonction  (+ 0.5 ( / (random 5) 10)  ) )
                                                (set! Coeff_Y_fonction  (+ 0.5 ( / (random 5) 10)  ) )
                                                
                                                ; sélection elliptique
                                                (gimp-image-select-ellipse 
                                                    img_brosse_fonction					; image 
                                                    0									; operation 
                                                    2									; x 
                                                    2									; y 
                                                    (round (* diametre_exterieur_bulle_a_creer_fonction Coeff_X_fonction))	; width 
                                                    (round (* diametre_exterieur_bulle_a_creer_fonction Coeff_Y_fonction))	; height
                                                )
                                                
                                            )
                                    )
                                    
                                    
                                    
                        )   
                    )
                    
                    ; remplir la sélection de PP	
                    (gimp-edit-fill calque_brosse_fonction 0)
                    
                    
;					; remplissage extérieur des bulles
;					(if (= Remplissage_des_bulles_fonction 1)
;						(begin
;							
;							; choix aléatoire du motif 
;							(gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) (round (random Nombre_total_de_motifs_fonction))  ) )
;							
;							; remplir de motif
;							(gimp-edit-fill calque_brosse_fonction 4)
;
;						)
;					)					
                    
                    ; modifier couleur de premier plan
                    (if
                        (= Remplissage_des_bulles_fonction 2)
                            (begin
                                ; mode bulles multicolores
                                (gimp-context-set-foreground (list (round (random 255)) (round (random 255)) (round (random 255))))
                            )
                            ;else
                            (begin
                                (gimp-context-set-foreground Couleur_interieur_des_bulles_fonction)
                            )
                    )
                    
                    ; ne rien sélectionner
                    (gimp-selection-none img_brosse_fonction)
                    
                    ; selection circulaire partie + foncée
;					(gimp-ellipse-select 
;						img_brosse_fonction 							; image 
;						(/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 )		; x 
;						(/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 )		; y 
;						diametre_interieur_bulle_a_creer_fonction		; width 
;						diametre_interieur_bulle_a_creer_fonction		; height 
;						0												; operation 
;						TRUE											; antialias 
;						TRUE											; feather 
;						1												; feather-radius
;					)
                    
                    
                    ; forme ronde
                    (if
                        (= Forme_des_bulles_fonction 0)
                            
                            ; sélection elliptique
                            (gimp-image-select-ellipse 
                                img_brosse_fonction							; image 
                                0											; operation 
                                (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; x 
                                (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; y 
                                diametre_interieur_bulle_a_creer_fonction	; width 
                                diametre_interieur_bulle_a_creer_fonction	; height 
                            )
                    )
                    
                    
                    ; forme elliptique
                    (if
                        (= Forme_des_bulles_fonction 1)
                            (begin
                                    ; ne rien sélectionner
                                    (gimp-selection-none img_brosse_fonction)
                                    
                                    (gimp-image-select-ellipse 
                                        img_brosse_fonction							; image 
                                        0											; operation 
                                        (+ 2 (round (* Coeff_X_fonction (/ (- diametre_exterieur_bulle_a_creer_fonction diametre_interieur_bulle_a_creer_fonction) 2 ))	))	; x 
                                        (+ 2 (round (* Coeff_Y_fonction (/ (- diametre_exterieur_bulle_a_creer_fonction diametre_interieur_bulle_a_creer_fonction) 2 ))	))	; y 
                                        (round (* diametre_interieur_bulle_a_creer_fonction Coeff_X_fonction)) 	; width 
                                        (round (* diametre_interieur_bulle_a_creer_fonction Coeff_Y_fonction))	; height 
                                    )
                            )
                    )
                    
                    
                    ; forme carrée
                    (if
                        (= Forme_des_bulles_fonction 2)
                            
                            (gimp-image-select-rectangle 
                                img_brosse_fonction							; image 
                                0											; operation 
                                (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; x 
                                (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; y 
                                diametre_interieur_bulle_a_creer_fonction	; width 
                                diametre_interieur_bulle_a_creer_fonction	; height 
                            )
                    )
                    
                    
                    
                    ; mélange de formes
                    (if
                        (= Forme_des_bulles_fonction 3)
                            (begin						
                                    ; ne rien sélectionner
                                    (gimp-selection-none img_brosse_fonction)
                                    
                                    (if
                                        (= Choix_Melange_fonction 1) ; renvoie 0 ou 1 ; was 0
                                            (begin
                                                ; forme carrée
                                                (gimp-image-select-rectangle 
                                                    img_brosse_fonction							; image 
                                                    0											; operation 
                                                    (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; x 
                                                    (round (/ (- (+ 4 diametre_exterieur_bulle_a_creer_fonction) diametre_interieur_bulle_a_creer_fonction) 2 ))		; y 
                                                    diametre_interieur_bulle_a_creer_fonction	; width 
                                                    diametre_interieur_bulle_a_creer_fonction	; height 
                                                )
                                                 
                                            )
                                            ; Else
                                            (begin
                                                ; forme elliptique
                                                (gimp-image-select-ellipse 
                                                    img_brosse_fonction							; image 
                                                    0											; operation 
                                                    (+ 2 (round (* Coeff_X_fonction (/ (- diametre_exterieur_bulle_a_creer_fonction diametre_interieur_bulle_a_creer_fonction) 2 ))	))	; x 
                                                    (+ 2 (round (* Coeff_Y_fonction (/ (- diametre_exterieur_bulle_a_creer_fonction diametre_interieur_bulle_a_creer_fonction) 2 ))	))	; y 
                                                    (round (* diametre_interieur_bulle_a_creer_fonction Coeff_X_fonction)) 	; width 
                                                    (round (* diametre_interieur_bulle_a_creer_fonction Coeff_Y_fonction))	; height 
                                                )
                                                
                                            )
                                    )
                                    
                                    
                            )
                    )
                    
                    
                    ; remplir la sélection de PP	
                    (gimp-edit-fill calque_brosse_fonction 0)	
                    
                    
                    ; remplissage intérieur des bulles
                    (if (= Remplissage_des_bulles_fonction 1)
                        (begin
                            
                            ; choix aléatoire du motif 
                            (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) (round (random Nombre_total_de_motifs_fonction))  ) )
                            
                            ; remplir de motif
                            (gimp-edit-fill calque_brosse_fonction 4)
                            
                        )
                    )					
                    
                    
                    
                    ; ne rien sélectionner
                    (gimp-selection-none img_brosse_fonction)
                    
                    ; mode bulles multicolores ou couleurs aléatoires
                    (if
                        (or (= Options_rendu_fonction 1) (= Remplissage_des_bulles_fonction 2) )
                            (gimp-drawable-colorize-hsl  
                                calque_brosse_fonction	; drawable 
                                (random 360)			; hue 
                                (random 100)			; lightness 
                                (- (random 200) 100)	; saturation
                            )
                    )					
                    
                    ; ajuster l'opacité du calque
                    (gimp-layer-set-opacity calque_brosse_fonction Opacite_des_bulles_fonction)
                    
                    ; copier le motif
                    (gimp-edit-copy-visible img_brosse_fonction)
                    
                    ; astuce de RobA  http://www.gimpchat.com/viewtopic.php?f=8&t=1221&start=40 pour que le presse-papiers devienne le motif sans avoir à choisir le nom qui varie selon les langues de Gimp
                    ; (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0)) ; set patten to clipboard (first in list)
                    (gimp-context-set-brush (list-ref (cadr (gimp-brushes-get-list "")) 0)) ; set brush to clipboard (first in list)
                    
                    ; modification jontait2   http://www.gimpchat.com/viewtopic.php?f=9&t=4592&start=50#p165084
                    (if
						(defined? 'gimp-context-set-brush-size)
							(gimp-context-set-brush-size (+ 4 diametre_exterieur_bulle_a_creer_fonction))
					)
                    
                    
					; afficher l'image pour tests
					;(gimp-display-new img_brosse_fonction)
                    
					; supprimer l'image
					(gimp-image-delete img_brosse_fonction)		
                    
                    
				;; FIN image de la brosse ***************************************************************************
                    
                    

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIN FONCTION Creer_Brosse_Pour_Bokeh_Effect  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;             Début du Script-Fu               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (Bokeh_Effect_version2
                width
                height
                couleur
                Coeff_densite_des_bulles
                Couleur_contour_des_bulles
                Couleur_interieur_des_bulles
                Coeff_dimension_des_bulles
                Opacite_des_bulles
                Flou_1
                Flou_2
                Flou_3
                Mode_calque_eclipse2
                Mode_calque_eclipse3
                Choix_Degrade_Suite_Probleme_Gimp_2_8_4  ; Degrade_du_calque_gradient
                Options_rendu
                Forme_des_bulles
                Remplissage_des_bulles
                flatten
                
        )
    
    (let*
        (
            ; affectation des variables		
            
            ; mémoriser les couleurs PP et AP
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            ; mémoriser la brosse active
            (old-brush (car (gimp-context-get-brush)))	
            
            ; mémoriser le dégradé actif
            (old_gradient (car (gimp-context-get-gradient)))
            
            ; image qui servira à créer le motif
            (img (car (gimp-image-new width height 0)))
            
            ; nombre de bulles par calque
            (nombre_de_bulles_par_calque)
            
            ; diamètre mini d'une bulle
            (diametre_mini_bulle)
            
            ; diamètre maxi d'une bulle
            (diametre_maxi_bulle)			
            
            ; calque_fond
            (calque_fond)
            
            ; calque_couleur
            (calque_couleur)
            
            ; calque_eclipse
            (calque_eclipse)
            
            ; calque_eclipse2
            (calque_eclipse2)
            
            ; calque_eclipse3
            (calque_eclipse3)
            
            ; calque_Clouds
            (calque_Clouds)
            
            ; calque_gradient
            (calque_gradient)			
            
            ; où appliquer la brosse
            (*array_points_brosse* (cons-array 2 'double))
            
            ; index
            (index)
            
            ;points où appliquer la brosse
            (X_brosse)
            (Y_brosse)
            
            ; Nombre_total_de_motifs
            (Nombre_total_de_motifs (car (gimp-patterns-get-list "")))
            
            ; index
            (index_motif)			
            
            ; choix du dégrade suite aux problèmes gimp 2.8.4
            (Degrade_du_calque_gradient (car (gimp-context-set-gradient "Full saturation spectrum CCW")))
            
        )
        
        
        ; vérification
        ;   (set! index_motif 0)
        ;   (if (= Remplissage_des_bulles 1)
        ;       (begin
        ;           
        ;           (while (< index_motif Nombre_total_de_motifs)
        ;               
        ;               (gimp-message (strcat "Motif : "  (list-ref (cadr (gimp-patterns-get-list "")) index_motif  )  )  ) ; nom du motif index_motif
        ;               
        ;               (set! index_motif (+ index_motif 1) )
        ;           )
        ;       )
        ;   )
        
        
        
        
        ;; Start undo group.
        (gimp-image-undo-group-start img)			
        
        ; suite au pb gimp 2.8.4 sélectionner le dégradé actif 
        (if (= Choix_Degrade_Suite_Probleme_Gimp_2_8_4 1)
            (set! Degrade_du_calque_gradient (car (gimp-context-set-gradient old_gradient)))
        )
        
        
        ; calque_fond*********************************************************************************
        
        
        ; créer calque_fond
        (set! calque_fond (car (gimp-layer-new img width height 1 "calque_fond" 100 0)))	
        
        ; ajouter le calque calque_fond
        (gimp-image-insert-layer img calque_fond 0 -1)
        
        ; modifier couleur de premier plan
        (gimp-context-set-foreground '(0 0 0))
        
        ; modifier couleur d'arrière plan
        (gimp-context-set-background '(255 255 255))
        
        ; remplir de PP	
        (gimp-drawable-fill calque_fond 0)	
        
        
        
        
        
        ; calcul des bulles**************************************************************************
        
        
        (set! nombre_de_bulles_par_calque  (round (* Coeff_densite_des_bulles (/ (* width height) 48000)) )  )
        
        ;; vérifier qu'il y a au moins 6 bulles par calque
        (if (< nombre_de_bulles_par_calque 6)
            (set! nombre_de_bulles_par_calque 6)
        )
        
        ; déterminer les diamètres
        (set! diametre_mini_bulle  (round (* (/ width 30) Coeff_dimension_des_bulles) )  )
        (set! diametre_maxi_bulle  (round (* (/ width 5) Coeff_dimension_des_bulles) )  )
        
        (if (> width height)
            (begin
                (set! diametre_mini_bulle  (round (* (/ height 30) Coeff_dimension_des_bulles) )  )
                (set! diametre_maxi_bulle  (round (* (/ height 5) Coeff_dimension_des_bulles) )  )
            )
        )
        
        ; vérifier que le mini soit au moins 20 pixels
        (if (< diametre_mini_bulle 20)
            (set! diametre_mini_bulle 20)
        )
        
        
        
        ; vérifications
        ;	(gimp-message "nombre_de_bulles_par_calque")			
        ;	(gimp-message (number->string nombre_de_bulles_par_calque))	
        ;	(gimp-message "diametre_mini_bulle")			
        ;	(gimp-message (number->string diametre_mini_bulle))
        ;	(gimp-message "diametre_maxi_bulle")			
        ;	(gimp-message (number->string diametre_maxi_bulle))
        
        
        
        
        
        
        
        ; calque_couleur*********************************************************************************
        
        
        ; créer calque_couleur
        (set! calque_couleur (car (gimp-layer-new img width height 1 "calque_couleur" 100 0)))	
        
        ; ajouter le calque calque_couleur
        (gimp-image-insert-layer img calque_couleur 0 -1)
        
        ; modifier couleur de premier plan
        (gimp-context-set-foreground couleur)
        
        ; modifier couleur d'arrière plan
        (gimp-context-set-background '(0 0 0))
        
        ; remplir de PP	
        (gimp-drawable-fill calque_couleur 0)
        
        ; appliquer un dégradé sur calque_degrade
        (gimp-edit-blend 
            calque_couleur 
            0 						; MODE
            0 
            2 						;  radial
            100 
            0 
            0 
            FALSE 
            FALSE 
            0 
            0 
            FALSE 
            (round (/ width 2))		; x1 
            (round (/ height 2))	; y1 
            0						; x2
            0 						; y2
        )
        
        
        
        
        
        ; calque_eclipse*********************************************************************************
        
        
        
        ; créer calque_eclipse
        (set! calque_eclipse (car (gimp-layer-new img width height 1 "calque_eclipse" 100 0)))	
        
        ; ajouter le calque calque_eclipse
        (gimp-image-insert-layer img calque_eclipse 0 -1)	
        
        ;; boucle pour appliquer des bulles
        (set! index 1)
        
        (while (< index nombre_de_bulles_par_calque)
                    
                    ; Appel FONCTION Creer_Brosse_Pour_Bokeh_Effect
                    (Creer_Brosse_Pour_Bokeh_Effect diametre_maxi_bulle diametre_mini_bulle Options_rendu 1 1 Couleur_contour_des_bulles 
                        Couleur_interieur_des_bulles 
                        Opacite_des_bulles 
                        Forme_des_bulles 
                        0.7 0.9 0 0 2 
                        Nombre_total_de_motifs 
                        Remplissage_des_bulles)
                    
                    ; modifier couleur de premier plan
                    (gimp-context-set-foreground '(255 255 255))
                    
                    ; modifier couleur d'arrière plan
                    (gimp-context-set-background '(0 0 0))	
                    
                    ; coordonnées où appliquer la brosse
                    (set! X_brosse (random width))
                    (set! Y_brosse (random height))
                    
                    ; contenu des 2 valeurs de array
                    (aset *array_points_brosse* 0 X_brosse)
                    (aset *array_points_brosse* 1 Y_brosse)
                    
                    ; appliquer pinceau
                    (gimp-paintbrush 
                            calque_eclipse			; drawable
                            0						; fade-out
                            2						; num-strokes 
                            *array_points_brosse*	; strokes 
                            0						; method 
                            0						; gradient-length
                    )
                    
                    (set! index (+ index 1))
                    
        )
        
	; ajouter du flou gaussien
	(plug-in-gauss 
		1						; run-mode 
		img						; image 
		calque_eclipse			; drawable 
		Flou_1						; horizontal 
		Flou_1						; vertical 
		1						; method
	)




; calque_eclipse2*********************************************************************************



	; créer calque_eclipse2
	(set! calque_eclipse2 (car (gimp-layer-new img width height 1 "calque_eclipse2" 100 0)))	

	; ajouter le calque calque_eclipse2
	(gimp-image-insert-layer img calque_eclipse2 0 -1)	

	;; boucle pour appliquer des bulles
	(set! index 1)

	(while (< index nombre_de_bulles_par_calque)

					; Appel FONCTION Creer_Brosse_Pour_Bokeh_Effect
					(Creer_Brosse_Pour_Bokeh_Effect diametre_maxi_bulle diametre_mini_bulle Options_rendu 1 1 Couleur_contour_des_bulles Couleur_interieur_des_bulles Opacite_des_bulles Forme_des_bulles 0.7 0.9 0 0 2 Nombre_total_de_motifs Remplissage_des_bulles)

					; modifier couleur de premier plan
					(gimp-context-set-foreground '(255 255 255))

					; modifier couleur d'arrière plan
					(gimp-context-set-background '(0 0 0))	

					; coordonnées où appliquer la brosse
					(set! X_brosse (random width))
					(set! Y_brosse (random height))

					; contenu des 2 valeurs de array
					(aset *array_points_brosse* 0 X_brosse)
					(aset *array_points_brosse* 1 Y_brosse)

					; appliquer pinceau
					(gimp-paintbrush 
							calque_eclipse2			; drawable
							0						; fade-out
							2						; num-strokes 
							*array_points_brosse*	; strokes 
							0						; method 
							0						; gradient-length
					)

					(set! index (+ index 1))

	)

	; ajouter du flou gaussien
	(plug-in-gauss 
		1						; run-mode 
		img						; image 
		calque_eclipse2			; drawable 
		Flou_2					; horizontal 
		Flou_2					; vertical 
		1						; method
	)

	; mode du calque calque_eclipse2
	(if
		(= Mode_calque_eclipse2 TRUE)
			; mettre en mode superposer Hard Light
			(gimp-layer-set-mode calque_eclipse2 18)	
	)











	
; calque_eclipse3*********************************************************************************



	; créer calque_eclipse3
	(set! calque_eclipse3 (car (gimp-layer-new img width height 1 "calque_eclipse3" 100 0)))	

	; ajouter le calque calque_eclipse3
	(gimp-image-insert-layer img calque_eclipse3 0 -1)	

	;; boucle pour appliquer des bulles
	(set! index 1)

	(while (< index nombre_de_bulles_par_calque)

					; Appel FONCTION Creer_Brosse_Pour_Bokeh_Effect
					(Creer_Brosse_Pour_Bokeh_Effect diametre_maxi_bulle diametre_mini_bulle Options_rendu 1 1 Couleur_contour_des_bulles Couleur_interieur_des_bulles Opacite_des_bulles Forme_des_bulles 0.7 0.9 0 0 2 Nombre_total_de_motifs Remplissage_des_bulles)

					; modifier couleur de premier plan
					(gimp-context-set-foreground '(255 255 255))

					; modifier couleur d'arrière plan
					(gimp-context-set-background '(0 0 0))	

					; coordonnées où appliquer la brosse
					(set! X_brosse (random width))
					(set! Y_brosse (random height))

					; contenu des 2 valeurs de array
					(aset *array_points_brosse* 0 X_brosse)
					(aset *array_points_brosse* 1 Y_brosse)

					; appliquer pinceau
					(gimp-paintbrush 
							calque_eclipse3			; drawable
							0						; fade-out
							2						; num-strokes 
							*array_points_brosse*	; strokes 
							0						; method 
							0						; gradient-length
					)

					(set! index (+ index 1))

	)

	; ajouter du flou gaussien
	(plug-in-gauss 
		1						; run-mode 
		img						; image 
		calque_eclipse3			; drawable 
		Flou_3					; horizontal 
		Flou_3					; vertical 
		1						; method
	)


	; mode du calque calque_eclipse3
	(if
		(= Mode_calque_eclipse3 TRUE)
			; mettre en mode superposer Grain Merge
			(gimp-layer-set-mode calque_eclipse3 21)	
	)	










; calque_Clouds*********************************************************************************



	; créer calque_Clouds
	(set! calque_Clouds (car (gimp-layer-new img width height 1 "calque_Clouds" 100 0)))	

	; ajouter le calque calque_Clouds
	(gimp-image-insert-layer img calque_Clouds 0 -1)

	; modifier couleur de premier plan
	(gimp-context-set-foreground '(0 0 0))	

	; modifier couleur d'arrière plan
	(gimp-context-set-background '(255 255 255))	

        ; nuages
        (plug-in-solid-noise 
            1							; run-mode 
            img							; image 
            calque_Clouds				; drawable 
            TRUE						; tilable 
            FALSE						; turbulent 
            (round (random 2147483647))	; seed   (round (random 9999999999)) ne fonctionne pas sous Fedora 17 64bits (max = 2^31 - 1)
            4							; detail 
            4							; xsize 
            4							; ysize
        )
        
        ; mettre en mode superposer overlay
        (gimp-layer-set-mode calque_Clouds 5)
        
        
        
        
        
        
        ; calque_gradient*********************************************************************************
        
        
        ; créer calque_gradient
        (set! calque_gradient (car (gimp-layer-new img width height 1 "calque_gradient" 100 0)))	
        
        ; ajouter le calque calque_gradient
        (gimp-image-insert-layer img calque_gradient 0 -1)
        
        ; modifier couleur de premier plan
        (gimp-context-set-foreground '(0 0 0))	
        
        ; modifier couleur d'arrière plan
        (gimp-context-set-background '(255 255 255))	
        
        ; sélectionner le dégradé choisi
        ;(gimp-context-set-gradient Degrade_du_calque_gradient)	
        
        ; appliquer un dégradé sur calque_gradient
        (gimp-edit-blend 
            calque_gradient 
            3 						; MODE
            0 
            0 						; GRADIENT-LINEAR
            100 
            0 
            0 
            FALSE 
            FALSE 
            0 
            0 
            FALSE 
            0						; x1 
            height					; y1 
            width					; x2
            0 						; y2
        )
        
        ; mettre en mode superposer overlay
        (gimp-layer-set-mode calque_gradient 5)
        
        
        
        
        
        
        
        
        
        
        ;*******************************************************************************************
        ; aplatir l'image
        (if (= flatten TRUE)
            (gimp-image-flatten img)
        )
        
        
        ;*******************************************************************************************
        
        ; restaurer PP et AP
        (gimp-context-set-foreground  old-fg)
        (gimp-context-set-background old-bg)
        
        ; restaurer ancien dégradé
        (gimp-context-set-gradient old_gradient)
        
        ; restaurer brosse
        (gimp-context-set-brush old-brush)
        
        ; ne rien sélectionner
        ;(gimp-selection-none img)
        
        ; afficher l'image
        (gimp-display-new img)
        
        ; End undo group.
        (gimp-image-undo-group-end img)	
        
        
        
        
        
    )

)



(script-fu-register
    "Bokeh_Effect_version2"
    "<Image>/File/Create/Patterns/Bokeh Effect Version 2.3 ..."
    "Créer une image pleine de bulles sur une idee de Ash Davies :o) - Version 2.3. \nfile:Bokeh_Effect_By_Starlight86_V2_Gimp_2_8.scm"
    "samj jontait2"
    "samj jontait2"
    "2015-03-30"
    ""
    SF-ADJUSTMENT "Largeur motif / Width (pixels)"  '(1600 256 2048 1 8 0 1) ; pixels
    SF-ADJUSTMENT "Hauteur motif / Height (pixels)"  '(1200 256 2048 1 8 0 1) ; pixels
    SF-COLOR "Couleur du fond / Background color" '(177 9 147) ; b10993
    SF-ADJUSTMENT "Densite des bulles / Amount of bubbles"  '(1 0.5 4 0.1 0.5 1 1) ; coeff
    SF-COLOR "Couleur contour des bulles / Outline color bubbles" '(255 255 255) ; blanc
    SF-COLOR "Couleur interieur des bulles / Color inside the bubbles" '(180 180 180) ; gris clair 180 180 180
    SF-ADJUSTMENT "Dimension des bulles / Bubble size"  '(1 0.5 4 0.1 0.5 1 1) ; coeff
    SF-ADJUSTMENT "Opacite des bulles / Opacity of the bubble"  '(90 30 100 0.1 5 1 1) ; Opacite_des_bulles
    SF-ADJUSTMENT "Flou calque__eclipse / Blur layer calque__eclipse"  '(40 0.1 200 1 10 1 1) ; Flou_1
    SF-ADJUSTMENT "Flou calque__eclipse2/ Blur layer calque__eclipse2"  '(10 0.1 200 1 10 1 1) ; Flou_2
    SF-ADJUSTMENT "Flou calque__eclipse3/ Blur layer calque__eclipse3"  '(1 0.1 200 1 10 1 1) ; Flou_3
    SF-TOGGLE "Mode calque__eclipse2 = Lumiere Dure / Hard Light" TRUE  ; Mode_calque_eclipse2
    SF-TOGGLE "Mode calque__eclipse3 = Fusion de Grain / Grain Merge" TRUE  ; Mode_calque_eclipse3
    ;; la fonction SF-GRADIENT ne fonctionne pas avec Gimp 2.8.4  ;;   SF-GRADIENT "Degrade / Gradient (calque__gradient)" "Full saturation spectrum CCW"  ; Degrade_du_calque_gradient
    SF-OPTION "Degrade / Gradient (calque__gradient)" '(" Full saturation spectrum CCW " " Degrade Actif / Active Gradient") ; Choix_Degrade_Suite_Probleme_Gimp_2_8_4  ;  
    SF-OPTION "Options" '(" Standard " " Bulles multicolores / Multicolored bubbles") ; Options_rendu
    SF-OPTION "Forme des bulles / Shape of the bubble" '(" Ronde / Round " " Elliptique / Elliptical " " Carree / Square  :o) "  " Melange /  Mixture ") ; Forme_des_bulles
    SF-OPTION "Remplissage des bulles / Filling bubbles" '(" Standard " " Motifs / Patterns" "Couleurs Aleatoires / Random Colors") ; Remplissage_des_bulles
    SF-TOGGLE "Aplatir / Flatten" FALSE ; flatten
    
)

; FIN du script