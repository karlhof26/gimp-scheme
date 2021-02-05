; Début du script-fu feu_d_artifice_fireworks_2_8.scm 
;
; Accès par :   Fichier > Créer > Motifs > Feu d artifice - Fireworks...
;               File > Create > Patterns > Feu d artifice - Fireworks...
;
;                __________________________________________________________
; 
;
; script-fu adapté du didacticiel vidéo    http://www.youtube.com/watch?v=t6mc5ItPy1o
;				   
; Merci à DavidWoodFX ( http://www.youtube.com/user/DavidWoodFX ), l'auteur.
;
;
;
; Licence GNU/GPL
;
; --------------------------------------------------------------------
; édité avec Notepad++    http://notepad-plus-plus.org/
;
; version 1.0 par samj (  http://www.aljacom.com/~gimp       http://samjcreations.blogspot.com  ) 15 juin 2012
; version 1.1 par samj 23 juin 2012 : ajout de dégradé pour obtenir un feu avec des couleurs concentriques
; version 1.2 by karlhof26 - 5 Feb 2021 for GIMP-2.10.22
;
; --------------------------------------------------------------------
;
; Remarque : Le filtre Qbiste permet d'obtenir des couleurs avec le mode Multicolore.


(define (feu_d_artifice_fireworks
                width
                height
                couleur
                degrade_couleurs_concentriques
                intensite
                Options_couleurs
                flatten
                
        )
        
    (let* (
            ; affectation des variables		
            
            ; mémoriser les couleurs PP et AP
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            ; mémoriser le dégradé actif
            (old_gradient (car (gimp-context-get-gradient)))
            
            ; créer une nouvelle image rgb
            (img (car (gimp-image-new width height 0)))
            
            ;calque_fond
            (calque_fond)
            
            ;calque_fond_2
            (calque_fond_2)
            
            ;calque_fond_3
            (calque_fond_3)		
            
            ;calque_couleur
            (calque_couleur)
            
            ;calque_multicolore
            (calque_multicolore)
            
            ;calque_degrade
            (calque_degrade)
            
            ; intensité du feu d'artifice
            (holdness 2)
            (value-distance 227)
            
            (bluramt 3.0)
        )
    
    ;; Start undo group.
    (gimp-image-undo-group-start img)	
    
    
    
    
    
    
    
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
    
    ; créer une sélection circulaire
    (gimp-image-select-ellipse 
        img							; image 
        0							; operation 
        (round (* width 0.05 ))		; x 
        (round (* height 0.05 ))	; y 
        (round (* width 0.9 ))		; width 
        (round (* height 0.9 ))		; height ; was width-why?
    )
    
    
    ; déterminer l'intensité
    
    (if (= intensite 1)
            (begin
                (set! holdness 1)
                (set! value-distance 210)
            )
    )
    
    (if 
        (= intensite 2)
            (begin
                (set! holdness 1)
                (set! value-distance 190)
            )
    )
    
    (if 
        (= intensite 3)
            (begin
                (set! holdness 1)
                (set! value-distance 170)
            )
    )
    
    (if 
        (= intensite 4)
            (begin
                (set! holdness 2)
                (set! value-distance 255)
            )
    )
    
    (if 
        (= intensite 5)
            (begin
                (set! holdness 2)
                (set! value-distance 200)
            )
    )
    
    ; ajouter du bruit TSV
    (plug-in-hsv-noise 
        1							; run-mode 
        img							; image 
        calque_fond					; drawable 
        holdness					; holdness   défaut 2
        3							; hue-distance 
        10							; saturation-distance 
        value-distance				; value-distance  défaut 227
    )
    
    ; ajouter du flou
    (plug-in-blur 1 img calque_fond)
    
    ; ajuster niveaux de couleurs
    (gimp-drawable-levels 
        calque_fond				; drawable 
        HISTOGRAM-VALUE						; channel 
        0.0						; low-input 
        0.21						; high-input was 52 
        TRUE                    ; clamp
        0.29					; gamma  was 0.29
        0.0						; low-output 
        1.0						; high-output
        TRUE
    )
    
    
    ; ne rien sélectionner
    (gimp-selection-none img)
    
    ; coordonnées polaires
    (plug-in-polar-coords 
        1                       ; run-mode 
        img                     ; image 
        calque_fond             ; drawable 
        100                     ; circle 
        0                       ; angle 
        FALSE                   ; backwards 
        TRUE                    ; inverse 
        FALSE                   ; polrec
    )
    
    
    ; tourner le calque de 90° sens horaire
    (gimp-item-transform-rotate-simple 
        calque_fond				; item 
        0						; rotate-type 
        TRUE					; auto-center 
        (* width 0.5 )			; center-x 
        (* height 0.5 )			; center-y
    )
    
    
    ; créer un effet de vent
    (plug-in-wind 
        1                       ; run-mode 
        img                     ; image 
        calque_fond             ; drawable 
        7                       ; threshold 
        0                       ; direction 
        12                      ; strength 
        0                       ; algorithm 
        1                       ; edge
    )
    
    ; tourner le calque de 90° sens anti-horaire
    (gimp-item-transform-rotate-simple
        calque_fond             ; item 
        2                       ; rotate-type was 2
        TRUE                    ; auto-center 
        (* width 0.5 )          ; center-x 
        (* height 0.5 )         ; center-y
    )
   
    
    ; coordonnées polaires
    (plug-in-polar-coords 
        1						; run-mode 
        img						; image 
        calque_fond				; drawable 
        100						; circle 
        0						; angle 
        FALSE                   ; backwards  Was F
        TRUE					; inverse 
        TRUE					; polrec
    )
     

    
    ; modifier couleur d'arrière plan
    (gimp-context-set-background '(0 0 0))
    
    ; aplatir le calque
    ;(gimp-layer-flatten calque_fond)
    
    
     
    ; modifier couleur d'arrière plan
    (gimp-context-set-background '(255 255 255))
    
    
    ; ajuster niveaux de couleurs
    (gimp-drawable-levels 
        calque_fond				; drawable 
        HISTOGRAM-VALUE						; channel 
        (/ 130 255)						; low-input 
        (/ 230 255)						; high-input 
        TRUE                            ; clamp input
        0.83                    ; gamma  was 0.72
        0.0                     ; low-output was 0
        1.0                     ; high-output was 1
        TRUE                    ; clamp output
    )
    
    
                
    
    
    ; calque_fond_2**********************************************************
    
    ; copier le calque calque_fond
    (set! calque_fond_2 (car (gimp-layer-new-from-visible img img "calque_fond_2")))
    
    ; ajouter le calque calque_fond_2
    (gimp-image-insert-layer img calque_fond_2 0 -1)
    
    ; mettre le calque en mode addition
    (gimp-layer-set-mode calque_fond_2 LAYER-MODE-MULTIPLY-LEGACY) ; was 7addition
    
    
    
    ; ajouter du flou gaussien
    (plug-in-gauss 
        1                       ; run-mode 
        img                     ; image 
        calque_fond_2           ; drawable 
        bluramt                 ; horizontal  was 10
        bluramt                 ; vertical was 10
        1                       ; method
    )
    
                 
    ; calque_fond_3**********************************************************
    
    ; copier le calque calque_fond
    (set! calque_fond_3 (car (gimp-layer-new-from-visible img img "calque_fond_3")))
     
     ; ajouter le calque calque_fond_3
     (gimp-image-insert-layer img calque_fond_3 0 -1)
     
     ; mettre le calque en mode addition
     (gimp-layer-set-mode calque_fond_3 LAYER-MODE-ADDITION-LEGACY) ; was 7
    
     
    
    ; ajouter du flou gaussien
    (plug-in-gauss 
        1                       ; run-mode 
        img                     ; image 
        calque_fond_3           ; drawable 
        bluramt                      ; horizontal 
        bluramt                      ; vertical 
        1                       ; method
    )
    ; set it invisible
    (gimp-item-set-visible calque_fond_3 FALSE)
    
     
    
    ; calque_couleur**********************************************************
    
    ; créer le calque calque_couleur
    (set! calque_couleur (car (gimp-layer-new img width height 1 "calque_couleur" 100 LAYER-MODE-NORMAL)))
    
    ; ajouter le calque calque_couleur
    (gimp-image-insert-layer img calque_couleur 0 -1)
    
    ; mettre le calque en mode couleur
    (gimp-layer-set-mode calque_couleur 13)
    
    ; modifier couleur de premier plan
    (gimp-context-set-foreground couleur)
    
    ; remplir de PP	
    (gimp-drawable-fill calque_couleur FILL-FOREGROUND)
    
    
    
    ; calque_degrade**********************************************************
    
    ; si option  Couleurs concentriques (degrade)  validéé
    (if
        (= Options_couleurs 1)
            
            (begin
                
                ; créer le calque calque_degrade
                (set! calque_degrade (car (gimp-layer-new img width height 1 "calque_degrade" 100 0)))
                
                ; ajouter le calque calque_degrade
                (gimp-image-insert-layer img calque_degrade 0 -1)
                
                ; mettre le calque en mode couleur
                (gimp-layer-set-mode calque_degrade 13)
                
                ; sélectionner le dégradé choisi
                (gimp-context-set-gradient degrade_couleurs_concentriques)				
                
                
                ; appliquer un dégradé sur calque_degrade
                (gimp-edit-blend 
                    calque_degrade 
                    BLEND-CUSTOM 						; MODE
                    LAYER-MODE-OVERLAY  ; was normal
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
                    (round (/ width 2))		; x2
                    0                       ; y2
                )
                
                
                ; calque_couleur**********************************************************
                
                ; supprimer visibilité calque_couleur
                (gimp-layer-set-visible calque_couleur FALSE)
                
                
            )
    )
    
    
    
    
    
    
    
    
    
    
    
    ; calque_multicolore**********************************************************
    
    ; si option multicolore validéé
    (if (= Options_couleurs 2)
            
            (begin
                
                ; créer le calque calque_multicolore
                (set! calque_multicolore (car (gimp-layer-new img width height 1 "calque_multicolore" 100 0)))
                
                ; ajouter le calque calque_multicolore
                (gimp-image-insert-layer img calque_multicolore 0 -1)
                
                ; mettre le calque en mode couleur
                (gimp-layer-set-mode calque_multicolore 13) ; was 13hsl-color
                
                ; Qbist ne fonctionne pas en run-mode = 1
                (plug-in-qbist 
                    0                       ; run-mode 
                    img                     ; image 
                    calque_multicolore      ; drawable
                )
                
                
                
                
                ; calque_couleur**********************************************************
                
                ; supprimer visibilité calque_couleur
                (gimp-layer-set-visible calque_couleur FALSE)
                 
                
            )
    )
    
    
    
    
    
    
    ;*******************************************************************************************
    
    ; aplatir l'image
    (if
        (= flatten TRUE)
            (gimp-image-flatten img)
    )
    
    
    ;*******************************************************************************************
    
    ; restaurer PP et AP
    (gimp-context-set-foreground  old-fg)
    (gimp-context-set-background old-bg)
    
    ; restaurer ancien dégradé
    (gimp-context-set-gradient old_gradient)
    
    ; ne rien sélectionner
    ;(gimp-selection-none img)
    
    ; afficher l'image
    (gimp-display-new img)
    
    ; End undo group.
    (gimp-image-undo-group-end img)
    
    
    )

)



(script-fu-register
    "feu_d_artifice_fireworks"
    "<Image>/File/Create/Patterns/Feu d artifice - Fireworks..."
    "Créer un feu d artifice (Fireworks). View and hide layers to see variations. \nfile:feu_d_artifice_fireworks_2_8.scm"
    "samj"
    "samj"
    "2012-06-15"
    ""
    SF-ADJUSTMENT   "Largeur motif / Width (pixels)"    '(200 100 4096 1 8 0 1) ; pixels
    SF-ADJUSTMENT   "Hauteur motif / Height (pixels)"   '(200 100 4096 1 8 0 1) ; pixels
    SF-COLOR        "Couleur"                           '(232 122 106) ; e87a6a
    SF-GRADIENT     "Degrade / Gradient (Couleurs concentriques)"   "Abstract 2"
    SF-OPTION       "Intensite"                 '("Normale" " + + + + " " + + + " " + + " " + " " - ") ; intensite
    SF-OPTION       "Options couleur(s)"        '(" 1 couleur " " Couleurs concentriques (Gradient Concentric colours) " " Multicolore ") ; Options_couleurs intensite
    SF-TOGGLE       "Aplatir / Flatten"             FALSE

)


; FIN du script