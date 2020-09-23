;--- dimensions du sprite ----
large equ 180 / 4
haut equ 188
loadingaddress equ #800

;-----------------------------

;Uncomment this line for SNA Export instead of DSK
EXPORT_SNA EQU 1

ifdef EXPORT_SNA
 buildsna  v2
 bankset 0
 ;snaset cpc_type,2 
endif

org loadingaddress
run loadingaddress

start


ld bc,#7f8d ; Mode 1
out (c),c 
call sizescreen


 call xvbl

 call palettegatearray	

  ld de,#c000 ; adresse de l'ecran 
    ld hl,urlbasecran16px384px ; pointeur sur l'image en memoire 
    ld b, 16 ; hauteur de l'image 
    loop 
    push bc ; sauve le compteur hauteur dans la pile 
    push de ; sauvegarde de l'adresse ecran dans la pile
    ld bc, 96 ; largeur de l'image a afficher
    ldir ; remplissage de n * largeur octets a l'adresse dans de 
    pop de ; recuperation de l'adresse d'origine 
    ex de,hl ; echange des valeurs des adresses
    call bc26 ; calcul de l'adresse de la ligne suivante
    ex de,hl ; echange des valeurs des adresses
    pop bc ; retabli le compteur 
    djnz loop


main 
jr main

;---- recuperation de l'adresse de la ligne en dessous ------------
bc26
    ld   a,h
    add  8
    ld   h,a
    and  #38
    ret  nz
    ld   a,h
    sub  #40
    ld   h,a
    ld   a,l
    add  #60      ; 96 chars
    ld   l,a
    ret  nc
    inc  h
    ld   a,h
    and  7
    ret  nz
    ld   a,h
    sub  8
    ld   h,a
    ret
;-----------------------------------------------------------------


sizescreen
 ld bc, #bc00+2
 out (c),c
 ld bc, #bd00+48
 out (c),c
 ld bc, #bc00+1
 out (c),c
 ld bc, #bd00+48      ; on met la valeur 48 au lieu de 40 dans le registre 1
 out (c),c
 ld bc,#bc00+6
 out (c),c
 ld bc,#bd00+21
 out (c),c
 ld bc, #bc00+7
 out (c),c
 ld bc,#bd00+#1b
 out (c),c
 ret



;--- application palette hardware -------------
palettegatearray
 ld b,#7F          ; gatearray pointer to ink 0 
 ld hl,hardpalette    ; gateraray colors valeus
 xor a                ; ink number start with 0

palettearrayloop
 ld e,(hl)
 out (c),a            ; on selectionne la couleur
 out (c),e            ; on envoie la couleur
 inc hl
 inc a 
 cp 4
 jr nz,palettearrayloop

 ld bc,#7F10          ; meme chose pour le border avec la couleur 0
 out (c),c
 ld a,#54
 out (c),a

ret
;----------------------------------------------



 ;---------------------------------------------------------------
;
; attente de plusieurs vbl
;
xvbl ld e,4
.lp:
	call waitvbl
	dec e
	jr nz,.lp
	
	ret
;-----------------------------------

;---- attente vbl ----------
waitvbl
    ld b,#f5 ; attente vbl
vbl     
    in a,(c)
    rra
    jr nc,vbl
nvbl     
    in a,(c)
    rra
    jr c,nvbl
    ret
;---------------------------

;------- data ---------------------------
hardpalette db #54, #44, #5c, #40, #36


include 'url_data.asm'
