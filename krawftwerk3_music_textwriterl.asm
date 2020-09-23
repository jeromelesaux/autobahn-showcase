;--- dimensions du sprite ----
large equ 180 / 4
haut equ 148; //16*8 ; 188
loadingaddress equ #800
kw_offset equ 96*1+4  ; 2 lignes et 4 octets
linewidth equ 96
logo_position equ #c000+96*19
;-----------------------------

;Uncomment this line for SNA Export instead of DSK
;EXPORT_SNA EQU 1

ifdef EXPORT_SNA
 buildsna  v2
 bankset 0
endif

org loadingaddress
run loadingaddress

start

    di

    ld bc,#7f8d ; Mode 1
    out (c),c
    call sizescreen
    ld a,#c3
    ld (#38),a
    ld hl,music_int
    ld (#39), hl

    ld sp,loadingaddress

    ;---- Initializes the music.
    ld hl,Music
    xor a                                   ;Subsong 0.
    call PLY_AKG_Init
    ;-------------------------------


    ; All inkk set to black
    ld hl,palette0
    call setpalette
    call xvbl

    ;--- affichage du sprite initial --
    ld de,#d091 ; adresse de l'ecran
    ld hl,sprite ; pointeur sur l'image en memoire
    ld bc, large ; hauteur de l'image
    ld a,haut
    call drawSprite

    ; Calcule la table des adresses contenant des pixels
    ; dans le logo, pour son apparition progressive, rapide
    ld ix,logo_table
    ld hl,logo_position
    ld de,url_logo
    ld c,16
    ld iy,0

calc_logo_table:
    ld b,96
    push hl
.lp0
    push bc
    ld a,(de)
    or a
    jr z,.skip_pix

    ld (ix),e
    inc ix
    ld (ix),d
    inc ix
    ld (ix),l
    inc ix
    ld (ix),h
    inc ix

    inc iy
.skip_pix:
    inc hl
    inc de
    pop bc
    djnz .lp0

    pop hl
    call bc26
    dec c
    jr nz,calc_logo_table
    ld (numpix_logo_table),iy

    ;---- init text writer ---------
    ld ix,texte
    ld (fontptr),ix
    ;-------------------------------

    call xvbl
    ei

    ld b,46
wait0:
    push bc
    call xvbl
    pop bc
    djnz wait0

    ld hl,hardpalette
    call setpalette

    ld a,96*2
    ld (draw_lines+1),a

    ; Tempo avant de lancer l'animation et le texte
    ld b,46
wait:
    push bc
    call xvbl
    pop bc
    djnz wait

;    ld hl,url_logo
;    ld de,logo_position
;    ld a,16
;    ld bc, 96
    ;call drawSprite




;------------------------------------
mainloop
    call waitvbl
    ld a,#00
    ld (font_mask+1),a
    call drawtext

    ;call waitvbl
    ld e,3
    call xvbl.lp
    call next_delta

    call waitvbl
    ld a,#55
    ld (font_mask+1),a
    call drawtext

    ld e,4
    call xvbl.lp
    call next_delta

    ld e,4
    call xvbl.lp
    ld a,#ff
    ld (font_mask+1),a
    call fontwriter

    jp mainloop

music_int:
    push af,bc,de,hl

;    ld b,#F5
;    in a,(c)
;    jr nc, end_int
cntit:
    ld a,6
    dec a
    ld (cntit+1),a
    jr nz,end_int
    ld a,6
    ld (cntit+1),a
    exx
    ex af,af'
    push af,bc,de,hl
    push ix,iy
    call PLY_AKG_Play
    pop iy,ix
    pop hl,de,bc,af
    ex af,af'
    exx
end_int:
    pop hl,de,bc,af
    ei
    ret

;--- routine de deltapacking --------------------------

next_delta:

table_index:
    ld a,-1
    inc a
    and 15
    ld (table_index+1),a
    add a
    ld e,a
    ld d,0
    ld hl,table_delta
    add hl,de
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    ; jp delta

delta
    ld a,(hl) ; nombre de byte a poker
    inc hl
init
    ex af,af'
    ld a,(hl) ; octet a poker
    ld (pixel1+1),a
    inc hl
    ld c,(hl) ; nbfois
    inc hl
    ld b,(hl)
    inc hl
;
poke_octet
    ld e,(hl)
    inc hl
    ld d,(hl) ; de=adresse
    inc hl
pixel1:
     ld a,0

 ld (de),a ; poke a l'adresse dans de

; TEMPORAIRE pour caller l'anim
;    push hl
;    ld hl, kw_offset
;    add hl,de
;    ld (hl),a ; poke a l'adresse dans de
;    pop hl
;------------------
    dec bc
    ld a,b ; test a t'on poke toutes les adresses compteur bc
    or c  ; optimisation siko
    jr nz, poke_octet

    ex af,af'
    dec a ; reste t'il d'autres bytes a poker ?
    jr nz, init
    ret
;---------------------------------------------------

;----- writer variables ----------------------------
fontscreenaddress dw #C000+kw_offset; //-(#0030+#60+#90+#90) ; adresse ecran de la fonte
nbfontdisplayed db 12   ; nombre de colonne affichee
nblinedisplayed db 0   ; nombre de ligne affichee
fontptr dw 0           ; pointeur vers le texte a afficher
;---------------------------------------------------

drawSprite:
.loop
    push af ; sauve le compteur hauteur dans la pile
    push de ; sauvegarde de l'adresse ecran dans la pile
    push bc
    ldir ; remplissage de n * largeur octets a l'adresse dans de
    pop bc
    pop de ; recuperation de l'adresse d'origine
    ex de,hl ; echange des valeurs des adresses
    call bc26 ; calcul de l'adresse de la ligne suivante
    ex de,hl ; echange des valeurs des adresses
    pop af ; retabli le compteur
    dec a
    jr nz, .loop
    ret

drawtext
    ld ix,(fontptr)           ; on recupere le pointeur de la fonte actuelle
    call findchar             ; trouve la bonne fonte
    ld de,(fontscreenaddress) ; on recupere le pointeur de l'adresse ecran
    call displayfont          ; on affiche la fonte
    ret

;----- fontwriter main routine ---------------
fontwriter
    ld ix,(fontptr)           ; on recupere le pointeur de la fonte actuelle
    call testfonteol          ; est ce la fin du texte ?
    call findchar             ; trouve la bonne fonte
    inc ix                    ; on incremente ix sur le char suivant
    ld (fontptr),ix           ; sauvegarde du pointeur de la fonte actuelle

    ld de,(fontscreenaddress) ; on recupere le pointeur de l'adresse ecran
    call displayfont          ; on affiche la fonte
    ld (fontscreenaddress),de ; on stocke le pointeur de l'adresse ecran

    ld a,(nbfontdisplayed)
    dec a                     ; decremente le compteur
    ld (nbfontdisplayed),a

    ret nz     ; si a != 0 on boucle sur l'affichage des fontes
    call initwritervalues
    ret
;--------------------------------------------

;----- test the end of the text -------------
testfonteol
    ld a,(ix+0)               ; test si on arrive a la fin de la fonte
    or a
    jr nz,noreset             ; si oui reset le pointeur ix de la fonte
    ld ix,texte
noreset
    ret
;-------------------------------------------


;------ intialise values writer -------
initwritervalues
    ld a,12                   ; compteur de lettre a afficher
    ld (nbfontdisplayed), a   ; et on le stocke
    ld a, (nblinedisplayed)   ; recuperation du compteur de ligne
    inc a
    cp 5
    jr nz, noresetline        ; est on arrive au 4 lignes ?
    ld hl,#C000+kw_offset               ; adresse ecran ou afficher la fonte
    xor a                     ; si oui alors on reset sa valeur
    ld (nblinedisplayed),a
    ld (fontscreenaddress),hl

    ld a,(showlogo+1)
    inc a
    cp 12
    jr nz,.norazcnt
    xor a
.norazcnt
    ld (showlogo+1),a
    ret

noresetline
    ld (nblinedisplayed),a    ; on sauve en memoire la valeur du nombre de ligne
    ld de,#30+#30+#90     ; on ajoute a de la valeur d'un retour a la ligne
    ld hl,(fontscreenaddress)
    add hl, de
    ld (fontscreenaddress),hl ; stocke en memoire l'adresse ecran
ret

;----- recherche de la bonne lettre dans la fonte -----------------------
; hl adresse du texte
findchar
    ld a,(ix+0)
    sub 65            ; soustrait la premiere lettre ascii de la fonte A
    ld l,a
    ld h,0
    add hl,hl ;*2     ;
    add hl,hl ;*4
    add hl,hl ;*8
    add hl,hl ;*16
    add hl,hl ;*32
    add hl,hl ;*64
    ld de,font        ; recuperation du pointeur de la font
    add hl,de         ; hl pointe sur la bonne lettre dans la fonte
    ret
;----------------------------------------------

;---- display a font on screen --------------------------------------
displayfont
    ;ld hl,letteracol             ; adresse de la fonte
    ;ld de,#C000            ; adresse ecran
    ld c,4                 ; nombre bit de large de la fonte
loopcolumnfont
    push de                ; sauvegarde de l'adresse ecran
    ;push af                ; sauvegarde du compteur a
    ld b,16		       ; b contient le nombre de lignes de la fonte
looprowfont
    ld a,(hl)              ; recupere octet de la fonte
font_mask:
    and #ff

    ld (de),a              ; poke l'octer de la fonte
    inc hl ;: push hl       ; on incremente dans la fonte
    push bc                ; stockage du compteur b altere par bc26
    ex hl,de               ;
    call bc26              ; recuperation de l'adresse de la ligne suivante
    ex hl,de               ; adresse ligne suivante de hl -> de
    ld a,(font_mask+1)
    rrca
    ld (font_mask+1),a
    pop bc                 ; recuperation du compteur b
    djnz looprowfont       ; tant que b > 0 on boucle
    ;pop af                 ; recuperation du compteur a
    pop de                 ; recuperation de l'adresse ecran precedente
    inc de
    dec c                  ; decremente compteur large
    jr nz, loopcolumnfont
    ret

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
    ;ld b,#f5 ; attente vbl

showlogo:
    ld a,0
    cp 11
    jr z,.clearlogo
    cp 1
    jr nz,.skiplogo
.drawlogo
    ld a,#ff
    ld (show_prog_logo.showlogo_mask+1),a
    jr .logo
.clearlogo
    xor a
    ld (show_prog_logo.showlogo_mask+1),a

.logo
    push de
    call show_prog_logo
    pop de
.skiplogo

draw_lines:
    ld a,0
    or a
    ;and 1
    jr z,skip_drawlines
    bit 0,a
    jr z,.skip2
    ; Permutations d'adresses?
    ;Dessin des lignes
    ; A rendre progressif?
.adr1
    ld hl,#c000
    ld (hl),#ff
    inc hl
    ld (.adr1+1),hl
.adr2
    ld hl,#c000 + 96*17+#3000+95
    ld (hl),#ff
    dec hl
    ld (.adr2+1),hl
.skip2
    ;ld a,(draw_lines+1)
    dec a
    ld (draw_lines+1),a

skip_drawlines:


    ld b,#f5 ; attente vbl



nvbl
    in a,(c)
    rra
    jr c,nvbl
    ret
;---------------------------

sizescreen
    ld bc, #bc00+2
    out (c),c
    ld bc, #bd00+50
    out (c),c
    ld bc, #bc00+1
    out (c),c
    ld bc, #bd00+(linewidth>>1)      ; on met la valeur 48 au lieu de 40 dans le registre 1
    out (c),c
    ld bc,#bc00+6
    out (c),c
    ld bc,#bd00+21
    out (c),c
    ld bc, #bc00+7
    out (c),c
    ld bc,#bd00+#1e
    out (c),c
    ret

;--- application palette hardware -------------
setpalette
    ld b,#7F          ; gatearray pointer to ink 0
    xor a                ; ink number start with 0
.loop
    ld e,(hl)
    out (c),a            ; on selectionne la couleur
    out (c),e            ; on envoie la couleur
    inc hl
    inc a
    cp 4
    jr nz,.loop
    ld c,#10          ; meme chose pour le border avec la couleur 0
    out (c),c
    ld e,(hl)
    out (c),e
    ret
;----------------------------------------------

show_prog_logo:
    ld hl,0
    ld b,16
    ; Parcours aléatoire de la table
.perm
    push bc
    ld a,r
    ;ld a,1
    ld e,a
    ld d,0
    add hl,de
    ; comparaison avec le max de la table
    ld de,(numpix_logo_table)
    ;dec de
    ld bc,hl

    ; Comparer hl et de
    ;
    or a    ; clear flag
    sbc hl,de ; hl = hl - de - 0
    jp p,.noovf ; si >0 garde hl-de
    ld hl,bc    ; si <0 garde hl

    ;ld a,h
    ;cp d
    ;jp nc,.noovf
    ;ld a,l
    ;cp e
    ;jp nc,.noovf
    ;sbc hl,de

.noovf:
    push hl
    add hl,hl ; *4
    add hl,hl
    ld de,logo_table
    add hl,de
    ; Récupere l'adresse du logo
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ; La destination
    ld c,(hl)
    inc hl
    ld b,(hl)

;   ld a,r
;    and #0f
;    ld h,a
;    rlca
;    rlca
;    rlca
;    rlca
;    or h
;    or #f0
;    ld h,a

    ld a,(de)
    ; Masquage
 .showlogo_mask:
    and #ff
 ;   ld a,(de)
 ;   and h
     ld (bc),a
    pop hl

    pop bc
    ld (show_prog_logo+1),hl

    djnz .perm


    ret

;---- recuperation de l'adresse de la ligne en dessous ------------
; CE: hl
; CS: hl contient l'adresse de la ligne suivante
;     A,F modifiés
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
    add  linewidth ; #60      ; 96 chars
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

;------- data ---------------------------
; Palettes 0 1 2 3 B
palette0    db #54, #54, #54, #54,#54       ; All black
hardpalette db #54, #44, #5c, #40,#54       ;

texte
    db 'THIS IS NOT '
    db 'A DEMO JUST '
    db 'A SHOWCASE  '
    db 'FOR MARTINE '
    db 'SOFTWARE    '

    db 'THIS SHORT  '
    db 'ANIMATION   '
    db 'IS A TRIBUTE'
    db 'TO KRAFTWERK'
    db '            '

    db 'FATHERS     '
    db 'OF          '
    db 'ELECTRONIC  '
    db 'MUSIC       '
    db '            '

    db 'ANIMATION   '
    db 'FRAMES WERE '
    db 'TRANSFERED  '
    db 'TO CPC USING'
    db 'MARTINE     '

    db 'MARTINE     '
    db 'AND         '
    db 'ASSEMBLY    '
    db 'CODE BY     '
    db 'SID AND SIKO'

    db 'THIS PROJECT'
    db 'WAS         '
    db 'ASSEMBLED   '
    db 'USING RASM  '
    db 'BY ROUDOUDOU'

    db 'MUSIC BY    '
    db 'SIKO        '
    db '            '
    db 'ARKOS PLAYER'
    db 'BY TARGHAN  '

    db 'ALL CODE AND'
    db 'DATA FILES  '
    db 'AVAILABLE   '
    db 'ON MARTINE S'
    db 'GITHUB      '


    db 'STAY TUNED  '
    db 'FOR THE NEXT'
    db 'IMPACT      '
    db 'PRODUCTION  '
    db '            '

    db 'GET MARTINE '
    db 'TOOL ON     '
    db 'GITHUB      '
    db 'WEBSITE     '
    db '            '

    db 'OOO OOO OOO '
    db 'O    O  O  O'
    db 'OOO  O  O  O'
    db '  O  O  O  O'
    db 'OOO OOO OOO '

    db '            '
    db '            '
    db '            '
    db '            '
    db '            ',0

include "font1.asm"
;---------------

Music:
    ;include "autobahn-siko_playerconfig.asm"
    include "autobahn-siko.asm"

Player:
    include "PlayerAkg.asm"

include 'data.asm'

table_delta:
    dw delta00
    dw delta01
    dw delta02
    dw delta03
    dw delta04
    dw delta05
    dw delta06
    dw delta07
    dw delta08
    dw delta09
    dw delta10
    dw delta11
    dw delta12
    dw delta13
    dw delta14
    dw delta15

url_logo:
    include "url_data.asm"
;    incbin "urlbaspx.win"


ligne:
    ds 96,#ff

numpix_logo_table:
    dw 0

; Reserver 96*16*4 octets (mais moins seront necessaires)
logo_table:


end

ifndef EXPORT_SNA
    save'disc.bin',loadingaddress,end-start,DSK,'autobahn-martine-showcase.dsk'
endif

