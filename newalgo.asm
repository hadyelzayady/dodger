include cls.inc
include newline.inc
include movecur.inc
include dispMsg.inc
include delchar.inc
include drawchar.inc
.model huge
.stack 64
.data
 cursor_Top_Position dw  0000h 
    Cursor_Bottom_Position dw 0D00h 
    Character_Sent db 'S' 
 	Character_Received db 'R'
 	vdivider db '||$'
 	hdivider db '-$'
 	char_scancode db 'S'
 	enterName db 'Please, enter your name: $'
 	continue db 'Please, Enter key to continue$'
 	playername LABEL BYTE
 	maxSize db 11
 	ActualSize db ?
 	nameofplayer db 11 dup("$")
 	nameofplayer2 db 11 dup("$")
	ActualSize2 db ?
 	option1 db '#To start chatting press F1$'
 	option2 db '#To start our game press F2$'
 	option3 db '#To end the program press ESC$'
 	diffmsg db '#Choose Difficulty press any number from (1-3)$'
	easymsg db ' 1- Easy$'
	mediummsg db ' 2- Medium (recommended)$'
	hardmsg db ' 3- Hard$'
 	msgg db 'Congratulaions$'
 	player1XY dw 0f45h
 	player1   db  0b0h
 	player2XY dw  0f05h
 	player2   db  0b0h
 	bullet1 db 11h
 	bullet2 db 10h
 	bullet1XY dw 0ffffh,0ffffh,0ffffh,0ffffh,0ffffh
 	bullet2XY dw 0ffffh,0ffffh,0ffffh,0ffffh,0ffffh
 	health1 db 0Ah
 	health2 db 0Ah
 	heart db 10 dup(03h),"$"
 	heartXY dw 1143h
 	heart2XY dw 111eh
 	shootedcolor1 db 03h
 	shootedcolor2 db 06h
	;;;;;;;BLOCK
	obstacley db 0ffh
	Xmin1 db 00
	Xmax1 db 00
	Xmin2 db 00
	Xmax2 db 00
	numofchars db 00
	spacesize db 9
	speedcounter dw 0 ;to draw on star
	speed dw 09h
	bulletsSpeed dw 50
	mypos db 0; 0 -> right player
	pausedmsg db "paused$"
	msg1 db 'Winner is$'
	gameinvitation db '- you have sent a game invitation to $'
	chatinvitation db '- you have sent a chat invitation to $'
	game_rec_inv db ' have sent you a game invitaion, to accept press F2$'
	chat_rec_inv db ' have sent you a chat invitaion, to accept press F1$'
	sender db 1
	temp db 0
	pauseflag db 0
	ingame db 0
	mkan dw 1400h
	mkan2 dw 1800h
	sentagame db 0
	verifieChar db 0
.code 
;////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
flushkeyQ proc
push ax
mov ah,0ch
mov al,0
int 21h
pop ax

ret
flushkeyQ endp
sound proc
		MOV     DX,2000          ; Numbe of times to repeat whole routine.

		MOV     BX,1             ; Frequency value.

		MOV     AL, 10110110B    ; The Magic Number (use this binary number only)
		OUT     43H, AL          ; Send it to the initializing port 43H Timer 2.		

		NEXT_FREQUENCY:          ; This is were we will jump back to 2000 times.		

		MOV     AX, BX           ; Move our Frequency value into AX.		

		OUT     42H, AL          ; Send LSB to port 42H.
		MOV     AL, AH           ; Move MSB into AL  
		OUT     42H, AL          ; Send MSB to port 42H.		

		IN      AL, 61H          ; Get current value of port 61H.
		OR      AL, 00000011B    ; OR AL to this value, forcing first two bits high.
		OUT     61H, AL          ; Copy it to port 61H of the PPI Chip
                         ; to turn ON the speaker.

	   ;MOV     CX, 100          ; Repeat loop 100 times
	   ;DELAY_LOOP:              ; Here is where we loop back too.
	   ;LOOP    DELAY_LOOP       ; Jump repeatedly to DELAY_LOOP until CX = 0  
	   	 

	   INC     BX               ; Incrementing the value of BX lowers                          ; whole routine  	 

	   DEC     DX               ; Decrement repeat routine count  	 

	   CMP     DX, 0            ; Is DX (repeat count) = to 0
	   JNZ     NEXT_FREQUENCY   ; If not jump to NEXT_FREQUENCY  	                         ; Else DX = 0 time to turn speaker OFF  	 

	   IN      AL,61H           ; Get current value of port 61H.
	   AND     AL,11111100B     ; AND AL to this value, forcing first two bits low.
	   OUT     61H,AL           ; Copy it to port 61H of the PPI Chip                         ret
	   ret
sound endp

Initialize_Serial_Port proc 
     ;for baud rate
  	 mov al , 10000000b ;1 Divisor Latch Access Bit 
  	 mov dx , 3fbh
  	 out dx , al  ;3ayz a set el baud rate   
	
  	 mov al , 01h
  	 mov dx , 3f8h    ;lSB "12=C -> BAUDRATE=  9600   "
  	 out dx , al   
	
  	 mov al , 00h
  	 mov dx , 3f9h    ;MSB
  	 out dx,al   
	
  	 mov al , 00010011b   ;AWEL BIT MN 3LSHMAL LAZM B ZERO
  	 mov dx , 3fbh        ;3ASHAN 2A2OLO MSH 3AYZ A8YR FE EL BAUD RATE
  	 out dx , al          ;NO PARITY
  	                      ;NUMBER OF STOP BITS=2
  	 ret                  ; WORD LEGNTH 8 BITS
Initialize_Serial_Port endp

Send_Recieve proc
	
 lable:	
 call far ptr Get_KeyBoard_Buffer    ;3ASHAN N3ML CHECK
 		                            ;HOA KTB WLA L2
 		
 		cmp al,0h
 			
 		JZ RECIEVE         ;LW HOA MGALOSH HAGA 
 			               ;HYROO7 L SEND 
 		
 		Send: 

 			mov dx,3fdh
 			in al,dx
 			test al,00100000b  ;GAHZ ANK TB3T 
 			                   ;Empty Transmitter Holding Register
 			Jz Send 
 			mov al,Character_Sent
 			
 			cmp al,27d    ;BY3ML CHECK LW DAS ESCAPE
 			jz exitsr
 			;cmp al,0dh  ;enter
 			mov dx,3f8h   ;CONTROL REGISTER
 			out dx,al     ;OUT -> 2ARET MN COM1
 		    ;cmp al,08h
 			;je backspace
 			go_on:
 			call far ptr Print_Top_Half;BYTB3 FE COM1
 		 

 		RECIEVE:
 			mov dx,3fdh ;STATUS REGISTER
 			in al,dx
 			test al,1 ;DATA READY CHECK
 			jz lable  ;LESA MSH READY LW RA7 LE LABLE  
 			
 			mov dx,3f8h
 			in al,dx
 		 ; cmp al,8
 		  ;je backspace	
 			;go_on2:
 			mov Character_Received,al
 			
 			call far ptr Print_Bottom_Half
 			
 			jmp lable 

 		backspace:
 		 mov Character_Sent,8
 		jmp go_on
 		exitsr: ;LW DAS ESCAPE
 		RET	 
Send_Recieve endp

Get_KeyBoard_Buffer proc ; return the character in AL , ZF = 0 ; if no character return al = 0 
	
	mov ah , 1 
	int 16h
	 
	jz Empty 
	
	mov ah,0  ;LW KTB HYSAVE FE EL AL
	int 16h
	mov Character_Sent, al
	jmp Exit42
	
	
	Empty : mov al , 0 	
	
	
	Exit42:
	ret 
Get_KeyBoard_Buffer endp 

Print_Top_Half proc

	mov bx , 1 
	mov ah , 02
	mov dx , cursor_Top_Position 
	int 10h 
	cmp character_sent,0dh
		je entere
	cmp character_sent,08h
		je backspace2
	cmp dl , 79 
	jz new_line
	inc dl 
	jmp Exit1
	new_line : 
		cmp dh , 10
		ja Cursor_Last_Row
		inc dh
		jmp DontScroll 
	Cursor_Last_Row :
		mov bx , 0
		;call Scroll_Top
		DontScroll:
		mov dl , 00 
	Exit1 :   
		mov cursor_Top_Position , dx 
		kml:mov dl,Character_Sent
		int 21h 
		
		cmp bx,1 
		jz out1  
		call far ptr Scroll_Top

		jmp out1
		backspace2:
		mov ah,3h
         mov bh,0h
         int 10h
         cmp dl,0
         je backline
         sub dl,1
         k2:
         mov ah,2
         int 10h
         mov cursor_Top_Position,dx
         mov Character_Sent,20h
         jmp kml
         backline:
         cmp dh,0
         je out11
         mov dl,79
         dec dh
         jmp k2
         entere:
         mov dl,0 
         inc dh 
         mov cursor_Top_Position,dx
         cmp dh,11
         ja sc
         jmp out1
         out11:
         mov dl,0 
         mov dh,0
         mov cursor_Top_Position,dx
         jmp out1
         sc:
         call far ptr Scroll_Top
         dec dh
         mov cursor_Top_Position,dx

         out1 : 
         mov ah,2
		mov dx,cursor_Top_Position
		int 10h
		ret 
Print_Top_Half endp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Print_Bottom_Half proc 
	
	mov bx , 1 
	mov ah , 02
	mov dx , cursor_Bottom_Position 
	int 10h 
	cmp Character_Received,0dh
		je enteree
	cmp Character_Received,08h
		je backspace22
	cmp dl , 79  ;A5R EL SHASHA
	jz new_line2
	
	inc dl ;YA3NE HYZWD EL X  
	
	jmp Exit2
	
	new_line2 : 
		cmp dh , 23 ;A5R EL SHASHA THT
		jz Cursor_Last_Row2
		inc dh
		jmp DontScroll2 
	
	Cursor_Last_Row2 :
		mov bx , 0 ; BX=0 YB2A ANA WESLT L A5R EL SHASHA
		
	DontScroll2:
		mov dl , 00 ;HYBD2 STR GDEED 
	
	Exit2 :   
		
		mov ah , 02 
		mov cursor_Bottom_Position , dx ;BY3ML SAVE LE  
		
		kmll:

		mov dl , Character_Received     ;EL CURSOR 
		int 21h                       ;BYTB3 EL CHAR
		
		cmp bx , 1  ;LW BX=1 YB2A LESA MSH A5R EL SHASHA
		jz out2  
		
		call far ptr Scroll_Bottom   ;LW BX=0 YA3NE 3AYZ A3ML SCROLL
		  jmp out2
		backspace22:
		mov ah,3h
         mov bh,0h
         int 10h
         cmp dl,0
         je backlinee
         sub dl,1
         k22:
         mov ah,2
         int 10h
         mov cursor_Bottom_Position,dx
         mov Character_Received,20h
         jmp kmll
         backlinee:
         cmp dh,0dh
         je out111
         mov dl,79
         dec dh
         jmp k22
         enteree:
         mov dl,0 
         inc dh 
         mov cursor_Bottom_Position,dx
         cmp dh,23
         ja scc
         jmp out2
         out111:
         mov dl,0 
         mov dh,0dh
         mov cursor_Bottom_Position,dx
         jmp out2
         scc:
         call far ptr Scroll_Bottom
         dec dh
         mov cursor_Bottom_Position,dx
         		  
		  out2 : 
		    mov ah,2
           mov dx,cursor_Bottom_Position
           int 10h
	       ret 
Print_Bottom_Half endp 

Scroll_Top proc 
	push ax 
	push bx 
	push cx 
	push dx 
	push ds 
	push es 
	cld 
	mov ax ,0b800h  ;video memory 
	mov ES , ax 
	mov ds , ax 
	mov di ,0 
	mov Si , 160 
	mov Cx , 160 *12
	rep movsb 
	mov di , 11 *160 
	mov ax ,0720h 
	mov cx , 80
	Rep stosw 
	pop es 
	pop ds 
	pop  dx
	pop  cx 
	pop  bx 
	pop  ax
 ret
Scroll_Top endp

Scroll_Bottom proc 
	push ax 
	push bx 
	push cx 
	push dx 
	push ds 
	push es 
	cld 
	mov ax ,0b800h 
	mov ES , ax 
	mov ds , ax 
	mov di ,13 *160 
	mov Si , 14*160 
	mov Cx , 160 *12 
	rep movsb 
	mov di , 23 *160 
	mov ax ,7020h
	mov cx , 80
	Rep stosw 
	pop es 
	pop ds 
	pop  dx
	pop  cx 
	pop  bx 
	pop  ax
 ret 
Scroll_Bottom endp

Clear_Screen proc
	push es
	mov ax,0b800h
	mov es,ax
	mov di,0
	mov ax,0720h
	cld
	mov cx,12*80
	rep stosw
	mov ax,7020h
	mov cx,12*80
	rep stosw
	pop es
	mov ah,06h
	ret
Clear_Screen endp

Initialize_MiddleLine proc
	push ax
	push es
	push di
	push cx
	mov ax,0b800h
	mov es,ax
	mov di,12*160
	mov ax, 072Dh
	mov cx,80
	rep stosw
	pop cx
	pop di
	pop es
	pop ax
	ret 
Initialize_MiddleLine endp

;////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
chatting proc ;far

     movecur 0000h
	 call Clear_Screen
	 call Initialize_MiddleLine
	 ;call Initialize_Serial_Port
	 call Send_Recieve
	cls
	call far ptr options
	ret
chatting endp
	; 1right ,2 left
	;***exit
	exit proc 
	cls
	mov ah,04ch
	mov al,0
	int 21h
	ret
exit endp
initialzation_ofgame proc      ;inorder to start game over and over
mov bullet2XY[0],0ffffh
mov bullet2XY[2],0ffffh
mov bullet2XY[4],0ffffh
mov bullet2XY[6],0ffffh
mov bullet2XY[8],0ffffh
;
mov bullet1XY[0],0ffffh
mov bullet1XY[2],0ffffh
mov bullet1XY[4],0ffffh
mov bullet1XY[6],0ffffh
mov bullet1XY[8],0ffffh



mov obstacley,0ffh
mov speedcounter,0 
mov sender,1
mov verifieChar,0

mov sentagame,0
mov heartXY ,1143h
mov heart2XY ,111eh
mov xmin1 ,0
mov xmax1 ,0
mov xmin2 ,0
mov xmax2 ,0
;------
;---------------------
mov player1XY ,0f45h
mov player1 ,178
mov player2XY,0f05h
mov player2 ,178


mov bullet1 ,0011h
mov bullet2,0010h
mov health1 ,0Ah
mov health2 ,0Ah
mov si,0
heartloop:
mov heart[si],03h
inc si
cmp si,10
jne heartloop
mov heart[si],'$'
mov shootedcolor1 , 03h
mov shootedcolor2 , 06h
mov	mkan , 1400h
mov	mkan2 ,1800h
ret
initialzation_ofgame endp
;((((((((((((( End game )))))))))))))
endgame proc 
	 
cls

	movecur 0915h
	dispMsg msgg 
	cmp health1,0    ; elplayer elfat7ha hwa emat
	jz check_sender
	jnz check_sender_2






	check_sender:
	cmp sender,1
	jnz print_other
	movecur 0D15h
	dispMsg nameofplayer
	jmp delayend2




	print_other:
	movecur 0D15h
	dispMsg nameofplayer2
	jmp delayend2

	check_sender_2:
	cmp sender,1
	jnz print_other2
	movecur 0D15h
	dispMsg nameofplayer2
	jmp delayend2




	print_other2:
	movecur 0D15h
	dispMsg nameofplayer
	jmp delayend2
;	jz dispplayer12
;	jnz dispplayer22
;	dispplayer12:
;	cmp sender,1
;	jz printrec
;	movecur 0D15h
;	dispMsg nameofplayer2
;	jmp delayend2
;	
;
;	printrec:
;	dispplayer22:
;
;	movecur 0D15h
;	dispMsg nameofplayer

	delayend2:
	MOV     Cx, 2fh
	MOV     DX, 0D050H
	MOV     AH, 86H
	INT     15H

	pop ax
jmp initize    ;lma 3mlna jump init zabtt
endgame endp
;**************************
;------------------------------------------------------
checkhittblock proc
	;;;;if hitted by block
	;(((((((((((((((            PLAYER 1       RIGHT              )))))))))))))))
	mov dx,player1XY
	cmp dh,obstacley 
	jz sameYblock
	jmp hitplayer2
	sameYblock:
	inc dl;xmin is not counted from zero so instead of decrement it we inc dl  
	cmp dl,Xmin1
	jbe collidblock
	dec dl
	cmp dl,Xmax1
	jae collidblock
	jmp hitplayer2
	collidblock:
	mov al,0001h
	call far ptr wounded
	mov shootedcolor1,04h
	;call far ptr drawplayer1
	;;;;;;;;;;;;;;;;;;;;;
	;(((((((((((((((((((      PLAYER 2             LEFT              )))))))))))))))))))
	hitplayer2:
	mov dx,player2XY
	cmp dh,obstacley 
	jz sameYblock2
	ret
	sameYblock2:
	inc dl
	cmp dl,Xmin2
	jbe collidblock2
	dec dl
	cmp dl,Xmax2
	jae collidblock2
	ret
	collidblock2:
	mov al,0002h
	call far ptr wounded
	mov shootedcolor2,04h
	;call far ptr drawplayer1	

	ret

checkhittblock endp
;***************************************************
change_time proc
	push ax
	push bx
	push cx
	push dx
	mov ah,2ch               ;read system time
	int 21h                ;ch hh ,cl mm ,dh ss
	mov ah,0
	mov al,dh
	mov bl,26h
	sub bl,spacesize
	div bl
	mov Xmin2,ah ;;of left size
	add ah,spacesize ;second part
	mov Xmax2,ah
	;;;;;; right side
	mov ah,Xmin2
	add ah,29h
	mov Xmin1,ah
	add ah,spacesize;second part
	mov Xmax1,ah
	;numofchars
	mov dl,27h
	sub dl,Xmax2
	mov numofchars,dl
	pop dx
	pop cx
	pop bx
	pop ax   
	ret    
change_time endp 


delobstacles proc
	push ax
	push bx
	push cx
	push dx
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RIGHT side
	mov dh,obstacley
	mov dl,29h
	movecur dx
	drawchar 20h,0fh,Xmin2; Xmin2 is the same as numofchars in first part
	;second part
	mov dl,Xmax1
	movecur dx
	drawchar 20h,0fh,numofchars
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;///////////////////////////; LEFT side
	;first part
	mov dl,00h
	movecur dx
	drawchar 20h,0fh,Xmin2
	;second part
	mov dl,Xmax2
	movecur dx
	drawchar 20h,0fh,numofchars	

	pop dx
	pop cx
	pop bx
	pop ax    
	 ret 
delobstacles endp

drawobstacles proc 

	cmp speedcounter,0
	jnz returnobs	

	call delobstacles
	inc obstacley
	cmp obstacley,10h
	jz changespace
	jmp startdraw
	changespace:
	dec obstacley
	call checkhittblock ;check hitt before changing
	cmp sender,0
	jz slaveobs
	call change_time
	call far ptr sendXmin2
	jmp redraw
	slaveobs:
	call far ptr receiveXmin2
	redraw:
	mov obstacley,0ffh
	mov speedcounter,1
	returnobs:
	ret
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;sync obstacles
	;call syncObs
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;////////////////////////RIGHT side
	startdraw:
	dec obstacley
	call checkhittblock
	inc obstacley
	;;first part
	mov dh,obstacley
	mov dl,29h;first x in right side
	movecur dx
	drawchar 178,0fh,Xmin2
	;;second part
	mov dl,Xmax1
	mov dh,obstacley
	;draw second part
	movecur dx
	drawchar 178,0fh,numofchars

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;))))))))))))))))))))))LEFT SIDE
	mov dh,obstacley
	mov dl,00
	movecur dx
	drawchar 178,0fh,Xmin2
 	;second part
 	;get x of second part
	mov dh,obstacley
	mov dl,Xmax2
	movecur dx
	;draw second part
	drawchar 178,0fh,numofchars
	mov ax,speed
	mov speedcounter ,ax; determine speed her
	ret

drawobstacles endp
;------------------------------------------------------start proc
start proc
	cls
	;move cursor to the middle 
	movecur 0915h
	;disp entername msg
	lea dx,enterName
	mov ah,9
	int 21h
	newline
	;disp second msg
	movecur 0d15h
	dispMsg continue
	noname:
	;movecursor under msg
	movecur 0b18h
	;enter name
	mov ah,0Ah
	lea dx,playername
	int 21h
	cmp ActualSize,0
	jz noname
	cmp nameofplayer[0],32
	jz noname
mov si,0h
mov bx,0h      ; en hwa byb3at fel ghaz el fata7 fela5r byb3at elesm sa7 enma
Sendactualsize:
            
			mov dx,3fdh
			in al,dx
			test al,00100000b  ;GAHZ ANK TB3T 
			                   ;Empty Transmitter Holding Register
			Jz Sendactualsize 
			mov al,ActualSize
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1
RECIEVEactualsize:
			mov dx,3fdh ;STATUS REGISTER
			in al,dx
			test al,1
			jz RECIEVEactualsize

			mov dx,3f8h
			in al,dx
			mov ActualSize2,al

mov ch,0h
mov cl,ActualSize2
cmp cl,ActualSize   ;bshuf anhy elmax 3shan eletnen ykmlo ellop we ysendo 3ady
jg readothername
mov cl,ActualSize

readothername:
 ; bec of differen pcs and different ActualSize2

Sendname:
            
			mov dx,3fdh
			in al,dx
			test al,00100000b  ;GAHZ ANK TB3T 
			                   ;Empty Transmitter Holding Register
			Jz Sendname 
			mov al,nameofplayer[si]
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1
			inc si
RECIEVEname:
			mov dx,3fdh ;STATUS REGISTER
			in al,dx
			test al,1
			jz RECIEVEname   ; mfesh data waslalo

			mov dx,3f8h
			in al,dx
			mov nameofplayer2[bx],al
			inc bx                    ; mushkila en f wa7d by5ls abl ltany
            LOOP readothername       ; lw 5ltha 3al actual size btboz
			
			mov bx,0
			mov bl,ActualSize2
			mov nameofplayer2[bx],'$'

			ret                     ; wekda feha azma f wa7d weltany la
	ret
start endp
;;------------------------------------------------
;***************************************************options proc
options proc
begining:
	cls
	;disp options
	movecur 0915h
	dispMsg option1
	movecur 0b15h
	dispMsg option2
	movecur 0d15h
	dispMsg option3
	movecur 0f15h
	mov dx,1600h
call far ptr hdrawdivider

movecur 0f15h
;select option
mov character_sent,0   ;if they finish the game then send a game inv again it will not gofor sure but wait till the other acepts
mov char_scancode,0
enterOption:

;---------------------communicate here ---------------------------


    mov ah , 1 
	int 16h
	

	
	JZ RECIEVEinv 
	mov ah,0  ;LW KTB HYSAVE FE EL AL
	int 16h
	mov character_sent,al
	mov char_scancode,ah
		mov al,char_scancode
		cmp al,0h
			
		        ;LW HOA MGALOSH HAGA 
			               ;HYROO7 L SEND 
		
Sendinv:

            mov dx,3fdh
			in al,dx
			test al,00100000b  ;GAHZ ANK TB3T 
			                   ;Empty Transmitter Holding Register
			Jz RECIEVEinv

			mov al,char_scancode
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1
            
            cmp al,3Bh ;F1
            je chat22

            cmp al,3Ch ;F2
            jz gamestart

            cmp character_sent,27d ;ESC
            jz exitlabel
            

            mov ah,2          
            mov dl,7
            jmp RECIEVEinv

            chat22:
            dispMsg nameofplayer
            call far ptr sendchatinv
            jmp RECIEVEinv
            gamestart:
            call far ptr  sendgameinv
            


             
RECIEVEinv:        ;if nothing rec then i will send an invit
			mov dx,3fdh ;STATUS REGISTER
			in al,dx
			test al,1
			jz enterOption   ; mfesh data waslalo

			mov dx,3f8h
			in al,dx
			  
			cmp al,3Bh ;F1
            je chat22rec

            cmp al,3Ch ;F2
            jz gamestartrec

            cmp al,27d ;ESC
            jz exitlabel  
            jmp enterOption

gamestartrec:
;dispMsg nameofplayer
cmp Character_Sent,3Ch
jz returntogame                 ; ne need for checking sender as it will be 1 for the sender and 0 for the accepter
call far ptr recgameinv
call flushkeyQ
jmp returntogame
chat22rec:             ;mushkila lma ba5ush chatw e a5rog menu lgame 
call far ptr recchatinv
call flushkeyQ
jmp begining
             

            exitlabel:
            mov al,character_sent

            mov dx,3f8h   ;CONTROL REGISTER
			out dx,al 
            call far ptr exit
			;else invalid input,make bell sound
returntogame:
ret
options endp
;***************************************************
sendchatinv proc
mov ax,0600h           ;to clear this line 
mov bh,07
mov cx,1700h
mov dx,174FH
int 10h
movecur 1700h
dispMsg chatinvitation
dispMsg nameofplayer2
mov character_sent,3Bh              
ret
sendchatinv endp
;---------------------
;----------------recieve chat inv ---------------
recchatinv proc
call flushkeyQ
push ax
push bx
push cx
push dx
mov ax,0600h           ;to clear this line 
mov bh,07
mov cx,1800h
mov dx,184FH
int 10h
movecur 1800h 
dispMsg nameofplayer2   ;bec now player two has send me an inv from my prespective
dispMsg chat_rec_inv
cmp character_sent,3Bh     ;THIS MEANS THAT I HAVE SENT A CHAT INV BEFORE
jz immedchat
mov ah,0
int 16h
mov al,ah
mov char_scancode,ah
cmp al,3Bh ;F1
jne dontchat

acceptchat:
          
            
			mov dx,3fdh
			in al,dx
			test al,00100000b  ;GAHZ ANK TB3T 
			                   ;Empty Transmitter Holding Register
			Jz acceptchat 
			mov al,3Bh
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1
pop dx
pop cx
pop bx
pop ax
call flushkeyQ
immedchat:
MOV Character_Sent,'s'
call far ptr chatting
ret
dontchat: 
;jmp sendinv         
ret
recchatinv endp
;--------------
;--------------------game inv---------------
sendgameinv proc
mov ax,0600h           ;to clear this line 
mov bh,07
mov cx,1700h
mov dx,174FH
int 10h
movecur 1700h
dispMsg gameinvitation
dispMsg nameofplayer2
mov sentagame,1
mov Character_Sent,3ch     ; in order to go for the game once the second player accept the invitation

ret
sendgameinv endp
;--------
;---------------receive game inv-----------------
recgameinv proc
call flushkeyQ
push ax
push bx
push cx
push dx
mov ax,0600h           ;to clear this line 
mov bh,07
mov cx,1800h
mov dx,184FH
int 10h
movecur 1800h 
cmp Character_Sent,3ch     ;THIS MEANS THAT I HAVE SENT A CHAT INV BEFORE
jz immedgame
dispMsg nameofplayer2   ;bec now player two has send me an inv from my prespective
dispMsg game_rec_inv
mov ah,0
int 16h
mov al,ah
mov char_scancode,ah
cmp al,3ch ;F2
jne dontgame

acceptgame:
          
            
			mov dx,3fdh
			in al,dx
			test al,00100000b  ;GAHZ ANK TB3T 
			                   ;Empty Transmitter Holding Register
			Jz acceptgame 
			mov al,3ch
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1
pop dx
pop cx
pop bx
pop ax
call flushkeyQ
immedgame:
mov sender,0
MOV Character_Sent,'s'
ret
dontgame:
mov sender ,0 
;jmp sendinv         




ret
recgameinv endp
;''''''''''''''''''''''''''''''''''''''''''''''delete player
delplayer1 proc
	push ax
	push bx
	push cx
	movecur player1XY
	;;;;;;;;;;;;;;;;;;delete players before move
	mov ah,02
	mov dx,20h 
	int 21h          ;
	;
	pop cx
	pop bx
	pop ax   
	ret
delplayer1 endp
;\\\\\\\\\\\\\\del player2
delplayer2 proc
	push ax
	push bx
	movecur player2XY
	;;;;;;;;;;;;;;;;;;delete players before move
	mov ah,02
	mov dx,20h 
	int 21h          ;
	;
	pop bx
	pop ax 	

	ret
delplayer2 endp
;////////////////////////////////////////////////////move player 1
moveplayer1 proc
	mov dx,player1XY
	cmp ah,48h  ;check up
	jz up     
	cmp ah,50h ;check down
	jz down 
	cmp ah,4Dh ;right arrow
	jz right
	cmp ah,4Bh ;left
	jz leftpasser
	call far ptr  shoot1 ;;else if the user starts to shoot(clicked space bar)
	ret
	up:
	;;;;;;check position
	call flushkeyQ
	cmp dh,0
	jz returne21 ;if out of limits
	;;;;;;
	movecur player1XY 
	delchar
	call checkhittblock
	sub player1XY,0100h 
	call far ptr  checkhitt1
	ret
	down:
	call flushkeyQ
	;;;;;;check position
	cmp dh,0fh
	jz returne21 ;if out of limits
	;;;;;;
	movecur player1XY 
	delchar
	call checkhittblock
	add player1XY,0100h
	call far ptr  checkhitt1
	ret
	returne21:
	ret
	leftpasser:
	jmp left
	right:
	;;;;;;check position
	call flushkeyQ
	cmp dl,50h
	jz returne12 ;if out of limits
	;;;;;;
	movecur player1XY 
	delchar
	call checkhittblock
	add player1XY,0001h
	call far ptr  checkhitt1
	ret
	left:
	;;;;;;check position
	call flushkeyQ
	cmp dl,29h
	jz returne12 ;if out of limits
	;;;;;;
	movecur player1XY 
	delchar
	call checkhittblock
	sub player1XY,0001h
	call far ptr  checkhitt1
	returne12:
	ret
moveplayer1 endp
;==============================================================move player2
;=================;==============================================================
moveplayer2 proc
	mov dx,player2XY
	cmp ah,48h  ;check up
	jz up2
	cmp ah,50h ;check down
	jz down2
	cmp ah,4Dh ;right arrow
	jz right2
	cmp ah,4Bh ;left
	jz left2passer
	call far ptr  shoot2 ;;else if the user starts to shoot(clicked space bar or f)
	ret
	;;;;;;;;;;;;;;;;;;;;;;;;;;player 2
	up2:
	;;;;;;check position
	call flushkeyQ
	cmp dh,0
	jz returne2 ;if out of limits
	;;;;;;
	movecur player2XY 
	delchar
	call checkhittblock
	sub player2XY,0100h 
	call far ptr  checkhitt2

	ret
	down2:
	call flushkeyQ
	;;;;;;check position
	cmp dh,0fh
	jz returne2 ;if out of limits
	;;;;;;
	movecur player2XY 
	delchar
	call checkhittblock
	add player2XY,0100h
	call far ptr  checkhitt2
	ret
	returne2:
	ret
	left2passer:
	jmp left2
	right2:
	;;;;;;check position
	call flushkeyQ
	cmp dl,26h
	jz returne ;if out of limits
	;;;;;;
	movecur player2XY 
	delchar
	call checkhittblock
	add player2XY,0001h
	call far ptr  checkhitt2
	ret
	left2:
	;;;;;;check position
	call flushkeyQ
	cmp dl,0
	jz returne ;if out of limits
	;;;;;;
	movecur player2XY 
	delchar
	call checkhittblock
	sub player2XY,0001h
	call far ptr  checkhitt2
	returne:
	ret
;'''''''''''''''''''''''''''''''''''''''''''''
moveplayer2 endp
;;==============================================================mov bullet
shoot1 proc
	;;;;for loop
	mov cx,5;number of iterations
	mov si,0 ;index
	;;;;;;scan code of space bar 3920h 
	cmp ah,39h;space bar 
	jz startshoot1
	call flushkeyQ
;	call pause
	ret
	startshoot1:;flag the bullet that will be shooten ;for player1(space bar);for player2 (f)
	call flushkeyQ
	cmp word ptr bullet1XY[si],0ffffh
	jz shootit1
	add si,2
	loop startshoot1
	ret
	shootit1:
	call far ptr  sound
	mov ax,player1XY
	dec ax
	mov bullet1XY[si],ax
	ret
shoot1 endp
shoot2 proc
	;;;;for loop
	mov cx,5;number of iterations
	mov si,0 ;index
	;;;;;;scan code of space bar 3920h 
	cmp ah,39h;space bar 
	jz startshoot2
;	call pause
	call flushkeyQ
	ret
	startshoot2:
	call flushkeyQ
	cmp word ptr bullet2XY[si],0ffffh
	jz shootit2
	add si,2
	loop startshoot2
	ret
	shootit2:
	call far ptr  sound
	mov ax,player2XY
	inc ax
	mov bullet2XY[si],ax
	ret
shoot2 endp

;/////////////////////////////////////////////////////
;----------------------------------------------------draw player proc
drawplayer1 proc
movecur player1XY
	mov ah,9
	mov al,player1
	mov cx,1
	mov bh,0
	mov bl,shootedcolor1
	int 10h
	mov shootedcolor1,09h
	ret
drawplayer1 endp
;-----------------------------------------------------
drawplayer2 proc
movecur player2XY
	mov ah,9
	mov al,player2
	mov cx,1
	mov bh,0
	mov bl,shootedcolor2
	int 10h
	mov shootedcolor2,03h
	ret
drawplayer2 endp
;((((((((((  CHECK IF BULLET HIT A PLAYER      ))))))))))
checkhitt1 proc
	mov cx,5 ;number of bullets
	mov si,0
	bulletloop:
	mov di,bullet2XY[si]
	cmp player1XY,di;
	jz hitted1
	add si,2
	loop bulletloop
	ret
	hitted1:
	;;change heart color
	mov al,0001h
	call far ptr wounded
	mov bullet2XY[si],0ffffh
	mov shootedcolor1,04h
	ret
checkhitt1 endp
;(((((((((((((((())))))))))))))))
checkhitt2 proc
	mov cx,5 ;number of bullets
	mov si,0
	bulletloop2:
	mov di,bullet1XY[si]
	cmp player2XY,di;
	jz hitted2
	add si,2
	loop bulletloop2
	ret
	hitted2:
	;;change heart color
	mov al,0002h ;wounded checks players with ax
	call far ptr wounded
	mov bullet1XY[si],0ffffh
	mov shootedcolor2,04h
	ret
checkhitt2 endp
;((((((        DRAW HEART       ) )))))
drawheart proc
	mov ah,13h
	mov bp,offset heart
	mov bh,0    
	mov bl,04h
	mov dx,1115h
	mov cx,10
	int 10h
	ret
drawheart endp
;((((((((((   Draw heart 2 ))))))))))
drawheart2 proc
	mov ah,13h
	mov bp,offset heart
	mov bh,0    
	mov bl,04h
	mov dx,113ah
	mov cx,10
	int 10h 
	ret
drawheart2 endp
;/////////NAMES
writenames proc
movecur 1101h
dispMsg nameofplayer
ret
writenames endp
;++++++++++++++++++++++++++++++++++++++++++++++++++++shoot
;///////////////////////////////////////////
wounded proc; player num on ax( ah=0 bullet hit; 1 block hit)
	cmp  al,2
	jz   player2wounded
	player1wounded:
	dec  health1
	cmp health1,0
	jz gameover
	movecur heartXY
	delchar
	dec heartXY
	;dec al; if block then al will be zero to check on loop
	;cmp ah,0;if block hit
	;jz player1wounded
	ret
	player2wounded:
	dec  health2
	cmp health2,0
	jz gameover
	movecur heart2XY
	delchar
	dec heart2XY
	;dec al; if block then al will be zero to check on loop
	;cmp ah,0;if block hit
	;jz player2wounded
	ret
	gameover:
	;movecur 0112h
	;dispMsg pausedmsg
	;		sendend:
	;		mov dx,3fdh
	;		in al,dx
	;		test al,00100000b  ;GAHZ ANK TB3T 
	;		                   ;Empty Transmitter Holding Register
	;		Jz sendend
	;		mov al,'e'
	;		mov dx,3f8h   ;CONTROL REGISTER
	;		out dx,al     ;OUT -> 2ARET MN COM1
	;		receiveendlbl:
	;		mov dx,3fdh ;STATUS REGISTER
	;		in al,dx
	;		test al,1 ;DATA READY CHECK
	;		jz receiveendlbl  ;LESA MSH READY LW RA7 LE LABLE			
	;		mov dx,3f8h
	;		in al,dx
	;		cmp al,'e'
	;		jne sendend
	call endgame
	ret
wounded endp

movebullets proc ;iterate over the bullets and move them
	mov cx,5
	mov si,0
	movebullts:
	cmp bullet1XY[si],0ffffh
	jnz moveit1
	player2_:
	cmp bullet2XY[si],0ffffh
	jnz moveit2passer
	cont:
	add si,2
	loop movebullts
	returnbullet2:
	ret
	;;;;;
	moveit1:
	;check hit
	mov ax,player2XY
	cmp bullet1XY[si],ax
	jz hit2
	;;;
	movecur bullet1XY[si]
	delchar 
	;;;;if bullets reachs the divider
	;cmp byte ptr bullet1XY[si],28h
	;jz drawdividerlbl
	conMove:
	sub bullet1XY[si],0001h
	;drawbullet
	movecur bullet1XY[si]
	drawchar bullet1,0fh,1
	;bullet hit the other player
	mov ax,player2XY
	cmp bullet1XY[si],ax
	jz hit2
	cont2:
	;bullet at the most left
	cmp byte ptr bullet1XY[si],00h
	jz deletebullet1
	;;;
	jmp player2_
	moveit2passer:
	jmp moveit2
	drawdividerlbl:
	call far ptr drawdivider
	jmp conMove
	hit2:
	mov ax,0002h
	call far ptr wounded
	;;;draw blood
	mov shootedcolor2,04h
	jmp deletebullet1	

	deletebullet1:
	movecur bullet1XY[si]
	delchar bullet1
	call far ptr  drawplayer2
	mov bullet1XY[si],0ffffh
	jmp player2_
	;;;;player2
	moveit2:
	movecur bullet2XY[si]
	delchar 
	;cmp byte ptr bullet2XY[si],29h
	;jz drawdividerlbl2
	conMove2:
	;move
	add bullet2XY[si],0001h
	;bullet at the most left
	;drawbullet
	movecur bullet2XY[si]
	drawchar bullet2,0fh,1
	;bullet hit the other player
	mov ax,player1XY
	cmp bullet2XY[si],ax
	jz hit1
	cont3:
	;most right
	cmp byte ptr bullet2XY[si],50h
	jz deletebullet2
	jmp cont
	drawdividerlbl2:
	call far ptr drawdivider
	jmp conMove2
	;;;;;;;jumps
	hit1:
	mov ah,0001h
	call far ptr wounded
	;;;draw blood
	mov shootedcolor1,04h
	;;
	jmp deletebullet2
	deletebullet2:
	movecur bullet2XY[si]
	delchar 
	call far ptr  drawplayer1
	mov bullet2XY[si],0ffffh
	jmp cont	

	returnbullet:
	ret
movebullets endp
;/////////////////////////////////////////////////////draw drawdivider
drawdivider proc
	push si
	push cx
	push dx
	push ax
	mov si,0027h
	movecur si
	mov cx,10h
	lea dx,vdivider
	mov ah,09h
	divloop:
	int 21h
	add si,0100h
	movecur si
	loop divloop
	pop ax
	pop dx
	pop cx
	pop si
	ret
drawdivider endp
;/////////////////////////////////////////////////////
hdrawdivider proc
	mov bl,2
	mov si,1000h
	twolines:
	movecur si
	mov cx,50h
	lea dx,hdivider
	mov ah,09h
	divloop2:
	int 21h
	loop divloop2
	mov si,1200h
	dec bl
	cmp bl,2
	jnz twolines
	ret
hdrawdivider endp
;(((((((((((((((((((((((((((((((((DELTE BULLET)))))))))))))))))))))))))))))))))
hidecursor proc
	mov ah,1
	mov CX,2607h
	int 10h
	ret
hidecursor endp



sendAction proc
	;call far ptr initialzation_ofgame
	sendActionlbl:
	mov cx,ax;temp to save ax value
 	mov dx,3fdh
	in al,dx
	test al,00100000b  ;GAHZ ANK TB3T ;Empty Transmitter Holding Register	               
	Jz sendActionlbl
	mov dx,3f8h   ;CONTROL REGISTER
	mov al,ch    ;send scan code not ascii
	out dx,al     ;OUT -> 2ARET MN COM1; ah to send scan code
	;;;;;;;waiting for verification that action was sent
	notverified:
    mov dx,3fdh ;STATUS REGISTER
	in al,dx
	test al,1
	jz  notverified  ; mfesh data wasla
	mov dx,3f8h
	in al,dx
	mov ah,'a'
	cmp ah,al
	jne sendActionlbl
	ret
sendAction endp

receiveAction proc ;;if no send then no receive 
	receiveActionlbl:
	mov dx,3fdh ;STATUS REGISTER
	in al,dx
	test al,1
	jz  receiveActionlbl
	mov dx,3f8h
	in al,dx
	mov ah,al
	;;; SEND that he received the command (verifie)
	sendcommandlbl:
 	mov dx,3fdh
	in al,dx
	test al,00100000b  ;GAHZ ANK TB3T ;Empty Transmitter Holding Register	               
	Jz sendcommandlbl
	mov dx,3f8h   ;CONTROL REGISTER
	mov al,'a'    
	out dx,al 
	ret
receiveAction endp
receiveXmin2 proc
	receiveXmin2lbl:
	mov dx,3fdh ;STATUS REGISTER
	in al,dx
	test al,1
	jz  receiveXmin2lbl
	mov dx,3f8h
	in al,dx
	mov Xmin2,al
	add al,spacesize ;second part
	mov Xmax2,al
	;;;;;; right side
	mov ah,Xmin2
	add ah,29h
	mov Xmin1,ah
	add ah,spacesize;second part
	mov Xmax1,ah
	;numofchars
	mov dl,27h
	sub dl,Xmax2
	mov numofchars,dl
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;send to boss that he received 
	sendverified:
 	mov dx,3fdh
	in al,dx
	test al,00100000b  ;GAHZ ANK TB3T ;Empty Transmitter Holding Register	               
	Jz sendverified
	mov dx,3f8h   ;CONTROL REGISTER
	mov al,'o'    
	out dx,al 
	ret
receiveXmin2 endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;slave
slave proc

	slavegame:
	;read from keyboard
	mov ah,1
	int 16h
	push ax
	
	call moveplayer2
	call drawplayer2
	call receiveAction
	call moveplayer1
	call drawplayer1
	pop ax	
	call sendAction	
	;call drawdivider	
	

		call drawobstacles

	call movebullets	

	dec speedcounter
	MOV     Cx, 0
	MOV     DX, 04050H
	MOV     AH, 86H
	INT     15H

	jmp slavegame


	ret
slave endp

sendXmin2 proc
	sendcommandlbl23:
 	mov dx,3fdh
	in al,dx
	test al,00100000b  ;GAHZ ANK TB3T ;Empty Transmitter Holding Register	               
	Jz sendcommandlbl23
	mov dx,3f8h   ;CONTROL REGISTER
	mov al,Xmin2   
	out dx,al 
	;;;;;;;wait for verification that slave received Xmin2
	receiveverifi:
	mov dx,3fdh ;STATUS REGISTER
	in al,dx
	test al,1
	jz  receiveverifi
	mov dx,3f8h
	in al,dx
	mov ah,'o'
	cmp ah,al
	jne sendcommandlbl23
	ret
sendXmin2 endp

receiveend proc
			mov dx,3fdh ;STATUS REGISTER
			in al,dx
			test al,1 ;DATA READY CHECK		
			mov dx,3f8h
			in al,dx
			cmp al,'e'
			jz endgamelbl
			ret
			endgamelbl:
			mov al,'e'
			mov dx,3f8h   ;CONTROL REGISTER
			out dx,al     ;OUT -> 2ARET MN COM1

ret
receiveend endp
;===============================================================================================MAIN
main proc far
mov ax,@data
mov ds,ax
mov es,ax
call Initialize_Serial_Port
call start
initize:
;call initialzation_ofgame
call options
call hidecursor
call change_time
cls

	call drawheart
	call drawheart2
	call writenames
	call hdrawdivider
	call drawdivider
	call drawplayer1
	call drawplayer2
	call flushkeyQ

	;
	cmp sender,0
	jne game
	call slave
	
game:
;read from keyboard
mov ah,1
int 16h
push ax

call moveplayer1
call drawplayer1

pop ax
call sendAction
call receiveAction
call moveplayer2  	;execute;

  ;execute
call drawplayer2
call movebullets

;call receiveend
;call drawdivider
;call receiveendgame
call drawobstacles
dec speedcounter ;speed of obstacles
MOV     Cx, 0
MOV     DX, 04050H
MOV     AH, 86H
INT     15H
jmp game
;==============================================================================================main
main endp
end main
