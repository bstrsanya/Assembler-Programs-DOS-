.model tiny
.code
org 100h

Start:
	;mov cx, 0Ch
	;mov bx, 04h
	mov ch, 05h

	call ClearScreen
	call ReadStr
	call StartLocation
	
	call DrawFrame
	call PrintText

	mov ax, 4c00h
	int 21h

;---------------------------------------------
;	Clean console from other commands
;	Entry: 	Noun
;	Exit: 	CL = frame length
;		CH = frame height
;		AH = color attr
;		AL = style frame
;	Destr: 	AX
;---------------------------------------------

ClearScreen	proc
		mov al, 3h
		int 10h
		ret
		endp

;---------------------------------------------
;	Read command str
;	Entry: 	Noun
;	Exit:  	CL = frame length
;		CH = frame height
;		AH = color attr
;		AL = style frame
;	Destr: 	SI, DX, BX
;---------------------------------------------

Readstr	proc
	
	mov si, 80H
	mov cl, es:[si]
	
	mov al, 27H
	mov di, 81H
	repne scasb
	mov bx, di
	
	add cl, 6

	xor dx, dx
	mov si, 82H

	;call Atoi 
	;mov cl, dl

	xor dx, dx

	;call Atoi
	;mov ch, dl

	xor dx, dx

	call Atoi
	mov ah, dl

	xor dx, dx

	call Atoi
	mov [ColorText], dl

	xor dx, dx

	call Atoi
	mov al, dl
	
	;mov bx, si	

	ret
	endp

;---------------------------------------------
;	Func Atoi
;	Entry: SI = addr for read
;	Exit:  DX = read digit
;	Destr: AL, SI
;---------------------------------------------

Atoi	proc

	lodsb               
	BEGIN:  cmp al, 0Dh
		je FINISH   
		cmp al, 39H     
		jle DIGIT

		HEX:       
			sub al, 37H        
			jmp NUMBER

		DIGIT:
			sub al, 30H
			jmp NUMBER

		NUMBER:
			shl dx, 4        
			add dx, ax       
			lodsb            
			
			cmp al, 20H      
			jne BEGIN
	FINISH:

	ret	
	endp
	
;---------------------------------------------
;	Program execution delay
;	Entry: Noun
;	Exit:  Noun
;	Destr: AH, CX, DX
;---------------------------------------------

Slowdown	proc
		mov ah, 86h
		mov cx, 0000h
		mov dx, 8480h
		int 15h
		
		ret
		endp
	
;----------------------------------------------
;	Calculates coordinates based on frame sizes
;	Entry:	CH = frame height
;		CL = frame length
;	Exit:	DI = frame start offset (byte)
;	Destr:	AX DX
;----------------------------------------------

StartLocation	proc

		push ax

		mov ax, 0019h
		sub al, ch
		shr ax, 1
		mov dh, al

		mov ax, 0050h
		and cx, 0FFFEH
		sub al, cl
		mov dl, al

		dec dh
		mov al, 00A0h
		mul dh
		xor dh, dh
		add dx, ax

		mov di, dx

		pop ax

		ret
		endp

;----------------------------------------------
;	Management drawing frame
;	Entry:	DI = frame start offset (byte)
;		AH = color attr
;		CL = frame length
;		CH = frame height
;	Exit:	None
;	Destr:	DX, DI, ES, SI, CL, CH
;----------------------------------------------

DrawFrame	proc

		sub cl, 2

		mov dx, 0b800h
		mov es, dx

		mov si, offset FrameStyle_1

		dec al

		jmp IF_1
		WHILE_1:
			add si, 09h
			dec al
		IF_1:
			cmp al, 0h
			jne WHILE_1


		push di
		call DrawStr

		dec ch

		MiddleLines:	
				pop di
				add di, 00A0h
				push di
				call DrawStr

				sub si, 3h
				dec ch	

				cmp ch, 1h
				jne MiddleLines
				
		add si, 3h
		pop di
		add di, 00A0h
		call DrawStr	

		ret
		endp

;----------------------------------------------
;	Draw one str
;	Entry:	SI = address current character
;		ES = video memory start address
;		AH = color attr
;		CL = line length
;       	CH = frame height
;	Assumes: DF = 0
;	Exit:	None
;	Destr:	AL, DI, SI
;----------------------------------------------

DrawStr	proc
	cld
	push cx

	xor ch, ch

	lodsb
	stosw
	lodsb
	rep	stosw
	lodsb
	stosw

	pop cx

	ret 
	endp

PrintText	proc

		mov ax, 50H
		sub al, cl
		add ax, 4H
		add ax, 11*80*2
		mov di, ax
		mov si, bx
		
		mov al, [ColorText]		

		W:
		movsb
		stosb
		mov ah, [si]
		cmp ah, 27H
		jne W

		ret
		endp					

FrameStyle_1 db 0dah, 0c4h, 0bfh, 0b3h, ' ', 0b3h, 0c0h, 0c4h, 0d9h 
FrameStyle_2 db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch
FrameStyle_3 db 03h, 03h, 03h, 03h, ' ', 03h, 03h, 03h, 03h
FrameStyle_4 db 0fh, 0fh, 0fh, 0fh, ' ', 0fh, 0fh, 0fh, 0fh
FrameStyle_5 db '**** ****'
FrameStyle_6 db '//// ////'
ColorText db 0
end Start	