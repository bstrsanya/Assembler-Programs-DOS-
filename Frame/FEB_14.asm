.model tiny
.code
org 100h
locals @@

VideoSeg   equ 0b800h
LengthCons equ 50h
HeightCons equ 19h
BaseHeightFrame equ 05h

Start:

	call ClearScreen	;
	call ReadComLine
	call StartLocation
	
	call DrawFrame
	call PrintText

	mov ax, 4c00h
	int 21h

;---------------------------------------------
;	Clean console from other commands
;	Entry: 	Noun
;	Exit: 	Noun
;	Destr: 	AL
;---------------------------------------------

ClearScreen	proc

		mov al, 3h
		int 10h

		ret
		endp

;---------------------------------------------
;	Read command line
;	Entry: 	Noun
;	Exit:  	AH = color attr
;		AL = style frame
;		BX = addr str
;		CL = length frame
;		CH = height frame
;	Destr: 	SI, DI, DX
;---------------------------------------------

ReadComLine	proc
	
		mov si, 80H	;const	; addr length command line
		mov di, 80H
		mov cl, es:[si]		; get length command line
	
		mov al, "'"		; while (cx-- && ds:[di++] != "'")
		repne scasb

		mov bx, di		; save addr beginnig str
	
		add cl, 6	;		

		mov si, 82H		; addr beginnig need data

		xor dx, dx		; read height frame
		call Atoi
		mov ch, dl

		cmp ch, 3h		; if (height < 3) height =
		jae @@Continue		;		= BaseHeightFrame	
		mov ch, BaseHeightFrame


	@@Continue:
		xor dx, dx		; read color attr
		call Atoi
		mov ah, dl

		xor dx, dx		; read color str
		call Atoi
		mov [ColorStr], dl

		xor dx, dx		; read style frame
		call Atoi
		mov al, dl

		cmp al, 0h		; user style frame 
		jne @@Finish

		push cx

		xor cx, cx
		mov cx, 9h			; number of characters
		mov di, offset FrameStyle_0	; addr to save characters
		rep movsb			; while (cx--) 
						;	es:[di++] = ds:[si++]

		pop cx
		xor al, al
		 
	@@Finish:	

		ret
		endp

;---------------------------------------------
;	Func Atoi
;	Entry: SI = addr for read
;	Exit:  DX = read digit
;	Destr: SI
;---------------------------------------------

Atoi		proc

		push ax
		xor ax, ax

		lodsb   
            
	@@Begin:
		cmp al, 0Dh		; if (al == '\r') jmp @@Finish
		je @@Finish   
		cmp al, '9'     	; if (al <= '9')
		jle @@Dec

	@@Hex:  			; ASCII letter -> hex_digit     
		sub al, 'A' - 0Ah        
		jmp @@Number

	@@Dec:				; ASCII digit -> dec_digit
		sub al, '0'
		jmp @@Number

	@@Number:			; Convert: digit -> number
		shl dx, 4        	
		add dx, ax       

		lodsb            			
		cmp al, ' '      	; if (al != ' ') jmp @@Begin
		jne @@Begin
	
	@@Finish:
		pop ax

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
;	Destr:	DX
;----------------------------------------------

StartLocation	proc

		push ax

		mov ax, HeightCons	; dh = number missing lines
		sub al, ch
		shr ax, 1
		mov dh, al
		dec dh

		xor ax, ax		; ax = dh * 160
		mov al, dh		
		shl ax, 2
		add al, dh
		shl ax, 5

		xor dx, dx

		mov dx, LengthCons	; dx = number missing column * 2	
		sub dl, cl

		add dx, ax		; dx = shift (byte) from beginnig
		and dl, 0FEh		; alignment shift

		mov di, dx

		pop ax

		ret
		endp

;----------------------------------------------
;	Management drawing frame
;	Entry:	DI = frame start offset (byte)
;		AH = color attr
;		AL = frame style
;		CL = frame length
;		CH = frame height
;	Exit:	None
;	Destr:	DX, DI, ES, SI, CL, CH
;----------------------------------------------

DrawFrame	proc

		mov si, offset FrameStyle_0	; choose frame style
		jmp @@If_1	
	@@While_1:
		add si, 09h			; 9 - number smb in one style
		dec al
	@@If_1:
		cmp al, 0h
		jne @@While_1

		mov dx, VideoSeg		; es - video segment
		mov es, dx

		sub cl, 2			; number smb without
						;     first and last
		push di
		call DrawStr			; print first line
		dec ch

	@@MiddleLines:	
		pop di
		add di, 00A0h			; shift in video memory
		push di
		call DrawStr			; print middle line

		sub si, 3h			; return to need smb
		dec ch	

		cmp ch, 1h
		jne @@MiddleLines
				
		add si, 3h			; 
		pop di
		add di, 00A0h			; shift in video memory
		call DrawStr			; print last line

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

DrawStr		proc

		cld			; flag DF = 0
		push cx

		xor ch, ch

		lodsb			; al = smb from style, ah = color
		stosw			; output 2 byte in console
		lodsb
		rep	stosw		; while (cx--) stosw
		lodsb
		stosw

		pop cx

		ret 
		endp

PrintText	proc

		mov ax, 50H
		sub al, cl
		and al, 0FEh
		add ax, 4H
		add ax, 11 * 80 * 2
		mov di, ax
		mov si, bx
		
		mov al, [ColorStr]		

	@@OutputSmb:
		movsb
		stosb
		mov ah, [si]
		cmp ah, "'"
		jne @@OutputSmb

		ret
		endp					

FrameStyle_0 db '         '
FrameStyle_1 db 0dah, 0c4h, 0bfh, 0b3h, ' ', 0b3h, 0c0h, 0c4h, 0d9h 
FrameStyle_2 db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch
FrameStyle_3 db 03h, 03h, 03h, 03h, ' ', 03h, 03h, 03h, 03h
FrameStyle_4 db 0fh, 0fh, 0fh, 0fh, ' ', 0fh, 0fh, 0fh, 0fh
FrameStyle_5 db '**** ****'
FrameStyle_6 db '//// ////'
ColorStr     db 0

end Start	