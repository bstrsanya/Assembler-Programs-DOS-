.model tiny
.code
.286
org 100h
locals @@

Color 	    equ 1Fh					; color text and frame
VideoSeg    equ 0b800h				; addr video segment
StartCoord  equ 160 * 2 + 65 * 2	; start coord print
NumReg      equ 13					; number registers
HeightFrame equ 15					; height frame
LengthFrame equ 11					; length frame
LenConsByte equ 160					; length consol in byte
ScanCode_Q  equ 10h					; scan code letter Q
ScanCode_W  equ 11h					; scan code letter W

;---------------------------------------------
;	MACROS. Write in DrawBuffer need registers
;	Entry: 	shift across the ValueReg
;	Exit: 	Noun
;	Destr: 	DI
;---------------------------------------------

PutBuffer	macro	shift
			mov ax, ValueReg + shift 	; get value reg by
			pop di						;            shift
			add di, LengthFrame * 2		; go to next str
			push di
			call WriteBuf				; write in DrawBuffer
		endm

Start:
        mov ax, 3508h			; func 35h int 21h - get interrupt vector
        int 21h					; AL = number interrupt      return ES:BX
        mov old08seg, es		; ES = segment
        mov old08ofs, bx		; BX = offset

        mov ax, 2508h			; func 25h int 21h - set interrupt vector
        push cs					; AL = number interrupt	 
        pop ds					; DS = segment
        mov dx, offset Int08    ; DX = offset
        int 21h

		mov ax, 3509h			; func 35h int 21h - get interrupt vector
        int 21h					; AL = number interrupt      return ES:BX
        mov old09seg, es		; ES = segment
        mov old09ofs, bx		; BX = offset

        mov ax, 2509h			; func 25h int 21h - set interrupt vector
        push cs					; AL = number interrupt	 
        pop ds					; DS = segment
        mov dx, offset Int09    ; DX = offset
        int 21h

        mov ax, 3100h			; func 31h int 21h - stay resident 
        mov dx, offset EOP		; DX = memory size in paragraphs (16-byte)
        shr dx, 4		      
        inc dx
        int 21h

;---------------------------------------------
;	Override interrupt 08 (Timer)
;	Entry: 	Noun 
;	Exit: 	Noun
;	Destr: 	Noun
;---------------------------------------------

Int08   proc
		
		mov cs:[ValueReg + 0],  ax	; save value registers
		mov cs:[ValueReg + 2],  bx	;	     in memory
		mov cs:[ValueReg + 4],  cx
		mov cs:[ValueReg + 6],  dx
		mov cs:[ValueReg + 8],  si
		mov cs:[ValueReg + 10], di
		mov cs:[ValueReg + 12], bp
		mov cs:[ValueReg + 14], sp
		mov cs:[ValueReg + 16], ds
		mov cs:[ValueReg + 18], es
		mov cs:[ValueReg + 20], ss
		mov cs:[ValueReg + 22], cs
				
		pop ax							; ax = ip
		push ax
		mov cs:[ValueReg + 24], ax		; save value ip
		mov ax, cs:[ValueReg]			; recovery ax

        push ax bx cx dx es si di ds	; save registers

		push cs							; ds = cs
		pop ds
		
        mov si, offset NameReg			; si = addr name's registers
        mov di, offset DrawBuffer		; di = addr DrawBuffer

		call DrawFrame					; style frame -> DrawBuffer

		mov di, offset DrawBuffer		
		add di, 4						; skip first and second cell

		push di

        PutBuffer 0						; PutBuffer - MACRO
		PutBuffer 2						; Get value reg and put
		PutBuffer 4						; 		DrawBuffer	
		PutBuffer 6						; PutBuffer shift:
		PutBuffer 8						; mov ax, ValueReg + shift
		PutBuffer 10					; pop di	
		PutBuffer 12					; add di, LengthFrame * 2
		PutBuffer 14					; push di
		PutBuffer 16					; call WriteBuf
		PutBuffer 18			 
		PutBuffer 20					; 0 - shift AX, 2 - shift BX
		PutBuffer 22					;      ...     22 - shift CS
		PutBuffer 24
		
		pop di

        mov bx, VideoSeg				; es:[di] =
        mov es, bx						;      = VideoSeg:[StartCoord]
        mov di, StartCoord

        mov si, offset DrawBuffer
        mov cx, HeightFrame		
		
		mov al, [Active]				; if (keyboard signal == 1)
		cmp al, 1						;	draw
		jne @@Finish	

	@@Begin:							; Start print DrawBuffer
        push di cx
		
        mov cx, LengthFrame				; while (cx--)
     	rep movsw						; 	es:[di++] = ds:[si++]

        pop cx di

        add di, LenConsByte				; jmp next str
        loop @@Begin					; if (--cx) jmp @@Begin

	@@Finish:
        pop ds di si es dx cx bx ax		; recovery registers
        
		db 0eah							; jmp far segment:offset
        old08ofs dw 0					; offset
        old08seg dw 0					; segment
	    
        endp  

;---------------------------------------------
;	Override interrupt 09 (Keyboard)
;	Entry: 	Noun 
;	Exit: 	Noun
;	Destr: 	Noun
;---------------------------------------------

Int09	proc

		push ax bx cx dx es si di ds	; save registers

		push cs							; ds = cs
		pop ds

		in al, 60H						; read last pressed keys
		cmp al, ScanCode_Q
		jne @@Check
		mov al, 1						; if (pressed 'Q')
		mov [Active], al				; 	Active = 1

	@@Check:
		cmp al, ScanCode_W				; if (pressed 'W')
		jne @@Finish					; 	Active = 0
		xor al, al
		mov [Active], al

	@@Finish:
		pop ds di si es dx cx bx ax		; recovery registers

		db 0eah							; jmp far segment:offset
		old09ofs dw 0					; offset
		old09seg dw 0					; segment

		endp

;---------------------------------------------
;	Writing registers names and values to buffer
;	Entry: 	SI = addr name's registers
;		DI = addr DrawBuffer
;	Assumes: DS = CS
;	Exit: 	Noun
;	Destr: 	SI, DI
;---------------------------------------------

WriteBuf    
		proc

        push es

        push cs						; [si] = NameReg
        pop es						; [di] = DrawBuffer

        movsb						; es:[di++] = ds:[si++]
		inc di						; skip attr color
        movsb						; es:[di++] = ds:[si++]
        inc di						; skip attr color

		mov es:[di], byte ptr ' '	; es:[di] = space
		add di, 2					; skip attr color
                
    	pop es
                
        call ConvertReg				; value register -> 
									; -> buffer (ASCII)
    	ret
        endp

;---------------------------------------------
;	Converting number to ASCII code
;	Entry: 	AL = digit in the right half
;			DI = addr DrawBuffer
;	Assumes: DS = CS
;	Exit: 	Noun
;	Destr: 	AL, DI
;---------------------------------------------

ConvertNum	
		proc
			
	@@Begin:
		and ax, 000Fh			; ax = last digit in number
		cmp al, 9				; if (ax <= 9)
		jle @@Dec				; 	dec digit
		add al, 'A' - 0Ah		; else
		jmp @@Finish			;	hex digit
	@@Dec:	
		add al, '0'
	@@Finish:
		mov [di], al			; [di] = DrawBuffer
		add di, 2				; skip attr color
		
        ret
        endp

;---------------------------------------------
;	Converting register to ASCII code
;	Entry: 	AX = value registers
;	Assumes: DS = CS
;	Exit: 	Noun
;	Destr: 	Noun
;---------------------------------------------

ConvertReg	
		proc

		mov cl, 16			; number of byte in registers

	@@Begin:
		push ax

		sub cl, 4			
		shr ax, cl			; bit shift for get every digit
		call ConvertNum		; func: digit -> ASCII

		pop ax
		
		cmp cl, 0			; if (cl != 0)
		jne @@Begin			; 	jmp @@Begin

		ret
		endp

;---------------------------------------------
;	Drawing frame to the DrawBuffer
;	Entry: 	Noun
;	Assumes: DS = CS
;	Exit: 	Noun
;	Destr: 	Noun
;---------------------------------------------

DrawFrame	
		proc

		push si cx

		mov ch, HeightFrame
		mov si, offset FrameStyle	; [si] = addr frame style

		call DrawStr				; print first line
		dec ch

	@@MiddleLines:	
		call DrawStr				; print middle line
		sub si, 3h					; return to need smb
		dec ch	
		cmp ch, 1h					; if (ch != 1)
		jne @@MiddleLines			;	jmp @@MiddleLines
				
		add si, 3h					; go to need smb
		call DrawStr				; print last line

		pop cx si 

		ret
		endp

;---------------------------------------------
;	Drawing one str to the DrawBuffer
;	Entry: 	SI = addr frame style
;			DI = addr DrawBuffer
;	Assumes: DS = CS
;	Exit: 	Noun
;	Destr: 	DI
;---------------------------------------------

DrawStr		
		proc

		cld							; flag DF = 0
		push es cx

		push cs			
		pop es						; es = cs

		movsb						; es:[di++] = ds:[si++]
		inc di						; skip attr color

		mov cx, LengthFrame - 2		; number middle smb

	@@Begin:
		movsb						; es:[di++] = ds:[si++]
		inc di						; skip attr color
		dec si						; return to need smb
		loop @@Begin				; if (--cx) jmp @@Begin

		inc si						; go to need smb
		movsb						; es:[di++] = ds:[si++]
		inc di						; skip attr color

		pop cx es

		ret 
		endp
  
Active     db 0
ValueReg   dw 13 dup (0)
NameReg    db 'ax', 'bx', 'cx', 'dx', 'si', 'di', 'bp', 'sp', 'ds', 'es' 
	       db 'ss', 'cs', 'ip'
DrawBuffer db LengthFrame * HeightFrame * 2 dup(0, Color)
FrameStyle db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch
EOP:
end Start
