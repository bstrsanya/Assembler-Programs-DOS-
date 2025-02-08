.model tiny
.code
org 100h

Start:
	mov ch, 48h
	mov cl, 17h

	call StartLocation
	call DrawFrame

	mov ax, 4c00h
	int 21h
	
;----------------------------------------------
;	Calculates coordinates based on frame sizes
;	Entry:	CL = frame height
;			CH = frame length
;	Exit:	DX = frame start offset (byte)
;	Destr:	AX
;----------------------------------------------

StartLocation	proc

				mov ax, 0019h
				sub al, cl
				shr ax, 1
				mov dh, al

				mov ax, 0050h
				sub al, ch
				mov dl, al

				dec dh
				mov al, 00A0h
				mul dh
				mov dh, 0h
				add dx, ax

				ret
				endp

;----------------------------------------------
;	Management drawing frame
;	Entry:	DX = frame start offset (byte)
;			CL = frame height
;	Exit:	None
;	Destr:	BX ES SI CL
;----------------------------------------------

DrawFrame		proc

				mov bx, 0b800h
				mov es, bx

				mov bx, dx

				mov si, offset FrameStyle

				push bx
				call DrawStr

				inc si
				dec cl

				MiddleLines:	
							pop bx
							add bx, 00A0h
							push bx
							call DrawStr

							sub si, 2h
							dec cl	

							cmp cl, 1h
							jne MiddleLines
				
				add si, 3h
				pop bx
				add bx, 00A0h
				call DrawStr	

				ret
				endp

;----------------------------------------------
;	Draw one str
;	Entry:	SI = address current character
;			ES = video memory start address
;	Exit:	None
;	Destr:	BX DX SI
;----------------------------------------------

DrawStr			proc

				push cx

				mov dl, byte ptr [si]
				mov dh, 0ah
				mov es:[bx], dx

				dec ch
				add bx, 2

				inc si
				mov dl, byte ptr [si]

				MainLine:
						mov es:[bx], dx

						dec ch
						add bx, 2

						cmp ch, 1h
						jne MainLine

				inc si
				mov dl, byte ptr [si]
				mov es:[bx], dx

				pop cx

				ret 
				endp
	

FrameStyle db 0dah, 0c4h, 0bfh, 0b3h, ' ', 0b3h, 0c0h, 0c4h, 0d9h 
end Start	
