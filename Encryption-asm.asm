TITLE Final_Project
; Program Description:	Allows the user to input a phrase and key
;						to use for encryption and decryption.
; Authors:				Erin Fast, Younes Yassine, and Logan Sherman
; Creation Date:		11/28/2022

INCLUDE Irvine32.inc
ClrRegs PROTO
DisplayMenu PROTO Input:PTR DWORD
PickAProc PROTO Input:PTR DWORD, Phrase:PTR DWORD, Key:PTR DWORD, PLength:PTR DWORD, KLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
Opt1 PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Key:PTR DWORD, KLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
Opt2 PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Key:PTR DWORD, KLength:PTR DWORD
Encrypt PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Key:PTR DWORD, KLength:PTR DWORD
EncryptChar PROTO Char:BYTE, Sort:BYTE, Val:BYTE
Opt3 PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Key:PTR DWORD, KLength:PTR DWORD
Decrypt PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Key:PTR DWORD, KLength:PTR DWORD
DecryptChar PROTO Char:BYTE, Sort:BYTE, Val:BYTE
Modulus PROTO Char:BYTE, Sort:BYTE
CheckPhrase PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
EnterPhrase PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
CheckKey PROTO Key:PTR DWORD, KLength:PTR DWORD
EnterKey PROTO Key:PTR DWORD, KLength:PTR DWORD
Uppercase PROTO Phrase:PTR DWORD, PLength:PTR DWORD
AlphaNum PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
CpyString PROTO Phrase:PTR DWORD, PLength:PTR DWORD, Temp:PTR DWORD, TLength:PTR DWORD
Print PROTO Phrase:PTR DWORD, PLength:PTR DWORD
WhichType PROTO Char:BYTE
ClrString PROTO String:PTR DWORD, Len:PTR DWORD

;constants
max = 151d
newLine EQU<0ah, 0dh>

.data
tempString BYTE max DUP(0h)
tempLen BYTE max
userString BYTE max DUP(0h)
stringLen BYTE max
userOption BYTE 0h
userkey BYTE max DUP(0h)
keyLen BYTE max

errorMsg BYTE "You have selected an invalid option.", 
			   newline, "Please try again.", newline, 0h

.code
main PROC 
INVOKE ClrRegs
startHere:
	call clrscr
	INVOKE DisplayMenu, OFFSET userOption
	cmp userOption, 1d
		jb invalid
	cmp userOption, 4d
		jb driver
	cmp userOption, 4d
		je done

invalid:
	push EDX
	mov EDX, OFFSET errorMsg
	call WriteString
	call WaitMsg
	jmp startHere

driver:
	INVOKE PickAProc, ADDR userOption, ADDR userString, ADDR userKey, ADDR stringLen, ADDR keyLen, ADDR tempString, ADDR tempLen
	jmp startHere

done:

exit
main ENDP


;--------------------------------------------------
DisplayMenu PROC USES EAX EBX EDX,
	Input:PTR DWORD				;pointer to variable
;
; Displays a menu for the user, and allows for the
; user to chose which item on the menu to use.
; Retuns: Nothing
;--------------------------------------------------
	.data
	MainMenu BYTE "Main Menu", newline, 
	"1.  Enter a Phrase", newline, 
	"2.  Encrypt the phrase", newline, 
	"3.  Decrypt a phrase", newline,
	"4.  Exit", newline, 
	"         Please make a selection ==>   ", 0h

	.code
	mov EBX, Input
	mov EDX, OFFSET MainMenu
	call WriteString
	call ReadDec
	mov BYTE PTR[EBX], AL
	ret
DisplayMenu ENDP


;--------------------------------------------------
PickAProc PROC USES EAX EBX,
	Input:PTR DWORD,			;pointer to variable
	Phrase:PTR DWORD,			;pointer to an array
	Key:PTR DWORD,				;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	KLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Based on the users input an option will be chosen
; and invoke its specific function.
; Returns: Nothing
;--------------------------------------------------
	mov EBX, Input
	mov EAX, [EBX]
	cmp AL, 2d
	jb option1

	cmp AL, 3d
	jb option2

	cmp AL, 3d
	je option3

	jmp QuitIt

	option1:
		INVOKE Opt1, Phrase, PLength, Key, KLength, Temp, TLength
		jmp QuitIt
	option2:
		INVOKE Opt2, Phrase, PLength, Key, KLength
		jmp QuitIt
	option3:
		INVOKE Opt3, Phrase, PLength, Key, KLength
		jmp QuitIt
	QuitIt:
	ret

PickAProc ENDP


;--------------------------------------------------
Opt1 PROC USES EAX EDX EDI ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Checks if the user has previously input a phrase
; and key.
; Returns: Nothing
;--------------------------------------------------
	INVOKE CheckPhrase, Phrase, PLength, Temp, TLength
	
	INVOKE CheckKey, Key, KLength

	ret
Opt1 ENDP


;--------------------------------------------------
Opt2 PROC USES EDX,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Invokes the functions Encrypt and Print for the 
; phrase entered by the user.
; Returns: Nothing
;--------------------------------------------------
	.data
	opt2Prompt BYTE "Encrypting the phrase", newLine, 0h
	noPhrase2 BYTE "Please select option 1 before option 2", newline, 0h
	
	.code
	call Clrscr
	mov EBX, Phrase				;checks if a phrase has been entered
	mov AL, BYTE PTR[EBX]
	cmp AL, 0h
	je empty

	mov EDX, OFFSET opt2Prompt
	call WriteString
	
	INVOKE Encrypt, Phrase, PLength, Key, KLength
	
	INVOKE Print, Phrase, PLength
	call WaitMsg
	jmp done
	
	empty:
		mov EDX, OFFSET noPhrase2
		call WriteString
		call WaitMsg
	
	done:

	ret
Opt2 ENDP


PhraseChar EQU DWORD PTR [EBP - 4]
KeyChar EQU DWORD PTR [EBP - 8]
PLen EQU DWORD PTR [EBP - 12]
KLen EQU DWORD PTR [EBP - 16]
PType EQU DWORD PTR [EBP - 20]
ModVal EQU DWORD PTR [EBP - 24]
PPos EQU DWORD PTR [EBP - 28]
KPos EQU DWORD PTR [EBP - 32]
;--------------------------------------------------
Encrypt PROC USES EAX EDX EDI ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Uses the key to encrypt the user's phrase.
; Returns: Nothing
;--------------------------------------------------
	INVOKE ClrRegs
	mov ESI, Phrase
	mov EDI, Key
	mov EBX, PLength
	mov EDX, KLength
	
	push EBP					;creates local variables
	mov EBP, ESP
	sub ESP, 32
	
	mov PPos, 0h				;position within prhrase
	mov KPos, 0h				;position within key
	
	mov AL, BYTE PTR[EBX]		;moves PLength into PLen
	mov PLen, EAX
	dec PLen					;decrement PLen for array position
	mov AL, BYTE PTR[EDX]		;moves KLength into KLen
	mov KLen, EAX
	dec KLen					;decrement KLen for array position
	
	next:
		;gets the type of char is in the phrase position
		mov CL, BYTE PTR[PPos]
		mov AL, BYTE PTR[ESI + ECX]
		mov PhraseChar, EAX
		INVOKE WhichType, BYTE PTR[PhraseChar]
		mov PType, EAX
		
		;gets the modulus of the key position using char type
		mov CL, BYTE PTR[KPos]
		mov AL, BYTE PTR[EDI + ECX]
		mov KeyChar, EAX
		INVOKE Modulus, BYTE PTR[KeyChar], BYTE PTR[PType]
		mov ModVal, EAX
		
		mov CL, BYTE PTR[PPos]
		INVOKE EncryptChar, BYTE PTR[PhraseChar], BYTE PTR[PType], BYTE PTR[ModVal]
		mov BYTE PTR[ESI + ECX], AL
		
		inc PPos
		inc KPos
		
		;if the phrase position is larger than the phrase length end
		mov AL, BYTE PTR[PPos]
		mov BL, BYTE PTR[PLen]
		cmp AL, BL
		ja done
		;if the key position is larger than the key repeat the key
		mov AL, BYTE PTR[KPos]
		mov BL, BYTE PTR[KLen]
		cmp AL, BL
		jbe next
		mov KPos, 0h
		jmp next
	
	done:
	
	mov ESP, EBP
	pop EBP
	ret
Encrypt ENDP

;--------------------------------------------------
EncryptChar PROC USES EBX EDX ECX EDI ESI,
	Char:BYTE,					;variable
	Sort:BYTE,					;variable
	Val:BYTE					;variable
;
; Takes the current character in the phrase and 
; changes it based on its type and the key value.
; Then return the new character.
; Retruns: EAX = changed Char
;--------------------------------------------------
	INVOKE ClrRegs
	mov BL, Char
	mov CL, Sort
	mov DL, Val
	
	cmp CL, 0Ah
	je number
	cmp CL, 1Ah
	je letter
	
	number:
		sub BL, DL
		cmp BL, 30h
		jb wrapN
		jmp done
	
	wrapN:
		mov AL, 2Fh		;if BL is below 30h it is no longer a number char
		sub AL, BL		;subtract from the position directly under 0
		mov BL, 39h
		sub BL, AL		;subtract from the 9 position to get the proper number char
		jmp done
	
	letter:
		sub BL, DL
		cmp BL, 41h
		jb wrapL
		jmp done
	
	wrapL:
		mov AL, 40h		;if BL is below 40h it is no longer a letter char
		sub AL, BL		;subtract from the position directly under A
		mov BL, 5Ah
		sub BL, AL		;subtract from the Z position to get the proper letter char
	
	done:
		mov AL, BL
	
	ret
EncryptChar ENDP


;--------------------------------------------------
Opt3 PROC USES EDX,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Invokes the functions Decrypt and Print for the 
; phrase entered by the user.
; Returns: Nothing
;--------------------------------------------------
	.data
	opt3Prompt BYTE "Decrypting the phrase", newLine, 0h
	noPhrase3 BYTE "Please select option 1 before option 3", newLine, 0h
	
	.code
	call Clrscr
	call Clrscr
	mov EBX, Phrase
	mov AL, BYTE PTR[EBX]
	cmp AL, 0h
	je empty
	
	mov EDX, OFFSET opt3Prompt
	call WriteString
	
	INVOKE Decrypt, Phrase, PLength, Key, KLength
	
	INVOKE Print, Phrase, PLength
	call WaitMsg
	jmp done
	
	empty:
		mov EDX, OFFSET noPhrase3
		call WriteString
		call WaitMsg

	done:

	ret
Opt3 ENDP


PhraseChar EQU DWORD PTR [EBP - 4]
KeyChar EQU DWORD PTR [EBP - 8]
PLen EQU DWORD PTR [EBP - 12]
KLen EQU DWORD PTR [EBP - 16]
PType EQU DWORD PTR [EBP - 20]
ModVal EQU DWORD PTR [EBP - 24]
PPos EQU DWORD PTR [EBP - 28]
KPos EQU DWORD PTR [EBP - 32]
;--------------------------------------------------
Decrypt PROC USES EAX EDX EDI ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Uses the key to decrypt the users phrase.
; Returns: Nothing
;--------------------------------------------------
	INVOKE ClrRegs
	mov ESI, Phrase
	mov EDI, Key
	mov EBX, PLength
	mov EDX, KLength
	
	push EBP					;creates local variables
	mov EBP, ESP
	sub ESP, 32
	
	mov PPos, 0h				;position within phrase
	mov KPos, 0h				;position within key
	
	mov AL, BYTE PTR[EBX]		;moves PLength into PLen
	mov PLen, EAX
	dec PLen					;decrement PLen for array position
	mov AL, BYTE PTR[EDX]		;moves KLength into KLen
	mov KLen, EAX
	dec KLen					;decrement KLen for array position
	
	next:
		;gets the type of char in the phrase position
		mov CL, BYTE PTR[PPos]
		mov AL, BYTE PTR[ESI + ECX]
		mov PhraseChar, EAX
		INVOKE WhichType, BYTE PTR[PhraseChar]
		mov PType, EAX
		
		;gets the modulus of the key position using char type
		mov CL, BYTE PTR[KPos]
		mov AL, BYTE PTR[EDI + ECX]
		mov KeyChar, EAX
		INVOKE Modulus, BYTE PTR[KeyChar], BYTE PTR[PType]
		mov ModVal, EAX
		
		mov CL, BYTE PTR[PPos]
		INVOKE DecryptChar, BYTE PTR[PhraseChar], BYTE PTR[PType], BYTE PTR[ModVal]
		mov BYTE PTR[ESI + ECX], AL
		
		inc PPos
		inc KPos
		
		;if the phrase position is larger than the phrase length end
		mov AL, BYTE PTR[PPos]
		mov BL, BYTE PTR[PLen]
		cmp AL, BL
		ja done
		;if the key position is larger than the key repeat the key
		mov AL, BYTE PTR[KPos]
		mov BL, BYTE PTR[KLen]
		cmp AL, BL
		jbe next
		mov KPos, 0h
		jmp next
	
	done:
	
	mov ESP, EBP
	pop EBP
	ret
Decrypt ENDP

;--------------------------------------------------
DecryptChar PROC USES EBX EDX ECX EDI ESI,
	Char:BYTE,					;variable
	Sort:BYTE,					;variable
	Val:BYTE					;variable
;
; Takes the current character in the phrase and 
; changes it based on its type and the key value.
; Then return the new character.
; Retruns: EAX = changed Char
;--------------------------------------------------
	INVOKE ClrRegs
	mov BL, Char
	mov CL, Sort
	mov DL, Val
	
	cmp CL, 0Ah
	je number
	cmp CL, 1Ah
	je letter
	
	number:
		add BL, DL
		cmp BL, 39h
		ja wrapN
		jmp done
	
	wrapN:
		sub BL, 3Ah		;if BL is above 39h it is no longer a number char
		mov AL, BL		;subtract from the position directly above 9
		mov BL, 30h
		add BL, AL		;add to the 0 position to get the proper number
		jmp done
	
	letter:
		add BL, DL
		cmp BL, 5Ah
		ja wrapL
		jmp done
	
	wrapL:
		sub BL, 5Bh		;if BL is above 5Ah it is no longer a letter char
		mov AL, BL		;subtract from the position directly above Z
		mov BL, 41h
		add BL, AL		;add to the A position to get the proper number
	
	done:
		mov AL, BL
	
	ret
DecryptChar ENDP


;--------------------------------------------------
Modulus PROC USES EBX EDX EDI ESI,
	Char:BYTE,					;variable
	Sort:BYTE					;variable
;
; Returns the mod of the key character based on its
; type.
; Returns: EAX = Mod
;--------------------------------------------------
	INVOKE ClrRegs
	mov BL, Char
	mov DL, Sort
	
	start:
		cmp BL, DL
		jb moveOn				;mod is in BL
		cmp BL, DL
		ja subtract				;subtract Sort from char
	
	moveOn:
		mov AL, BL
		jmp done
	
	subtract:
		sub BL, DL
		jmp start
	
	done:
	
	ret
Modulus ENDP


;--------------------------------------------------
CheckPhrase PROC USES EAX EBX EDX,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Sees if the user has input a phrase previously.
; If not it prompts the user for a phrase. If so it
; asks the user if they would like to change the 
; phrase.
; Returns: Nothing
;--------------------------------------------------
	.data
	phraseYes BYTE "Would you like to enter a new phrase? [ Y or N ]", newLine,
				   "    ==>    ", 0h
	phraseNo BYTE "Enter a Phrase", newLine,
				  "    ==>    ", 0h
	
	.code
	call Clrscr
	mov EBX, Phrase
	mov AL, BYTE PTR[EBX]
	cmp AL, 0h
	je empty
	mov EDX, OFFSET phraseYes
	call WriteString
	
	;checks for presses of n or N and y or Y
	call ReadChar
	call Clrscr
	cmp AL, 4Eh
	je done
	cmp AL, 59h
	je clear
	cmp AL, 6Eh
	je done
	cmp AL, 79h
	je clear
	
	clear:
		INVOKE ClrString, Phrase, PLength
		
	empty:
		mov EDX, OFFSET phraseNo
		call WriteString
		INVOKE EnterPhrase, Phrase, PLength, Temp, TLength
	
	done:
	
	ret
CheckPhrase ENDP


;--------------------------------------------------
EnterPhrase PROC USES EAX ECX EDX ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Takes the user's input string and changes all 
; letters to uppercase and removes any non-
; alphanumeric characters.
; Returns: Nothing
;--------------------------------------------------
	mov EDX, Phrase
	mov ESI, PLength
	movzx ECX, BYTE PTR[ESI]
	call ReadString
	mov BYTE PTR[ESI], AL
	
	INVOKE Uppercase, Phrase, PLength
	
	INVOKE AlphaNum, Phrase, PLength, Temp, TLength

ret
EnterPhrase ENDP


;--------------------------------------------------
CheckKey PROC USES EAX EBX EDX,
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Sees if the user has input a key previously.
; If not it prompts the user for a key. If so it
; asks the user if they would like to change the 
; key.
; Returns: Nothing
;--------------------------------------------------
	.data
	KeyYes BYTE "Would you like to enter a new key? [ Y or N ]", newLine,
				   "    ==>    ", 0h
	KeyNo BYTE "Enter a key", newLine,
				  "    ==>    ", 0h
	
	.code
	call Clrscr
	mov EBX, Key
	mov AL, BYTE PTR[EBX]
	cmp AL, 0h
	je empty
	mov EDX, OFFSET KeyYes
	call WriteString
	
	;checks for presses of n or N and y or Y
	call ReadChar
	call Clrscr
	cmp AL, 4Eh
	je done
	cmp AL, 59h
	je clear
	cmp AL, 6Eh
	je done
	cmp AL, 79h
	je clear

	clear:
		INVOKE ClrString, Key, KLength
		
	empty:
		mov EDX, OFFSET KeyNo
		call WriteString
		INVOKE EnterKey, Key, KLength
	
	done:
	
	ret
CheckKey ENDP


;--------------------------------------------------
EnterKey PROC USES EAX ECX EDX ESI,
	Key:PTR DWORD,				;pointer to an array
	KLength:PTR DWORD			;pointer to variable
;
; Takes the user's input as the key.
; Returns: Nothing
;--------------------------------------------------
	mov EDX, Key
	mov ESI, KLength
	movzx ECX, BYTE PTR[ESI]
	call ReadString
	mov BYTE PTR[ESI], AL

ret
EnterKey ENDP


;--------------------------------------------------
Uppercase PROC USES EAX EBX ECX ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD			;pointer to variable
;
; Changes all lowercase letters into uppercase.
; Returns: Nothing
;--------------------------------------------------
	mov ESI, Phrase
	mov EBX, PLength
	mov CL, BYTE PTR[EBX]
	
	ChangeCase:
		mov AL, BYTE PTR[ESI]
		cmp AL, 61h
		jb skip
		cmp AL, 7Bh
		jb change
		jmp skip
		change:
		sub AL, 20h
		mov BYTE PTR[ESI], AL
		skip:
			inc ESI
		loop ChangeCase

ret
Uppercase ENDP


;--------------------------------------------------
AlphaNum PROC USES EAX EBX ECX EDX EDI ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Removes all non-alphanumeric characters.
; Returns: Nothing
;--------------------------------------------------
	mov ESI, Phrase
	mov EDI, Temp
	mov EBX, PLength
	mov CL, BYTE PTR[EBX]
	mov EBX, 0h
	
	remove:
		mov AL, BYTE PTR[ESI]
		cmp AL, 30h
		jb skip
		cmp AL, 3Ah
		jb move
		cmp AL, 41h
		jb skip
		cmp AL, 5Bh
		jb move
		jmp skip
		move:
			mov BYTE PTR[EDI], AL
			inc EDI
			inc BL
		skip:
			inc ESI
			loop remove

	mov EDX, TLength
	mov BYTE PTR[EDX], BL
	
	INVOKE ClrString, Phrase, PLength
	
	INVOKE CpyString, Phrase, PLength, Temp, TLength

ret
AlphaNum ENDP


;--------------------------------------------------
CpyString PROC USES EAX EBX ECX EDX EDI ESI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD,			;pointer to variable
	Temp:PTR DWORD,				;pointer to an array
	TLength:PTR DWORD			;pointer to variable
;
; Moves the temp string into the phrase string.
; Returns: Nothing
;--------------------------------------------------
	mov EDI, Phrase
	mov ESI, Temp
	mov EBX, TLength
	mov CL, BYTE PTR[EBX]
	
	copyLoop:
		mov AL, BYTE PTR[ESI]
		mov BYTE PTR[EDI], AL
		inc ESI
		inc EDI
		loop copyLoop
	
	mov AL, BYTE PTR[EBX]
	mov EBX, PLength
	mov BYTE PTR[EBX], AL
	
	INVOKE ClrString, Temp, TLength

ret
CpyString ENDP


;--------------------------------------------------
Print PROC USES ECX EDX EBX EDI,
	Phrase:PTR DWORD,			;pointer to an array
	PLength:PTR DWORD			;pointer to variable
;
; Prints out the string with seven characters
; followed by three spaces, repeated.
; Returns: Nothing
;--------------------------------------------------
	INVOKE ClrRegs
	mov EDI, Phrase
	mov EBX, PLength
	mov CL, BYTE PTR[EBX]
	
	characters:
		mov AL, BYTE PTR[EDI]
		call WriteChar
		inc EDI
		inc DL
		
		cmp DL, 7h
		jb continue
		
		cmp DL, 7h
		je spaces
		
		spaces:
			mov AL, 20h
			call WriteChar
			call WriteChar
			call WriteChar
			mov DL, 0h
		
		continue:
		loop characters
		
	call Crlf

ret
Print ENDP


;--------------------------------------------------
WhichType PROC USES EDI ESI,
	Char:BYTE					;variable
;
; Takes input of a character and returns whether
; the type of character is a number or letter.
; Returns: EAX as 1Ah or Ah
;--------------------------------------------------
	mov AL, Char
	cmp AL, 3Ah
	jb number
	cmp AL, 5Bh
	jb letter
	
	number:
		mov EAX, 0Ah
		jmp return
	
	letter:
		mov EAX, 1Ah
	
	return:
ret
WhichType ENDP


;--------------------------------------------------
ClrString PROC USES EAX ECX ESI,
	String:PTR DWORD,			;pointer to an array
	Len:PTR DWORD				;pointer to variable
;
; Empties a string and resets the size to max.
; Returns: Nothing
;--------------------------------------------------
	mov ESI, String
	mov EAX, Len
	mov CL, BYTE PTR[EAX]
	
	Clr:
		mov BYTE PTR [ESI], 0h 
		inc ESI
		loop Clr
	
	mov BYTE PTR[EAX], 151d
	
	ret
ClrString ENDP


;--------------------------------------------------
ClrRegs PROC
;
; Clears registers.
; Returns: Nothing
;--------------------------------------------------
	mov EAX, 0h
	mov EBX, 0h
	mov ECX, 0h
	mov EDX, 0h
	mov ESI, 0h
	mov EDI, 0h

	ret 
ClrRegs ENDP


END main