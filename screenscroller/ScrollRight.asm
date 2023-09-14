;Copyright (c) 2012, Christopher Just
;All rights reserved.

;Redistribution and use in source and binary forms, with or without 
;modification, are permitted provided that the following conditions 
;are met:

;    Redistributions of source code must retain the above copyright 
;    notice, this list of conditions and the following disclaimer.

;    Redistributions in binary form must reproduce the above 
;    copyright notice, this list of conditions and the following 
;    disclaimer in the documentation and/or other materials 
;    provided with the distribution.

;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
;"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
;FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
;COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
;INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
;BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
;CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
;STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
;ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED 
;OF THE POSSIBILITY OF SUCH DAMAGE.


ScrollRight1
        ; scroll interrupt - moves the screen one
        ; pixel and does whatever character shift
        ; step is necessary.
        ; first, let's check if the raster status
        ; bit is set. if it is, we need to clear it.
        ; if it isn't, then this interrupt wasn't 
        ; for us and we should jump to the regular
        ; interrupt vector.  i think.
        lda $d019
        and #$01        ; we only care about raster bit
        bne @scrintb
        jmp (storirq)   ; jump to original interrupt vector
@scrintb
        sta $d019       ; a is already 1; write to clear the raster bit

        inc xscroll 

        ; every frame we need to set the horizontal scroll
        ; amount.  There are also 4 frames where we do
        ; something special.
        lda xscroll
        cmp #7
        bne @scrint6

; xscroll = 0 - shift top half of color memory
        ;lda #0
        ;sta 53280

        lda $d016       ; set shift amount
        and #$f8
        ora xscroll
        sta $d016

        ; wait till raster is about 80
@scrinta lda $d012
        cmp #80
        bne @scrinta

        lda #9         ; shift top half of color ram after it's drawn
        sta numrows

        lda #$00
        sta srcrow
        sta dstrow
        lda #$d8
        sta srcrow+1
        sta dstrow+1
        jsr CopyRowsRight

        jmp EndScrollRight1


@scrint6 lda xscroll
        cmp #8
        bne scrint77

; xscroll = 8 (ie 0) - swap buffers.  shift bottom of color memory
        ;lda #5
        ;sta 53280
        lda #$00        ; rollover xscroll from 8 to 0
        sta xscroll


        lda $d016       ; set scroll register
        and #$f8
        ora xscroll
        sta $d016

        jsr SwapBuffers



        ;jsr SwapBuffers


        lda #16         ; shift bottom half of color ram before it's drawn
        sta numrows

        lda #$68
        sta srcrow
        sta dstrow
        lda #$d9
        sta srcrow+1
        sta dstrow+1
        jsr CopyRowsRight

        jmp EndScrollRight1

scrint77 
        lda xscroll
        cmp #$06
        bne @scrint8

; xscroll = 1 - copy top half of screen memory
        lda $d016       ; set shift amount
        and #$f8
        ora xscroll
        sta $d016

        lda #9         ; copy top half of screen
        sta numrows
        lda buffera
        sta srcrow
        lda buffera+1
        sta srcrow+1  
        lda bufferb
        sta dstrow
        lda bufferb+1
        sta dstrow+1      
        jsr CopyRowsRight
        jmp EndScrollRight1

@scrint8 
        lda xscroll
        cmp #$05
        bne @scrint9

; xscroll = 2 - copy bottom of screen memory
        lda $d016       ; set shift amount
        and #$f8
        ora xscroll
        sta $d016

        lda #16         
        sta numrows
        ; this time, our rows will be the buffer starts
        ; + 9 rows = 360 bytes = $0168
        clc
        lda buffera
        adc #$68
        sta srcrow
        lda buffera+1
        adc #$01
        sta srcrow+1
        clc  
        lda bufferb
        adc #$68
        sta dstrow
        lda bufferb+1
        adc #$01
        sta dstrow+1
      
        jsr copyRowsRight
        jmp EndScrollRight1

@scrint9

        lda xscroll
        cmp #4
        bne skip7

;        jmp @skip
        ; check if the R or L key is pressed to change speed
        jsr $ffe4       ; getin
        cmp #76         ; L
        beq faster2
        ;cmp #82         ; R
        ;beq slower2
        jmp skip7
faster2 ; insert our interrupt
        sei 
        lda #<scroll0
        sta $0314
        lda #>scroll0
        sta $0315
        cli
        jmp skip7
slower2 ; insert our interrupt
        sei 
        lda #<scroll0
        sta $0314
        lda #>scroll0
        sta $0315
        cli
       ; jmp @skip
skip7


; xscroll = anything else - just shift 1 pixel
        ;lda #8
        ;sta 53280
        lda $d016       ; set shift amount
        and #$f8
        ora xscroll
        sta $d016

EndScrollRight1
        lda #14
        sta 53280
        jmp (storirq) ; jump to original interrupt vector
; end of scrint





;======================================================



; CopyRowsRight.  Copies numrows rows of data from srcrow 
; to dstrow, shifting the characters in each row one spot 
; to the right each time.
; It is safe to do an in-place copy of the data by using 
; the same value for srcrow and dstrow.
CopyRowsRight
        ldx numrows

cpyrowr  ldy #39

        lda (srcrow),y
        ;ldy #0
        ;sta (dstrow),y
        sta t
        inc dstrow      ; dstrow was even, so this can't carry
        ldy #38
cpycolr 
        ; loop unrolling ftw? saves about 24 scan lines of time?
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        cpy #3
        bne cpycolr
        lda (srcrow),y          ; these next 4 are the remainder
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y
        dey
        lda (srcrow),y
        sta (dstrow),y

        dec dstrow              ; this is odd, so it can't underflow
        lda t
        sta (dstrow),y          ; y is already 0

        ; update srcrow and dstrow. 
        clc
        lda srcrow
        adc #40
        sta srcrow
        lda srcrow+1
        adc #0
        sta srcrow+1
        clc
        lda dstrow
        adc #40
        sta dstrow
        lda dstrow+1
        adc #0
        sta dstrow+1

        ; dec row counter x
        dex
        bne cpyrowr
        rts
