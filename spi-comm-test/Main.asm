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


; Test file for sending text back and forth to an arduino.
; Characters typed on the keyboard are transmitted to the 
; SPI slave, and text received from the slave is printed to
; the screen.
; This code uses a very simple bit-banging version of the
; SPI protocol, using 4 of the parallel port lines to
; implement the protocol
; Here's the pin mapping for the parallel port of the 
; user port:
;    PIN   BIT VALUE   USE              Arduino Uno pin
;     C      $01      CS (chip select)    10
;     D      $02      Clock                      13
;     E      $04      Output              11
;     F      $08      Input               12
;

; 10 SYS (49152)

PORT_DDR=$dd03
PORT_DATA=$dd01

*=$800

        BYTE        $00, $0F, $08, $0A, $00, $9E, $20, $28, $34, $39, $31, $35, $32, $29, $00, $00, $00


*=$C000
        jmp start

buffer  TEXT "                                                                  "
echo    BYTE 0

        ; set up parallel port
start   lda #$F7
        sta PORT_DDR
        ldx #0

loop

        jsr getnextchar 
        ;beq loop
        jsr sendchar
        lda readbuffer
        beq loop
        sta buffer,x
        jsr $ffd2       ;chrout
        lda buffer,x
        cmp #13
        beq outputsection
        cmp #10
        beq outputsection
        cpx #40
        beq outputsection
retloop inx
        ; wraparound x
        txa
        and #$3F
        tax
        jmp loop

outputsection
        lda #0
        sta buffer,x
        lda #<string1
        sta $fb
        lda #>string1
        sta $fc
        jsr outputstring
        lda #<buffer
        sta $fb
        lda #>buffer
        sta $fc
        jsr outputstring
        lda #<string2
        sta $fb
        lda #>string2
        sta $fc
        jsr outputstring

        ldx #$FF         ; it gets immediately inx'd to 0
        jmp retloop

; returns the next output character - either echoes the most
; recent input, or gets something from the c64 keyboard with
; getin
temp byte 0
getnextchar
        lda readbuffer
        bne usereadbuffer
        stx temp
        jsr $ffe4       ; getin
        ldx temp
usereadbuffer
        rts



string1 byte 13, 28
        TEXT "YOU SAID "
        byte 34, 0

string2 byte 34, 5, 13, 0

outputstring
        ldy #0
outloop lda ($fb),y
        beq done
        ;jsr $ffd2       ; chrout
        jsr sendchar
        iny
        jmp outloop
done    rts



; sends the character in accumulator out the spi port
command byte 0
sendchar
        sta command
        ; toggle slave select
        lda #1
        sta PORT_DATA
        lda #0
        sta PORT_DATA

        ; send 8 bits
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit        
        asl command
        jsr sendcarrybit  
        rts

   






; sendcarrybit sends the value of the status register
; carry bit over the faux spi interface.
; Basically, we just set the value and then
; toggle the clock line
sendcarrybit
        bcs @nonzero
        lda #0
        sta PORT_DATA    ; set output bit
        lda #2
        sta PORT_DATA    ; set clock high    
        jsr recbit    
        nop
        nop
        nop
        nop

        lda #0
        sta PORT_DATA    ; set clock low
        rts
@nonzero 
        lda #4
        sta PORT_DATA
        lda #6
        sta PORT_DATA
        jsr recbit
        nop
        nop
        nop
        nop
        lda #4
        sta PORT_DATA
        rts



readbuffer byte 0       ; temp buffer for reading

; read a single bit from the spi port 
; into readbuffer.  The existing contents
; of readbuffer are shifted left.
recbit
        asl readbuffer
        lda PORT_DATA
        and #$8
        bne nonzer2
        rts
nonzer2 lda readbuffer
        ora #1
        sta readbuffer
        rts









