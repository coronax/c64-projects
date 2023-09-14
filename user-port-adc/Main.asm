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


; Test file for interfacing with an MCP3008 ADC over the
; user port.  
; This code uses a very simple bit-banging version of the
; SPI protocol, using 4 of the parallel port lines to
; implement the protocol
; Here's the pin mapping for the parallel port of the 
; user port:
;    PIN   BIT VALUE   USE
;     C      $01      CS (chip select)
;     D      $02      Clock
;     E      $04      Output
;     F      $08      Input
;
; The sample program assumes that a trimmer pot is
; attached to the ADC's input 2, and a photoresistor
; is attached to input 4.
; The program will print a message that the lights are
; on or off depending on whether the value read from
; the photoresistor is >= the value set by the pot.

; 10 SYS (49152)

*=$800

        BYTE        $00, $0F, $08, $0A, $00, $9E, $20, $28, $34, $39, $31, $35, $32, $29, $00, $00, $00

*=$C000

;        lda #<lightson
;        sta $00fb
;        lda #>lightson
;        sta $00fc
;        jsr printstring
;        lda #<lightsoff
;        sta $00fb
;        lda #>lightsoff
;        sta $00fc
;        jsr printstring

        ; set up parallel port
        lda #$F7
        sta $dd03

loop
        lda #%11010000
        sta command
        jsr getsample1

;        lda #$02
;        sta readresulthi
;        lda #$40
;        sta readresultlow


        jsr leftpad

        ; we use the basic 16-bit int print routine
        ldx readresultlow  ; lower byte to x register
        lda readresulthi          ; upper byte in acc
        stx threshold
        sta threshold+1
        jsr $bdcd       ; basic print integer routine

        lda #32         ; space
        jsr $ffd2       ; chrout
        jsr $ffd2       ; chrout
        jsr $ffd2       ; chrout
        jsr $ffd2       ; chrout
        jsr $ffd2       ; chrout
        jsr $ffd2       ; chrout

        lda #%11100000
        sta command
        jsr getsample1

;        lda #$01
;        sta readresulthi
;        lda #$10
;        sta readresultlow

        jsr leftpad

        ldx readresultlow  ; lower byte to x register
        lda readresulthi          ; upper byte in acc
        stx lightlevel
        sta lightlevel+1
        jsr $bdcd       ; basic print integer routine


        jsr docomparison ; print lights on/off

        jsr delay

        jmp loop
        rts

threshold byte 0,0
lightlevel byte 0,0



; if lightlevel >= threshold, print "lights on" message.
; else print "lights off" message.
docomparison
        lda threshold+1
        cmp lightlevel+1
        bcc printlightson       ; high byte of lightlevel > threshold
        bne printlightsoff      ; high byte of lightlevel < threshold
        lda lightlevel
        cmp threshold 
        bcs printlightson       ; hi bytes equal, low byte of lightlevel >=
        jmp printlightsoff      ; hi bytes equal, low byte of lightlevel <
printlightson
        lda #<lightson
        sta $00fb
        lda #>lightson
        sta $00fc
        jsr printstring
        rts
printlightsoff
        lda #<lightsoff
        sta $00fb
        lda #>lightsoff
        sta $00fc
        jsr printstring
        rts




; delay loop
delay
lp0     ldx #255
lp1     ldy #255
lp2     dey
        bne lp2
        dex
        bne lp1
        rts


userport = 56577
command byte 0
; reads a sample from the userport and puts it in result
getsample1
        lda #1
        sta userport
        lda #0
        sta userport

        ; send command - 5 bits
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

        lda #0
        sta readresulthi
        sta readresultlow
        sta readbuffer

      
        ; the documentation and samples i read
        ; indicate that I should need one clock 
        ; cycle of doing nothing while the
        ; conversion is underway, and that reading
        ; an ignored bit is the way to do that.
        ; but it seems like the first thing I read
        ; is the null bit that begins a response.
        ; I don't understand if this is a timing
        ; issue or what.
        ;jsr recbit      ; empty 
        jsr recbit      ; null
        jsr recbit      ; most significant data bit
        jsr recbit

        lda readbuffer
        sta readresulthi
        lda #0
        sta readbuffer

        jsr recbit
        jsr recbit
        jsr recbit
        jsr recbit
        jsr recbit
        jsr recbit      
        jsr recbit      
        jsr recbit      ; least significant data bit

        lda readbuffer
        sta readresultlow

        rts




; sendcarrybit sends the value of the status register
; carry bit over the faux spi interface.
; Basically, we just set the value and then
; toggle the clock line
sendcarrybit
        bcs @nonzero
        lda #0
        sta userport    ; set output bit
        lda #2
        sta userport    ; set clock high        
        nop
        nop
        nop
        nop
        lda #0
        sta userport    ; set clock low
        rts
@nonzero 
        lda #4
        sta userport
        lda #6
        sta userport
        lda #4
        sta userport
        rts



readresultlow byte 0       ; stores read result byte
readresulthi byte 0
readbuffer byte 0       ; temp buffer for reading

; read a single bit from the spi port 
; into readbuffer.  The existing contents
; of readbuffer are shifted left.
recbit
        lda #2
        sta userport
        nop
        nop
        nop
        nop
        lda #0
        sta userport    ; toggle clock
        asl readbuffer
        lda userport
        and #$8
        bne nonzer2
        rts
nonzer2 lda readbuffer
        ora #1
        sta readbuffer
        rts



lightson  byte 5
          text "   THE LIGHTS ARE ON" 
          byte 13, 154, 0
lightsoff byte 144
          text "   THE LIGHTS ARE OFF" 
          byte 13, 154, 0

; prints the string pointed to by 00fb 00fc
printstring
        ldy #0
@loop   lda ($fb),y
        beq done
        jsr $ffd2       ; chrout
        iny
        jmp @loop
done    rts



; pad 0 to 3 spaces before we print the value in
; readresultlow and hi...
leftpad
        ; if < 1000 ($03e8) print a space
        lda readresulthi
        cmp #4
        bcs lptest2 ;>= 4, skip
        cmp #3      
        bne lptest4 ;< 3, so always print space
        ; if high byte = 3, we need to compare lowest byte
        lda readresultlow
        cmp #$e9
        bcs lptest2; >e e9, so don't print a space
lptest4
        lda #32         ; space
        jsr $ffd2       ; chrout
lptest2
        ; if < 100 ($0064) print a space
        lda readresulthi
        cmp #0
        bne lptest3
        lda readresultlow
        cmp #$65
        bcs lptest3
        lda #32         ; space
        jsr $ffd2       ; chrout
lptest3
        ; if < 10 ($000a) print a space
        lda readresulthi
        cmp #0
        bne lpdone
        lda readresultlow
        cmp #$0b
        bcs lpdone
        lda #32         ; space
        jsr $ffd2       ; chrout
lpdone
        rts






