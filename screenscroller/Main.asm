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


; 10 SYS (49152)

*=$800

        BYTE        $00, $0F, $08, $0A, $00, $9E, $20, $28, $34, $39, $31, $35, $32, $29, $00, $00, $00

; A display hack to make the screen scroll smoothly 
; to the left.  All the drawing is done in interrupts,
; so the screen editor is still functional.  That
; means that you can type on the screen while it's
; scrolling - though you probably won't be able to 
; enter a basic command or anything like that.

; There are two problems: First, moving an entire 
; screen's worth of characters takes quite a while.
; A naive screen shift implementation can take up 
; almost an entire frame.  Second, there's color
; memory to worry about.  Color memory on the 64
; can't be moved, so you can't double buffer it.

; I ended up splitting the memory shift between four 
; frames, using an interrupt triggered at the bottom
; of the frame.  That's ultimately kind of
; conservative, but it worked.  And we've got a lot 
; of raster time left to do something more 
; interesting.

; We use two frames to shift the character memory;
; this is easy since it's double buffered.  We use
; another two to shift the color memory, but here's
; the tricky part: We shift the top half of color 
; memory the frame _before_ the shifted data needs
; to be displayed (but after the raster has passed 
; it).  Then we shift the bottom half of color
; memory while the top half of the frame is being
; displayed.

*=$C000
      jmp start

; variables
storirq byte 0,0 ; store original IRQ vector
xscroll byte 4   ; xscroll is horizontal scroll in pixels

; Screen memory is double-buffered.  buffer a 
; points to the currently displayed buffer,
; and buffer b is the one we can draw into.
buffera byte $00,$04    ; pointer to frame buffer a
bufferb byte $00,$3c    ; pointer to frame buffer b

lastchar    byte 0          ; last char read - used for input

numrows   byte 25       ; Number of rows to copy
t         byte 0        ; temp char

; srcrow and dstrow are allocated in zero page so that
; the copy routine can use them for indirect addressing.
srcrow  = $00fb 
dstrow  = $00fd 


; Initialization function
start 
        ; go to 38-column mode
        lda $d016
        and #$f7
        sta $d016

        jsr DrawScreen

        ; initialize scroll amount
        lda #4
        sta xscroll
        ; copy original interrupt address to storirq
        lda $0314
        sta storirq
        lda $0315
        sta storirq+1
        ; insert our interrupt
        sei 
        lda #<ScrollRight1
        sta $0314
        lda #>ScrollRight1
        sta $0315
        cli
        ; turn on raster interrupts at raster 252
        ; because that's below the bottom of the
        ; screen
        lda #$fc
        sta $d012 ; d012 is lowest 8 bits of raster pos     
        lda $d011 ; d011 contains highest raster bit
        and #$7f  ; clear bit 7
        sta $d011
        lda #$01
        sta $d019 ; set vic interrupt status register
        sta $d01a ; set vic interrupt enable register

loop    jmp loop
        rts


; Draws the intro screen.  This is a completed
; hardcoded draw routine to copy our screen
; data into character and color memory.
DrawScreen
        lda #0
        sta $d021       ; background color
        ldx #0
drwlp1  lda $7400,x
        sta $400,x
        lda $7500,x
        sta $500,x
        lda $7600,x
        sta $600,x
        lda $7700,x
        sta $700,x

        lda $7000,x
        sta $d800,x
        lda $7100,x
        sta $d900,x
        lda $7200,x
        sta $da00,x
        lda $7300,x
        sta $db00,x
        inx
        bne drwlp1      ; loop until x wraps around to zero
        rts


; Swaps between two character memory buffers, whose 
; positions are hardcoded.  Buffer a always refers
; to the buffer that's currently displayed.  Buffer b
; is the buffer that should be drawn to.
; When swapbuffers is called, it swaps the pointers 
; of buffera and bufferb and tells the VIC-II chip to
; display the new buffer a.
SwapBuffers
       ; swap screen buffers
        lda buffera+1
        cmp #$04
        bne @swap2
        lda #$3c
        sta buffera+1
        lda #$04
        sta bufferb+1
        lda $d018
        ora #$f0
        sta $d018
        rts
@swap2  lda #$3c
        sta bufferb+1
        lda #$04
        sta buffera+1
        lda $d018
        and #$1f
        sta $d018
        rts



ScrollLeft1
        ; scroll interrupt - moves the screen one
        ; pixel left and does whatever character shift
        ; step is necessary.
        ; first, let's check if the raster status
        ; bit is set. if it is, we need to clear it.
        ; if it isn't, then this interrupt wasn't 
        ; for us and we should jump to the regular
        ; interrupt vector.  i think.
        lda $d019
        and #$01        ; we only care about raster bit
        bne scrintb
        jmp (storirq)   ; jump to original interrupt vector
scrintb
        sta $d019       ; a is already 1; write to clear the raster bit

        dec xscroll

        ; every frame we need to set the horizontal scroll
        ; amount.  There are also 4 frames where we do
        ; something special.
        bne scrint6

; xscroll = 0 - shift top half of color memory
        ;lda #0
        ;sta 53280

        lda $d016       ; set shift amount
        and #$f8
        ora #$00
        sta $d016

        ; wait till raster is about 80
scrinta lda $d012
        cmp #80
        bne scrinta

        lda #9         ; shift top half of color ram after it's drawn
        sta numrows

        lda #$00
        sta srcrow
        sta dstrow
        lda #$d8
        sta srcrow+1
        sta dstrow+1

        jsr CopyRowsLeft

scrint6 lda xscroll
        cmp #$ff
        bne scrint7

; xscroll = -1 (ie 7) - swap buffers.  shift bottom of color memory
        ;lda #5
        ;sta 53280
        lda #$07        ; rollover xscroll from -1 to 7
        sta xscroll

        ; check if the L key is pressed to change speed
        ; if not, store the key in lastchar so we can 
        ; look at it later.
        jsr $ffe4       ; getin
        cmp #76         ; L
        beq @faster
        sta lastchar
        jmp @skip
@faster ; insert our interrupt
        sei 
        lda #<scrollleft2
        sta $0314
        lda #>scrollleft2
        sta $0315
        cli
        
@skip

        lda $d016       ; set scroll register
        and #$f8
        ora #$07
        sta $d016

        jsr SwapBuffers

        lda #16         ; shift bottom half of color ram before it's drawn
        sta numrows

        lda #$68
        sta srcrow
        sta dstrow
        lda #$d9
        sta srcrow+1
        sta dstrow+1

        jsr CopyRowsLeft

scrint7 
        lda xscroll
        cmp #$01
        bne scrint8

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
        jsr copyrowsleft
        jmp scrint2

scrint8 
        lda xscroll
        cmp #$02
        bne scrint9

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
      
        jsr copyrowsleft
        jmp scrint2

scrint9

        lda xscroll
        cmp #4
        bne @scrinta
        ; if this is xscroll = 4, see if we need to drop down to scroll0
        lda lastchar
        cmp #82         ; R
        bne @scrinta
        ; clear lastchar
        lda #0
        sta lastchar
        ; insert our interrupt
        sei 
        lda #<scroll0
        sta $0314
        lda #>scroll0
        sta $0315
        cli
       
@scrinta
; xscroll = anything else - just shift 1 pixel
        ;lda #8
        ;sta 53280
        lda $d016       ; set shift amount
        and #$f8
        ora xscroll
        sta $d016

scrint2
        lda #14
        sta 53280
        jmp (storirq) ; jump to original interrupt vector
; end of ScrollLeft1


;======================================================


scrollleft2
        ; scroll interrupt - moves the screen two
        ; pixels to the left and does whatever 
        ; character shift step is necessary.
        ; First, let's check if the raster status
        ; bit is set. if it is, we need to clear it.
        ; If it isn't, then this interrupt wasn't 
        ; for us and we should jump to the regular
        ; interrupt vector.  i think.
        lda $d019
        and #$01        ; we only care about raster bit
        bne @scrintb
        jmp (storirq)   ; jump to original interrupt vector
@scrintb
        sta $d019       ; a is already 1; write to clear the raster bit

        dec xscroll
        dec xscroll

        ; every frame we need to set the horizontal scroll
        ; amount.  There are also 4 frames where we do
        ; something special.
        lda xscroll
        cmp #$01
        bne @scrint6

; xscroll = 1 - shift top half of color memory
        ;lda #0
        ;sta 53280

        lda $d016       ; set shift amount
        and #$f8
        ora #$00
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
        jsr CopyRowsLeft

        jmp endscrollleft2


@scrint6 lda xscroll
        cmp #$ff
        bne @scrint7

; xscroll = -1 (ie 7) - swap buffers.  shift bottom of color memory
        ;lda #5
        ;sta 53280
        lda #$07        ; rollover xscroll from -1 to 7
        sta xscroll

        ; check if the R key is pressed to decrease speed
        jsr $ffe4       ; getin
        cmp #82         ; R
        bne @skip2
        ; insert our interrupt
        sei 
        lda #<scrollleft1
        sta $0314
        lda #>scrollleft1
        sta $0315
        cli
@skip2

        lda $d016       ; set scroll register
        and #$f8
        ora #$07
        sta $d016

        jsr SwapBuffers

        lda #16         ; shift bottom half of color ram before it's drawn
        sta numrows
        lda #$68
        sta srcrow
        sta dstrow
        lda #$d9
        sta srcrow+1
        sta dstrow+1
        jsr CopyRowsLeft

        jmp endscrollleft2

@scrint7 
        lda xscroll
        cmp #$03
        bne @scrint8

; xscroll = 3 - copy top half of screen memory
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
        jsr copyrowsleft
        jmp endscrollleft2

@scrint8 
        ;lda xscroll
        ;cmp #$05
        ;bne scrint9

; xscroll = 5 - copy bottom of screen memory
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
      
        jsr copyrowsleft


endscrollleft2
        lda #14
        sta 53280
        jmp (storirq) ; jump to original interrupt vector
; end of scrollleft2


;======================================================


scroll0
        ; scroll interrupt - 
        ; This version keeps the screen stationary, 
        ; so all it has to do is listen for 
        ; keypresses and jump to a different
        ; scroll interrupt as necessary.
        lda $d019
        and #$01        ; we only care about raster bit
        bne @scrintb
        jmp (storirq)   ; jump to original interrupt vector
@scrintb
        sta $d019       ; a is already 1; write to clear the raster bit

        ; check if the R key is pressed to increase speed
        jsr $ffe4       ; getin
        cmp #76         ; L
        beq @goleft
        cmp #82         ; R
        beq @goright
        jmp @skip3
@goleft ; insert our interrupt
        sei 
        lda #<scrollleft1
        sta $0314
        lda #>scrollleft1
        sta $0315
        cli
        jmp @skip3
@goright
        sei 
        lda #<scrollright1
        sta $0314
        lda #>scrollright1
        sta $0315
        cli

@skip3
        lda #14
        sta 53280
        jmp (storirq) ; jump to original interrupt vector

;======================================================



; CopyrowsLeft.  Copies numrows rows of data from srcrow 
; to dstrow, shifting the characters in each row one spot 
; to the left each time.
; It is safe to do an in-place copy of the data by using 
; the same value for srcrow and dstrow.
CopyRowsLeft
        ldx numrows

cpyrow  ldy #0

        lda (srcrow),y
        sta t
        inc srcrow      ; srcrow was even, so this can't carry

cpycol 
        ; loop unrolling ftw? saves about 24 scan lines of time?
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        cpy #35
        bne cpycol
        lda (srcrow),y          ; these next 4 are the remainder
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny
        lda (srcrow),y
        sta (dstrow),y
        iny

        lda t
        sta (dstrow),y

        ; update srcrow and dstrow. remember that srcrow is
        ; already pointing one byte past the actual start of
        ; the row.
        clc
        lda srcrow
        adc #39
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
        bne cpyrow
        rts


