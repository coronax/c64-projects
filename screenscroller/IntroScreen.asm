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



*=$7000
; Screen 1 - Colour data
        BYTE        $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
        BYTE        $00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$00,$00,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00,$02,$00,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$03
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$02,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$02,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$00,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$00,$00,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0F,$01,$00
        BYTE        $00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00
        BYTE        $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00

*=$7400
; Screen 1 - 
        BYTE        $20,$70,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$6E,$20
        BYTE        $20,$42,$6C,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$7B,$42,$20
        BYTE        $20,$42,$E1,$EC,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$FB,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$20,$20,$4E,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$4D,$20,$20,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$20,$4E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$4D,$20,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$4E,$20,$20,$20,$20,$20,$20,$20,$20,$13,$03,$12,$0F,$0C,$0C,$05,$12,$20,$20,$04,$05,$0D,$0F,$20,$20,$20,$20,$20,$20,$20,$20,$4D,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$15,$13,$05,$20,$22,$0C,$22,$20,$01,$0E,$04,$20,$22,$12,$22,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$14,$0F,$20,$03,$08,$01,$0E,$07,$05,$20,$13,$10,$05,$05,$04,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$20,$55,$43,$43,$49,$20,$20,$55,$43,$43,$49,$20,$20,$55,$43,$43,$49,$20,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$55,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$49,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$42,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$42,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$42,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$20,$20,$42,$42,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$74,$20,$20,$20,$20,$20,$20,$4A,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$43,$43,$5B,$4B,$20,$20,$20,$20,$20,$20,$6A,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$4D,$20,$20,$20,$20,$20,$20,$20,$4A,$43,$43,$4B,$20,$20,$4A,$43,$43,$4B,$20,$20,$4A,$43,$43,$4B,$20,$20,$20,$20,$20,$20,$20,$4E,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$20,$4D,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$4E,$20,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$61,$20,$20,$4D,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$6F,$4E,$20,$20,$E1,$61,$42,$20
        BYTE        $20,$42,$E1,$FC,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$62,$FE,$61,$42,$20
        BYTE        $20,$42,$7C,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$7E,$42,$20
        BYTE        $20,$6D,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$7D,$20
