
;;; EVAL-A-TIMES-U
; disassembly for EVAL-A-TIMES-U
; Size: 282 bytes. Origin: #xB8000487BC                       ; EVAL-A-TIMES-U
; 7BC:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 7C0:       488945F8         MOV [RBP-8], RAX
; 7C4:       4585F6           TEST R14D, R14D
; 7C7:       0F88F0000000     JS L7
; 7CD:       498BD6           MOV RDX, R14
; 7D0:       4C8D0C951F000000 LEA R9, [RDX*4+31]
; 7D8:       4983E1F0         AND R9, -16
; 7DC:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 7E0:       498B4570         MOV RAX, [R13+112]              ; thread.mixed-tlab
; 7E4:       4C0FC1C8         XADD RAX, R9
; 7E8:       493B4578         CMP RAX, [R13+120]
; 7EC:       0F87D0000000     JA L8
; 7F2:       49894570         MOV [R13+112], RAX              ; thread.mixed-tlab
; 7F6: L0:   41C601D5         MOV BYTE PTR [R9], -43
; 7FA:       49895108         MOV [R9+8], RDX
; 7FE:       4180C90F         OR R9B, 15
; 802:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 806:       7402             JE L1
; 808:       0F0B             UD2
; 80A: L1:   4531C0           XOR R8D, R8D
; 80D:       E996000000       JMP L5
; 812:       0F1F8000000000   NOP
; 819:       0F1F8000000000   NOP
; 820: L2:   31F6             XOR ESI, ESI
; 822:       EB78             JMP L4
; 824:       660F1F440000     NOP
; 82A:       660F1F440000     NOP
; 830: L3:   4D8D1C30         LEA R11, [R8+RSI]
; 834:       4D8D1430         LEA R10, [R8+RSI]
; 838:       4983C202         ADD R10, 2
; 83C:       49D1FA           SAR R10, 1
; 83F:       4D0FAFD3         IMUL R10, R11
; 843:       49D1FA           SAR R10, 1
; 846:       4983E2FE         AND R10, -2
; 84A:       4D01C2           ADD R10, R8
; 84D:       4983C202         ADD R10, 2
; 851:       498BC2           MOV RAX, R10
; 854:       48D1F8           SAR RAX, 1
; 857:       660F57C9         XORPD XMM1, XMM1
; 85B:       F2480F2AC8       CVTSI2SD XMM1, RAX
; 860:       F20F1005D8FEFFFF MOVSD XMM0, [RIP-296]           ; [#xB800048740]
; 868:       F20F5EC1         DIVSD XMM0, XMM1
; 86C:       660F28C8         MOVAPD XMM1, XMM0
; 870:       807FF200         CMP BYTE PTR [RDI-14], 0
; 874:       7544             JNE L6
; 876:       483B77F9         CMP RSI, [RDI-7]
; 87A:       7356             JAE L9
; 87C:       F20F1054B701     MOVSD XMM2, [RDI+RSI*4+1]
; 882:       F20F59D1         MULSD XMM2, XMM1
; 886:       F2430F104C8101   MOVSD XMM1, [R9+R8*4+1]
; 88D:       F20F58CA         ADDSD XMM1, XMM2
; 891:       F2430F114C8101   MOVSD [R9+R8*4+1], XMM1
; 898:       4883C602         ADD RSI, 2
; 89C: L4:   498BD6           MOV RDX, R14
; 89F:       4839D6           CMP RSI, RDX
; 8A2:       7C8C             JL L3
; 8A4:       4983C002         ADD R8, 2
; 8A8: L5:   498BD6           MOV RDX, R14
; 8AB:       4939D0           CMP R8, RDX
; 8AE:       0F8C6CFFFFFF     JL L2
; 8B4:       498BD1           MOV RDX, R9
; 8B7:       C9               LEAVE
; 8B8:       F8               CLC
; 8B9:       C3               RET
; 8BA: L6:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; 8BC:       1C               BYTE #X1C                       ; RDI(d)
; 8BD: L7:   CCAF             INT3 175                        ; OBJECT-NOT-INDEX-OR-LIST-ERROR
; 8BF:       39               BYTE #X39                       ; R14(a)
; 8C0:       CC0F             INT3 15                         ; Invalid argument count trap
; 8C2: L8:   4C29C8           SUB RAX, R9
; 8C5:       50               PUSH RAX
; 8C6:       E8B57CFBFF       CALL #xB800000580               ; SB-VM::ALLOC-TRAMP
; 8CB:       4159             POP R9
; 8CD:       E924FFFFFF       JMP L0
; 8D2: L9:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 8D4:       1C               BYTE #X1C                       ; RDI(d)
; 8D5:       19               BYTE #X19                       ; RSI(a)

;;; EVAL-AT-TIMES-U
; disassembly for EVAL-AT-TIMES-U
; Size: 282 bytes. Origin: #xB80004899C                       ; EVAL-AT-TIMES-U
; 99C:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 9A0:       488945F8         MOV [RBP-8], RAX
; 9A4:       4585F6           TEST R14D, R14D
; 9A7:       0F88F0000000     JS L7
; 9AD:       498BD6           MOV RDX, R14
; 9B0:       4C8D0C951F000000 LEA R9, [RDX*4+31]
; 9B8:       4983E1F0         AND R9, -16
; 9BC:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 9C0:       498B4570         MOV RAX, [R13+112]              ; thread.mixed-tlab
; 9C4:       4C0FC1C8         XADD RAX, R9
; 9C8:       493B4578         CMP RAX, [R13+120]
; 9CC:       0F87D0000000     JA L8
; 9D2:       49894570         MOV [R13+112], RAX              ; thread.mixed-tlab
; 9D6: L0:   41C601D5         MOV BYTE PTR [R9], -43
; 9DA:       49895108         MOV [R9+8], RDX
; 9DE:       4180C90F         OR R9B, 15
; 9E2:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 9E6:       7402             JE L1
; 9E8:       0F0B             UD2
; 9EA: L1:   4531C0           XOR R8D, R8D
; 9ED:       E996000000       JMP L5
; 9F2:       0F1F8000000000   NOP
; 9F9:       0F1F8000000000   NOP
; A00: L2:   31F6             XOR ESI, ESI
; A02:       EB78             JMP L4
; A04:       660F1F440000     NOP
; A0A:       660F1F440000     NOP
; A10: L3:   4E8D1C06         LEA R11, [RSI+R8]
; A14:       4E8D1406         LEA R10, [RSI+R8]
; A18:       4983C202         ADD R10, 2
; A1C:       49D1FA           SAR R10, 1
; A1F:       4D0FAFD3         IMUL R10, R11
; A23:       49D1FA           SAR R10, 1
; A26:       4983E2FE         AND R10, -2
; A2A:       4901F2           ADD R10, RSI
; A2D:       4983C202         ADD R10, 2
; A31:       498BC2           MOV RAX, R10
; A34:       48D1F8           SAR RAX, 1
; A37:       660F57C9         XORPD XMM1, XMM1
; A3B:       F2480F2AC8       CVTSI2SD XMM1, RAX
; A40:       F20F1005D8FEFFFF MOVSD XMM0, [RIP-296]           ; [#xB800048920]
; A48:       F20F5EC1         DIVSD XMM0, XMM1
; A4C:       660F28C8         MOVAPD XMM1, XMM0
; A50:       807FF200         CMP BYTE PTR [RDI-14], 0
; A54:       7544             JNE L6
; A56:       483B77F9         CMP RSI, [RDI-7]
; A5A:       7356             JAE L9
; A5C:       F20F1054B701     MOVSD XMM2, [RDI+RSI*4+1]
; A62:       F20F59D1         MULSD XMM2, XMM1
; A66:       F2430F104C8101   MOVSD XMM1, [R9+R8*4+1]
; A6D:       F20F58CA         ADDSD XMM1, XMM2
; A71:       F2430F114C8101   MOVSD [R9+R8*4+1], XMM1
; A78:       4883C602         ADD RSI, 2
; A7C: L4:   498BD6           MOV RDX, R14
; A7F:       4839D6           CMP RSI, RDX
; A82:       7C8C             JL L3
; A84:       4983C002         ADD R8, 2
; A88: L5:   498BD6           MOV RDX, R14
; A8B:       4939D0           CMP R8, RDX
; A8E:       0F8C6CFFFFFF     JL L2
; A94:       498BD1           MOV RDX, R9
; A97:       C9               LEAVE
; A98:       F8               CLC
; A99:       C3               RET
; A9A: L6:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; A9C:       1C               BYTE #X1C                       ; RDI(d)
; A9D: L7:   CCAF             INT3 175                        ; OBJECT-NOT-INDEX-OR-LIST-ERROR
; A9F:       39               BYTE #X39                       ; R14(a)
; AA0:       CC0F             INT3 15                         ; Invalid argument count trap
; AA2: L8:   4C29C8           SUB RAX, R9
; AA5:       50               PUSH RAX
; AA6:       E8D57AFBFF       CALL #xB800000580               ; SB-VM::ALLOC-TRAMP
; AAB:       4159             POP R9
; AAD:       E924FFFFFF       JMP L0
; AB2: L9:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; AB4:       1C               BYTE #X1C                       ; RDI(d)
; AB5:       19               BYTE #X19                       ; RSI(a)
