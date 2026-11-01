;; spectralnorm-sbcl-3, SBCL 2.6.0.debian, compiled with the program's own declarations

;;; EVAL-A-TIMES-U
; disassembly for EVAL-A-TIMES-U
; Size: 254 bytes. Origin: #xB800B13C76                       ; EVAL-A-TIMES-U
; C76:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; C7A:       488945F8         MOV [RBP-8], RAX
; C7E:       4C8B55F0         MOV R10, [RBP-16]
; C82:       E9C0000000       JMP L3
; C87:       660F1F840000000000 NOP
; C90: L0:   498BCA           MOV RCX, R10
; C93:       4531C9           XOR R9D, R9D
; C96:       660F57C9         XORPD XMM1, XMM1
; C9A:       E989000000       JMP L2
; C9F:       90               NOP
; CA0: L1:   41807BF200       CMP BYTE PTR [R11-14], 0
; CA5:       0F85B9000000     JNE L7
; CAB:       4D3B4BF9         CMP R9, [R11-7]
; CAF:       0F83B4000000     JAE L8
; CB5:       F2430F105C8B01   MOVSD XMM3, [R11+R9*4+1]
; CBC:       4D8BF2           MOV R14, R10
; CBF:       4D01CE           ADD R14, R9
; CC2:       0F80A5000000     JO L9
; CC8:       4981FEE0AB0000   CMP R14, 44000
; CCF:       0F878B000000     JA L6
; CD5:       4D8D4602         LEA R8, [R14+2]
; CD9:       4D8BF8           MOV R15, R8
; CDC:       49D1FF           SAR R15, 1
; CDF:       4981FFF1550000   CMP R15, 22001
; CE6:       7471             JE L5
; CE8:       49D1F8           SAR R8, 1
; CEB:       4D0FAFC6         IMUL R8, R14
; CEF:       49D1F8           SAR R8, 1
; CF2:       4983E0FE         AND R8, -2
; CF6:       4D01D0           ADD R8, R10
; CF9:       4983C002         ADD R8, 2
; CFD:       498BC0           MOV RAX, R8
; D00:       48D1F8           SAR RAX, 1
; D03:       660F57D2         XORPD XMM2, XMM2
; D07:       F2480F2AD0       CVTSI2SD XMM2, RAX
; D0C:       F20F100594FEFFFF MOVSD XMM0, [RIP-364]           ; [#xB800B13BA8]
; D14:       F20F5EC2         DIVSD XMM0, XMM2
; D18:       660F28D0         MOVAPD XMM2, XMM0
; D1C:       F20F59D3         MULSD XMM2, XMM3
; D20:       F20F58CA         ADDSD XMM1, XMM2
; D24:       4983C102         ADD R9, 2
; D28: L2:   4939F9           CMP R9, RDI
; D2B:       0F8C6FFFFFFF     JL L1
; D31:       807EF200         CMP BYTE PTR [RSI-14], 0
; D35:       751F             JNE L4
; D37:       483B4EF9         CMP RCX, [RSI-7]
; D3B:       7333             JAE L10
; D3D:       F20F114C8E01     MOVSD [RSI+RCX*4+1], XMM1
; D43:       4983C202         ADD R10, 2
; D47: L3:   4939DA           CMP R10, RBX
; D4A:       0F8C40FFFFFF     JL L0
; D50:       498BD4           MOV RDX, R12                    ; NIL
; D53:       C9               LEAVE
; D54:       F8               CLC
; D55:       C3               RET
; D56: L4:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; D58:       18               BYTE #X18                       ; RSI(d)
; D59: L5:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; D5B:       C6AF8510         BYTE #XC6, #XAF, #X85, #X10     ; 22001
; D5F:       17               BYTE #X17                       ; '(MOD 22001)
; D60: L6:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; D62:       39               BYTE #X39                       ; R14(a)
; D63:       17               BYTE #X17                       ; '(MOD 22001)
; D64: L7:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; D66:       2C               BYTE #X2C                       ; R11(d)
; D67:       CC0F             INT3 15                         ; Invalid argument count trap
; D69: L8:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; D6B:       2C               BYTE #X2C                       ; R11(d)
; D6C:       25               BYTE #X25                       ; R9(a)
; D6D: L9:   CC2E             INT3 46                         ; ADD-SUB-OVERFLOW-ERROR
; D6F:       39               BYTE #X39                       ; R14(a)
; D70: L10:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; D72:       18               BYTE #X18                       ; RSI(d)
; D73:       05               BYTE #X05                       ; RCX(a)

;;; EVAL-AT-TIMES-U
; disassembly for EVAL-AT-TIMES-U
; Size: 254 bytes. Origin: #xB800B13A66                       ; EVAL-AT-TIMES-U
; A66:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; A6A:       488945F8         MOV [RBP-8], RAX
; A6E:       4C8B55F0         MOV R10, [RBP-16]
; A72:       E9C0000000       JMP L3
; A77:       660F1F840000000000 NOP
; A80: L0:   498BCA           MOV RCX, R10
; A83:       4531C9           XOR R9D, R9D
; A86:       660F57C9         XORPD XMM1, XMM1
; A8A:       E989000000       JMP L2
; A8F:       90               NOP
; A90: L1:   41807BF200       CMP BYTE PTR [R11-14], 0
; A95:       0F85B9000000     JNE L7
; A9B:       4D3B4BF9         CMP R9, [R11-7]
; A9F:       0F83B4000000     JAE L8
; AA5:       F2430F105C8B01   MOVSD XMM3, [R11+R9*4+1]
; AAC:       4D8BF1           MOV R14, R9
; AAF:       4D01D6           ADD R14, R10
; AB2:       0F80A5000000     JO L9
; AB8:       4981FEE0AB0000   CMP R14, 44000
; ABF:       0F878B000000     JA L6
; AC5:       4D8D4602         LEA R8, [R14+2]
; AC9:       4D8BF8           MOV R15, R8
; ACC:       49D1FF           SAR R15, 1
; ACF:       4981FFF1550000   CMP R15, 22001
; AD6:       7471             JE L5
; AD8:       49D1F8           SAR R8, 1
; ADB:       4D0FAFC6         IMUL R8, R14
; ADF:       49D1F8           SAR R8, 1
; AE2:       4983E0FE         AND R8, -2
; AE6:       4D01C8           ADD R8, R9
; AE9:       4983C002         ADD R8, 2
; AED:       498BC0           MOV RAX, R8
; AF0:       48D1F8           SAR RAX, 1
; AF3:       660F57D2         XORPD XMM2, XMM2
; AF7:       F2480F2AD0       CVTSI2SD XMM2, RAX
; AFC:       F20F100594FEFFFF MOVSD XMM0, [RIP-364]           ; [#xB800B13998]
; B04:       F20F5EC2         DIVSD XMM0, XMM2
; B08:       660F28D0         MOVAPD XMM2, XMM0
; B0C:       F20F59D3         MULSD XMM2, XMM3
; B10:       F20F58CA         ADDSD XMM1, XMM2
; B14:       4983C102         ADD R9, 2
; B18: L2:   4939F9           CMP R9, RDI
; B1B:       0F8C6FFFFFFF     JL L1
; B21:       807EF200         CMP BYTE PTR [RSI-14], 0
; B25:       751F             JNE L4
; B27:       483B4EF9         CMP RCX, [RSI-7]
; B2B:       7333             JAE L10
; B2D:       F20F114C8E01     MOVSD [RSI+RCX*4+1], XMM1
; B33:       4983C202         ADD R10, 2
; B37: L3:   4939DA           CMP R10, RBX
; B3A:       0F8C40FFFFFF     JL L0
; B40:       498BD4           MOV RDX, R12                    ; NIL
; B43:       C9               LEAVE
; B44:       F8               CLC
; B45:       C3               RET
; B46: L4:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; B48:       18               BYTE #X18                       ; RSI(d)
; B49: L5:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; B4B:       C6AF8510         BYTE #XC6, #XAF, #X85, #X10     ; 22001
; B4F:       17               BYTE #X17                       ; '(MOD 22001)
; B50: L6:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; B52:       39               BYTE #X39                       ; R14(a)
; B53:       17               BYTE #X17                       ; '(MOD 22001)
; B54: L7:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; B56:       2C               BYTE #X2C                       ; R11(d)
; B57:       CC0F             INT3 15                         ; Invalid argument count trap
; B59: L8:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; B5B:       2C               BYTE #X2C                       ; R11(d)
; B5C:       25               BYTE #X25                       ; R9(a)
; B5D: L9:   CC2E             INT3 46                         ; ADD-SUB-OVERFLOW-ERROR
; B5F:       39               BYTE #X39                       ; R14(a)
; B60: L10:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; B62:       18               BYTE #X18                       ; RSI(d)
; B63:       05               BYTE #X05                       ; RCX(a)
