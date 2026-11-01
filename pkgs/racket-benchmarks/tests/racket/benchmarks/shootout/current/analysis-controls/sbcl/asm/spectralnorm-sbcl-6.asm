;; spectralnorm-sbcl-6, SBCL 2.6.0.debian, compiled with the program's own declarations

;;; EVAL-A-TIMES-U
; disassembly for EVAL-A-TIMES-U
; Size: 282 bytes. Origin: #xB800B1366C                       ; EVAL-A-TIMES-U
; 66C:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 670:       488945F8         MOV [RBP-8], RAX
; 674:       4585F6           TEST R14D, R14D
; 677:       0F88F0000000     JS L7
; 67D:       498BD6           MOV RDX, R14
; 680:       4C8D0C951F000000 LEA R9, [RDX*4+31]
; 688:       4983E1F0         AND R9, -16
; 68C:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 690:       498B4570         MOV RAX, [R13+112]              ; thread.mixed-tlab
; 694:       4C0FC1C8         XADD RAX, R9
; 698:       493B4578         CMP RAX, [R13+120]
; 69C:       0F87D0000000     JA L8
; 6A2:       49894570         MOV [R13+112], RAX              ; thread.mixed-tlab
; 6A6: L0:   41C601D5         MOV BYTE PTR [R9], -43
; 6AA:       49895108         MOV [R9+8], RDX
; 6AE:       4180C90F         OR R9B, 15
; 6B2:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 6B6:       7402             JE L1
; 6B8:       0F0B             UD2
; 6BA: L1:   4531C0           XOR R8D, R8D
; 6BD:       E996000000       JMP L5
; 6C2:       0F1F8000000000   NOP
; 6C9:       0F1F8000000000   NOP
; 6D0: L2:   31F6             XOR ESI, ESI
; 6D2:       EB78             JMP L4
; 6D4:       660F1F440000     NOP
; 6DA:       660F1F440000     NOP
; 6E0: L3:   4D8D1C30         LEA R11, [R8+RSI]
; 6E4:       4D8D1430         LEA R10, [R8+RSI]
; 6E8:       4983C202         ADD R10, 2
; 6EC:       49D1FA           SAR R10, 1
; 6EF:       4D0FAFD3         IMUL R10, R11
; 6F3:       49D1FA           SAR R10, 1
; 6F6:       4983E2FE         AND R10, -2
; 6FA:       4D01C2           ADD R10, R8
; 6FD:       4983C202         ADD R10, 2
; 701:       498BC2           MOV RAX, R10
; 704:       48D1F8           SAR RAX, 1
; 707:       660F57C9         XORPD XMM1, XMM1
; 70B:       F2480F2AC8       CVTSI2SD XMM1, RAX
; 710:       F20F1005D8FEFFFF MOVSD XMM0, [RIP-296]           ; [#xB800B135F0]
; 718:       F20F5EC1         DIVSD XMM0, XMM1
; 71C:       660F28C8         MOVAPD XMM1, XMM0
; 720:       807FF200         CMP BYTE PTR [RDI-14], 0
; 724:       7544             JNE L6
; 726:       483B77F9         CMP RSI, [RDI-7]
; 72A:       7356             JAE L9
; 72C:       F20F1054B701     MOVSD XMM2, [RDI+RSI*4+1]
; 732:       F20F59D1         MULSD XMM2, XMM1
; 736:       F2430F104C8101   MOVSD XMM1, [R9+R8*4+1]
; 73D:       F20F58CA         ADDSD XMM1, XMM2
; 741:       F2430F114C8101   MOVSD [R9+R8*4+1], XMM1
; 748:       4883C602         ADD RSI, 2
; 74C: L4:   498BD6           MOV RDX, R14
; 74F:       4839D6           CMP RSI, RDX
; 752:       7C8C             JL L3
; 754:       4983C002         ADD R8, 2
; 758: L5:   498BD6           MOV RDX, R14
; 75B:       4939D0           CMP R8, RDX
; 75E:       0F8C6CFFFFFF     JL L2
; 764:       498BD1           MOV RDX, R9
; 767:       C9               LEAVE
; 768:       F8               CLC
; 769:       C3               RET
; 76A: L6:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; 76C:       1C               BYTE #X1C                       ; RDI(d)
; 76D: L7:   CCAF             INT3 175                        ; OBJECT-NOT-INDEX-OR-LIST-ERROR
; 76F:       39               BYTE #X39                       ; R14(a)
; 770:       CC0F             INT3 15                         ; Invalid argument count trap
; 772: L8:   4C29C8           SUB RAX, R9
; 775:       50               PUSH RAX
; 776:       E805CE4EFF       CALL #xB800000580               ; SB-VM::ALLOC-TRAMP
; 77B:       4159             POP R9
; 77D:       E924FFFFFF       JMP L0
; 782: L9:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 784:       1C               BYTE #X1C                       ; RDI(d)
; 785:       19               BYTE #X19                       ; RSI(a)

;;; EVAL-AT-TIMES-U
; disassembly for EVAL-AT-TIMES-U
; Size: 282 bytes. Origin: #xB800B1384C                       ; EVAL-AT-TIMES-U
; 84C:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 850:       488945F8         MOV [RBP-8], RAX
; 854:       4585F6           TEST R14D, R14D
; 857:       0F88F0000000     JS L7
; 85D:       498BD6           MOV RDX, R14
; 860:       4C8D0C951F000000 LEA R9, [RDX*4+31]
; 868:       4983E1F0         AND R9, -16
; 86C:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 870:       498B4570         MOV RAX, [R13+112]              ; thread.mixed-tlab
; 874:       4C0FC1C8         XADD RAX, R9
; 878:       493B4578         CMP RAX, [R13+120]
; 87C:       0F87D0000000     JA L8
; 882:       49894570         MOV [R13+112], RAX              ; thread.mixed-tlab
; 886: L0:   41C601D5         MOV BYTE PTR [R9], -43
; 88A:       49895108         MOV [R9+8], RDX
; 88E:       4180C90F         OR R9B, 15
; 892:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 896:       7402             JE L1
; 898:       0F0B             UD2
; 89A: L1:   4531C0           XOR R8D, R8D
; 89D:       E996000000       JMP L5
; 8A2:       0F1F8000000000   NOP
; 8A9:       0F1F8000000000   NOP
; 8B0: L2:   31F6             XOR ESI, ESI
; 8B2:       EB78             JMP L4
; 8B4:       660F1F440000     NOP
; 8BA:       660F1F440000     NOP
; 8C0: L3:   4E8D1C06         LEA R11, [RSI+R8]
; 8C4:       4E8D1406         LEA R10, [RSI+R8]
; 8C8:       4983C202         ADD R10, 2
; 8CC:       49D1FA           SAR R10, 1
; 8CF:       4D0FAFD3         IMUL R10, R11
; 8D3:       49D1FA           SAR R10, 1
; 8D6:       4983E2FE         AND R10, -2
; 8DA:       4901F2           ADD R10, RSI
; 8DD:       4983C202         ADD R10, 2
; 8E1:       498BC2           MOV RAX, R10
; 8E4:       48D1F8           SAR RAX, 1
; 8E7:       660F57C9         XORPD XMM1, XMM1
; 8EB:       F2480F2AC8       CVTSI2SD XMM1, RAX
; 8F0:       F20F1005D8FEFFFF MOVSD XMM0, [RIP-296]           ; [#xB800B137D0]
; 8F8:       F20F5EC1         DIVSD XMM0, XMM1
; 8FC:       660F28C8         MOVAPD XMM1, XMM0
; 900:       807FF200         CMP BYTE PTR [RDI-14], 0
; 904:       7544             JNE L6
; 906:       483B77F9         CMP RSI, [RDI-7]
; 90A:       7356             JAE L9
; 90C:       F20F1054B701     MOVSD XMM2, [RDI+RSI*4+1]
; 912:       F20F59D1         MULSD XMM2, XMM1
; 916:       F2430F104C8101   MOVSD XMM1, [R9+R8*4+1]
; 91D:       F20F58CA         ADDSD XMM1, XMM2
; 921:       F2430F114C8101   MOVSD [R9+R8*4+1], XMM1
; 928:       4883C602         ADD RSI, 2
; 92C: L4:   498BD6           MOV RDX, R14
; 92F:       4839D6           CMP RSI, RDX
; 932:       7C8C             JL L3
; 934:       4983C002         ADD R8, 2
; 938: L5:   498BD6           MOV RDX, R14
; 93B:       4939D0           CMP R8, RDX
; 93E:       0F8C6CFFFFFF     JL L2
; 944:       498BD1           MOV RDX, R9
; 947:       C9               LEAVE
; 948:       F8               CLC
; 949:       C3               RET
; 94A: L6:   CC5E             INT3 94                         ; OBJECT-NOT-VECTOR-ERROR
; 94C:       1C               BYTE #X1C                       ; RDI(d)
; 94D: L7:   CCAF             INT3 175                        ; OBJECT-NOT-INDEX-OR-LIST-ERROR
; 94F:       39               BYTE #X39                       ; R14(a)
; 950:       CC0F             INT3 15                         ; Invalid argument count trap
; 952: L8:   4C29C8           SUB RAX, R9
; 955:       50               PUSH RAX
; 956:       E825CC4EFF       CALL #xB800000580               ; SB-VM::ALLOC-TRAMP
; 95B:       4159             POP R9
; 95D:       E924FFFFFF       JMP L0
; 962: L9:   CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 964:       1C               BYTE #X1C                       ; RDI(d)
; 965:       19               BYTE #X19                       ; RSI(a)
