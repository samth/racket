;; nbody-sbcl-2, SBCL 2.6.0.debian, compiled with the program's own declarations

;;; ADVANCE
; disassembly for ADVANCE
; Size: 460 bytes. Origin: #xB800B1426F                       ; ADVANCE
; 26F:       488BDE           MOV RBX, RSI
; 272:       0F1F8000000000   NOP
; 279:       0F1F8000000000   NOP
; 280: L0:   4C39E3           CMP RBX, R12                    ; NIL
; 283:       0F8446010000     JE L3
; 289:       8D43F9           LEA EAX, [RBX-7]
; 28C:       A80F             TEST AL, 15
; 28E:       0F853B010000     JNE L3
; 294:       488BC3           MOV RAX, RBX
; 297:       488B48F9         MOV RCX, [RAX-7]
; 29B:       488B4001         MOV RAX, [RAX+1]
; 29F:       488B5B01         MOV RBX, [RBX+1]
; 2A3:       488BD0           MOV RDX, RAX
; 2A6:       E916010000       JMP L2
; 2AB:       0F1F440000       NOP
; 2B0: L1:   488B42F9         MOV RAX, [RDX-7]
; 2B4:       488B5201         MOV RDX, [RDX+1]
; 2B8:       F20F106901       MOVSD XMM5, [RCX+1]
; 2BD:       F20F105001       MOVSD XMM2, [RAX+1]
; 2C2:       F20F5CEA         SUBSD XMM5, XMM2
; 2C6:       F20F107109       MOVSD XMM6, [RCX+9]
; 2CB:       F20F105009       MOVSD XMM2, [RAX+9]
; 2D0:       F20F5CF2         SUBSD XMM6, XMM2
; 2D4:       F20F105911       MOVSD XMM3, [RCX+17]
; 2D9:       F20F105011       MOVSD XMM2, [RAX+17]
; 2DE:       660F28CB         MOVAPD XMM1, XMM3
; 2E2:       F20F5CCA         SUBSD XMM1, XMM2
; 2E6:       660F28DD         MOVAPD XMM3, XMM5
; 2EA:       F20F59DD         MULSD XMM3, XMM5
; 2EE:       660F28E6         MOVAPD XMM4, XMM6
; 2F2:       F20F59E6         MULSD XMM4, XMM6
; 2F6:       F20F58E3         ADDSD XMM4, XMM3
; 2FA:       660F28D9         MOVAPD XMM3, XMM1
; 2FE:       F20F59D9         MULSD XMM3, XMM1
; 302:       F20F58DC         ADDSD XMM3, XMM4
; 306:       660F57D2         XORPD XMM2, XMM2
; 30A:       F20F51D3         SQRTSD XMM2, XMM3
; 30E:       660F28DA         MOVAPD XMM3, XMM2
; 312:       F20F59DA         MULSD XMM3, XMM2
; 316:       F20F59D3         MULSD XMM2, XMM3
; 31A:       66410F28F8       MOVAPD XMM7, XMM8
; 31F:       F20F5EFA         DIVSD XMM7, XMM2
; 323:       660F28DD         MOVAPD XMM3, XMM5
; 327:       F20F59DF         MULSD XMM3, XMM7
; 32B:       660F28E6         MOVAPD XMM4, XMM6
; 32F:       F20F59E7         MULSD XMM4, XMM7
; 333:       F20F59F9         MULSD XMM7, XMM1
; 337:       F20F105031       MOVSD XMM2, [RAX+49]
; 33C:       F20F59D3         MULSD XMM2, XMM3
; 340:       F20F104919       MOVSD XMM1, [RCX+25]
; 345:       F20F5CCA         SUBSD XMM1, XMM2
; 349:       F20F114919       MOVSD [RCX+25], XMM1
; 34E:       F20F105031       MOVSD XMM2, [RAX+49]
; 353:       F20F59D4         MULSD XMM2, XMM4
; 357:       F20F104921       MOVSD XMM1, [RCX+33]
; 35C:       F20F5CCA         SUBSD XMM1, XMM2
; 360:       F20F114921       MOVSD [RCX+33], XMM1
; 365:       F20F105031       MOVSD XMM2, [RAX+49]
; 36A:       F20F59D7         MULSD XMM2, XMM7
; 36E:       F20F104929       MOVSD XMM1, [RCX+41]
; 373:       F20F5CCA         SUBSD XMM1, XMM2
; 377:       F20F114929       MOVSD [RCX+41], XMM1
; 37C:       F20F104931       MOVSD XMM1, [RCX+49]
; 381:       F20F59CB         MULSD XMM1, XMM3
; 385:       F20F105019       MOVSD XMM2, [RAX+25]
; 38A:       F20F58D1         ADDSD XMM2, XMM1
; 38E:       F20F115019       MOVSD [RAX+25], XMM2
; 393:       F20F104931       MOVSD XMM1, [RCX+49]
; 398:       F20F59CC         MULSD XMM1, XMM4
; 39C:       F20F105021       MOVSD XMM2, [RAX+33]
; 3A1:       F20F58D1         ADDSD XMM2, XMM1
; 3A5:       F20F115021       MOVSD [RAX+33], XMM2
; 3AA:       F20F104931       MOVSD XMM1, [RCX+49]
; 3AF:       F20F59CF         MULSD XMM1, XMM7
; 3B3:       F20F105029       MOVSD XMM2, [RAX+41]
; 3B8:       F20F58D1         ADDSD XMM2, XMM1
; 3BC:       F20F115029       MOVSD [RAX+41], XMM2
; 3C1: L2:   4C39E2           CMP RDX, R12                    ; NIL
; 3C4:       0F84B6FEFFFF     JE L0
; 3CA:       E9E1FEFFFF       JMP L1
; 3CF: L3:   488BCE           MOV RCX, RSI
; 3D2:       EB5C             JMP L5
; 3D4:       660F1F440000     NOP
; 3DA:       660F1F440000     NOP
; 3E0: L4:   488B41F9         MOV RAX, [RCX-7]
; 3E4:       488B4901         MOV RCX, [RCX+1]
; 3E8:       F20F104819       MOVSD XMM1, [RAX+25]
; 3ED:       F2410F59C8       MULSD XMM1, XMM8
; 3F2:       F20F105001       MOVSD XMM2, [RAX+1]
; 3F7:       F20F58D1         ADDSD XMM2, XMM1
; 3FB:       F20F115001       MOVSD [RAX+1], XMM2
; 400:       F20F104821       MOVSD XMM1, [RAX+33]
; 405:       F2410F59C8       MULSD XMM1, XMM8
; 40A:       F20F105009       MOVSD XMM2, [RAX+9]
; 40F:       F20F58D1         ADDSD XMM2, XMM1
; 413:       F20F115009       MOVSD [RAX+9], XMM2
; 418:       F20F104829       MOVSD XMM1, [RAX+41]
; 41D:       F2410F59C8       MULSD XMM1, XMM8
; 422:       F20F105011       MOVSD XMM2, [RAX+17]
; 427:       F20F58D1         ADDSD XMM2, XMM1
; 42B:       F20F115011       MOVSD [RAX+17], XMM2
; 430: L5:   4C39E1           CMP RCX, R12                    ; NIL
; 433:       75AB             JNE L4
; 435:       488BD1           MOV RDX, RCX
; 438:       C9               LEAVE
; 439:       F8               CLC
; 43A:       C3               RET
