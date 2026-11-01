;; nbody-sbcl-1, SBCL 2.6.0.debian, compiled with the program's own declarations

;;; ADVANCE
; disassembly for ADVANCE
; Size: 885 bytes. Origin: #xB800B141AA                       ; ADVANCE
; 1AA:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 1AE:       488945F8         MOV [RBP-8], RAX
; 1B2:       488BCB           MOV RCX, RBX
; 1B5:       660F1F440000     NOP
; 1BB:       0F1F440000       NOP
; 1C0: L0:   4C39E1           CMP RCX, R12                    ; NIL
; 1C3:       0F84A4020000     JE L13
; 1C9:       8D41F9           LEA EAX, [RCX-7]
; 1CC:       A80F             TEST AL, 15
; 1CE:       0F8599020000     JNE L13
; 1D4:       488BC1           MOV RAX, RCX
; 1D7:       4C8B50F9         MOV R10, [RAX-7]
; 1DB:       488B4001         MOV RAX, [RAX+1]
; 1DF:       488B4901         MOV RCX, [RCX+1]
; 1E3:       4C8BD8           MOV R11, RAX
; 1E6:       E941020000       JMP L8
; 1EB:       0F1F440000       NOP
; 1F0: L1:   4D39E3           CMP R11, R12                    ; NIL
; 1F3:       74CB             JE L0
; 1F5:       4D8B4BF9         MOV R9, [R11-7]
; 1F9:       4D8B5B01         MOV R11, [R11+1]
; 1FD:       4D8BFA           MOV R15, R10
; 200:       418D42F1         LEA EAX, [R10-15]
; 204:       A80F             TEST AL, 15
; 206:       756A             JNE L3
; 208:       410FB672F1       MOVZX ESI, BYTE PTR [R10-15]
; 20D:       4C8BC6           MOV R8, RSI
; 210:       4981F8D5000000   CMP R8, 213
; 217:       0F843A020000     JE L12
; 21D:       4881FEF5000000   CMP RSI, 245
; 224:       754C             JNE L3
; 226:       498B7229         MOV RSI, [R10+41]
; 22A:       48D1FE           SAR RSI, 1
; 22D:       4883FE07         CMP RSI, 7
; 231:       753F             JNE L3
; 233:       4D8BC2           MOV R8, R10
; 236:       0F1F440000       NOP
; 23B:       0F1F440000       NOP
; 240: L2:   4D8B4009         MOV R8, [R8+9]
; 244:       410FB670F1       MOVZX ESI, BYTE PTR [R8-15]
; 249:       D1E6             SHL ESI, 1
; 24B:       4C8BF6           MOV R14, RSI
; 24E:       49D1FE           SAR R14, 1
; 251:       4981FED5000000   CMP R14, 213
; 258:       741C             JE L4
; 25A:       4C8BF6           MOV R14, RSI
; 25D:       49D1FE           SAR R14, 1
; 260:       4981FE81000000   CMP R14, 129
; 267:       74D7             JE L2
; 269:       4881FEEA010000   CMP RSI, 490
; 270:       7DCE             JGE L2
; 272: L3:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; 274:       3C               BYTE #X3C                       ; R15(d)
; 275:       13               BYTE #X13                       ; '(VECTOR DOUBLE-FLOAT 7)
; 276: L4:   4D8BC7           MOV R8, R15
; 279:       498BF1           MOV RSI, R9
; 27C:       418D41F1         LEA EAX, [R9-15]
; 280:       A80F             TEST AL, 15
; 282:       756F             JNE L6
; 284:       450FB671F1       MOVZX R14D, BYTE PTR [R9-15]
; 289:       4D8BFE           MOV R15, R14
; 28C:       4981FFD5000000   CMP R15, 213
; 293:       0F84A8010000     JE L11
; 299:       4981FEF5000000   CMP R14, 245
; 2A0:       7551             JNE L6
; 2A2:       4D8B7129         MOV R14, [R9+41]
; 2A6:       49D1FE           SAR R14, 1
; 2A9:       4983FE07         CMP R14, 7
; 2AD:       7544             JNE L6
; 2AF:       4D8BF1           MOV R14, R9
; 2B2:       0F1F8000000000   NOP
; 2B9:       0F1F8000000000   NOP
; 2C0: L5:   4D8B7609         MOV R14, [R14+9]
; 2C4:       450FB64EF1       MOVZX R9D, BYTE PTR [R14-15]
; 2C9:       41D1E1           SHL R9D, 1
; 2CC:       4D8BF9           MOV R15, R9
; 2CF:       49D1FF           SAR R15, 1
; 2D2:       4981FFD5000000   CMP R15, 213
; 2D9:       741C             JE L7
; 2DB:       4D8BF9           MOV R15, R9
; 2DE:       49D1FF           SAR R15, 1
; 2E1:       4981FF81000000   CMP R15, 129
; 2E8:       74D6             JE L5
; 2EA:       4981F9EA010000   CMP R9, 490
; 2F1:       7DCD             JGE L5
; 2F3: L6:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
; 2F5:       18               BYTE #X18                       ; RSI(d)
; 2F6:       13               BYTE #X13                       ; '(VECTOR DOUBLE-FLOAT 7)
; 2F7: L7:   418078F1E5       CMP BYTE PTR [R8-15], -27
; 2FC:       0F8F3C010000     JG L10
; 302:       F2410F106001     MOVSD XMM4, [R8+1]
; 308:       807EF1E5         CMP BYTE PTR [RSI-15], -27
; 30C:       0F8F29010000     JG L9
; 312:       F20F104E01       MOVSD XMM1, [RSI+1]
; 317:       F20F5CE1         SUBSD XMM4, XMM1
; 31B:       F2410F105809     MOVSD XMM3, [R8+9]
; 321:       F20F104E09       MOVSD XMM1, [RSI+9]
; 326:       F20F5CD9         SUBSD XMM3, XMM1
; 32A:       F2410F105011     MOVSD XMM2, [R8+17]
; 330:       F20F104E11       MOVSD XMM1, [RSI+17]
; 335:       F20F5CD1         SUBSD XMM2, XMM1
; 339:       660F28CC         MOVAPD XMM1, XMM4
; 33D:       F20F59CC         MULSD XMM1, XMM4
; 341:       660F28F3         MOVAPD XMM6, XMM3
; 345:       F20F59F3         MULSD XMM6, XMM3
; 349:       F20F58F1         ADDSD XMM6, XMM1
; 34D:       660F28CA         MOVAPD XMM1, XMM2
; 351:       F20F59CA         MULSD XMM1, XMM2
; 355:       F20F58CE         ADDSD XMM1, XMM6
; 359:       F20F51C9         SQRTSD XMM1, XMM1
; 35D:       660F28F1         MOVAPD XMM6, XMM1
; 361:       F20F59F1         MULSD XMM6, XMM1
; 365:       F20F59CE         MULSD XMM1, XMM6
; 369:       660F28C5         MOVAPD XMM0, XMM5
; 36D:       F20F5EC1         DIVSD XMM0, XMM1
; 371:       660F28C8         MOVAPD XMM1, XMM0
; 375:       F20F59E1         MULSD XMM4, XMM1
; 379:       F20F59D9         MULSD XMM3, XMM1
; 37D:       F20F59CA         MULSD XMM1, XMM2
; 381:       F20F105631       MOVSD XMM2, [RSI+49]
; 386:       F20F59D4         MULSD XMM2, XMM4
; 38A:       F2410F107019     MOVSD XMM6, [R8+25]
; 390:       660F28C6         MOVAPD XMM0, XMM6
; 394:       F20F5CC2         SUBSD XMM0, XMM2
; 398:       660F28D0         MOVAPD XMM2, XMM0
; 39C:       F2410F115019     MOVSD [R8+25], XMM2
; 3A2:       F20F105631       MOVSD XMM2, [RSI+49]
; 3A7:       F20F59D3         MULSD XMM2, XMM3
; 3AB:       F2410F107021     MOVSD XMM6, [R8+33]
; 3B1:       660F28C6         MOVAPD XMM0, XMM6
; 3B5:       F20F5CC2         SUBSD XMM0, XMM2
; 3B9:       660F28D0         MOVAPD XMM2, XMM0
; 3BD:       F2410F115021     MOVSD [R8+33], XMM2
; 3C3:       F20F105631       MOVSD XMM2, [RSI+49]
; 3C8:       F20F59D1         MULSD XMM2, XMM1
; 3CC:       F2410F107029     MOVSD XMM6, [R8+41]
; 3D2:       660F28C6         MOVAPD XMM0, XMM6
; 3D6:       F20F5CC2         SUBSD XMM0, XMM2
; 3DA:       660F28D0         MOVAPD XMM2, XMM0
; 3DE:       F2410F115029     MOVSD [R8+41], XMM2
; 3E4:       F2410F105031     MOVSD XMM2, [R8+49]
; 3EA:       F20F59E2         MULSD XMM4, XMM2
; 3EE:       F20F105619       MOVSD XMM2, [RSI+25]
; 3F3:       F20F58D4         ADDSD XMM2, XMM4
; 3F7:       F20F115619       MOVSD [RSI+25], XMM2
; 3FC:       F2410F105031     MOVSD XMM2, [R8+49]
; 402:       F20F59DA         MULSD XMM3, XMM2
; 406:       F20F105621       MOVSD XMM2, [RSI+33]
; 40B:       F20F58D3         ADDSD XMM2, XMM3
; 40F:       F20F115621       MOVSD [RSI+33], XMM2
; 414:       F2410F105031     MOVSD XMM2, [R8+49]
; 41A:       F20F59D1         MULSD XMM2, XMM1
; 41E:       F20F104E29       MOVSD XMM1, [RSI+41]
; 423:       F20F58CA         ADDSD XMM1, XMM2
; 427:       F20F114E29       MOVSD [RSI+41], XMM1
; 42C: L8:   418D43F9         LEA EAX, [R11-7]
; 430:       A80F             TEST AL, 15
; 432:       0F84B8FDFFFF     JE L1
; 438:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
; 43A:       2C               BYTE #X2C                       ; R11(d)
; 43B: L9:   CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
; 43D:       18               BYTE #X18                       ; RSI(d)
; 43E: L10:  CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
; 440:       20               BYTE #X20                       ; R8(d)
; 441: L11:  4D8B49F9         MOV R9, [R9-7]
; 445:       49D1F9           SAR R9, 1
; 448:       4983F907         CMP R9, 7
; 44C:       0F84A5FEFFFF     JE L7
; 452:       E99CFEFFFF       JMP L6
; 457: L12:  498B72F9         MOV RSI, [R10-7]
; 45B:       48D1FE           SAR RSI, 1
; 45E:       4883FE07         CMP RSI, 7
; 462:       0F840EFEFFFF     JE L4
; 468:       E905FEFFFF       JMP L3
; 46D: L13:  4C8BC3           MOV R8, RBX
; 470:       E97F000000       JMP L15
; 475:       660F1F440000     NOP
; 47B:       0F1F440000       NOP
; 480: L14:  4D39E0           CMP R8, R12                     ; NIL
; 483:       747D             JE L17
; 485:       498B70F9         MOV RSI, [R8-7]
; 489:       4D8B4001         MOV R8, [R8+1]
; 48D:       488D46F1         LEA RAX, [RSI-15]
; 491:       A80F             TEST AL, 15
; 493:       756A             JNE L16
; 495:       8038D5           CMP BYTE PTR [RAX], -43
; 498:       7565             JNE L16
; 49A:       48837EF906       CMP QWORD PTR [RSI-7], 6
; 49F:       7669             JBE L18
; 4A1:       F20F105619       MOVSD XMM2, [RSI+25]
; 4A6:       F20F59D5         MULSD XMM2, XMM5
; 4AA:       F20F104E01       MOVSD XMM1, [RSI+1]
; 4AF:       F20F58CA         ADDSD XMM1, XMM2
; 4B3:       F20F114E01       MOVSD [RSI+1], XMM1
; 4B8:       48837EF908       CMP QWORD PTR [RSI-7], 8
; 4BD:       7652             JBE L19
; 4BF:       F20F105621       MOVSD XMM2, [RSI+33]
; 4C4:       F20F59D5         MULSD XMM2, XMM5
; 4C8:       F20F104E09       MOVSD XMM1, [RSI+9]
; 4CD:       F20F58CA         ADDSD XMM1, XMM2
; 4D1:       F20F114E09       MOVSD [RSI+9], XMM1
; 4D6:       48837EF90A       CMP QWORD PTR [RSI-7], 10
; 4DB:       763B             JBE L20
; 4DD:       F20F105629       MOVSD XMM2, [RSI+41]
; 4E2:       F20F59D5         MULSD XMM2, XMM5
; 4E6:       F20F104E11       MOVSD XMM1, [RSI+17]
; 4EB:       F20F58CA         ADDSD XMM1, XMM2
; 4EF:       F20F114E11       MOVSD [RSI+17], XMM1
; 4F4: L15:  418D40F9         LEA EAX, [R8-7]
; 4F8:       A80F             TEST AL, 15
; 4FA:       7484             JE L14
; 4FC:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
; 4FE:       20               BYTE #X20                       ; R8(d)
; 4FF: L16:  CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
; 501:       18               BYTE #X18                       ; RSI(d)
; 502: L17:  498BD0           MOV RDX, R8
; 505:       C9               LEAVE
; 506:       F8               CLC
; 507:       C3               RET
; 508:       CC0F             INT3 15                         ; Invalid argument count trap
; 50A: L18:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 50C:       18               BYTE #X18                       ; RSI(d)
; 50D:       8E808010         BYTE #X8E, #X80, #X80, #X10     ; 3
; 511: L19:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 513:       18               BYTE #X18                       ; RSI(d)
; 514:       92808010         BYTE #X92, #X80, #X80, #X10     ; 4
; 518: L20:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
; 51A:       18               BYTE #X18                       ; RSI(d)
; 51B:       96808010         BYTE #X96, #X80, #X80, #X10     ; 5
