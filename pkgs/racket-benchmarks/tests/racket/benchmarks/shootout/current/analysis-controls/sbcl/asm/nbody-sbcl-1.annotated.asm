;; percent of all cpu_core/cycles/pp samples (467606 samples) on each instruction
       
       ;;; ADVANCE
       ; disassembly for ADVANCE
       ; Size: 885 bytes. Origin: #xB80004939A                       ; ADVANCE
       ; 39A:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
       ; 39E:       488945F8         MOV [RBP-8], RAX
       ; 3A2:       488BCB           MOV RCX, RBX
  0.38 ; 3A5:       660F1F440000     NOP
       ; 3AB:       0F1F440000       NOP
       ; 3B0: L0:   4C39E1           CMP RCX, R12                    ; NIL
  0.49 ; 3B3:       0F84A4020000     JE L13
       ; 3B9:       8D41F9           LEA EAX, [RCX-7]
  0.42 ; 3BC:       A80F             TEST AL, 15
       ; 3BE:       0F8599020000     JNE L13
  1.22 ; 3C4:       488BC1           MOV RAX, RCX
       ; 3C7:       4C8B50F9         MOV R10, [RAX-7]
       ; 3CB:       488B4001         MOV RAX, [RAX+1]
  0.50 ; 3CF:       488B4901         MOV RCX, [RCX+1]
  1.17 ; 3D3:       4C8BD8           MOV R11, RAX
  1.65 ; 3D6:       E941020000       JMP L8
       ; 3DB:       0F1F440000       NOP
       ; 3E0: L1:   4D39E3           CMP R11, R12                    ; NIL
  1.55 ; 3E3:       74CB             JE L0
       ; 3E5:       4D8B4BF9         MOV R9, [R11-7]
       ; 3E9:       4D8B5B01         MOV R11, [R11+1]
  3.78 ; 3ED:       4D8BFA           MOV R15, R10
       ; 3F0:       418D42F1         LEA EAX, [R10-15]
       ; 3F4:       A80F             TEST AL, 15
       ; 3F6:       756A             JNE L3
       ; 3F8:       410FB672F1       MOVZX ESI, BYTE PTR [R10-15]
  3.31 ; 3FD:       4C8BC6           MOV R8, RSI
       ; 400:       4981F8D5000000   CMP R8, 213
  3.22 ; 407:       0F843A020000     JE L12
       ; 40D:       4881FEF5000000   CMP RSI, 245
       ; 414:       754C             JNE L3
       ; 416:       498B7229         MOV RSI, [R10+41]
       ; 41A:       48D1FE           SAR RSI, 1
       ; 41D:       4883FE07         CMP RSI, 7
       ; 421:       753F             JNE L3
       ; 423:       4D8BC2           MOV R8, R10
       ; 426:       0F1F440000       NOP
       ; 42B:       0F1F440000       NOP
       ; 430: L2:   4D8B4009         MOV R8, [R8+9]
       ; 434:       410FB670F1       MOVZX ESI, BYTE PTR [R8-15]
       ; 439:       D1E6             SHL ESI, 1
       ; 43B:       4C8BF6           MOV R14, RSI
       ; 43E:       49D1FE           SAR R14, 1
       ; 441:       4981FED5000000   CMP R14, 213
       ; 448:       741C             JE L4
       ; 44A:       4C8BF6           MOV R14, RSI
       ; 44D:       49D1FE           SAR R14, 1
       ; 450:       4981FE81000000   CMP R14, 129
       ; 457:       74D7             JE L2
       ; 459:       4881FEEA010000   CMP RSI, 490
       ; 460:       7DCE             JGE L2
       ; 462: L3:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
       ; 464:       3C               BYTE #X3C                       ; R15(d)
       ; 465:       13               BYTE #X13                       ; '(VECTOR DOUBLE-FLOAT 7)
       ; 466: L4:   4D8BC7           MOV R8, R15
       ; 469:       498BF1           MOV RSI, R9
       ; 46C:       418D41F1         LEA EAX, [R9-15]
  3.10 ; 470:       A80F             TEST AL, 15
       ; 472:       756F             JNE L6
       ; 474:       450FB671F1       MOVZX R14D, BYTE PTR [R9-15]
       ; 479:       4D8BFE           MOV R15, R14
       ; 47C:       4981FFD5000000   CMP R15, 213
  3.39 ; 483:       0F84A8010000     JE L11
       ; 489:       4981FEF5000000   CMP R14, 245
       ; 490:       7551             JNE L6
       ; 492:       4D8B7129         MOV R14, [R9+41]
       ; 496:       49D1FE           SAR R14, 1
       ; 499:       4983FE07         CMP R14, 7
       ; 49D:       7544             JNE L6
       ; 49F:       4D8BF1           MOV R14, R9
       ; 4A2:       0F1F8000000000   NOP
       ; 4A9:       0F1F8000000000   NOP
       ; 4B0: L5:   4D8B7609         MOV R14, [R14+9]
       ; 4B4:       450FB64EF1       MOVZX R9D, BYTE PTR [R14-15]
       ; 4B9:       41D1E1           SHL R9D, 1
       ; 4BC:       4D8BF9           MOV R15, R9
       ; 4BF:       49D1FF           SAR R15, 1
       ; 4C2:       4981FFD5000000   CMP R15, 213
       ; 4C9:       741C             JE L7
       ; 4CB:       4D8BF9           MOV R15, R9
       ; 4CE:       49D1FF           SAR R15, 1
       ; 4D1:       4981FF81000000   CMP R15, 129
       ; 4D8:       74D6             JE L5
       ; 4DA:       4981F9EA010000   CMP R9, 490
       ; 4E1:       7DCD             JGE L5
       ; 4E3: L6:   CC1D             INT3 29                         ; OBJECT-NOT-TYPE-ERROR
       ; 4E5:       18               BYTE #X18                       ; RSI(d)
       ; 4E6:       13               BYTE #X13                       ; '(VECTOR DOUBLE-FLOAT 7)
       ; 4E7: L7:   418078F1E5       CMP BYTE PTR [R8-15], -27
       ; 4EC:       0F8F3C010000     JG L10
  0.00 ; 4F2:       F2410F106001     MOVSD XMM4, [R8+1]
  3.04 ; 4F8:       807EF1E5         CMP BYTE PTR [RSI-15], -27
       ; 4FC:       0F8F29010000     JG L9
       ; 502:       F20F104E01       MOVSD XMM1, [RSI+1]
       ; 507:       F20F5CE1         SUBSD XMM4, XMM1
  2.55 ; 50B:       F2410F105809     MOVSD XMM3, [R8+9]
       ; 511:       F20F104E09       MOVSD XMM1, [RSI+9]
       ; 516:       F20F5CD9         SUBSD XMM3, XMM1
       ; 51A:       F2410F105011     MOVSD XMM2, [R8+17]
  3.89 ; 520:       F20F104E11       MOVSD XMM1, [RSI+17]
  0.00 ; 525:       F20F5CD1         SUBSD XMM2, XMM1
       ; 529:       660F28CC         MOVAPD XMM1, XMM4
       ; 52D:       F20F59CC         MULSD XMM1, XMM4
  3.62 ; 531:       660F28F3         MOVAPD XMM6, XMM3
       ; 535:       F20F59F3         MULSD XMM6, XMM3
       ; 539:       F20F58F1         ADDSD XMM6, XMM1
       ; 53D:       660F28CA         MOVAPD XMM1, XMM2
  2.24 ; 541:       F20F59CA         MULSD XMM1, XMM2
  0.00 ; 545:       F20F58CE         ADDSD XMM1, XMM6
  0.00 ; 549:       F20F51C9         SQRTSD XMM1, XMM1
       ; 54D:       660F28F1         MOVAPD XMM6, XMM1
  2.96 ; 551:       F20F59F1         MULSD XMM6, XMM1
  0.00 ; 555:       F20F59CE         MULSD XMM1, XMM6
       ; 559:       660F28C5         MOVAPD XMM0, XMM5
  0.00 ; 55D:       F20F5EC1         DIVSD XMM0, XMM1
  2.67 ; 561:       660F28C8         MOVAPD XMM1, XMM0
  0.00 ; 565:       F20F59E1         MULSD XMM4, XMM1
  0.00 ; 569:       F20F59D9         MULSD XMM3, XMM1
       ; 56D:       F20F59CA         MULSD XMM1, XMM2
  2.83 ; 571:       F20F105631       MOVSD XMM2, [RSI+49]
       ; 576:       F20F59D4         MULSD XMM2, XMM4
  0.00 ; 57A:       F2410F107019     MOVSD XMM6, [R8+25]
       ; 580:       660F28C6         MOVAPD XMM0, XMM6
  2.94 ; 584:       F20F5CC2         SUBSD XMM0, XMM2
  0.00 ; 588:       660F28D0         MOVAPD XMM2, XMM0
       ; 58C:       F2410F115019     MOVSD [R8+25], XMM2
  0.00 ; 592:       F20F105631       MOVSD XMM2, [RSI+49]
  1.75 ; 597:       F20F59D3         MULSD XMM2, XMM3
       ; 59B:       F2410F107021     MOVSD XMM6, [R8+33]
       ; 5A1:       660F28C6         MOVAPD XMM0, XMM6
       ; 5A5:       F20F5CC2         SUBSD XMM0, XMM2
  2.47 ; 5A9:       660F28D0         MOVAPD XMM2, XMM0
  0.00 ; 5AD:       F2410F115021     MOVSD [R8+33], XMM2
       ; 5B3:       F20F105631       MOVSD XMM2, [RSI+49]
       ; 5B8:       F20F59D1         MULSD XMM2, XMM1
  3.13 ; 5BC:       F2410F107029     MOVSD XMM6, [R8+41]
       ; 5C2:       660F28C6         MOVAPD XMM0, XMM6
       ; 5C6:       F20F5CC2         SUBSD XMM0, XMM2
       ; 5CA:       660F28D0         MOVAPD XMM2, XMM0
  2.66 ; 5CE:       F2410F115029     MOVSD [R8+41], XMM2
  0.00 ; 5D4:       F2410F105031     MOVSD XMM2, [R8+49]
       ; 5DA:       F20F59E2         MULSD XMM4, XMM2
       ; 5DE:       F20F105619       MOVSD XMM2, [RSI+25]
  2.24 ; 5E3:       F20F58D4         ADDSD XMM2, XMM4
       ; 5E7:       F20F115619       MOVSD [RSI+25], XMM2
       ; 5EC:       F2410F105031     MOVSD XMM2, [R8+49]
       ; 5F2:       F20F59DA         MULSD XMM3, XMM2
  2.50 ; 5F6:       F20F105621       MOVSD XMM2, [RSI+33]
  0.00 ; 5FB:       F20F58D3         ADDSD XMM2, XMM3
  0.00 ; 5FF:       F20F115621       MOVSD [RSI+33], XMM2
  0.00 ; 604:       F2410F105031     MOVSD XMM2, [R8+49]
  3.23 ; 60A:       F20F59D1         MULSD XMM2, XMM1
       ; 60E:       F20F104E29       MOVSD XMM1, [RSI+41]
  0.00 ; 613:       F20F58CA         ADDSD XMM1, XMM2
       ; 617:       F20F114E29       MOVSD [RSI+41], XMM1
       ; 61C: L8:   418D43F9         LEA EAX, [R11-7]
       ; 620:       A80F             TEST AL, 15
  4.16 ; 622:       0F84B8FDFFFF     JE L1
       ; 628:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
       ; 62A:       2C               BYTE #X2C                       ; R11(d)
       ; 62B: L9:   CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
       ; 62D:       18               BYTE #X18                       ; RSI(d)
       ; 62E: L10:  CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
       ; 630:       20               BYTE #X20                       ; R8(d)
       ; 631: L11:  4D8B49F9         MOV R9, [R9-7]
       ; 635:       49D1F9           SAR R9, 1
       ; 638:       4983F907         CMP R9, 7
  3.60 ; 63C:       0F84A5FEFFFF     JE L7
       ; 642:       E99CFEFFFF       JMP L6
       ; 647: L12:  498B72F9         MOV RSI, [R10-7]
       ; 64B:       48D1FE           SAR RSI, 1
       ; 64E:       4883FE07         CMP RSI, 7
  3.55 ; 652:       0F840EFEFFFF     JE L4
       ; 658:       E905FEFFFF       JMP L3
       ; 65D: L13:  4C8BC3           MOV R8, RBX
  0.52 ; 660:       E97F000000       JMP L15
       ; 665:       660F1F440000     NOP
       ; 66B:       0F1F440000       NOP
       ; 670: L14:  4D39E0           CMP R8, R12                     ; NIL
  0.20 ; 673:       747D             JE L17
       ; 675:       498B70F9         MOV RSI, [R8-7]
       ; 679:       4D8B4001         MOV R8, [R8+1]
  1.41 ; 67D:       488D46F1         LEA RAX, [RSI-15]
       ; 681:       A80F             TEST AL, 15
       ; 683:       756A             JNE L16
       ; 685:       8038D5           CMP BYTE PTR [RAX], -43
       ; 688:       7565             JNE L16
  1.82 ; 68A:       48837EF906       CMP QWORD PTR [RSI-7], 6
  0.00 ; 68F:       7669             JBE L18
       ; 691:       F20F105619       MOVSD XMM2, [RSI+25]
       ; 696:       F20F59D5         MULSD XMM2, XMM5
  1.41 ; 69A:       F20F104E01       MOVSD XMM1, [RSI+1]
       ; 69F:       F20F58CA         ADDSD XMM1, XMM2
       ; 6A3:       F20F114E01       MOVSD [RSI+1], XMM1
       ; 6A8:       48837EF908       CMP QWORD PTR [RSI-7], 8
  1.51 ; 6AD:       7652             JBE L19
       ; 6AF:       F20F105621       MOVSD XMM2, [RSI+33]
  0.00 ; 6B4:       F20F59D5         MULSD XMM2, XMM5
       ; 6B8:       F20F104E09       MOVSD XMM1, [RSI+9]
  1.52 ; 6BD:       F20F58CA         ADDSD XMM1, XMM2
       ; 6C1:       F20F114E09       MOVSD [RSI+9], XMM1
       ; 6C6:       48837EF90A       CMP QWORD PTR [RSI-7], 10
       ; 6CB:       763B             JBE L20
  1.65 ; 6CD:       F20F105629       MOVSD XMM2, [RSI+41]
       ; 6D2:       F20F59D5         MULSD XMM2, XMM5
       ; 6D6:       F20F104E11       MOVSD XMM1, [RSI+17]
       ; 6DB:       F20F58CA         ADDSD XMM1, XMM2
  1.56 ; 6DF:       F20F114E11       MOVSD [RSI+17], XMM1
       ; 6E4: L15:  418D40F9         LEA EAX, [R8-7]
       ; 6E8:       A80F             TEST AL, 15
  1.66 ; 6EA:       7484             JE L14
       ; 6EC:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
       ; 6EE:       20               BYTE #X20                       ; R8(d)
       ; 6EF: L16:  CC3E             INT3 62                         ; OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR
       ; 6F1:       18               BYTE #X18                       ; RSI(d)
       ; 6F2: L17:  498BD0           MOV RDX, R8
  0.28 ; 6F5:       C9               LEAVE
       ; 6F6:       F8               CLC
  0.26 ; 6F7:       C3               RET
       ; 6F8:       CC0F             INT3 15                         ; Invalid argument count trap
       ; 6FA: L18:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
       ; 6FC:       18               BYTE #X18                       ; RSI(d)
       ; 6FD:       8E808010         BYTE #X8E, #X80, #X80, #X10     ; 3
       ; 701: L19:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
       ; 703:       18               BYTE #X18                       ; RSI(d)
       ; 704:       92808010         BYTE #X92, #X80, #X80, #X10     ; 4
       ; 708: L20:  CC24             INT3 36                         ; INVALID-VECTOR-INDEX-ERROR
       ; 70A:       18               BYTE #X18                       ; RSI(d)
       ; 70B:       96808010         BYTE #X96, #X80, #X80, #X10     ; 5
