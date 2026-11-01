;; percent of all cpu_core/cycles/pp samples (1473716 samples) on each instruction
       
       ;;; BUILD-BTREE
       ; disassembly for BUILD-BTREE
       ; Size: 332 bytes. Origin: #xB800048521                       ; BUILD-BTREE
  0.02 ; 521:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
  0.02 ; 525:       488945F8         MOV [RBP-8], RAX
  0.20 ; 529:       488B7DF0         MOV RDI, [RBP-16]
  1.80 ; 52D:       48D1FF           SAR RDI, 1
  0.03 ; 530:       4885FF           TEST RDI, RDI
  0.99 ; 533:       0F84E1000000     JE L4
  0.00 ; 539:       498BF3           MOV RSI, R11
  0.08 ; 53C:       4C01DE           ADD RSI, R11
  0.00 ; 53F:       0F8001010000     JO L6
  0.87 ; 545:       488B45F0         MOV RAX, [RBP-16]
  0.03 ; 549:       4C8BC0           MOV R8, RAX
  0.01 ; 54C:       4983E802         SUB R8, 2
       ; 550:       0F80F3000000     JO L7
  0.08 ; 556:       4C8BD6           MOV R10, RSI
  0.87 ; 559:       4C8955D8         MOV [RBP-40], R10
  0.03 ; 55D:       4C8945E0         MOV [RBP-32], R8
  0.00 ; 561:       488BD6           MOV RDX, RSI
  0.07 ; 564:       4883EA02         SUB RDX, 2
       ; 568:       0F80DE000000     JO L8
  0.94 ; 56E:       4883EC10         SUB RSP, 16
  0.02 ; 572:       498BF8           MOV RDI, R8
  0.00 ; 575:       B904000000       MOV ECX, 4
  0.11 ; 57A:       48892C24         MOV [RSP], RBP
  0.79 ; 57E:       488BEC           MOV RBP, RSP
  0.88 ; 581:       FF151190ABFF     CALL [RIP-5533679]              ; BUILD-BTREE
  0.01 ; 587:       480F42E3         CMOVB RSP, RBX
  0.06 ; 58B:       4C8B55D8         MOV R10, [RBP-40]
  0.02 ; 58F:       4C8B45E0         MOV R8, [RBP-32]
  0.92 ; 593:       4C8B5DE8         MOV R11, [RBP-24]
  0.01 ; 597:       4C8BCA           MOV R9, RDX
  0.06 ; 59A:       4C894DD0         MOV [RBP-48], R9
  0.02 ; 59E:       4883EC10         SUB RSP, 16
  0.91 ; 5A2:       498BD2           MOV RDX, R10
  0.01 ; 5A5:       498BF8           MOV RDI, R8
  0.04 ; 5A8:       B904000000       MOV ECX, 4
  0.04 ; 5AD:       48892C24         MOV [RSP], RBP
  0.89 ; 5B1:       488BEC           MOV RBP, RSP
  0.99 ; 5B4:       FF15DE8FABFF     CALL [RIP-5533730]              ; BUILD-BTREE
  0.03 ; 5BA:       480F42E3         CMOVB RSP, RBX
  0.05 ; 5BE:       4C8B4DD0         MOV R9, [RBP-48]
  0.11 ; 5C2:       4C8B5DE8         MOV R11, [RBP-24]
  0.94 ; 5C6:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
  0.08 ; 5CA:       498B7558         MOV RSI, [R13+88]               ; thread.cons-tlab
  0.02 ; 5CE:       4C8D4610         LEA R8, [RSI+16]
  0.09 ; 5D2:       4D3B4560         CMP R8, [R13+96]
  0.00 ; 5D6:       7777             JA L9
  1.00 ; 5D8:       4D894558         MOV [R13+88], R8                ; thread.cons-tlab
       ; 5DC: L0:   4C890E           MOV [RSI], R9
  0.07 ; 5DF:       48895608         MOV [RSI+8], RDX
  0.07 ; 5E3:       4C8D4607         LEA R8, [RSI+7]
  1.61 ; 5E7:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
  0.95 ; 5EB:       7402             JE L1
  0.00 ; 5ED:       0F0B             UD2
       ; 5EF: L1:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
  0.11 ; 5F3:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
  0.10 ; 5F7:       488D7210         LEA RSI, [RDX+16]
  0.89 ; 5FB:       493B7560         CMP RSI, [R13+96]
  0.00 ; 5FF:       7758             JA L10
  0.51 ; 601:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
       ; 605: L2:   4C891A           MOV [RDX], R11
  0.19 ; 608:       4C894208         MOV [RDX+8], R8
  0.73 ; 60C:       80CA07           OR DL, 7
  0.72 ; 60F:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
  0.91 ; 613:       7402             JE L3
  0.00 ; 615:       0F0B             UD2
       ; 617: L3:   C9               LEAVE
  1.89 ; 618:       F8               CLC
  1.92 ; 619:       C3               RET
       ; 61A: L4:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
  0.03 ; 61E:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
  0.03 ; 622:       488D7210         LEA RSI, [RDX+16]
  0.98 ; 626:       493B7560         CMP RSI, [R13+96]
  0.00 ; 62A:       7737             JA L11
  0.14 ; 62C:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
       ; 630: L5:   4C891A           MOV [RDX], R11
  0.09 ; 633:       4C896208         MOV [RDX+8], R12                ; NIL
  0.93 ; 637:       80CA07           OR DL, 7
  0.30 ; 63A:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
  0.93 ; 63E:       74D7             JE L3
  0.00 ; 640:       0F0B             UD2
  0.00 ; 642:       EBD3             JMP L3
       ; 644:       CC0F             INT3 15                         ; Invalid argument count trap
       ; 646: L6:   CC2E             INT3 46                         ; ADD-SUB-OVERFLOW-ERROR
       ; 648:       19               BYTE #X19                       ; RSI(a)
       ; 649: L7:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
       ; 64B:       21               BYTE #X21                       ; R8(a)
       ; 64C: L8:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
       ; 64E:       09               BYTE #X09                       ; RDX(a)
       ; 64F: L9:   6A10             PUSH 16
  0.00 ; 651:       E80A80FBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
  0.00 ; 656:       5E               POP RSI
  0.00 ; 657:       EB83             JMP L0
       ; 659: L10:  6A10             PUSH 16
  0.00 ; 65B:       E80080FBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
       ; 660:       5A               POP RDX
  0.00 ; 661:       EBA2             JMP L2
       ; 663: L11:  6A10             PUSH 16
  0.00 ; 665:       E8F67FFBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
       ; 66A:       5A               POP RDX
  0.00 ; 66B:       EBC3             JMP L5
       
       ;;; CHECK-NODE
       ; disassembly for CHECK-NODE
       ; Size: 201 bytes. Origin: #xB8000486D4                       ; CHECK-NODE
  0.02 ; 6D4:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
  0.00 ; 6D8:       488945F8         MOV [RBP-8], RAX
  1.45 ; 6DC:       488B45F0         MOV RAX, [RBP-16]
  0.01 ; 6E0:       8D70F9           LEA ESI, [RAX-7]
  0.16 ; 6E3:       40F6C60F         TEST SIL, 15
  0.00 ; 6E7:       0F85A7000000     JNE L4
  0.02 ; 6ED:       488B45F0         MOV RAX, [RBP-16]
  9.53 ; 6F1:       488B70F9         MOV RSI, [RAX-7]
  0.01 ; 6F5:       488B45F0         MOV RAX, [RBP-16]
  0.17 ; 6F9:       4C8B4001         MOV R8, [RAX+1]
  0.07 ; 6FD:       4C8945E8         MOV [RBP-24], R8
  1.35 ; 701:       40F6C601         TEST SIL, 1
       ; 705:       0F8586000000     JNE L3
  0.00 ; 70B:       4D39E0           CMP R8, R12                     ; NIL
  0.61 ; 70E:       7508             JNE L1
  0.00 ; 710:       BA02000000       MOV EDX, 2
       ; 715: L0:   C9               LEAVE
  0.59 ; 716:       F8               CLC
  1.22 ; 717:       C3               RET
       ; 718: L1:   418D70F9         LEA ESI, [R8-7]
  0.00 ; 71C:       40F6C60F         TEST SIL, 15
       ; 720:       756C             JNE L2
  1.87 ; 722:       498B50F9         MOV RDX, [R8-7]
  0.73 ; 726:       4883EC10         SUB RSP, 16
  0.01 ; 72A:       B902000000       MOV ECX, 2
  0.01 ; 72F:       48892C24         MOV [RSP], RBP
  0.09 ; 733:       488BEC           MOV RBP, RSP
  1.43 ; 736:       FF15E48EABFF     CALL [RIP-5533980]              ; CHECK-NODE
  0.00 ; 73C:       480F42E3         CMOVB RSP, RBX
  0.01 ; 740:       4C8B45E8         MOV R8, [RBP-24]
  0.00 ; 744:       BF02000000       MOV EDI, 2
  1.55 ; 749:       E8E288FBFF       CALL #xB800001030               ; SB-VM::GENERIC-+
  0.01 ; 74E:       4C8BCA           MOV R9, RDX
  0.00 ; 751:       4C894DE0         MOV [RBP-32], R9
       ; 755:       4C8B45E8         MOV R8, [RBP-24]
  0.71 ; 759:       498B5001         MOV RDX, [R8+1]
  0.01 ; 75D:       4883EC10         SUB RSP, 16
  0.01 ; 761:       B902000000       MOV ECX, 2
  0.01 ; 766:       48892C24         MOV [RSP], RBP
  0.81 ; 76A:       488BEC           MOV RBP, RSP
  0.95 ; 76D:       FF15AD8EABFF     CALL [RIP-5534035]              ; CHECK-NODE
  0.00 ; 773:       480F42E3         CMOVB RSP, RBX
  0.00 ; 777:       4C8B4DE0         MOV R9, [RBP-32]
  0.01 ; 77B:       488BFA           MOV RDI, RDX
  0.70 ; 77E:       498BD1           MOV RDX, R9
  0.79 ; 781:       E8AA88FBFF       CALL #xB800001030               ; SB-VM::GENERIC-+
  0.00 ; 786:       F6C201           TEST DL, 1
  0.64 ; 789:       748A             JE L0
       ; 78B:       CC5D             INT3 93                         ; OBJECT-NOT-FIXNUM-ERROR
       ; 78D:       08               BYTE #X08                       ; RDX(d)
       ; 78E: L2:   CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
       ; 790:       20               BYTE #X20                       ; R8(d)
       ; 791: L3:   CC5D             INT3 93                         ; OBJECT-NOT-FIXNUM-ERROR
       ; 793:       18               BYTE #X18                       ; RSI(d)
       ; 794: L4:   488B45F0         MOV RAX, [RBP-16]
       ; 798:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
       ; 79A:       00               BYTE #X00                       ; RAX(d)
       ; 79B:       CC0F             INT3 15                         ; Invalid argument count trap
