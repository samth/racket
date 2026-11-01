
;;; BUILD-BTREE
; disassembly for BUILD-BTREE
; Size: 332 bytes. Origin: #xB800048521                       ; BUILD-BTREE
; 521:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 525:       488945F8         MOV [RBP-8], RAX
; 529:       488B7DF0         MOV RDI, [RBP-16]
; 52D:       48D1FF           SAR RDI, 1
; 530:       4885FF           TEST RDI, RDI
; 533:       0F84E1000000     JE L4
; 539:       498BF3           MOV RSI, R11
; 53C:       4C01DE           ADD RSI, R11
; 53F:       0F8001010000     JO L6
; 545:       488B45F0         MOV RAX, [RBP-16]
; 549:       4C8BC0           MOV R8, RAX
; 54C:       4983E802         SUB R8, 2
; 550:       0F80F3000000     JO L7
; 556:       4C8BD6           MOV R10, RSI
; 559:       4C8955D8         MOV [RBP-40], R10
; 55D:       4C8945E0         MOV [RBP-32], R8
; 561:       488BD6           MOV RDX, RSI
; 564:       4883EA02         SUB RDX, 2
; 568:       0F80DE000000     JO L8
; 56E:       4883EC10         SUB RSP, 16
; 572:       498BF8           MOV RDI, R8
; 575:       B904000000       MOV ECX, 4
; 57A:       48892C24         MOV [RSP], RBP
; 57E:       488BEC           MOV RBP, RSP
; 581:       FF151190ABFF     CALL [RIP-5533679]              ; BUILD-BTREE
; 587:       480F42E3         CMOVB RSP, RBX
; 58B:       4C8B55D8         MOV R10, [RBP-40]
; 58F:       4C8B45E0         MOV R8, [RBP-32]
; 593:       4C8B5DE8         MOV R11, [RBP-24]
; 597:       4C8BCA           MOV R9, RDX
; 59A:       4C894DD0         MOV [RBP-48], R9
; 59E:       4883EC10         SUB RSP, 16
; 5A2:       498BD2           MOV RDX, R10
; 5A5:       498BF8           MOV RDI, R8
; 5A8:       B904000000       MOV ECX, 4
; 5AD:       48892C24         MOV [RSP], RBP
; 5B1:       488BEC           MOV RBP, RSP
; 5B4:       FF15DE8FABFF     CALL [RIP-5533730]              ; BUILD-BTREE
; 5BA:       480F42E3         CMOVB RSP, RBX
; 5BE:       4C8B4DD0         MOV R9, [RBP-48]
; 5C2:       4C8B5DE8         MOV R11, [RBP-24]
; 5C6:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 5CA:       498B7558         MOV RSI, [R13+88]               ; thread.cons-tlab
; 5CE:       4C8D4610         LEA R8, [RSI+16]
; 5D2:       4D3B4560         CMP R8, [R13+96]
; 5D6:       7777             JA L9
; 5D8:       4D894558         MOV [R13+88], R8                ; thread.cons-tlab
; 5DC: L0:   4C890E           MOV [RSI], R9
; 5DF:       48895608         MOV [RSI+8], RDX
; 5E3:       4C8D4607         LEA R8, [RSI+7]
; 5E7:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 5EB:       7402             JE L1
; 5ED:       0F0B             UD2
; 5EF: L1:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 5F3:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
; 5F7:       488D7210         LEA RSI, [RDX+16]
; 5FB:       493B7560         CMP RSI, [R13+96]
; 5FF:       7758             JA L10
; 601:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
; 605: L2:   4C891A           MOV [RDX], R11
; 608:       4C894208         MOV [RDX+8], R8
; 60C:       80CA07           OR DL, 7
; 60F:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 613:       7402             JE L3
; 615:       0F0B             UD2
; 617: L3:   C9               LEAVE
; 618:       F8               CLC
; 619:       C3               RET
; 61A: L4:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 61E:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
; 622:       488D7210         LEA RSI, [RDX+16]
; 626:       493B7560         CMP RSI, [R13+96]
; 62A:       7737             JA L11
; 62C:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
; 630: L5:   4C891A           MOV [RDX], R11
; 633:       4C896208         MOV [RDX+8], R12                ; NIL
; 637:       80CA07           OR DL, 7
; 63A:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 63E:       74D7             JE L3
; 640:       0F0B             UD2
; 642:       EBD3             JMP L3
; 644:       CC0F             INT3 15                         ; Invalid argument count trap
; 646: L6:   CC2E             INT3 46                         ; ADD-SUB-OVERFLOW-ERROR
; 648:       19               BYTE #X19                       ; RSI(a)
; 649: L7:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
; 64B:       21               BYTE #X21                       ; R8(a)
; 64C: L8:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
; 64E:       09               BYTE #X09                       ; RDX(a)
; 64F: L9:   6A10             PUSH 16
; 651:       E80A80FBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 656:       5E               POP RSI
; 657:       EB83             JMP L0
; 659: L10:  6A10             PUSH 16
; 65B:       E80080FBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 660:       5A               POP RDX
; 661:       EBA2             JMP L2
; 663: L11:  6A10             PUSH 16
; 665:       E8F67FFBFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 66A:       5A               POP RDX
; 66B:       EBC3             JMP L5

;;; CHECK-NODE
; disassembly for CHECK-NODE
; Size: 201 bytes. Origin: #xB8000486D4                       ; CHECK-NODE
; 6D4:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 6D8:       488945F8         MOV [RBP-8], RAX
; 6DC:       488B45F0         MOV RAX, [RBP-16]
; 6E0:       8D70F9           LEA ESI, [RAX-7]
; 6E3:       40F6C60F         TEST SIL, 15
; 6E7:       0F85A7000000     JNE L4
; 6ED:       488B45F0         MOV RAX, [RBP-16]
; 6F1:       488B70F9         MOV RSI, [RAX-7]
; 6F5:       488B45F0         MOV RAX, [RBP-16]
; 6F9:       4C8B4001         MOV R8, [RAX+1]
; 6FD:       4C8945E8         MOV [RBP-24], R8
; 701:       40F6C601         TEST SIL, 1
; 705:       0F8586000000     JNE L3
; 70B:       4D39E0           CMP R8, R12                     ; NIL
; 70E:       7508             JNE L1
; 710:       BA02000000       MOV EDX, 2
; 715: L0:   C9               LEAVE
; 716:       F8               CLC
; 717:       C3               RET
; 718: L1:   418D70F9         LEA ESI, [R8-7]
; 71C:       40F6C60F         TEST SIL, 15
; 720:       756C             JNE L2
; 722:       498B50F9         MOV RDX, [R8-7]
; 726:       4883EC10         SUB RSP, 16
; 72A:       B902000000       MOV ECX, 2
; 72F:       48892C24         MOV [RSP], RBP
; 733:       488BEC           MOV RBP, RSP
; 736:       FF15E48EABFF     CALL [RIP-5533980]              ; CHECK-NODE
; 73C:       480F42E3         CMOVB RSP, RBX
; 740:       4C8B45E8         MOV R8, [RBP-24]
; 744:       BF02000000       MOV EDI, 2
; 749:       E8E288FBFF       CALL #xB800001030               ; SB-VM::GENERIC-+
; 74E:       4C8BCA           MOV R9, RDX
; 751:       4C894DE0         MOV [RBP-32], R9
; 755:       4C8B45E8         MOV R8, [RBP-24]
; 759:       498B5001         MOV RDX, [R8+1]
; 75D:       4883EC10         SUB RSP, 16
; 761:       B902000000       MOV ECX, 2
; 766:       48892C24         MOV [RSP], RBP
; 76A:       488BEC           MOV RBP, RSP
; 76D:       FF15AD8EABFF     CALL [RIP-5534035]              ; CHECK-NODE
; 773:       480F42E3         CMOVB RSP, RBX
; 777:       4C8B4DE0         MOV R9, [RBP-32]
; 77B:       488BFA           MOV RDI, RDX
; 77E:       498BD1           MOV RDX, R9
; 781:       E8AA88FBFF       CALL #xB800001030               ; SB-VM::GENERIC-+
; 786:       F6C201           TEST DL, 1
; 789:       748A             JE L0
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
