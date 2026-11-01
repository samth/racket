;; binarytrees-sbcl-1, SBCL 2.6.0.debian, compiled with the program's own declarations

;;; BUILD-BTREE
; disassembly for BUILD-BTREE
; Size: 332 bytes. Origin: #xB800B13291                       ; BUILD-BTREE
; 291:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 295:       488945F8         MOV [RBP-8], RAX
; 299:       488B7DF0         MOV RDI, [RBP-16]
; 29D:       48D1FF           SAR RDI, 1
; 2A0:       4885FF           TEST RDI, RDI
; 2A3:       0F84E1000000     JE L4
; 2A9:       498BF3           MOV RSI, R11
; 2AC:       4C01DE           ADD RSI, R11
; 2AF:       0F8001010000     JO L6
; 2B5:       488B45F0         MOV RAX, [RBP-16]
; 2B9:       4C8BC0           MOV R8, RAX
; 2BC:       4983E802         SUB R8, 2
; 2C0:       0F80F3000000     JO L7
; 2C6:       4C8BD6           MOV R10, RSI
; 2C9:       4C8955D8         MOV [RBP-40], R10
; 2CD:       4C8945E0         MOV [RBP-32], R8
; 2D1:       488BD6           MOV RDX, RSI
; 2D4:       4883EA02         SUB RDX, 2
; 2D8:       0F80DE000000     JO L8
; 2DE:       4883EC10         SUB RSP, 16
; 2E2:       498BF8           MOV RDI, R8
; 2E5:       B904000000       MOV ECX, 4
; 2EA:       48892C24         MOV [RSP], RBP
; 2EE:       488BEC           MOV RBP, RSP
; 2F1:       FF15A1E2FEFE     CALL [RIP-16850271]             ; BUILD-BTREE
; 2F7:       480F42E3         CMOVB RSP, RBX
; 2FB:       4C8B55D8         MOV R10, [RBP-40]
; 2FF:       4C8B45E0         MOV R8, [RBP-32]
; 303:       4C8B5DE8         MOV R11, [RBP-24]
; 307:       4C8BCA           MOV R9, RDX
; 30A:       4C894DD0         MOV [RBP-48], R9
; 30E:       4883EC10         SUB RSP, 16
; 312:       498BD2           MOV RDX, R10
; 315:       498BF8           MOV RDI, R8
; 318:       B904000000       MOV ECX, 4
; 31D:       48892C24         MOV [RSP], RBP
; 321:       488BEC           MOV RBP, RSP
; 324:       FF156EE2FEFE     CALL [RIP-16850322]             ; BUILD-BTREE
; 32A:       480F42E3         CMOVB RSP, RBX
; 32E:       4C8B4DD0         MOV R9, [RBP-48]
; 332:       4C8B5DE8         MOV R11, [RBP-24]
; 336:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 33A:       498B7558         MOV RSI, [R13+88]               ; thread.cons-tlab
; 33E:       4C8D4610         LEA R8, [RSI+16]
; 342:       4D3B4560         CMP R8, [R13+96]
; 346:       7777             JA L9
; 348:       4D894558         MOV [R13+88], R8                ; thread.cons-tlab
; 34C: L0:   4C890E           MOV [RSI], R9
; 34F:       48895608         MOV [RSI+8], RDX
; 353:       4C8D4607         LEA R8, [RSI+7]
; 357:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 35B:       7402             JE L1
; 35D:       0F0B             UD2
; 35F: L1:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 363:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
; 367:       488D7210         LEA RSI, [RDX+16]
; 36B:       493B7560         CMP RSI, [R13+96]
; 36F:       7758             JA L10
; 371:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
; 375: L2:   4C891A           MOV [RDX], R11
; 378:       4C894208         MOV [RDX+8], R8
; 37C:       80CA07           OR DL, 7
; 37F:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 383:       7402             JE L3
; 385:       0F0B             UD2
; 387: L3:   C9               LEAVE
; 388:       F8               CLC
; 389:       C3               RET
; 38A: L4:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 38E:       498B5558         MOV RDX, [R13+88]               ; thread.cons-tlab
; 392:       488D7210         LEA RSI, [RDX+16]
; 396:       493B7560         CMP RSI, [R13+96]
; 39A:       7737             JA L11
; 39C:       49897558         MOV [R13+88], RSI               ; thread.cons-tlab
; 3A0: L5:   4C891A           MOV [RDX], R11
; 3A3:       4C896208         MOV [RDX+8], R12                ; NIL
; 3A7:       80CA07           OR DL, 7
; 3AA:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 3AE:       74D7             JE L3
; 3B0:       0F0B             UD2
; 3B2:       EBD3             JMP L3
; 3B4:       CC0F             INT3 15                         ; Invalid argument count trap
; 3B6: L6:   CC2E             INT3 46                         ; ADD-SUB-OVERFLOW-ERROR
; 3B8:       19               BYTE #X19                       ; RSI(a)
; 3B9: L7:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
; 3BB:       21               BYTE #X21                       ; R8(a)
; 3BC: L8:   CC2F             INT3 47                         ; SUB-OVERFLOW-ERROR
; 3BE:       09               BYTE #X09                       ; RDX(a)
; 3BF: L9:   6A10             PUSH 16
; 3C1:       E89AD24EFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 3C6:       5E               POP RSI
; 3C7:       EB83             JMP L0
; 3C9: L10:  6A10             PUSH 16
; 3CB:       E890D24EFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 3D0:       5A               POP RDX
; 3D1:       EBA2             JMP L2
; 3D3: L11:  6A10             PUSH 16
; 3D5:       E886D24EFF       CALL #xB800000660               ; SB-VM::LIST-ALLOC-TRAMP
; 3DA:       5A               POP RDX
; 3DB:       EBC3             JMP L5

;;; CHECK-NODE
; disassembly for CHECK-NODE
; Size: 201 bytes. Origin: #xB800B13444                       ; CHECK-NODE
; 444:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 448:       488945F8         MOV [RBP-8], RAX
; 44C:       488B45F0         MOV RAX, [RBP-16]
; 450:       8D70F9           LEA ESI, [RAX-7]
; 453:       40F6C60F         TEST SIL, 15
; 457:       0F85A7000000     JNE L4
; 45D:       488B45F0         MOV RAX, [RBP-16]
; 461:       488B70F9         MOV RSI, [RAX-7]
; 465:       488B45F0         MOV RAX, [RBP-16]
; 469:       4C8B4001         MOV R8, [RAX+1]
; 46D:       4C8945E8         MOV [RBP-24], R8
; 471:       40F6C601         TEST SIL, 1
; 475:       0F8586000000     JNE L3
; 47B:       4D39E0           CMP R8, R12                     ; NIL
; 47E:       7508             JNE L1
; 480:       BA02000000       MOV EDX, 2
; 485: L0:   C9               LEAVE
; 486:       F8               CLC
; 487:       C3               RET
; 488: L1:   418D70F9         LEA ESI, [R8-7]
; 48C:       40F6C60F         TEST SIL, 15
; 490:       756C             JNE L2
; 492:       498B50F9         MOV RDX, [R8-7]
; 496:       4883EC10         SUB RSP, 16
; 49A:       B902000000       MOV ECX, 2
; 49F:       48892C24         MOV [RSP], RBP
; 4A3:       488BEC           MOV RBP, RSP
; 4A6:       FF1574E1FEFE     CALL [RIP-16850572]             ; CHECK-NODE
; 4AC:       480F42E3         CMOVB RSP, RBX
; 4B0:       4C8B45E8         MOV R8, [RBP-24]
; 4B4:       BF02000000       MOV EDI, 2
; 4B9:       E872DB4EFF       CALL #xB800001030               ; SB-VM::GENERIC-+
; 4BE:       4C8BCA           MOV R9, RDX
; 4C1:       4C894DE0         MOV [RBP-32], R9
; 4C5:       4C8B45E8         MOV R8, [RBP-24]
; 4C9:       498B5001         MOV RDX, [R8+1]
; 4CD:       4883EC10         SUB RSP, 16
; 4D1:       B902000000       MOV ECX, 2
; 4D6:       48892C24         MOV [RSP], RBP
; 4DA:       488BEC           MOV RBP, RSP
; 4DD:       FF153DE1FEFE     CALL [RIP-16850627]             ; CHECK-NODE
; 4E3:       480F42E3         CMOVB RSP, RBX
; 4E7:       4C8B4DE0         MOV R9, [RBP-32]
; 4EB:       488BFA           MOV RDI, RDX
; 4EE:       498BD1           MOV RDX, R9
; 4F1:       E83ADB4EFF       CALL #xB800001030               ; SB-VM::GENERIC-+
; 4F6:       F6C201           TEST DL, 1
; 4F9:       748A             JE L0
; 4FB:       CC5D             INT3 93                         ; OBJECT-NOT-FIXNUM-ERROR
; 4FD:       08               BYTE #X08                       ; RDX(d)
; 4FE: L2:   CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
; 500:       20               BYTE #X20                       ; R8(d)
; 501: L3:   CC5D             INT3 93                         ; OBJECT-NOT-FIXNUM-ERROR
; 503:       18               BYTE #X18                       ; RSI(d)
; 504: L4:   488B45F0         MOV RAX, [RBP-16]
; 508:       CC57             INT3 87                         ; OBJECT-NOT-LIST-ERROR
; 50A:       00               BYTE #X00                       ; RAX(d)
; 50B:       CC0F             INT3 15                         ; Invalid argument count trap
