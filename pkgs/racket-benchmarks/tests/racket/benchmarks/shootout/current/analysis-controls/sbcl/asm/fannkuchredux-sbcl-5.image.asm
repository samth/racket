
;;; FANNKUCH
; disassembly for FANNKUCH
; Size: 4548 bytes. Origin: #xB800048664                      ; FANNKUCH
; 8664:       498B4510         MOV RAX, [R13+16]              ; thread.binding-stack-pointer
; 8668:       488945F8         MOV [RBP-8], RAX
; 866C:       488B45F0         MOV RAX, [RBP-16]
; 8670:       4C8D40F1         LEA R8, [RAX-15]
; 8674:       41F6C001         TEST R8B, 1
; 8678:       751A             JNE L0
; 867A:       4180F80A         CMP R8B, 10
; 867E:       7414             JE L0
; 8680:       41F6C00F         TEST R8B, 15
; 8684:       0F853A0F0000     JNE L93
; 868A:       4180381D         CMP BYTE PTR [R8], 29
; 868E:       0F87300F0000     JA L93
; 8694: L0:   488B45F0         MOV RAX, [RBP-16]
; 8698:       488945E0         MOV [RBP-32], RAX
; 869C:       41BB02000000     MOV R11D, 2
; 86A2:       488965E8         MOV [RBP-24], RSP
; 86A6:       4154             PUSH R12                       ; NIL
; 86A8:       4C8D4424FF       LEA R8, [RSP-1]
; 86AD:       4C8945D8         MOV [RBP-40], R8
; 86B1:       4C8945D0         MOV [RBP-48], R8
; 86B5:       E986000000       JMP L4
; 86BA:       660F1F440000     NOP
; 86C0: L1:   4C8B55D0         MOV R10, [RBP-48]
; 86C4:       4C8955C0         MOV [RBP-64], R10
; 86C8:       4883EC10         SUB RSP, 16
; 86CC:       4C895DC8         MOV [RBP-56], R11
; 86D0:       31C9             XOR ECX, ECX
; 86D2:       48892C24         MOV [RSP], RBP
; 86D6:       488BEC           MOV RBP, RSP
; 86D9:       FF15817AABFF     CALL [RIP-5539199]             ; GENSYM
; 86DF:       4C8B55C0         MOV R10, [RBP-64]
; 86E3:       4C8B5DC8         MOV R11, [RBP-56]
; 86E7:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 86EB:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 86EF:       4D8D4810         LEA R9, [R8+16]
; 86F3:       4D3B4D60         CMP R9, [R13+96]
; 86F7:       0F87D00E0000     JA L94
; 86FD:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8701: L2:   498910           MOV [R8], RDX
; 8704:       4D896008         MOV [R8+8], R12                ; NIL
; 8708:       4D8D4807         LEA R9, [R8+7]
; 870C:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8710:       7402             JE L3
; 8712:       0F0B             UD2
; 8714: L3:   4C894DD0         MOV [RBP-48], R9
; 8718:       4D8BC2           MOV R8, R10
; 871B:       49C1E80A         SHR R8, 10
; 871F:       4181E0FFFF0F00   AND R8D, 1048575
; 8726:       43C644046900     MOV BYTE PTR [R12+R8+105], 0
; 872C:       4D894A01         MOV [R10+1], R9
; 8730:       BF02000000       MOV EDI, 2
; 8735:       498BD3           MOV RDX, R11
; 8738:       E8F388FBFF       CALL #xB800001030              ; SB-VM::GENERIC-+
; 873D:       4C8BDA           MOV R11, RDX
; 8740: L4:   4C895DC8         MOV [RBP-56], R11
; 8744:       488B7DE0         MOV RDI, [RBP-32]
; 8748:       498BD3           MOV RDX, R11
; 874B:       E8E08AFBFF       CALL #xB800001230              ; SB-VM::GENERIC->
; 8750:       4C8B5DC8         MOV R11, [RBP-56]
; 8754:       0F8E66FFFFFF     JLE L1
; 875A:       488B45D8         MOV RAX, [RBP-40]
; 875E:       4C8B4001         MOV R8, [RAX+1]
; 8762:       488B65E8         MOV RSP, [RBP-24]
; 8766:       4C8945E8         MOV [RBP-24], R8
; 876A:       48C745E004000000 MOV QWORD PTR [RBP-32], 4
; 8772:       488965D8         MOV [RBP-40], RSP
; 8776:       4154             PUSH R12                       ; NIL
; 8778:       4C8D4424FF       LEA R8, [RSP-1]
; 877D:       4C8945D0         MOV [RBP-48], R8
; 8781:       4D8BD0           MOV R10, R8
; 8784:       E949010000       JMP L12
; 8789:       0F1F8000000000   NOP
; 8790: L5:   4C8955C8         MOV [RBP-56], R10
; 8794:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8798:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 879C:       4D8D4810         LEA R9, [R8+16]
; 87A0:       4D3B4D60         CMP R9, [R13+96]
; 87A4:       0F87310E0000     JA L95
; 87AA:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 87AE: L6:   4C8B4DE0         MOV R9, [RBP-32]
; 87B2:       4D8908           MOV [R8], R9
; 87B5:       4D896008         MOV [R8+8], R12                ; NIL
; 87B9:       4180C807         OR R8B, 7
; 87BD:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 87C1:       7402             JE L7
; 87C3:       0F0B             UD2
; 87C5: L7:   4C8945C0         MOV [RBP-64], R8
; 87C9:       488B75E0         MOV RSI, [RBP-32]
; 87CD:       4883EC10         SUB RSP, 16
; 87D1:       488B55E8         MOV RDX, [RBP-24]
; 87D5:       31FF             XOR EDI, EDI
; 87D7:       48892C24         MOV [RSP], RBP
; 87DB:       488BEC           MOV RBP, RSP
; 87DE:       FF157C94ABFF     CALL [RIP-5532548]             ; SB-KERNEL:LIST-SUBSEQ
; 87E4:       4883EC10         SUB RSP, 16
; 87E8:       B902000000       MOV ECX, 2
; 87ED:       48892C24         MOV [RSP], RBP
; 87F1:       488BEC           MOV RBP, RSP
; 87F4:       FF15A68CABFF     CALL [RIP-5534554]             ; SB-IMPL::LIST-REVERSE
; 87FA:       4C8BCA           MOV R9, RDX
; 87FD:       4C894DB8         MOV [RBP-72], R9
; 8801:       488B7DE0         MOV RDI, [RBP-32]
; 8805:       4883EC10         SUB RSP, 16
; 8809:       488B55E8         MOV RDX, [RBP-24]
; 880D:       498BF4           MOV RSI, R12                   ; NIL
; 8810:       48892C24         MOV [RSP], RBP
; 8814:       488BEC           MOV RBP, RSP
; 8817:       FF154394ABFF     CALL [RIP-5532605]             ; SB-KERNEL:LIST-SUBSEQ
; 881D:       4C8B4DB8         MOV R9, [RBP-72]
; 8821:       488BF2           MOV RSI, RDX
; 8824:       4883EC10         SUB RSP, 16
; 8828:       488B15A1FCFFFF   MOV RDX, [RIP-863]             ; '(FLIP (THE FIXNUM (1+ ACC)))
; 882F:       498BF9           MOV RDI, R9
; 8832:       B906000000       MOV ECX, 6
; 8837:       48892C24         MOV [RSP], RBP
; 883B:       488BEC           MOV RBP, RSP
; 883E:       FF153479ABFF     CALL [RIP-5539532]             ; APPEND
; 8844:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8848:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 884C:       4D8D4820         LEA R9, [R8+32]
; 8850:       4D3B4D60         CMP R9, [R13+96]
; 8854:       0F878F0D0000     JA L96
; 885A:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 885E: L8:   4D8D4817         LEA R9, [R8+23]
; 8862:       4D894808         MOV [R8+8], R9
; 8866:       4C8B4DC0         MOV R9, [RBP-64]
; 886A:       4D8908           MOV [R8], R9
; 886D:       49895010         MOV [R8+16], RDX
; 8871:       4D896018         MOV [R8+24], R12               ; NIL
; 8875:       4D8D5007         LEA R10, [R8+7]
; 8879:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 887D:       7402             JE L9
; 887F:       0F0B             UD2
; 8881: L9:   4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8885:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8889:       4D8D4810         LEA R9, [R8+16]
; 888D:       4D3B4D60         CMP R9, [R13+96]
; 8891:       0F87600D0000     JA L97
; 8897:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 889B: L10:  4D8910           MOV [R8], R10
; 889E:       4D896008         MOV [R8+8], R12                ; NIL
; 88A2:       4D8D4807         LEA R9, [R8+7]
; 88A6:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 88AA:       7402             JE L11
; 88AC:       0F0B             UD2
; 88AE: L11:  4D8BD1           MOV R10, R9
; 88B1:       488B45C8         MOV RAX, [RBP-56]
; 88B5:       4C8BC0           MOV R8, RAX
; 88B8:       49C1E80A         SHR R8, 10
; 88BC:       4181E0FFFF0F00   AND R8D, 1048575
; 88C3:       43C644046900     MOV BYTE PTR [R12+R8+105], 0
; 88C9:       4C894801         MOV [RAX+1], R9
; 88CD:       488345E002       ADD QWORD PTR [RBP-32], 2
; 88D2: L12:  4C8955C0         MOV [RBP-64], R10
; 88D6:       488B55E0         MOV RDX, [RBP-32]
; 88DA:       488B7DF0         MOV RDI, [RBP-16]
; 88DE:       E84D89FBFF       CALL #xB800001230              ; SB-VM::GENERIC->
; 88E3:       4C8B55C0         MOV R10, [RBP-64]
; 88E7:       0F8EA3FEFFFF     JLE L5
; 88ED:       488B45D0         MOV RAX, [RBP-48]
; 88F1:       4C8B4001         MOV R8, [RAX+1]
; 88F5:       4C8945E0         MOV [RBP-32], R8
; 88F9:       488B65D8         MOV RSP, [RBP-40]
; 88FD:       48C745D800000000 MOV QWORD PTR [RBP-40], 0
; 8905:       488965D0         MOV [RBP-48], RSP
; 8909:       4154             PUSH R12                       ; NIL
; 890B:       4C8D4424FF       LEA R8, [RSP-1]
; 8910:       4C8945C8         MOV [RBP-56], R8
; 8914:       4C8945C0         MOV [RBP-64], R8
; 8918:       E93D040000       JMP L42
; 891D:       0F1F00           NOP
; 8920: L13:  488B75D8         MOV RSI, [RBP-40]
; 8924:       4883C602         ADD RSI, 2
; 8928:       4883EC10         SUB RSP, 16
; 892C:       488B55E8         MOV RDX, [RBP-24]
; 8930:       BF02000000       MOV EDI, 2
; 8935:       48892C24         MOV [RSP], RBP
; 8939:       488BEC           MOV RBP, RSP
; 893C:       FF151E93ABFF     CALL [RIP-5532898]             ; SB-KERNEL:LIST-SUBSEQ
; 8942:       4C8BD2           MOV R10, RDX
; 8945:       4C8955B0         MOV [RBP-80], R10
; 8949:       488B45E8         MOV RAX, [RBP-24]
; 894D:       4C8B48F9         MOV R9, [RAX-7]
; 8951:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8955:       498B7D58         MOV RDI, [R13+88]              ; thread.cons-tlab
; 8959:       4C8D4710         LEA R8, [RDI+16]
; 895D:       4D3B4560         CMP R8, [R13+96]
; 8961:       0F879E0C0000     JA L98
; 8967:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 896B: L14:  4C890F           MOV [RDI], R9
; 896E:       4C896708         MOV [RDI+8], R12               ; NIL
; 8972:       4C8D4F07         LEA R9, [RDI+7]
; 8976:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 897A:       7402             JE L15
; 897C:       0F0B             UD2
; 897E: L15:  4C894DB8         MOV [RBP-72], R9
; 8982:       488B7DD8         MOV RDI, [RBP-40]
; 8986:       4883C702         ADD RDI, 2
; 898A:       4883EC10         SUB RSP, 16
; 898E:       488B55E8         MOV RDX, [RBP-24]
; 8992:       498BF4           MOV RSI, R12                   ; NIL
; 8995:       48892C24         MOV [RSP], RBP
; 8999:       488BEC           MOV RBP, RSP
; 899C:       FF15BE92ABFF     CALL [RIP-5532994]             ; SB-KERNEL:LIST-SUBSEQ
; 89A2:       4C8B55B0         MOV R10, [RBP-80]
; 89A6:       4C8B4DB8         MOV R9, [RBP-72]
; 89AA:       488BF2           MOV RSI, RDX
; 89AD:       4883EC10         SUB RSP, 16
; 89B1:       498BD2           MOV RDX, R10
; 89B4:       498BF9           MOV RDI, R9
; 89B7:       B906000000       MOV ECX, 6
; 89BC:       48892C24         MOV [RSP], RBP
; 89C0:       488BEC           MOV RBP, RSP
; 89C3:       FF15AF77ABFF     CALL [RIP-5539921]             ; APPEND
; 89C9:       4C8B55C0         MOV R10, [RBP-64]
; 89CD:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 89D1:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 89D5:       4D8D4810         LEA R9, [R8+16]
; 89D9:       4D3B4D60         CMP R9, [R13+96]
; 89DD:       0F872F0C0000     JA L99
; 89E3:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 89E7: L16:  4C8B4DD8         MOV R9, [RBP-40]
; 89EB:       4D8908           MOV [R8], R9
; 89EE:       4D896008         MOV [R8+8], R12                ; NIL
; 89F2:       4D8D7007         LEA R14, [R8+7]
; 89F6:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 89FA:       7402             JE L17
; 89FC:       0F0B             UD2
; 89FE: L17:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8A02:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8A06:       4D8D4820         LEA R9, [R8+32]
; 8A0A:       4D3B4D60         CMP R9, [R13+96]
; 8A0E:       0F870C0C0000     JA L100
; 8A14:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8A18: L18:  4D8D4817         LEA R9, [R8+23]
; 8A1C:       4D894808         MOV [R8+8], R9
; 8A20:       4C8B0DB1FAFFFF   MOV R9, [RIP-1359]             ; 'FLIP
; 8A27:       4D8908           MOV [R8], R9
; 8A2A:       49C7401000000000 MOV QWORD PTR [R8+16], 0
; 8A32:       49895018         MOV [R8+24], RDX
; 8A36:       4D8D5807         LEA R11, [R8+7]
; 8A3A:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8A3E:       7402             JE L19
; 8A40:       0F0B             UD2
; 8A42: L19:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8A46:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8A4A:       4D8D4130         LEA R8, [R9+48]
; 8A4E:       4D3B4560         CMP R8, [R13+96]
; 8A52:       0F87D60B0000     JA L101
; 8A58:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8A5C: L20:  49C701CFC30450   MOV QWORD PTR [R9], #x5004C3CF  ; 'THE
; 8A63:       49C741104F620450 MOV QWORD PTR [R9+16], #x5004624F  ; 'FIXNUM
; 8A6B:       4D895920         MOV [R9+32], R11
; 8A6F:       4D896128         MOV [R9+40], R12               ; NIL
; 8A73:       4D8D4117         LEA R8, [R9+23]
; 8A77:       4D8940F1         MOV [R8-15], R8
; 8A7B:       4983C010         ADD R8, 16
; 8A7F:       4D8940F1         MOV [R8-15], R8
; 8A83:       4D8D5907         LEA R11, [R9+7]
; 8A87:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8A8B:       7402             JE L21
; 8A8D:       0F0B             UD2
; 8A8F: L21:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8A93:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8A97:       4D8D4820         LEA R9, [R8+32]
; 8A9B:       4D3B4D60         CMP R9, [R13+96]
; 8A9F:       0F87970B0000     JA L102
; 8AA5:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8AA9: L22:  4D8D4817         LEA R9, [R8+23]
; 8AAD:       4D894808         MOV [R8+8], R9
; 8AB1:       4C8B0D38FAFFFF   MOV R9, [RIP-1480]             ; 'NFLIPS
; 8AB8:       4D8908           MOV [R8], R9
; 8ABB:       4D895810         MOV [R8+16], R11
; 8ABF:       4D896018         MOV [R8+24], R12               ; NIL
; 8AC3:       4D8D5807         LEA R11, [R8+7]
; 8AC7:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8ACB:       7402             JE L23
; 8ACD:       0F0B             UD2
; 8ACF: L23:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8AD3:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8AD7:       4D8D4810         LEA R9, [R8+16]
; 8ADB:       4D3B4D60         CMP R9, [R13+96]
; 8ADF:       0F87650B0000     JA L103
; 8AE5:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8AE9: L24:  4D8918           MOV [R8], R11
; 8AEC:       4D896008         MOV [R8+8], R12                ; NIL
; 8AF0:       4D8D5807         LEA R11, [R8+7]
; 8AF4:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8AF8:       7402             JE L25
; 8AFA:       0F0B             UD2
; 8AFC: L25:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8B00:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8B04:       4D8D4820         LEA R9, [R8+32]
; 8B08:       4D3B4D60         CMP R9, [R13+96]
; 8B0C:       0F87460B0000     JA L104
; 8B12:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8B16: L26:  4D8D4817         LEA R9, [R8+23]
; 8B1A:       4D894808         MOV [R8+8], R9
; 8B1E:       49C700CF760450   MOV QWORD PTR [R8], #x500476CF  ; 'LET
; 8B25:       4D895810         MOV [R8+16], R11
; 8B29:       4C8B0DD0F9FFFF   MOV R9, [RIP-1584]             ; '((DECLARE (TYPE FIXNUM NFLIPS)) (COND (EVEN # #) (T # #)) (WHEN (> NFLIPS MAX-FLIPS) (SETF #)))
; 8B30:       4D894818         MOV [R8+24], R9
; 8B34:       4D8D7807         LEA R15, [R8+7]
; 8B38:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8B3C:       7402             JE L27
; 8B3E:       0F0B             UD2
; 8B40: L27:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8B44:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8B48:       4D8D4820         LEA R9, [R8+32]
; 8B4C:       4D3B4D60         CMP R9, [R13+96]
; 8B50:       0F87100B0000     JA L105
; 8B56:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8B5A: L28:  4D8D4817         LEA R9, [R8+23]
; 8B5E:       4D894808         MOV [R8+8], R9
; 8B62:       4C8B0D9FF9FFFF   MOV R9, [RIP-1633]             ; 'PERM
; 8B69:       4D8908           MOV [R8], R9
; 8B6C:       49C7401002000000 MOV QWORD PTR [R8+16], 2
; 8B74:       49895018         MOV [R8+24], RDX
; 8B78:       4D8D5807         LEA R11, [R8+7]
; 8B7C:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8B80:       7402             JE L29
; 8B82:       0F0B             UD2
; 8B84: L29:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8B88:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8B8C:       4D8D4140         LEA R8, [R9+64]
; 8B90:       4D3B4560         CMP R8, [R13+96]
; 8B94:       0F87DA0A0000     JA L106
; 8B9A:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8B9E: L30:  49C701EF9D0450   MOV QWORD PTR [R9], #x50049DEF  ; 'PROGN
; 8BA5:       4C8B056CF9FFFF   MOV R8, [RIP-1684]             ; '(INCF (AREF C I))
; 8BAC:       4D894110         MOV [R9+16], R8
; 8BB0:       4D897920         MOV [R9+32], R15
; 8BB4:       4D895930         MOV [R9+48], R11
; 8BB8:       4D896138         MOV [R9+56], R12               ; NIL
; 8BBC:       4D8D4117         LEA R8, [R9+23]
; 8BC0:       4D8940F1         MOV [R8-15], R8
; 8BC4:       4983C010         ADD R8, 16
; 8BC8:       4D8940F1         MOV [R8-15], R8
; 8BCC:       4983C010         ADD R8, 16
; 8BD0:       4D8940F1         MOV [R8-15], R8
; 8BD4:       4D8D7907         LEA R15, [R9+7]
; 8BD8:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8BDC:       7402             JE L31
; 8BDE:       0F0B             UD2
; 8BE0: L31:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8BE4:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8BE8:       4D8D4820         LEA R9, [R8+32]
; 8BEC:       4D3B4D60         CMP R9, [R13+96]
; 8BF0:       0F878C0A0000     JA L107
; 8BF6:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8BFA: L32:  4D8D4817         LEA R9, [R8+23]
; 8BFE:       4D894808         MOV [R8+8], R9
; 8C02:       4C8B0DFFF8FFFF   MOV R9, [RIP-1793]             ; 'PERM
; 8C09:       4D8908           MOV [R8], R9
; 8C0C:       4C8B0D0DF9FFFF   MOV R9, [RIP-1779]             ; '(+ I 1)
; 8C13:       4D894810         MOV [R8+16], R9
; 8C17:       49895018         MOV [R8+24], RDX
; 8C1B:       4D8D5807         LEA R11, [R8+7]
; 8C1F:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8C23:       7402             JE L33
; 8C25:       0F0B             UD2
; 8C27: L33:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8C2B:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8C2F:       4D8D4130         LEA R8, [R9+48]
; 8C33:       4D3B4560         CMP R8, [R13+96]
; 8C37:       0F87530A0000     JA L108
; 8C3D:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8C41: L34:  49C701EF9D0450   MOV QWORD PTR [R9], #x50049DEF  ; 'PROGN
; 8C48:       4C8B05D9F8FFFF   MOV R8, [RIP-1831]             ; '(SETF (AREF C I) 0)
; 8C4F:       4D894110         MOV [R9+16], R8
; 8C53:       4D895920         MOV [R9+32], R11
; 8C57:       4D896128         MOV [R9+40], R12               ; NIL
; 8C5B:       4D8D4117         LEA R8, [R9+23]
; 8C5F:       4D8940F1         MOV [R8-15], R8
; 8C63:       4983C010         ADD R8, 16
; 8C67:       4D8940F1         MOV [R8-15], R8
; 8C6B:       4D8D5907         LEA R11, [R9+7]
; 8C6F:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8C73:       7402             JE L35
; 8C75:       0F0B             UD2
; 8C77: L35:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8C7B:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8C7F:       4D8D4140         LEA R8, [R9+64]
; 8C83:       4D3B4560         CMP R8, [R13+96]
; 8C87:       0F87110A0000     JA L109
; 8C8D:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8C91: L36:  49C701CF6C0450   MOV QWORD PTR [R9], #x50046CCF  ; 'IF
; 8C98:       4C8B0599F8FFFF   MOV R8, [RIP-1895]             ; '(< (AREF C I) I)
; 8C9F:       4D894110         MOV [R9+16], R8
; 8CA3:       4D897920         MOV [R9+32], R15
; 8CA7:       4D895930         MOV [R9+48], R11
; 8CAB:       4D896138         MOV [R9+56], R12               ; NIL
; 8CAF:       4D8D4117         LEA R8, [R9+23]
; 8CB3:       4D8940F1         MOV [R8-15], R8
; 8CB7:       4983C010         ADD R8, 16
; 8CBB:       4D8940F1         MOV [R8-15], R8
; 8CBF:       4983C010         ADD R8, 16
; 8CC3:       4D8940F1         MOV [R8-15], R8
; 8CC7:       4D8D5907         LEA R11, [R9+7]
; 8CCB:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8CCF:       7402             JE L37
; 8CD1:       0F0B             UD2
; 8CD3: L37:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8CD7:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8CDB:       4D8D4820         LEA R9, [R8+32]
; 8CDF:       4D3B4D60         CMP R9, [R13+96]
; 8CE3:       0F87C3090000     JA L110
; 8CE9:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8CED: L38:  4D8D4817         LEA R9, [R8+23]
; 8CF1:       4D894808         MOV [R8+8], R9
; 8CF5:       4D8930           MOV [R8], R14
; 8CF8:       4D895810         MOV [R8+16], R11
; 8CFC:       4D896018         MOV [R8+24], R12               ; NIL
; 8D00:       4D8D5807         LEA R11, [R8+7]
; 8D04:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8D08:       7402             JE L39
; 8D0A:       0F0B             UD2
; 8D0C: L39:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8D10:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8D14:       4D8D4810         LEA R9, [R8+16]
; 8D18:       4D3B4D60         CMP R9, [R13+96]
; 8D1C:       0F8798090000     JA L111
; 8D22:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8D26: L40:  4D8918           MOV [R8], R11
; 8D29:       4D896008         MOV [R8+8], R12                ; NIL
; 8D2D:       4D8D4807         LEA R9, [R8+7]
; 8D31:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8D35:       7402             JE L41
; 8D37:       0F0B             UD2
; 8D39: L41:  4C894DC0         MOV [RBP-64], R9
; 8D3D:       4D8BC2           MOV R8, R10
; 8D40:       49C1E80A         SHR R8, 10
; 8D44:       4181E0FFFF0F00   AND R8D, 1048575
; 8D4B:       43C644046900     MOV BYTE PTR [R12+R8+105], 0
; 8D51:       4D894A01         MOV [R10+1], R9
; 8D55:       488345D802       ADD QWORD PTR [RBP-40], 2
; 8D5A: L42:  488B55D8         MOV RDX, [RBP-40]
; 8D5E:       4883EC10         SUB RSP, 16
; 8D62:       488B7DF0         MOV RDI, [RBP-16]
; 8D66:       48892C24         MOV [RSP], RBP
; 8D6A:       488BEC           MOV RBP, RSP
; 8D6D:       FF151D77ABFF     CALL [RIP-5540067]             ; SB-KERNEL:TWO-ARG->=
; 8D73:       4438E2           CMP DL, R12B                   ; NIL
; 8D76:       0F84A4FBFFFF     JE L13
; 8D7C:       488B45C8         MOV RAX, [RBP-56]
; 8D80:       4C8B4001         MOV R8, [RAX+1]
; 8D84:       4D8BD0           MOV R10, R8
; 8D87:       488B65D0         MOV RSP, [RBP-48]
; 8D8B:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8D8F:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8D93:       4D8D4820         LEA R9, [R8+32]
; 8D97:       4D3B4D60         CMP R9, [R13+96]
; 8D9B:       0F8727090000     JA L112
; 8DA1:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8DA5: L43:  4D8D4817         LEA R9, [R8+23]
; 8DA9:       4D894808         MOV [R8+8], R9
; 8DAD:       49C7003F7F0450   MOV QWORD PTR [R8], #x50047F3F  ; 'MAKE-ARRAY
; 8DB4:       4C8B4DF0         MOV R9, [RBP-16]
; 8DB8:       4D894810         MOV [R8+16], R9
; 8DBC:       4C8B0D85F7FFFF   MOV R9, [RIP-2171]             ; '(:INITIAL-ELEMENT 0 :ELEMENT-TYPE 'FIXNUM)
; 8DC3:       4D894818         MOV [R8+24], R9
; 8DC7:       4D8D5807         LEA R11, [R8+7]
; 8DCB:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8DCF:       7402             JE L44
; 8DD1:       0F0B             UD2
; 8DD3: L44:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8DD7:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8DDB:       4D8D4820         LEA R9, [R8+32]
; 8DDF:       4D3B4D60         CMP R9, [R13+96]
; 8DE3:       0F87ED080000     JA L113
; 8DE9:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8DED: L45:  4D8D4817         LEA R9, [R8+23]
; 8DF1:       4D894808         MOV [R8+8], R9
; 8DF5:       4C8B0D54F7FFFF   MOV R9, [RIP-2220]             ; 'C
; 8DFC:       4D8908           MOV [R8], R9
; 8DFF:       4D895810         MOV [R8+16], R11
; 8E03:       4D896018         MOV [R8+24], R12               ; NIL
; 8E07:       4D8D5807         LEA R11, [R8+7]
; 8E0B:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8E0F:       7402             JE L46
; 8E11:       0F0B             UD2
; 8E13: L46:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8E17:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8E1B:       4D8D4140         LEA R8, [R9+64]
; 8E1F:       4D3B4560         CMP R8, [R13+96]
; 8E23:       0F87BB080000     JA L114
; 8E29:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8E2D: L47:  4C8B0524F7FFFF   MOV R8, [RIP-2268]             ; '(CSUM 0)
; 8E34:       4D8901           MOV [R9], R8
; 8E37:       4C8B0522F7FFFF   MOV R8, [RIP-2270]             ; '(MAX-FLIPS 0)
; 8E3E:       4D894110         MOV [R9+16], R8
; 8E42:       4C8B051FF7FFFF   MOV R8, [RIP-2273]             ; '(EVEN NIL)
; 8E49:       4D894120         MOV [R9+32], R8
; 8E4D:       4D895930         MOV [R9+48], R11
; 8E51:       4D896138         MOV [R9+56], R12               ; NIL
; 8E55:       4D8D4117         LEA R8, [R9+23]
; 8E59:       4D8940F1         MOV [R8-15], R8
; 8E5D:       4983C010         ADD R8, 16
; 8E61:       4D8940F1         MOV [R8-15], R8
; 8E65:       4983C010         ADD R8, 16
; 8E69:       4D8940F1         MOV [R8-15], R8
; 8E6D:       4D8D7107         LEA R14, [R9+7]
; 8E71:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8E75:       7402             JE L48
; 8E77:       0F0B             UD2
; 8E79: L48:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8E7D:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8E81:       4D8D4810         LEA R9, [R8+16]
; 8E85:       4D3B4D60         CMP R9, [R13+96]
; 8E89:       0F8763080000     JA L115
; 8E8F:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8E93: L49:  4C8B4DF0         MOV R9, [RBP-16]
; 8E97:       4D8908           MOV [R8], R9
; 8E9A:       4D896008         MOV [R8+8], R12                ; NIL
; 8E9E:       4D8D5807         LEA R11, [R8+7]
; 8EA2:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8EA6:       7402             JE L50
; 8EA8:       0F0B             UD2
; 8EAA: L50:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8EAE:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8EB2:       4D8D4130         LEA R8, [R9+48]
; 8EB6:       4D3B4560         CMP R8, [R13+96]
; 8EBA:       0F8740080000     JA L116
; 8EC0:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8EC4: L51:  49C701CFAF0450   MOV QWORD PTR [R9], #x5004AFCF  ; 'SIMPLE-ARRAY
; 8ECB:       49C741104F620450 MOV QWORD PTR [R9+16], #x5004624F  ; 'FIXNUM
; 8ED3:       4D895920         MOV [R9+32], R11
; 8ED7:       4D896128         MOV [R9+40], R12               ; NIL
; 8EDB:       4D8D4117         LEA R8, [R9+23]
; 8EDF:       4D8940F1         MOV [R8-15], R8
; 8EE3:       4983C010         ADD R8, 16
; 8EE7:       4D8940F1         MOV [R8-15], R8
; 8EEB:       4D8D5907         LEA R11, [R9+7]
; 8EEF:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8EF3:       7402             JE L52
; 8EF5:       0F0B             UD2
; 8EF7: L52:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8EFB:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8EFF:       4D8D4820         LEA R9, [R8+32]
; 8F03:       4D3B4D60         CMP R9, [R13+96]
; 8F07:       0F8701080000     JA L117
; 8F0D:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8F11: L53:  4D8D4817         LEA R9, [R8+23]
; 8F15:       4D894808         MOV [R8+8], R9
; 8F19:       49C7003FC60450   MOV QWORD PTR [R8], #x5004C63F  ; 'TYPE
; 8F20:       4D895810         MOV [R8+16], R11
; 8F24:       4C8B0D55F6FFFF   MOV R9, [RIP-2475]             ; '(C)
; 8F2B:       4D894818         MOV [R8+24], R9
; 8F2F:       4D8D5807         LEA R11, [R8+7]
; 8F33:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8F37:       7402             JE L54
; 8F39:       0F0B             UD2
; 8F3B: L54:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8F3F:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8F43:       4D8D4140         LEA R8, [R9+64]
; 8F47:       4D3B4560         CMP R8, [R13+96]
; 8F4B:       0F87CB070000     JA L118
; 8F51:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8F55: L55:  49C7014F4E0450   MOV QWORD PTR [R9], #x50044E4F  ; 'DECLARE
; 8F5C:       4C8B052DF6FFFF   MOV R8, [RIP-2515]             ; '(TYPE FIXNUM CSUM MAX-FLIPS)
; 8F63:       4D894110         MOV [R9+16], R8
; 8F67:       4C8B052AF6FFFF   MOV R8, [RIP-2518]             ; '(TYPE BOOLEAN EVEN)
; 8F6E:       4D894120         MOV [R9+32], R8
; 8F72:       4D895930         MOV [R9+48], R11
; 8F76:       4D896138         MOV [R9+56], R12               ; NIL
; 8F7A:       4D8D4117         LEA R8, [R9+23]
; 8F7E:       4D8940F1         MOV [R8-15], R8
; 8F82:       4983C010         ADD R8, 16
; 8F86:       4D8940F1         MOV [R8-15], R8
; 8F8A:       4983C010         ADD R8, 16
; 8F8E:       4D8940F1         MOV [R8-15], R8
; 8F92:       4D8D5907         LEA R11, [R9+7]
; 8F96:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8F9A:       7402             JE L56
; 8F9C:       0F0B             UD2
; 8F9E: L56:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8FA2:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 8FA6:       4D8D4810         LEA R9, [R8+16]
; 8FAA:       4D3B4D60         CMP R9, [R13+96]
; 8FAE:       0F8776070000     JA L119
; 8FB4:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 8FB8: L57:  4C8B0DE1F5FFFF   MOV R9, [RIP-2591]             ; 'ACC
; 8FBF:       4D8908           MOV [R8], R9
; 8FC2:       4C8B4DE8         MOV R9, [RBP-24]
; 8FC6:       4D894808         MOV [R8+8], R9
; 8FCA:       498D4807         LEA RCX, [R8+7]
; 8FCE:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 8FD2:       7402             JE L58
; 8FD4:       0F0B             UD2
; 8FD6: L58:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 8FDA:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 8FDE:       4D8D4130         LEA R8, [R9+48]
; 8FE2:       4D3B4560         CMP R8, [R13+96]
; 8FE6:       0F874C070000     JA L120
; 8FEC:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 8FF0: L59:  49C7013FC60450   MOV QWORD PTR [R9], #x5004C63F  ; 'TYPE
; 8FF7:       49C741104F620450 MOV QWORD PTR [R9+16], #x5004624F  ; 'FIXNUM
; 8FFF:       4C8B059AF5FFFF   MOV R8, [RIP-2662]             ; 'ACC
; 9006:       4D894120         MOV [R9+32], R8
; 900A:       4C8B45E8         MOV R8, [RBP-24]
; 900E:       4D894128         MOV [R9+40], R8
; 9012:       4D8D4117         LEA R8, [R9+23]
; 9016:       4D8940F1         MOV [R8-15], R8
; 901A:       4983C010         ADD R8, 16
; 901E:       4D8940F1         MOV [R8-15], R8
; 9022:       4D8D7907         LEA R15, [R9+7]
; 9026:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 902A:       7402             JE L60
; 902C:       0F0B             UD2
; 902E: L60:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 9032:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 9036:       4D8D4820         LEA R9, [R8+32]
; 903A:       4D3B4D60         CMP R9, [R13+96]
; 903E:       0F8702070000     JA L121
; 9044:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 9048: L61:  4D8D4817         LEA R9, [R8+23]
; 904C:       4D894808         MOV [R8+8], R9
; 9050:       49C7004F4E0450   MOV QWORD PTR [R8], #x50044E4F  ; 'DECLARE
; 9057:       4D897810         MOV [R8+16], R15
; 905B:       4D896018         MOV [R8+24], R12               ; NIL
; 905F:       498D4007         LEA RAX, [R8+7]
; 9063:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9067:       7402             JE L62
; 9069:       0F0B             UD2
; 906B: L62:  488B55E8         MOV RDX, [RBP-24]
; 906F:       4C8B7AF9         MOV R15, [RDX-7]
; 9073:       4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 9077:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 907B:       4D8D4130         LEA R8, [R9+48]
; 907F:       4D3B4560         CMP R8, [R13+96]
; 9083:       0F87CB060000     JA L122
; 9089:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 908D: L63:  49C7011F580450   MOV QWORD PTR [R9], #x5004581F  ; 'ECASE
; 9094:       4D897910         MOV [R9+16], R15
; 9098:       4C8B0511F5FFFF   MOV R8, [RIP-2799]             ; '((1) ACC)
; 909F:       4D894120         MOV [R9+32], R8
; 90A3:       4C8B45E0         MOV R8, [RBP-32]
; 90A7:       4D894128         MOV [R9+40], R8
; 90AB:       4D8D4117         LEA R8, [R9+23]
; 90AF:       4D8940F1         MOV [R8-15], R8
; 90B3:       4983C010         ADD R8, 16
; 90B7:       4D8940F1         MOV [R8-15], R8
; 90BB:       4D8D7907         LEA R15, [R9+7]
; 90BF:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 90C3:       7402             JE L64
; 90C5:       0F0B             UD2
; 90C7: L64:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 90CB:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 90CF:       4D8D4140         LEA R8, [R9+64]
; 90D3:       4D3B4560         CMP R8, [R13+96]
; 90D7:       0F8785060000     JA L123
; 90DD:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 90E1: L65:  4C8B05F0F3FFFF   MOV R8, [RIP-3088]             ; 'FLIP
; 90E8:       4D8901           MOV [R9], R8
; 90EB:       49894910         MOV [R9+16], RCX
; 90EF:       49894120         MOV [R9+32], RAX
; 90F3:       4D897930         MOV [R9+48], R15
; 90F7:       4D896138         MOV [R9+56], R12               ; NIL
; 90FB:       4D8D4117         LEA R8, [R9+23]
; 90FF:       4D8940F1         MOV [R8-15], R8
; 9103:       4983C010         ADD R8, 16
; 9107:       4D8940F1         MOV [R8-15], R8
; 910B:       4983C010         ADD R8, 16
; 910F:       4D8940F1         MOV [R8-15], R8
; 9113:       4D8D7907         LEA R15, [R9+7]
; 9117:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 911B:       7402             JE L66
; 911D:       0F0B             UD2
; 911F: L66:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 9123:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 9127:       4D8D4810         LEA R9, [R8+16]
; 912B:       4D3B4D60         CMP R9, [R13+96]
; 912F:       0F873B060000     JA L124
; 9135:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 9139: L67:  4C8B0D78F4FFFF   MOV R9, [RIP-2952]             ; 'I
; 9140:       4D8908           MOV [R8], R9
; 9143:       4C8B4DE8         MOV R9, [RBP-24]
; 9147:       4D894808         MOV [R8+8], R9
; 914B:       498D4807         LEA RCX, [R8+7]
; 914F:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9153:       7402             JE L68
; 9155:       0F0B             UD2
; 9157: L68:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 915B:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 915F:       4D8D4130         LEA R8, [R9+48]
; 9163:       4D3B4560         CMP R8, [R13+96]
; 9167:       0F8711060000     JA L125
; 916D:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 9171: L69:  49C7013FC60450   MOV QWORD PTR [R9], #x5004C63F  ; 'TYPE
; 9178:       49C741104F620450 MOV QWORD PTR [R9+16], #x5004624F  ; 'FIXNUM
; 9180:       4C8B0531F4FFFF   MOV R8, [RIP-3023]             ; 'I
; 9187:       4D894120         MOV [R9+32], R8
; 918B:       4C8B45E8         MOV R8, [RBP-24]
; 918F:       4D894128         MOV [R9+40], R8
; 9193:       4D8D4117         LEA R8, [R9+23]
; 9197:       4D8940F1         MOV [R8-15], R8
; 919B:       4983C010         ADD R8, 16
; 919F:       4D8940F1         MOV [R8-15], R8
; 91A3:       498D4107         LEA RAX, [R9+7]
; 91A7:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 91AB:       7402             JE L70
; 91AD:       0F0B             UD2
; 91AF: L70:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 91B3:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 91B7:       4D8D4820         LEA R9, [R8+32]
; 91BB:       4D3B4D60         CMP R9, [R13+96]
; 91BF:       0F87C7050000     JA L126
; 91C5:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 91C9: L71:  4D8D4817         LEA R9, [R8+23]
; 91CD:       4D894808         MOV [R8+8], R9
; 91D1:       49C7004F4E0450   MOV QWORD PTR [R8], #x50044E4F  ; 'DECLARE
; 91D8:       49894010         MOV [R8+16], RAX
; 91DC:       4D896018         MOV [R8+24], R12               ; NIL
; 91E0:       498D4007         LEA RAX, [R8+7]
; 91E4:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 91E8:       7402             JE L72
; 91EA:       0F0B             UD2
; 91EC: L72:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 91F0:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 91F4:       4D8D4150         LEA R8, [R9+80]
; 91F8:       4D3B4560         CMP R8, [R13+96]
; 91FC:       0F8798050000     JA L127
; 9202:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 9206: L73:  49C7011F650450   MOV QWORD PTR [R9], #x5004651F  ; 'FORMAT
; 920D:       4D8D442488       LEA R8, [R12-120]              ; T
; 9212:       4D894110         MOV [R9+16], R8
; 9216:       4C8B05ABF3FFFF   MOV R8, [RIP-3157]             ; "~s~%Pfannkuchen(~s) = ~s~%"
; 921D:       4D894120         MOV [R9+32], R8
; 9221:       4C8B05A8F3FFFF   MOV R8, [RIP-3160]             ; 'CSUM
; 9228:       4D894130         MOV [R9+48], R8
; 922C:       4C8B45F0         MOV R8, [RBP-16]
; 9230:       4D894140         MOV [R9+64], R8
; 9234:       4C8B059DF3FFFF   MOV R8, [RIP-3171]             ; '(MAX-FLIPS)
; 923B:       4D894148         MOV [R9+72], R8
; 923F:       4D8D4117         LEA R8, [R9+23]
; 9243:       4D8940F1         MOV [R8-15], R8
; 9247:       4983C010         ADD R8, 16
; 924B:       4D8940F1         MOV [R8-15], R8
; 924F:       4983C010         ADD R8, 16
; 9253:       4D8940F1         MOV [R8-15], R8
; 9257:       4983C010         ADD R8, 16
; 925B:       4D8940F1         MOV [R8-15], R8
; 925F:       498D5107         LEA RDX, [R9+7]
; 9263:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9267:       7402             JE L74
; 9269:       0F0B             UD2
; 926B: L74:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 926F:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 9273:       4D8D4820         LEA R9, [R8+32]
; 9277:       4D3B4D60         CMP R9, [R13+96]
; 927B:       0F8727050000     JA L128
; 9281:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 9285: L75:  4D8D4817         LEA R9, [R8+23]
; 9289:       4D894808         MOV [R8+8], R9
; 928D:       4C8B4DF0         MOV R9, [RBP-16]
; 9291:       4D8908           MOV [R8], R9
; 9294:       49895010         MOV [R8+16], RDX
; 9298:       4D896018         MOV [R8+24], R12               ; NIL
; 929C:       498D5007         LEA RDX, [R8+7]
; 92A0:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 92A4:       7402             JE L76
; 92A6:       0F0B             UD2
; 92A8: L76:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 92AC:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 92B0:       4D8D4130         LEA R8, [R9+48]
; 92B4:       4D3B4560         CMP R8, [R13+96]
; 92B8:       0F87F8040000     JA L129
; 92BE:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 92C2: L77:  49C7011F580450   MOV QWORD PTR [R9], #x5004581F  ; 'ECASE
; 92C9:       4C8B05E8F2FFFF   MOV R8, [RIP-3352]             ; 'I
; 92D0:       4D894110         MOV [R9+16], R8
; 92D4:       49895120         MOV [R9+32], RDX
; 92D8:       4D895128         MOV [R9+40], R10
; 92DC:       4D8D4117         LEA R8, [R9+23]
; 92E0:       4D8940F1         MOV [R8-15], R8
; 92E4:       4983C010         ADD R8, 16
; 92E8:       4D8940F1         MOV [R8-15], R8
; 92EC:       4D8D5107         LEA R10, [R9+7]
; 92F0:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 92F4:       7402             JE L78
; 92F6:       0F0B             UD2
; 92F8: L78:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 92FC:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 9300:       4D8D4140         LEA R8, [R9+64]
; 9304:       4D3B4560         CMP R8, [R13+96]
; 9308:       0F87B6040000     JA L130
; 930E:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 9312: L79:  4C8B05EFF1FFFF   MOV R8, [RIP-3601]             ; 'PERM
; 9319:       4D8901           MOV [R9], R8
; 931C:       49894910         MOV [R9+16], RCX
; 9320:       49894120         MOV [R9+32], RAX
; 9324:       4D895130         MOV [R9+48], R10
; 9328:       4D896138         MOV [R9+56], R12               ; NIL
; 932C:       4D8D4117         LEA R8, [R9+23]
; 9330:       4D8940F1         MOV [R8-15], R8
; 9334:       4983C010         ADD R8, 16
; 9338:       4D8940F1         MOV [R8-15], R8
; 933C:       4983C010         ADD R8, 16
; 9340:       4D8940F1         MOV [R8-15], R8
; 9344:       4D8D5107         LEA R10, [R9+7]
; 9348:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 934C:       7402             JE L80
; 934E:       0F0B             UD2
; 9350: L80:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 9354:       4D8B4558         MOV R8, [R13+88]               ; thread.cons-tlab
; 9358:       4D8D4820         LEA R9, [R8+32]
; 935C:       4D3B4D60         CMP R9, [R13+96]
; 9360:       0F876C040000     JA L131
; 9366:       4D894D58         MOV [R13+88], R9               ; thread.cons-tlab
; 936A: L81:  4D8D4817         LEA R9, [R8+23]
; 936E:       4D894808         MOV [R8+8], R9
; 9372:       4D8938           MOV [R8], R15
; 9375:       4D895010         MOV [R8+16], R10
; 9379:       4D896018         MOV [R8+24], R12               ; NIL
; 937D:       4D8D7807         LEA R15, [R8+7]
; 9381:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9385:       7402             JE L82
; 9387:       0F0B             UD2
; 9389: L82:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 938D:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 9391:       4D8D8190000000   LEA R8, [R9+144]
; 9398:       4D3B4560         CMP R8, [R13+96]
; 939C:       0F873E040000     JA L132
; 93A2:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 93A6: L83:  49C7015F7D0450   MOV QWORD PTR [R9], #x50047D5F  ; 'LOOP
; 93AD:       4C8B0534F2FFFF   MOV R8, [RIP-3532]             ; 'FOR
; 93B4:       4D894110         MOV [R9+16], R8
; 93B8:       4C8B0531F2FFFF   MOV R8, [RIP-3535]             ; 'J
; 93BF:       4D894120         MOV [R9+32], R8
; 93C3:       4C8B052EF2FFFF   MOV R8, [RIP-3538]             ; 'OF-TYPE
; 93CA:       4D894130         MOV [R9+48], R8
; 93CE:       49C741404F620450 MOV QWORD PTR [R9+64], #x5004624F  ; 'FIXNUM
; 93D6:       4C8B0523F2FFFF   MOV R8, [RIP-3549]             ; 'FROM
; 93DD:       4D894150         MOV [R9+80], R8
; 93E1:       49C7416002000000 MOV QWORD PTR [R9+96], 2
; 93E9:       4C8B0518F2FFFF   MOV R8, [RIP-3560]             ; 'TO
; 93F0:       4D894170         MOV [R9+112], R8
; 93F4:       4C8B45F0         MOV R8, [RBP-16]
; 93F8:       4D898180000000   MOV [R9+128], R8
; 93FF:       4C8B050AF2FFFF   MOV R8, [RIP-3574]             ; '(COLLECT J)
; 9406:       4D898188000000   MOV [R9+136], R8
; 940D:       4D8D4117         LEA R8, [R9+23]
; 9411:       4D8940F1         MOV [R8-15], R8
; 9415:       4983C010         ADD R8, 16
; 9419:       4D8940F1         MOV [R8-15], R8
; 941D:       4983C010         ADD R8, 16
; 9421:       4D8940F1         MOV [R8-15], R8
; 9425:       4983C010         ADD R8, 16
; 9429:       4D8940F1         MOV [R8-15], R8
; 942D:       4983C010         ADD R8, 16
; 9431:       4D8940F1         MOV [R8-15], R8
; 9435:       4983C010         ADD R8, 16
; 9439:       4D8940F1         MOV [R8-15], R8
; 943D:       4983C010         ADD R8, 16
; 9441:       4D8940F1         MOV [R8-15], R8
; 9445:       4983C010         ADD R8, 16
; 9449:       4D8940F1         MOV [R8-15], R8
; 944D:       4D8D5107         LEA R10, [R9+7]
; 9451:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9455:       7402             JE L84
; 9457:       0F0B             UD2
; 9459: L84:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 945D:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 9461:       4D8D4140         LEA R8, [R9+64]
; 9465:       4D3B4560         CMP R8, [R13+96]
; 9469:       0F8782030000     JA L133
; 946F:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 9473: L85:  49C701FF290450   MOV QWORD PTR [R9], #x500429FF  ; 'APPLY
; 947A:       4C8B059FF1FFFF   MOV R8, [RIP-3681]             ; '#'PERM
; 9481:       4D894110         MOV [R9+16], R8
; 9485:       49C7412002000000 MOV QWORD PTR [R9+32], 2
; 948D:       4D895130         MOV [R9+48], R10
; 9491:       4D896138         MOV [R9+56], R12               ; NIL
; 9495:       4D8D4117         LEA R8, [R9+23]
; 9499:       4D8940F1         MOV [R8-15], R8
; 949D:       4983C010         ADD R8, 16
; 94A1:       4D8940F1         MOV [R8-15], R8
; 94A5:       4983C010         ADD R8, 16
; 94A9:       4D8940F1         MOV [R8-15], R8
; 94AD:       4D8D5107         LEA R10, [R9+7]
; 94B1:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 94B5:       7402             JE L86
; 94B7:       0F0B             UD2
; 94B9: L86:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 94BD:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 94C1:       4D8D4130         LEA R8, [R9+48]
; 94C5:       4D3B4560         CMP R8, [R13+96]
; 94C9:       0F8730030000     JA L134
; 94CF:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 94D3: L87:  49C701EF710450   MOV QWORD PTR [R9], #x500471EF  ; 'LABELS
; 94DA:       4D897910         MOV [R9+16], R15
; 94DE:       4D895120         MOV [R9+32], R10
; 94E2:       4D896128         MOV [R9+40], R12               ; NIL
; 94E6:       4D8D4117         LEA R8, [R9+23]
; 94EA:       4D8940F1         MOV [R8-15], R8
; 94EE:       4983C010         ADD R8, 16
; 94F2:       4D8940F1         MOV [R8-15], R8
; 94F6:       4D8D5107         LEA R10, [R9+7]
; 94FA:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 94FE:       7402             JE L88
; 9500:       0F0B             UD2
; 9502: L88:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 9506:       4D8B4D58         MOV R9, [R13+88]               ; thread.cons-tlab
; 950A:       4D8D4140         LEA R8, [R9+64]
; 950E:       4D3B4560         CMP R8, [R13+96]
; 9512:       0F87F5020000     JA L135
; 9518:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 951C: L89:  49C701CF760450   MOV QWORD PTR [R9], #x500476CF  ; 'LET
; 9523:       4D897110         MOV [R9+16], R14
; 9527:       4D895920         MOV [R9+32], R11
; 952B:       4D895130         MOV [R9+48], R10
; 952F:       4D896138         MOV [R9+56], R12               ; NIL
; 9533:       4D8D4117         LEA R8, [R9+23]
; 9537:       4D8940F1         MOV [R8-15], R8
; 953B:       4983C010         ADD R8, 16
; 953F:       4D8940F1         MOV [R8-15], R8
; 9543:       4983C010         ADD R8, 16
; 9547:       4D8940F1         MOV [R8-15], R8
; 954B:       4180C907         OR R9B, 7
; 954F:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 9553:       7402             JE L90
; 9555:       0F0B             UD2
; 9557: L90:  4D896D28         MOV [R13+40], R13              ; thread.pseudo-atomic-bits
; 955B:       498B7D58         MOV RDI, [R13+88]              ; thread.cons-tlab
; 955F:       4C8D4730         LEA R8, [RDI+48]
; 9563:       4D3B4560         CMP R8, [R13+96]
; 9567:       0F87AE020000     JA L136
; 956D:       4D894558         MOV [R13+88], R8               ; thread.cons-tlab
; 9571: L91:  48C7071F720450   MOV QWORD PTR [RDI], #x5004721F  ; 'LAMBDA
; 9578:       4C896710         MOV [RDI+16], R12              ; NIL
; 957C:       4C894F20         MOV [RDI+32], R9
; 9580:       4C896728         MOV [RDI+40], R12              ; NIL
; 9584:       4C8D4717         LEA R8, [RDI+23]
; 9588:       4D8940F1         MOV [R8-15], R8
; 958C:       4983C010         ADD R8, 16
; 9590:       4D8940F1         MOV [R8-15], R8
; 9594:       4080CF07         OR DIL, 7
; 9598:       4D316D28         XOR [R13+40], R13              ; thread.pseudo-atomic-bits
; 959C:       7402             JE L92
; 959E:       0F0B             UD2
; 95A0: L92:  4883EC10         SUB RSP, 16
; 95A4:       498BD4           MOV RDX, R12                   ; NIL
; 95A7:       B904000000       MOV ECX, 4
; 95AC:       48892C24         MOV [RSP], RBP
; 95B0:       488BEC           MOV RBP, RSP
; 95B3:       FF15CF72ABFF     CALL [RIP-5541169]             ; COMPILE
; 95B9:       488BC2           MOV RAX, RDX
; 95BC:       31C9             XOR ECX, ECX
; 95BE:       FF7508           PUSH QWORD PTR [RBP+8]
; 95C1:       FF60FD           JMP [RAX-3]
; 95C4: L93:  488B45F0         MOV RAX, [RBP-16]
; 95C8:       CC68             INT3 104                       ; OBJECT-NOT-REAL-ERROR
; 95CA:       00               BYTE #X00                      ; RAX(d)
; 95CB:       CC0F             INT3 15                        ; Invalid argument count trap
; 95CD: L94:  6A10             PUSH 16
; 95CF:       E88C70FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 95D4:       4158             POP R8
; 95D6:       E926F1FFFF       JMP L2
; 95DB: L95:  6A10             PUSH 16
; 95DD:       E87E70FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 95E2:       4158             POP R8
; 95E4:       E9C5F1FFFF       JMP L6
; 95E9: L96:  6A20             PUSH 32
; 95EB:       E87070FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 95F0:       4158             POP R8
; 95F2:       E967F2FFFF       JMP L8
; 95F7: L97:  6A10             PUSH 16
; 95F9:       E86270FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 95FE:       4158             POP R8
; 9600:       E996F2FFFF       JMP L10
; 9605: L98:  6A10             PUSH 16
; 9607:       E85470FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 960C:       5F               POP RDI
; 960D:       E959F3FFFF       JMP L14
; 9612: L99:  6A10             PUSH 16
; 9614:       E84770FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9619:       4158             POP R8
; 961B:       E9C7F3FFFF       JMP L16
; 9620: L100: 6A20             PUSH 32
; 9622:       E83970FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9627:       4158             POP R8
; 9629:       E9EAF3FFFF       JMP L18
; 962E: L101: 6A30             PUSH 48
; 9630:       E82B70FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9635:       4159             POP R9
; 9637:       E920F4FFFF       JMP L20
; 963C: L102: 6A20             PUSH 32
; 963E:       E81D70FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9643:       4158             POP R8
; 9645:       E95FF4FFFF       JMP L22
; 964A: L103: 6A10             PUSH 16
; 964C:       E80F70FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9651:       4158             POP R8
; 9653:       E991F4FFFF       JMP L24
; 9658: L104: 6A20             PUSH 32
; 965A:       E80170FBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 965F:       4158             POP R8
; 9661:       E9B0F4FFFF       JMP L26
; 9666: L105: 6A20             PUSH 32
; 9668:       E8F36FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 966D:       4158             POP R8
; 966F:       E9E6F4FFFF       JMP L28
; 9674: L106: 6A40             PUSH 64
; 9676:       E8E56FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 967B:       4159             POP R9
; 967D:       E91CF5FFFF       JMP L30
; 9682: L107: 6A20             PUSH 32
; 9684:       E8D76FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9689:       4158             POP R8
; 968B:       E96AF5FFFF       JMP L32
; 9690: L108: 6A30             PUSH 48
; 9692:       E8C96FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9697:       4159             POP R9
; 9699:       E9A3F5FFFF       JMP L34
; 969E: L109: 6A40             PUSH 64
; 96A0:       E8BB6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96A5:       4159             POP R9
; 96A7:       E9E5F5FFFF       JMP L36
; 96AC: L110: 6A20             PUSH 32
; 96AE:       E8AD6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96B3:       4158             POP R8
; 96B5:       E933F6FFFF       JMP L38
; 96BA: L111: 6A10             PUSH 16
; 96BC:       E89F6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96C1:       4158             POP R8
; 96C3:       E95EF6FFFF       JMP L40
; 96C8: L112: 6A20             PUSH 32
; 96CA:       E8916FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96CF:       4158             POP R8
; 96D1:       E9CFF6FFFF       JMP L43
; 96D6: L113: 6A20             PUSH 32
; 96D8:       E8836FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96DD:       4158             POP R8
; 96DF:       E909F7FFFF       JMP L45
; 96E4: L114: 6A40             PUSH 64
; 96E6:       E8756FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96EB:       4159             POP R9
; 96ED:       E93BF7FFFF       JMP L47
; 96F2: L115: 6A10             PUSH 16
; 96F4:       E8676FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 96F9:       4158             POP R8
; 96FB:       E993F7FFFF       JMP L49
; 9700: L116: 6A30             PUSH 48
; 9702:       E8596FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9707:       4159             POP R9
; 9709:       E9B6F7FFFF       JMP L51
; 970E: L117: 6A20             PUSH 32
; 9710:       E84B6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9715:       4158             POP R8
; 9717:       E9F5F7FFFF       JMP L53
; 971C: L118: 6A40             PUSH 64
; 971E:       E83D6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9723:       4159             POP R9
; 9725:       E92BF8FFFF       JMP L55
; 972A: L119: 6A10             PUSH 16
; 972C:       E82F6FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9731:       4158             POP R8
; 9733:       E980F8FFFF       JMP L57
; 9738: L120: 6A30             PUSH 48
; 973A:       E8216FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 973F:       4159             POP R9
; 9741:       E9AAF8FFFF       JMP L59
; 9746: L121: 6A20             PUSH 32
; 9748:       E8136FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 974D:       4158             POP R8
; 974F:       E9F4F8FFFF       JMP L61
; 9754: L122: 6A30             PUSH 48
; 9756:       E8056FFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 975B:       4159             POP R9
; 975D:       E92BF9FFFF       JMP L63
; 9762: L123: 6A40             PUSH 64
; 9764:       E8F76EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9769:       4159             POP R9
; 976B:       E971F9FFFF       JMP L65
; 9770: L124: 6A10             PUSH 16
; 9772:       E8E96EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9777:       4158             POP R8
; 9779:       E9BBF9FFFF       JMP L67
; 977E: L125: 6A30             PUSH 48
; 9780:       E8DB6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9785:       4159             POP R9
; 9787:       E9E5F9FFFF       JMP L69
; 978C: L126: 6A20             PUSH 32
; 978E:       E8CD6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9793:       4158             POP R8
; 9795:       E92FFAFFFF       JMP L71
; 979A: L127: 6A50             PUSH 80
; 979C:       E8BF6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97A1:       4159             POP R9
; 97A3:       E95EFAFFFF       JMP L73
; 97A8: L128: 6A20             PUSH 32
; 97AA:       E8B16EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97AF:       4158             POP R8
; 97B1:       E9CFFAFFFF       JMP L75
; 97B6: L129: 6A30             PUSH 48
; 97B8:       E8A36EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97BD:       4159             POP R9
; 97BF:       E9FEFAFFFF       JMP L77
; 97C4: L130: 6A40             PUSH 64
; 97C6:       E8956EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97CB:       4159             POP R9
; 97CD:       E940FBFFFF       JMP L79
; 97D2: L131: 6A20             PUSH 32
; 97D4:       E8876EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97D9:       4158             POP R8
; 97DB:       E98AFBFFFF       JMP L81
; 97E0: L132: 6890000000       PUSH 144
; 97E5:       E8766EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97EA:       4159             POP R9
; 97EC:       E9B5FBFFFF       JMP L83
; 97F1: L133: 6A40             PUSH 64
; 97F3:       E8686EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 97F8:       4159             POP R9
; 97FA:       E974FCFFFF       JMP L85
; 97FF: L134: 6A30             PUSH 48
; 9801:       E85A6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9806:       4159             POP R9
; 9808:       E9C6FCFFFF       JMP L87
; 980D: L135: 6A40             PUSH 64
; 980F:       E84C6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9814:       4159             POP R9
; 9816:       E901FDFFFF       JMP L89
; 981B: L136: 6A30             PUSH 48
; 981D:       E83E6EFBFF       CALL #xB800000660              ; SB-VM::LIST-ALLOC-TRAMP
; 9822:       5F               POP RDI
; 9823:       E949FDFFFF       JMP L91
