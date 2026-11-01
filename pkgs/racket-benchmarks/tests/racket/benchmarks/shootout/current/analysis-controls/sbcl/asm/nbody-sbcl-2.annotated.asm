;; percent of all cpu_core/cycles/pp samples (447622 samples) on each instruction
       
       ;;; ADVANCE
       ; disassembly for ADVANCE
       ; Size: 460 bytes. Origin: #xB800048DDF                       ; ADVANCE
       ; DDF:       488BDE           MOV RBX, RSI
       ; DE2:       0F1F8000000000   NOP
  0.50 ; DE9:       0F1F8000000000   NOP
       ; DF0: L0:   4C39E3           CMP RBX, R12                    ; NIL
  0.36 ; DF3:       0F8446010000     JE L3
       ; DF9:       8D43F9           LEA EAX, [RBX-7]
       ; DFC:       A80F             TEST AL, 15
       ; DFE:       0F853B010000     JNE L3
  2.28 ; E04:       488BC3           MOV RAX, RBX
       ; E07:       488B48F9         MOV RCX, [RAX-7]
       ; E0B:       488B4001         MOV RAX, [RAX+1]
       ; E0F:       488B5B01         MOV RBX, [RBX+1]
  1.93 ; E13:       488BD0           MOV RDX, RAX
  2.27 ; E16:       E916010000       JMP L2
       ; E1B:       0F1F440000       NOP
       ; E20: L1:   488B42F9         MOV RAX, [RDX-7]
       ; E24:       488B5201         MOV RDX, [RDX+1]
       ; E28:       F20F106901       MOVSD XMM5, [RCX+1]
  4.79 ; E2D:       F20F105001       MOVSD XMM2, [RAX+1]
       ; E32:       F20F5CEA         SUBSD XMM5, XMM2
  0.00 ; E36:       F20F107109       MOVSD XMM6, [RCX+9]
       ; E3B:       F20F105009       MOVSD XMM2, [RAX+9]
  4.61 ; E40:       F20F5CF2         SUBSD XMM6, XMM2
       ; E44:       F20F105911       MOVSD XMM3, [RCX+17]
       ; E49:       F20F105011       MOVSD XMM2, [RAX+17]
       ; E4E:       660F28CB         MOVAPD XMM1, XMM3
  4.77 ; E52:       F20F5CCA         SUBSD XMM1, XMM2
       ; E56:       660F28DD         MOVAPD XMM3, XMM5
       ; E5A:       F20F59DD         MULSD XMM3, XMM5
       ; E5E:       660F28E6         MOVAPD XMM4, XMM6
  5.09 ; E62:       F20F59E6         MULSD XMM4, XMM6
  0.00 ; E66:       F20F58E3         ADDSD XMM4, XMM3
       ; E6A:       660F28D9         MOVAPD XMM3, XMM1
       ; E6E:       F20F59D9         MULSD XMM3, XMM1
  4.87 ; E72:       F20F58DC         ADDSD XMM3, XMM4
       ; E76:       660F57D2         XORPD XMM2, XMM2
  0.00 ; E7A:       F20F51D3         SQRTSD XMM2, XMM3
       ; E7E:       660F28DA         MOVAPD XMM3, XMM2
  4.70 ; E82:       F20F59DA         MULSD XMM3, XMM2
  0.00 ; E86:       F20F59D3         MULSD XMM2, XMM3
  0.00 ; E8A:       66410F28F8       MOVAPD XMM7, XMM8
  0.00 ; E8F:       F20F5EFA         DIVSD XMM7, XMM2
  5.10 ; E93:       660F28DD         MOVAPD XMM3, XMM5
  0.01 ; E97:       F20F59DF         MULSD XMM3, XMM7
  0.00 ; E9B:       660F28E6         MOVAPD XMM4, XMM6
  0.00 ; E9F:       F20F59E7         MULSD XMM4, XMM7
  5.15 ; EA3:       F20F59F9         MULSD XMM7, XMM1
  0.00 ; EA7:       F20F105031       MOVSD XMM2, [RAX+49]
  0.01 ; EAC:       F20F59D3         MULSD XMM2, XMM3
  0.00 ; EB0:       F20F104919       MOVSD XMM1, [RCX+25]
  4.76 ; EB5:       F20F5CCA         SUBSD XMM1, XMM2
  0.05 ; EB9:       F20F114919       MOVSD [RCX+25], XMM1
  0.00 ; EBE:       F20F105031       MOVSD XMM2, [RAX+49]
       ; EC3:       F20F59D4         MULSD XMM2, XMM4
  4.39 ; EC7:       F20F104921       MOVSD XMM1, [RCX+33]
  0.03 ; ECC:       F20F5CCA         SUBSD XMM1, XMM2
  0.00 ; ED0:       F20F114921       MOVSD [RCX+33], XMM1
  0.00 ; ED5:       F20F105031       MOVSD XMM2, [RAX+49]
  4.57 ; EDA:       F20F59D7         MULSD XMM2, XMM7
  0.02 ; EDE:       F20F104929       MOVSD XMM1, [RCX+41]
  0.00 ; EE3:       F20F5CCA         SUBSD XMM1, XMM2
  0.00 ; EE7:       F20F114929       MOVSD [RCX+41], XMM1
  4.54 ; EEC:       F20F104931       MOVSD XMM1, [RCX+49]
  0.03 ; EF1:       F20F59CB         MULSD XMM1, XMM3
  0.00 ; EF5:       F20F105019       MOVSD XMM2, [RAX+25]
  0.00 ; EFA:       F20F58D1         ADDSD XMM2, XMM1
  4.66 ; EFE:       F20F115019       MOVSD [RAX+25], XMM2
  0.03 ; F03:       F20F104931       MOVSD XMM1, [RCX+49]
  0.01 ; F08:       F20F59CC         MULSD XMM1, XMM4
  0.00 ; F0C:       F20F105021       MOVSD XMM2, [RAX+33]
  4.34 ; F11:       F20F58D1         ADDSD XMM2, XMM1
  0.03 ; F15:       F20F115021       MOVSD [RAX+33], XMM2
  0.00 ; F1A:       F20F104931       MOVSD XMM1, [RCX+49]
  0.00 ; F1F:       F20F59CF         MULSD XMM1, XMM7
  4.21 ; F23:       F20F105029       MOVSD XMM2, [RAX+41]
  0.03 ; F28:       F20F58D1         ADDSD XMM2, XMM1
  0.00 ; F2C:       F20F115029       MOVSD [RAX+41], XMM2
       ; F31: L2:   4C39E2           CMP RDX, R12                    ; NIL
  2.08 ; F34:       0F84B6FEFFFF     JE L0
  4.53 ; F3A:       E9E1FEFFFF       JMP L1
       ; F3F: L3:   488BCE           MOV RCX, RSI
  0.46 ; F42:       EB5C             JMP L5
       ; F44:       660F1F440000     NOP
       ; F4A:       660F1F440000     NOP
       ; F50: L4:   488B41F9         MOV RAX, [RCX-7]
       ; F54:       488B4901         MOV RCX, [RCX+1]
       ; F58:       F20F104819       MOVSD XMM1, [RAX+25]
  2.00 ; F5D:       F2410F59C8       MULSD XMM1, XMM8
       ; F62:       F20F105001       MOVSD XMM2, [RAX+1]
       ; F67:       F20F58D1         ADDSD XMM2, XMM1
       ; F6B:       F20F115001       MOVSD [RAX+1], XMM2
  2.29 ; F70:       F20F104821       MOVSD XMM1, [RAX+33]
       ; F75:       F2410F59C8       MULSD XMM1, XMM8
       ; F7A:       F20F105009       MOVSD XMM2, [RAX+9]
  0.00 ; F7F:       F20F58D1         ADDSD XMM2, XMM1
  2.35 ; F83:       F20F115009       MOVSD [RAX+9], XMM2
       ; F88:       F20F104829       MOVSD XMM1, [RAX+41]
       ; F8D:       F2410F59C8       MULSD XMM1, XMM8
       ; F92:       F20F105011       MOVSD XMM2, [RAX+17]
  2.24 ; F97:       F20F58D1         ADDSD XMM2, XMM1
  0.00 ; F9B:       F20F115011       MOVSD [RAX+17], XMM2
       ; FA0: L5:   4C39E1           CMP RCX, R12                    ; NIL
  2.31 ; FA3:       75AB             JNE L4
       ; FA5:       488BD1           MOV RDX, RCX
  0.46 ; FA8:       C9               LEAVE
       ; FA9:       F8               CLC
  0.49 ; FAA:       C3               RET
