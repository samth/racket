; disassembly for (LAMBDA ())
; Size: 452 bytes. Origin: #xB800B140F2                       ; (LAMBDA ())
; 0F2:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 0F6:       488945F8         MOV [RBP-8], RAX
; 0FA:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 0FE:       4D8B5570         MOV R10, [R13+112]              ; thread.mixed-tlab
; 102:       4D8D4250         LEA R8, [R10+80]
; 106:       4D3B4578         CMP R8, [R13+120]
; 10A:       0F87690D0000     JA #xB800B14E79
; 110:       4D894570         MOV [R13+112], R8               ; thread.mixed-tlab
; 114:       41C602C9         MOV BYTE PTR [R10], -55
; 118:       41C642080E       MOV BYTE PTR [R10+8], 14
; 11D:       4180CA0F         OR R10B, 15
; 121:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 125:       7402             JE L0
; 127:       0F0B             UD2
; 129: L0:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 12D:       4D8B7D70         MOV R15, [R13+112]              ; thread.mixed-tlab
; 131:       4983C710         ADD R15, 16
; 135:       4D3B7D78         CMP R15, [R13+120]
; 139:       0F874B0D0000     JA #xB800B14E8A
; 13F:       4D897D70         MOV [R13+112], R15              ; thread.mixed-tlab
; 143:       49FFCF           DEC R15
; 146:       6641C747F14D01   MOV WORD PTR [R15-15], 333
; 14D:       49C747F900000000 MOV QWORD PTR [R15-7], 0
; 155:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 159:       7402             JE L1
; 15B:       0F0B             UD2
; 15D: L1:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 161:       4D8B7570         MOV R14, [R13+112]              ; thread.mixed-tlab
; 165:       4983C610         ADD R14, 16
; 169:       4D3B7578         CMP R14, [R13+120]
; 16D:       0F872C0D0000     JA #xB800B14E9F
; 173:       4D897570         MOV [R13+112], R14              ; thread.mixed-tlab
; 177:       49FFCE           DEC R14
; 17A:       6641C746F14D01   MOV WORD PTR [R14-15], 333
; 181:       49C746F900000000 MOV QWORD PTR [R14-7], 0
; 189:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 18D:       7402             JE L2
; 18F:       0F0B             UD2
; 191: L2:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 195:       4D8B5D70         MOV R11, [R13+112]              ; thread.mixed-tlab
; 199:       4983C310         ADD R11, 16
; 19D:       4D3B5D78         CMP R11, [R13+120]
; 1A1:       0F870D0D0000     JA #xB800B14EB4
; 1A7:       4D895D70         MOV [R13+112], R11              ; thread.mixed-tlab
; 1AB:       49FFCB           DEC R11
; 1AE:       6641C743F14D01   MOV WORD PTR [R11-15], 333
; 1B5:       4D8963F9         MOV [R11-7], R12                ; NIL
; 1B9:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 1BD:       7402             JE L3
; 1BF:       0F0B             UD2
; 1C1: L3:   4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 1C5:       4D8B4D70         MOV R9, [R13+112]               ; thread.mixed-tlab
; 1C9:       4D8D4130         LEA R8, [R9+48]
; 1CD:       4D3B4578         CMP R8, [R13+120]
; 1D1:       0F87F20C0000     JA #xB800B14EC9
; 1D7:       4D894570         MOV [R13+112], R8               ; thread.mixed-tlab
; 1DB:       4D8B442439       MOV R8, [R12+57]                ; FUNCTION-LAYOUT
; 1E0:       6641B84505       MOV R8W, 1349
; 1E5:       4D8901           MOV [R9], R8
; 1E8:       4C8D05E1000000   LEA R8, [RIP+225]               ; = #xB800B142D0
; 1EF:       4D894108         MOV [R9+8], R8
; 1F3:       4180C90B         OR R9B, 11
; 1F7:       4D895105         MOV [R9+5], R10
; 1FB:       4D89590D         MOV [R9+13], R11
; 1FF:       4D897115         MOV [R9+21], R14
; 203:       4D89791D         MOV [R9+29], R15
; 207:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 20B:       7402             JE L4
; 20D:       0F0B             UD2
; 20F: L4:   4D8BF9           MOV R15, R9
; 212:       488BF4           MOV RSI, RSP
; 215:       6A02             PUSH 2
; 217:       41BA02000000     MOV R10D, 2
; 21D:       488BC4           MOV RAX, RSP
; 220:       4154             PUSH R12                        ; NIL
; 222:       4C8D4424FF       LEA R8, [RSP-1]
; 227:       498BC8           MOV RCX, R8
; 22A:       4D8BD8           MOV R11, R8
; 22D:       EB50             JMP L7
; 22F:       90               NOP
; 230: L5:   4D8BF3           MOV R14, R11
; 233:       4D896D28         MOV [R13+40], R13               ; thread.pseudo-atomic-bits
; 237:       4D8B4558         MOV R8, [R13+88]                ; thread.cons-tlab
; 23B:       4D8D4810         LEA R9, [R8+16]
; 23F:       4D3B4D60         CMP R9, [R13+96]
; 243:       0F87910C0000     JA #xB800B14EDA
; 249:       4D894D58         MOV [R13+88], R9                ; thread.cons-tlab
; 24D:       4D8910           MOV [R8], R10
; 250:       4D896008         MOV [R8+8], R12                 ; NIL
; 254:       4D8D4807         LEA R9, [R8+7]
; 258:       4D316D28         XOR [R13+40], R13               ; thread.pseudo-atomic-bits
; 25C:       7402             JE L6
; 25E:       0F0B             UD2
; 260: L6:   4D8BD9           MOV R11, R9
; 263:       4D8BC6           MOV R8, R14
; 266:       49C1E80A         SHR R8, 10
; 26A:       4181E0FFFF0F00   AND R8D, 1048575
; 271:       43C644046900     MOV BYTE PTR [R12+R8+105], 0
; 277:       4D894E01         MOV [R14+1], R9
; 27B:       4983C202         ADD R10, 2
; 27F: L7:   4D8BCA           MOV R9, R10
; 282:       49D1F9           SAR R9, 1
; 285:       4983F908         CMP R9, 8
; 289:       75A5             JNE L5
; 28B:       4C8B4101         MOV R8, [RCX+1]
; 28F:       488BE0           MOV RSP, RAX
; 292: L8:   4D39E0           CMP R8, R12                     ; NIL
; 295:       7414             JE L9
; 297:       41FF70F9         PUSH QWORD PTR [R8-7]
; 29B:       4D8B4001         MOV R8, [R8+1]
; 29F:       418D40F9         LEA EAX, [R8-7]
; 2A3:       A80F             TEST AL, 15
; 2A5:       74EB             JE L8
; 2A7:       CC0A             INT3 10                         ; cerror trap
; 2A9:       06               BYTE #X06                       ; BOGUS-ARG-TO-VALUES-LIST-ERROR
; 2AA:       20               BYTE #X20                       ; R8(d)
; 2AB: L9:   498BC7           MOV RAX, R15
; 2AE:       41FFA42491FCFFFF JMP [R12-879]                   ; [#5200000FFC28] = #B800000B20 ; TAIL-CALL-VARIABLE
