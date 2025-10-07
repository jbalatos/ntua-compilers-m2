.intel_syntax noprefix

.section .text
        .global readChar, readString, readByte, readInteger
        .global writeChar, writeString, writeByte, writeInteger
        .global extend, shrink, strlen, strcmp, strcpy, strcat
        .extern read, write

readChar:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        lea     rax, [rbp-1]
        mov     edx, 1
        mov     rsi, rax
        mov     edi, 0
        call    read
        movzx   eax, BYTE PTR [rbp-1]
        leave
        ret
readString:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     eax, edi
        mov     QWORD PTR [rbp-32], rsi
        mov     WORD PTR [rbp-20], ax
        mov     WORD PTR [rbp-2], 0
        jmp     .L4
.L7:
        call    readChar
        mov     ecx, eax
        movsx   rdx, WORD PTR [rbp-2]
        mov     rax, QWORD PTR [rbp-32]
        add     rax, rdx
        mov     edx, ecx
        mov     BYTE PTR [rax], dl
        movsx   rdx, WORD PTR [rbp-2]
        mov     rax, QWORD PTR [rbp-32]
        add     rax, rdx
        movzx   eax, BYTE PTR [rax]
        cmp     al, 10
        je      .L8
        movzx   eax, WORD PTR [rbp-2]
        add     eax, 1
        mov     WORD PTR [rbp-2], ax
.L4:
        movsx   edx, WORD PTR [rbp-2]
        movsx   eax, WORD PTR [rbp-20]
        sub     eax, 1
        cmp     edx, eax
        jl      .L7
        jmp     .L6
.L8:
        nop
.L6:
        movsx   rdx, WORD PTR [rbp-2]
        mov     rax, QWORD PTR [rbp-32]
        add     rax, rdx
        mov     BYTE PTR [rax], 0
        nop
        leave
        ret
readByte:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     BYTE PTR [rbp-1], 0
.L10:
        call    readChar
        mov     BYTE PTR [rbp-2], al
        cmp     BYTE PTR [rbp-2], 32
        je      .L10
        cmp     BYTE PTR [rbp-2], 9
        je      .L10
        cmp     BYTE PTR [rbp-2], 13
        je      .L10
        cmp     BYTE PTR [rbp-2], 10
        je      .L10
        jmp     .L11
.L13:
        movzx   edx, BYTE PTR [rbp-1]
        mov     eax, edx
        sal     eax, 2
        add     eax, edx
        add     eax, eax
        mov     edx, eax
        movzx   eax, BYTE PTR [rbp-2]
        add     eax, edx
        sub     eax, 48
        mov     BYTE PTR [rbp-1], al
        call    readChar
        mov     BYTE PTR [rbp-2], al
.L11:
        cmp     BYTE PTR [rbp-2], 47
        jle     .L12
        cmp     BYTE PTR [rbp-2], 57
        jle     .L13
.L12:
        movzx   eax, BYTE PTR [rbp-1]
        leave
        ret
readInteger:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     WORD PTR [rbp-2], 0
        mov     BYTE PTR [rbp-5], 1
        mov     BYTE PTR [rbp-4], 0
.L16:
        call    readChar
        mov     BYTE PTR [rbp-3], al
        cmp     BYTE PTR [rbp-3], 32
        je      .L16
        cmp     BYTE PTR [rbp-3], 9
        je      .L16
        cmp     BYTE PTR [rbp-3], 13
        je      .L16
        cmp     BYTE PTR [rbp-3], 10
        je      .L16
        cmp     BYTE PTR [rbp-3], 45
        jne     .L18
        mov     BYTE PTR [rbp-4], 1
        call    readChar
        mov     BYTE PTR [rbp-3], al
        jmp     .L18
.L20:
        movzx   edx, WORD PTR [rbp-2]
        mov     eax, edx
        sal     eax, 2
        add     eax, edx
        add     eax, eax
        mov     edx, eax
        movsx   ax, BYTE PTR [rbp-3]
        add     eax, edx
        sub     eax, 48
        mov     WORD PTR [rbp-2], ax
        call    readChar
        mov     BYTE PTR [rbp-3], al
.L18:
        cmp     BYTE PTR [rbp-3], 47
        jle     .L19
        cmp     BYTE PTR [rbp-3], 57
        jle     .L20
.L19:
        cmp     BYTE PTR [rbp-4], 0
        je      .L21
        movzx   eax, WORD PTR [rbp-2]
        neg     eax
        mov     WORD PTR [rbp-2], ax
.L21:
        movzx   eax, WORD PTR [rbp-2]
        leave
        ret
writeChar:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     eax, edi
        mov     BYTE PTR [rbp-4], al
        lea     rax, [rbp-4]
        mov     edx, 1
        mov     rsi, rax
        mov     edi, 1
        call    write
        nop
        leave
        ret
writeString:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     QWORD PTR [rbp-8], rdi
        jmp     .L25
.L26:
        mov     rax, QWORD PTR [rbp-8]
        lea     rdx, [rax+1]
        mov     QWORD PTR [rbp-8], rdx
        movzx   eax, BYTE PTR [rax]
        movsx   eax, al
        mov     edi, eax
        call    writeChar
.L25:
        mov     rax, QWORD PTR [rbp-8]
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L26
        nop
        nop
        leave
        ret
writeByte:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     eax, edi
        mov     BYTE PTR [rbp-20], al
        mov     BYTE PTR [rbp-1], 0
        cmp     BYTE PTR [rbp-20], 0
        jne     .L30
        mov     edi, 48
        call    writeChar
        jmp     .L27
.L31:
        movzx   ecx, BYTE PTR [rbp-20]
        mov     edx, -51
        mov     eax, edx
        mul     cl
        shr     ax, 8
        mov     edx, eax
        shr     dl, 3
        mov     eax, edx
        sal     eax, 2
        add     eax, edx
        add     eax, eax
        sub     ecx, eax
        mov     edx, ecx
        movzx   eax, BYTE PTR [rbp-1]
        lea     ecx, [rax+1]
        mov     BYTE PTR [rbp-1], cl
        movzx   eax, al
        cdqe
        mov     BYTE PTR [rbp-4+rax], dl
        movzx   eax, BYTE PTR [rbp-20]
        mov     edx, -51
        mul     dl
        shr     ax, 8
        shr     al, 3
        mov     BYTE PTR [rbp-20], al
.L30:
        cmp     BYTE PTR [rbp-20], 0
        jne     .L31
        jmp     .L32
.L33:
        movzx   eax, BYTE PTR [rbp-1]
        cdqe
        movzx   eax, BYTE PTR [rbp-4+rax]
        add     eax, 48
        movsx   eax, al
        mov     edi, eax
        call    writeChar
.L32:
        movzx   eax, BYTE PTR [rbp-1]
        lea     edx, [rax-1]
        mov     BYTE PTR [rbp-1], dl
        test    al, al
        jne     .L33
.L27:
        leave
        ret
writeInteger:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     eax, edi
        mov     WORD PTR [rbp-20], ax
        mov     BYTE PTR [rbp-1], 0
        cmp     WORD PTR [rbp-20], 0
        jns     .L35
        mov     edi, 45
        call    writeChar
        movzx   eax, WORD PTR [rbp-20]
        neg     eax
        mov     WORD PTR [rbp-20], ax
        jmp     .L38
.L35:
        cmp     WORD PTR [rbp-20], 0
        jne     .L38
        mov     edi, 48
        call    writeChar
        jmp     .L34
.L39:
        movzx   edx, WORD PTR [rbp-20]
        movsx   eax, dx
        imul    eax, eax, 26215
        shr     eax, 16
        sar     ax, 2
        mov     esi, edx
        sar     si, 15
        sub     eax, esi
        mov     ecx, eax
        mov     eax, ecx
        sal     eax, 2
        add     eax, ecx
        add     eax, eax
        mov     ecx, edx
        sub     ecx, eax
        movzx   eax, BYTE PTR [rbp-1]
        lea     edx, [rax+1]
        mov     BYTE PTR [rbp-1], dl
        movzx   eax, al
        mov     edx, ecx
        cdqe
        mov     BYTE PTR [rbp-6+rax], dl
        movzx   eax, WORD PTR [rbp-20]
        movsx   edx, ax
        imul    edx, edx, 26215
        shr     edx, 16
        sar     dx, 2
        sar     ax, 15
        mov     ecx, eax
        mov     eax, edx
        sub     eax, ecx
        mov     WORD PTR [rbp-20], ax
.L38:
        cmp     WORD PTR [rbp-20], 0
        jne     .L39
        jmp     .L40
.L41:
        movzx   eax, BYTE PTR [rbp-1]
        cdqe
        movzx   eax, BYTE PTR [rbp-6+rax]
        add     eax, 48
        movsx   eax, al
        mov     edi, eax
        call    writeChar
.L40:
        movzx   eax, BYTE PTR [rbp-1]
        lea     edx, [rax-1]
        mov     BYTE PTR [rbp-1], dl
        test    al, al
        jne     .L41
.L34:
        leave
        ret
extend:
        push    rbp
        mov     rbp, rsp
        mov     eax, edi
        mov     BYTE PTR [rbp-4], al
        movzx   eax, BYTE PTR [rbp-4]
        pop     rbp
        ret
shrink:
        push    rbp
        mov     rbp, rsp
        mov     eax, edi
        mov     WORD PTR [rbp-4], ax
        movzx   eax, WORD PTR [rbp-4]
        pop     rbp
        ret
strlen:
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-24], rdi
        mov     WORD PTR [rbp-2], 0
        jmp     .L47
.L48:
        movzx   eax, WORD PTR [rbp-2]
        add     eax, 1
        mov     WORD PTR [rbp-2], ax
.L47:
        mov     rax, QWORD PTR [rbp-24]
        lea     rdx, [rax+1]
        mov     QWORD PTR [rbp-24], rdx
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L48
        movzx   eax, WORD PTR [rbp-2]
        pop     rbp
        ret
strcmp:
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-8], rdi
        mov     QWORD PTR [rbp-16], rsi
        jmp     .L51
.L55:
        mov     rax, QWORD PTR [rbp-8]
        movzx   edx, BYTE PTR [rax]
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        cmp     dl, al
        je      .L52
        mov     rax, QWORD PTR [rbp-8]
        movzx   eax, BYTE PTR [rax]
        movzx   edx, al
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        movzx   eax, al
        sub     edx, eax
        mov     eax, edx
        jmp     .L53
.L52:
        add     QWORD PTR [rbp-8], 1
        add     QWORD PTR [rbp-16], 1
.L51:
        mov     rax, QWORD PTR [rbp-8]
        movzx   eax, BYTE PTR [rax]
        test    al, al
        je      .L54
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L55
.L54:
        mov     rax, QWORD PTR [rbp-8]
        movzx   eax, BYTE PTR [rax]
        movzx   edx, al
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        movzx   eax, al
        sub     edx, eax
        mov     eax, edx
.L53:
        pop     rbp
        ret
strcpy:
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-8], rdi
        mov     QWORD PTR [rbp-16], rsi
        jmp     .L57
.L58:
        mov     rdx, QWORD PTR [rbp-16]
        lea     rax, [rdx+1]
        mov     QWORD PTR [rbp-16], rax
        mov     rax, QWORD PTR [rbp-8]
        lea     rcx, [rax+1]
        mov     QWORD PTR [rbp-8], rcx
        movzx   edx, BYTE PTR [rdx]
        mov     BYTE PTR [rax], dl
.L57:
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L58
        nop
        nop
        pop     rbp
        ret
strcat:
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-8], rdi
        mov     QWORD PTR [rbp-16], rsi
        nop
.L60:
        mov     rax, QWORD PTR [rbp-8]
        lea     rdx, [rax+1]
        mov     QWORD PTR [rbp-8], rdx
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L60
        jmp     .L61
.L62:
        mov     rdx, QWORD PTR [rbp-16]
        lea     rax, [rdx+1]
        mov     QWORD PTR [rbp-16], rax
        mov     rax, QWORD PTR [rbp-8]
        lea     rcx, [rax+1]
        mov     QWORD PTR [rbp-8], rcx
        movzx   edx, BYTE PTR [rdx]
        mov     BYTE PTR [rax], dl
.L61:
        mov     rax, QWORD PTR [rbp-16]
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L62
        nop
        nop
        pop     rbp
        ret
