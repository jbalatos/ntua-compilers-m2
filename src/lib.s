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
        mov     QWORD PTR [rbp-24], rdi
        mov     QWORD PTR [rbp-32], rsi
        mov     QWORD PTR [rbp-8], 0
        jmp     .L4
.L7:
        call    readChar
        mov     ecx, eax
        mov     rdx, QWORD PTR [rbp-8]
        mov     rax, QWORD PTR [rbp-32]
        add     rax, rdx
        mov     edx, ecx
        mov     BYTE PTR [rax], dl
        mov     rdx, QWORD PTR [rbp-8]
        mov     rax, QWORD PTR [rbp-32]
        add     rax, rdx
        movzx   eax, BYTE PTR [rax]
        cmp     al, 10
        je      .L8
        add     QWORD PTR [rbp-8], 1
.L4:
        mov     rax, QWORD PTR [rbp-24]
        sub     rax, 1
        cmp     QWORD PTR [rbp-8], rax
        jl      .L7
        jmp     .L6
.L8:
        nop
.L6:
        mov     rdx, QWORD PTR [rbp-8]
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
        mov     QWORD PTR [rbp-8], 0
        mov     BYTE PTR [rbp-11], 1
        mov     BYTE PTR [rbp-10], 0
.L16:
        call    readChar
        mov     BYTE PTR [rbp-9], al
        cmp     BYTE PTR [rbp-9], 32
        je      .L16
        cmp     BYTE PTR [rbp-9], 9
        je      .L16
        cmp     BYTE PTR [rbp-9], 13
        je      .L16
        cmp     BYTE PTR [rbp-9], 10
        je      .L16
        cmp     BYTE PTR [rbp-9], 45
        jne     .L18
        mov     BYTE PTR [rbp-10], 1
        call    readChar
        mov     BYTE PTR [rbp-9], al
        jmp     .L18
.L20:
        mov     rdx, QWORD PTR [rbp-8]
        mov     rax, rdx
        sal     rax, 2
        add     rax, rdx
        add     rax, rax
        mov     rdx, rax
        movsx   eax, BYTE PTR [rbp-9]
        sub     eax, 48
        cdqe
        add     rax, rdx
        mov     QWORD PTR [rbp-8], rax
        call    readChar
        mov     BYTE PTR [rbp-9], al
.L18:
        cmp     BYTE PTR [rbp-9], 47
        jle     .L19
        cmp     BYTE PTR [rbp-9], 57
        jle     .L20
.L19:
        cmp     BYTE PTR [rbp-10], 0
        je      .L21
        neg     QWORD PTR [rbp-8]
.L21:
        mov     rax, QWORD PTR [rbp-8]
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
        sub     rsp, 48
        mov     QWORD PTR [rbp-40], rdi
        mov     BYTE PTR [rbp-1], 0
        cmp     QWORD PTR [rbp-40], 0
        jns     .L35
        mov     edi, 45
        call    writeChar
        neg     QWORD PTR [rbp-40]
        jmp     .L38
.L35:
        cmp     QWORD PTR [rbp-40], 0
        jne     .L38
        mov     edi, 48
        call    writeChar
        jmp     .L34
.L39:
        mov     rcx, QWORD PTR [rbp-40]
        movabs  rdx, 7378697629483820647
        mov     rax, rcx
        imul    rdx
        sar     rdx, 2
        mov     rax, rcx
        sar     rax, 63
        sub     rdx, rax
        mov     rax, rdx
        sal     rax, 2
        add     rax, rdx
        add     rax, rax
        sub     rcx, rax
        mov     rdx, rcx
        movzx   eax, BYTE PTR [rbp-1]
        lea     ecx, [rax+1]
        mov     BYTE PTR [rbp-1], cl
        movzx   eax, al
        cdqe
        mov     BYTE PTR [rbp-32+rax], dl
        mov     rcx, QWORD PTR [rbp-40]
        movabs  rdx, 7378697629483820647
        mov     rax, rcx
        imul    rdx
        mov     rax, rdx
        sar     rax, 2
        sar     rcx, 63
        mov     rdx, rcx
        sub     rax, rdx
        mov     QWORD PTR [rbp-40], rax
.L38:
        cmp     QWORD PTR [rbp-40], 0
        jne     .L39
        jmp     .L40
.L41:
        movzx   eax, BYTE PTR [rbp-1]
        cdqe
        movzx   eax, BYTE PTR [rbp-32+rax]
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
        mov     QWORD PTR [rbp-8], rdi
        mov     rax, QWORD PTR [rbp-8]
        pop     rbp
        ret
strlen:
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-24], rdi
        mov     QWORD PTR [rbp-8], 0
        jmp     .L47
.L48:
        add     QWORD PTR [rbp-8], 1
.L47:
        mov     rax, QWORD PTR [rbp-24]
        lea     rdx, [rax+1]
        mov     QWORD PTR [rbp-24], rdx
        movzx   eax, BYTE PTR [rax]
        test    al, al
        jne     .L48
        mov     rax, QWORD PTR [rbp-8]
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
        movsx   rax, edx
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
        movsx   rax, edx
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
