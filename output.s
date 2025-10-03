	.text
	.file	"my_module"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	andq	$-16, %rsp
	callq	main_1@PLT
	xorl	%eax, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	main_1                          # -- Begin function main_1
	.p2align	4, 0x90
	.type	main_1,@function
main_1:                                 # @main_1
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	andq	$-16, %rsp
	subq	$16, %rsp
	movl	$.Lstr.3, %edi
	callq	writeString@PLT
	callq	readInteger@PLT
                                        # kill: def $ax killed $ax def $eax
	movw	%ax, (%rsp)
	movl	$.Lstr.4, %esi
	movl	$.Lstr.5, %edx
	movl	$.Lstr.6, %ecx
	movl	%eax, %edi
	callq	main_1_2@PLT
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	main_1, .Lfunc_end1-main_1
	.cfi_endproc
                                        # -- End function
	.globl	main_1_2                        # -- Begin function main_1_2
	.p2align	4, 0x90
	.type	main_1_2,@function
main_1_2:                               # @main_1_2
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	andq	$-16, %rsp
	subq	$16, %rsp
	.cfi_offset %rbx, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movw	%di, (%rsp)
	testw	%di, %di
	jle	.LBB2_2
# %bb.1:                                # %then
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movl	(%rsp), %edi
	decl	%edi
	movq	%rcx, %rdx
	movq	%r14, %rcx
	callq	main_1_2@PLT
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	main_1_2_7@PLT
	movl	(%rsp), %edi
	decl	%edi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	main_1_2@PLT
.LBB2_2:                                # %after_if
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end2:
	.size	main_1_2, .Lfunc_end2-main_1_2
	.cfi_endproc
                                        # -- End function
	.globl	main_1_2_7                      # -- Begin function main_1_2_7
	.p2align	4, 0x90
	.type	main_1_2_7,@function
main_1_2_7:                             # @main_1_2_7
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	andq	$-16, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movl	$.Lstr, %edi
	callq	writeString@PLT
	movq	%r14, %rdi
	callq	writeString@PLT
	movl	$.Lstr.1, %edi
	callq	writeString@PLT
	movq	%rbx, %rdi
	callq	writeString@PLT
	movl	$.Lstr.2, %edi
	callq	writeString@PLT
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end3:
	.size	main_1_2_7, .Lfunc_end3-main_1_2_7
	.cfi_endproc
                                        # -- End function
	.type	.Lstr,@object                   # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.p2align	4, 0x0
.Lstr:
	.asciz	"Moving from "
	.size	.Lstr, 13

	.type	.Lstr.1,@object                 # @str.1
	.p2align	4, 0x0
.Lstr.1:
	.asciz	" to "
	.size	.Lstr.1, 5

	.type	.Lstr.2,@object                 # @str.2
	.p2align	4, 0x0
.Lstr.2:
	.asciz	".\\n"
	.size	.Lstr.2, 4

	.type	.Lstr.3,@object                 # @str.3
	.p2align	4, 0x0
.Lstr.3:
	.asciz	"Rings: "
	.size	.Lstr.3, 8

	.type	.Lstr.4,@object                 # @str.4
	.p2align	4, 0x0
.Lstr.4:
	.asciz	"left"
	.size	.Lstr.4, 5

	.type	.Lstr.5,@object                 # @str.5
	.p2align	4, 0x0
.Lstr.5:
	.asciz	"right"
	.size	.Lstr.5, 6

	.type	.Lstr.6,@object                 # @str.6
	.p2align	4, 0x0
.Lstr.6:
	.asciz	"middle"
	.size	.Lstr.6, 7

	.section	".note.GNU-stack","",@progbits
