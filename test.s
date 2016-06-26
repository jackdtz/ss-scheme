	.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp

	movq	$1, %rcx
	movq	$46, %rbx
	addq	$7, %rcx
	movq	%rcx, %rdx
	addq	$4, %rdx
	addq	%rbx, %rcx
	movq	%rdx, %rbx
	negq	%rbx
	addq	%rbx, %rcx
	movq	%rcx, %rax

	movq	%rax, %rdi
	callq	_print_int
	movq	$0, %rax
	addq	$0, %rsp
	popq	%rbp
	retq
