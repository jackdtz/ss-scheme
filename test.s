	.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

	movq	$33, -8(%rbp)
	addq	$19, -8(%rbp)
	movq	-8(%rbp), %rax

	movq	%rax, %rdi
	callq	_print_int
	movq	$0, %rax
	addq	$16, %rsp
	popq	%rbp
	retq
