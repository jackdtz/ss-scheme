	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_any_tag
	.align	4, 0x90
_any_tag:                               ## @any_tag
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	andq	$7, %rdi
	movl	%edi, %eax
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_is_ptr
	.align	4, 0x90
_is_ptr:                                ## @is_ptr
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp3:
	.cfi_def_cfa_offset 16
Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp5:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -24(%rbp)
	cmpq	$0, -24(%rbp)
	jne	LBB1_2
## BB#1:
	movl	$0, -4(%rbp)
	jmp	LBB1_5
LBB1_2:
	movq	-24(%rbp), %rdi
	callq	_any_tag
	movb	$1, %cl
	movl	%eax, -28(%rbp)
	cmpl	$0, -28(%rbp)
	movb	%cl, -29(%rbp)          ## 1-byte Spill
	je	LBB1_4
## BB#3:
	cmpl	$2, -28(%rbp)
	sete	%al
	movb	%al, -29(%rbp)          ## 1-byte Spill
LBB1_4:
	movb	-29(%rbp), %al          ## 1-byte Reload
	andb	$1, %al
	movzbl	%al, %ecx
	movl	%ecx, -4(%rbp)
LBB1_5:
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_to_ptr
	.align	4, 0x90
_to_ptr:                                ## @to_ptr
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp6:
	.cfi_def_cfa_offset 16
Ltmp7:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp8:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	_any_tag
	cmpl	$0, %eax
	jne	LBB2_2
## BB#1:
	movq	-16(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	LBB2_3
LBB2_2:
	movq	-24(%rbp), %rax
	andq	$-8, %rax
	movq	%rax, -8(%rbp)
LBB2_3:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_initialize
	.align	4, 0x90
_initialize:                            ## @initialize
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp9:
	.cfi_def_cfa_offset 16
Ltmp10:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp11:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rsi
	andq	$7, %rsi
	cmpq	$0, %rsi
	sete	%al
	xorb	$-1, %al
	andb	$1, %al
	movzbl	%al, %ecx
	movslq	%ecx, %rsi
	cmpq	$0, %rsi
	je	LBB3_2
## BB#1:
	leaq	L___func__.initialize(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$95, %edx
	leaq	L_.str.1(%rip), %rcx
	callq	___assert_rtn
LBB3_2:
	jmp	LBB3_3
LBB3_3:
	movq	-8(%rbp), %rax
	andq	$7, %rax
	cmpq	$0, %rax
	sete	%cl
	xorb	$-1, %cl
	andb	$1, %cl
	movzbl	%cl, %edx
	movslq	%edx, %rax
	cmpq	$0, %rax
	je	LBB3_5
## BB#4:
	leaq	L___func__.initialize(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$96, %edx
	leaq	L_.str.2(%rip), %rcx
	callq	___assert_rtn
LBB3_5:
	jmp	LBB3_6
LBB3_6:
	movq	-16(%rbp), %rdi
	callq	_malloc
	movq	_fromspace_begin@GOTPCREL(%rip), %rdi
	movq	%rax, (%rdi)
	cmpq	$0, %rax
	jne	LBB3_8
## BB#7:
	leaq	L_.str.3(%rip), %rdi
	movq	-16(%rbp), %rsi
	movb	$0, %al
	callq	_printf
	movl	$1, %edi
	movl	%eax, -20(%rbp)         ## 4-byte Spill
	callq	_exit
LBB3_8:
	movq	-16(%rbp), %rdi
	callq	_malloc
	movq	%rax, _tospace_begin(%rip)
	cmpq	$0, %rax
	jne	LBB3_10
## BB#9:
	leaq	L_.str.4(%rip), %rdi
	movq	-16(%rbp), %rsi
	movb	$0, %al
	callq	_printf
	movl	$1, %edi
	movl	%eax, -24(%rbp)         ## 4-byte Spill
	callq	_exit
LBB3_10:
	movq	-8(%rbp), %rdi
	callq	_malloc
	movq	_rootstack_begin@GOTPCREL(%rip), %rdi
	movq	%rax, (%rdi)
	cmpq	$0, %rax
	jne	LBB3_12
## BB#11:
	leaq	L_.str.5(%rip), %rdi
	movq	-8(%rbp), %rsi
	movb	$0, %al
	callq	_printf
	movl	$1, %edi
	movl	%eax, -28(%rbp)         ## 4-byte Spill
	callq	_exit
LBB3_12:
	movq	_free_ptr@GOTPCREL(%rip), %rax
	movq	_fromspace_begin@GOTPCREL(%rip), %rcx
	movq	_rootstack_end@GOTPCREL(%rip), %rdx
	movq	_rootstack_begin@GOTPCREL(%rip), %rsi
	movq	_fromspace_end@GOTPCREL(%rip), %rdi
	movq	(%rcx), %r8
	movq	-16(%rbp), %r9
	shrq	$3, %r9
	shlq	$3, %r9
	addq	%r9, %r8
	movq	%r8, (%rdi)
	movq	_tospace_begin(%rip), %rdi
	movq	-16(%rbp), %r8
	shrq	$3, %r8
	shlq	$3, %r8
	addq	%r8, %rdi
	movq	%rdi, _tospace_end(%rip)
	movq	(%rsi), %rsi
	movq	-8(%rbp), %rdi
	shrq	$3, %rdi
	shlq	$3, %rdi
	addq	%rdi, %rsi
	movq	%rsi, (%rdx)
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movl	$1, _initialized(%rip)
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_collect
	.align	4, 0x90
_collect:                               ## @collect
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp12:
	.cfi_def_cfa_offset 16
Ltmp13:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp14:
	.cfi_def_cfa_register %rbp
	subq	$192, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	cmpl	$0, _initialized(%rip)
	setne	%al
	xorb	$-1, %al
	andb	$1, %al
	movzbl	%al, %ecx
	movslq	%ecx, %rsi
	cmpq	$0, %rsi
	je	LBB4_2
## BB#1:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$136, %edx
	leaq	L_.str.6(%rip), %rcx
	callq	___assert_rtn
LBB4_2:
	jmp	LBB4_3
LBB4_3:
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movq	-8(%rbp), %rcx
	cmpq	(%rax), %rcx
	setae	%dl
	xorb	$-1, %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movslq	%esi, %rax
	cmpq	$0, %rax
	je	LBB4_5
## BB#4:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$137, %edx
	leaq	L_.str.7(%rip), %rcx
	callq	___assert_rtn
LBB4_5:
	jmp	LBB4_6
LBB4_6:
	movq	_rootstack_end@GOTPCREL(%rip), %rax
	movq	-8(%rbp), %rcx
	cmpq	(%rax), %rcx
	setb	%dl
	xorb	$-1, %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movslq	%esi, %rax
	cmpq	$0, %rax
	je	LBB4_8
## BB#7:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$138, %edx
	leaq	L_.str.8(%rip), %rcx
	callq	___assert_rtn
LBB4_8:
	jmp	LBB4_9
LBB4_9:
	movl	$0, -20(%rbp)
LBB4_10:                                ## =>This Inner Loop Header: Depth=1
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movl	-20(%rbp), %ecx
	movl	%ecx, %edx
	shlq	$3, %rdx
	addq	%rdx, %rax
	cmpq	-8(%rbp), %rax
	jae	LBB4_20
## BB#11:                               ##   in Loop: Header=BB4_10 Depth=1
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movl	-20(%rbp), %ecx
	movl	%ecx, %edx
	movq	(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	_is_ptr
	cmpl	$0, %eax
	je	LBB4_18
## BB#12:                               ##   in Loop: Header=BB4_10 Depth=1
	movq	-32(%rbp), %rdi
	callq	_to_ptr
	xorl	%ecx, %ecx
	movb	%cl, %dl
	movq	_fromspace_begin@GOTPCREL(%rip), %rdi
	movq	%rax, -40(%rbp)
	movq	(%rdi), %rax
	cmpq	-40(%rbp), %rax
	movb	%dl, -169(%rbp)         ## 1-byte Spill
	ja	LBB4_14
## BB#13:                               ##   in Loop: Header=BB4_10 Depth=1
	movq	_fromspace_end@GOTPCREL(%rip), %rax
	movq	-40(%rbp), %rcx
	cmpq	(%rax), %rcx
	setb	%dl
	movb	%dl, -169(%rbp)         ## 1-byte Spill
LBB4_14:                                ##   in Loop: Header=BB4_10 Depth=1
	movb	-169(%rbp), %al         ## 1-byte Reload
	xorb	$-1, %al
	andb	$1, %al
	movzbl	%al, %ecx
	movslq	%ecx, %rdx
	cmpq	$0, %rdx
	je	LBB4_16
## BB#15:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$146, %edx
	leaq	L_.str.9(%rip), %rcx
	callq	___assert_rtn
LBB4_16:                                ##   in Loop: Header=BB4_10 Depth=1
	jmp	LBB4_17
LBB4_17:                                ##   in Loop: Header=BB4_10 Depth=1
	jmp	LBB4_18
LBB4_18:                                ##   in Loop: Header=BB4_10 Depth=1
	jmp	LBB4_19
LBB4_19:                                ##   in Loop: Header=BB4_10 Depth=1
	movl	-20(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -20(%rbp)
	jmp	LBB4_10
LBB4_20:
	movq	-8(%rbp), %rdi
	callq	_cheney
	movq	_free_ptr@GOTPCREL(%rip), %rdi
	movq	_fromspace_end@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	(%rdi), %rdi
	subq	%rdi, %rax
	sarq	$3, %rax
	shlq	$3, %rax
	cmpq	-16(%rbp), %rax
	jae	LBB4_29
## BB#21:
	movq	_fromspace_begin@GOTPCREL(%rip), %rax
	movq	_fromspace_end@GOTPCREL(%rip), %rcx
	movq	_free_ptr@GOTPCREL(%rip), %rdx
	movq	(%rdx), %rdx
	movq	(%rax), %rsi
	subq	%rsi, %rdx
	sarq	$3, %rdx
	shlq	$3, %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	addq	-16(%rbp), %rdx
	movq	%rdx, -56(%rbp)
	movq	(%rcx), %rcx
	movq	(%rax), %rax
	subq	%rax, %rcx
	sarq	$3, %rcx
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rax
	shlq	$3, %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	%rax, -80(%rbp)
LBB4_22:                                ## =>This Inner Loop Header: Depth=1
	movq	-80(%rbp), %rax
	cmpq	-56(%rbp), %rax
	jae	LBB4_24
## BB#23:                               ##   in Loop: Header=BB4_22 Depth=1
	movq	-80(%rbp), %rax
	shlq	$1, %rax
	movq	%rax, -80(%rbp)
	jmp	LBB4_22
LBB4_24:
	movq	_tospace_begin(%rip), %rax
	movq	%rax, %rdi
	callq	_free
	movq	-80(%rbp), %rdi
	callq	_malloc
	movq	%rax, _tospace_begin(%rip)
	cmpq	$0, %rax
	jne	LBB4_26
## BB#25:
	leaq	L_.str.10(%rip), %rdi
	movq	-80(%rbp), %rsi
	movb	$0, %al
	callq	_printf
	movl	$1, %edi
	movl	%eax, -176(%rbp)        ## 4-byte Spill
	callq	_exit
LBB4_26:
	movq	_tospace_begin(%rip), %rax
	movq	-80(%rbp), %rcx
	shrq	$3, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rax
	movq	%rax, _tospace_end(%rip)
	movq	-8(%rbp), %rdi
	callq	_cheney
	movq	_tospace_begin(%rip), %rax
	movq	%rax, %rdi
	callq	_free
	movq	-80(%rbp), %rdi
	callq	_malloc
	movq	%rax, _tospace_begin(%rip)
	cmpq	$0, %rax
	jne	LBB4_28
## BB#27:
	leaq	L_.str.11(%rip), %rdi
	movq	-80(%rbp), %rsi
	movb	$0, %al
	callq	_printf
	movl	$1, %edi
	movl	%eax, -180(%rbp)        ## 4-byte Spill
	callq	_exit
LBB4_28:
	movq	_tospace_begin(%rip), %rax
	movq	-80(%rbp), %rcx
	shrq	$3, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rax
	movq	%rax, _tospace_end(%rip)
LBB4_29:
	movq	_fromspace_end@GOTPCREL(%rip), %rax
	movq	_free_ptr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rcx
	cmpq	(%rax), %rcx
	setb	%dl
	xorb	$-1, %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movslq	%esi, %rax
	cmpq	$0, %rax
	je	LBB4_31
## BB#30:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$222, %edx
	leaq	L_.str.12(%rip), %rcx
	callq	___assert_rtn
LBB4_31:
	jmp	LBB4_32
LBB4_32:
	movq	_fromspace_begin@GOTPCREL(%rip), %rax
	movq	_free_ptr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rcx
	cmpq	(%rax), %rcx
	setae	%dl
	xorb	$-1, %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movslq	%esi, %rax
	cmpq	$0, %rax
	je	LBB4_34
## BB#33:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$223, %edx
	leaq	L_.str.13(%rip), %rcx
	callq	___assert_rtn
LBB4_34:
	jmp	LBB4_35
LBB4_35:
	movq	$0, -88(%rbp)
LBB4_36:                                ## =>This Inner Loop Header: Depth=1
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	-88(%rbp), %rcx
	shlq	$3, %rcx
	addq	%rcx, %rax
	cmpq	-8(%rbp), %rax
	jae	LBB4_46
## BB#37:                               ##   in Loop: Header=BB4_36 Depth=1
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movq	-88(%rbp), %rcx
	movq	(%rax), %rax
	movq	(%rax,%rcx,8), %rax
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rdi
	callq	_is_ptr
	cmpl	$0, %eax
	je	LBB4_44
## BB#38:                               ##   in Loop: Header=BB4_36 Depth=1
	movq	-96(%rbp), %rdi
	callq	_to_ptr
	xorl	%ecx, %ecx
	movb	%cl, %dl
	movq	_fromspace_begin@GOTPCREL(%rip), %rdi
	movq	%rax, -104(%rbp)
	movq	(%rdi), %rax
	cmpq	-104(%rbp), %rax
	movb	%dl, -181(%rbp)         ## 1-byte Spill
	ja	LBB4_40
## BB#39:                               ##   in Loop: Header=BB4_36 Depth=1
	movq	_fromspace_end@GOTPCREL(%rip), %rax
	movq	-104(%rbp), %rcx
	cmpq	(%rax), %rcx
	setb	%dl
	movb	%dl, -181(%rbp)         ## 1-byte Spill
LBB4_40:                                ##   in Loop: Header=BB4_36 Depth=1
	movb	-181(%rbp), %al         ## 1-byte Reload
	xorb	$-1, %al
	andb	$1, %al
	movzbl	%al, %ecx
	movslq	%ecx, %rdx
	cmpq	$0, %rdx
	je	LBB4_42
## BB#41:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$230, %edx
	leaq	L_.str.9(%rip), %rcx
	callq	___assert_rtn
LBB4_42:                                ##   in Loop: Header=BB4_36 Depth=1
	jmp	LBB4_43
LBB4_43:                                ##   in Loop: Header=BB4_36 Depth=1
	jmp	LBB4_44
LBB4_44:                                ##   in Loop: Header=BB4_36 Depth=1
	jmp	LBB4_45
LBB4_45:                                ##   in Loop: Header=BB4_36 Depth=1
	movq	-88(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -88(%rbp)
	jmp	LBB4_36
LBB4_46:
	movq	_fromspace_begin@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, -112(%rbp)
LBB4_47:                                ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB4_49 Depth 2
	movq	_free_ptr@GOTPCREL(%rip), %rax
	movq	-112(%rbp), %rcx
	cmpq	(%rax), %rcx
	je	LBB4_63
## BB#48:                               ##   in Loop: Header=BB4_47 Depth=1
	movq	-112(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rdi
	callq	_get_length
	movb	%al, %cl
	movb	%cl, -121(%rbp)
	movq	-120(%rbp), %rdi
	callq	_get_ptr_bitfield
	movq	%rax, -136(%rbp)
	movq	-112(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -144(%rbp)
	movq	-112(%rbp), %rax
	movzbl	-121(%rbp), %edx
	movslq	%edx, %rdi
	shlq	$3, %rdi
	addq	%rdi, %rax
	addq	$8, %rax
	movq	%rax, -112(%rbp)
	movb	$0, -145(%rbp)
LBB4_49:                                ##   Parent Loop BB4_47 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movzbl	-145(%rbp), %eax
	movzbl	-121(%rbp), %ecx
	cmpl	%ecx, %eax
	je	LBB4_62
## BB#50:                               ##   in Loop: Header=BB4_49 Depth=2
	movq	-136(%rbp), %rax
	movzbl	-145(%rbp), %ecx
	movl	%ecx, %ecx
                                        ## 
                                        ## 
	sarq	%cl, %rax
	andq	$1, %rax
	cmpq	$0, %rax
	je	LBB4_60
## BB#51:                               ##   in Loop: Header=BB4_49 Depth=2
	movzbl	-145(%rbp), %eax
	movl	%eax, %ecx
	movq	-144(%rbp), %rdx
	movq	(%rdx,%rcx,8), %rcx
	movq	%rcx, -160(%rbp)
	movq	-160(%rbp), %rdi
	callq	_is_ptr
	cmpl	$0, %eax
	je	LBB4_59
## BB#52:                               ##   in Loop: Header=BB4_49 Depth=2
	movq	-160(%rbp), %rdi
	callq	_to_ptr
	movq	_fromspace_end@GOTPCREL(%rip), %rdi
	movq	%rax, -168(%rbp)
	movq	-168(%rbp), %rax
	cmpq	(%rdi), %rax
	setb	%cl
	xorb	$-1, %cl
	andb	$1, %cl
	movzbl	%cl, %edx
	movslq	%edx, %rax
	cmpq	$0, %rax
	je	LBB4_54
## BB#53:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$252, %edx
	leaq	L_.str.14(%rip), %rcx
	callq	___assert_rtn
LBB4_54:                                ##   in Loop: Header=BB4_49 Depth=2
	jmp	LBB4_55
LBB4_55:                                ##   in Loop: Header=BB4_49 Depth=2
	movq	_fromspace_begin@GOTPCREL(%rip), %rax
	movq	-168(%rbp), %rcx
	cmpq	(%rax), %rcx
	setae	%dl
	xorb	$-1, %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movslq	%esi, %rax
	cmpq	$0, %rax
	je	LBB4_57
## BB#56:
	leaq	L___func__.collect(%rip), %rdi
	leaq	L_.str(%rip), %rsi
	movl	$253, %edx
	leaq	L_.str.15(%rip), %rcx
	callq	___assert_rtn
LBB4_57:                                ##   in Loop: Header=BB4_49 Depth=2
	jmp	LBB4_58
LBB4_58:                                ##   in Loop: Header=BB4_49 Depth=2
	jmp	LBB4_59
LBB4_59:                                ##   in Loop: Header=BB4_49 Depth=2
	jmp	LBB4_60
LBB4_60:                                ##   in Loop: Header=BB4_49 Depth=2
	jmp	LBB4_61
LBB4_61:                                ##   in Loop: Header=BB4_49 Depth=2
	movb	-145(%rbp), %al
	addb	$1, %al
	movb	%al, -145(%rbp)
	jmp	LBB4_49
LBB4_62:                                ##   in Loop: Header=BB4_47 Depth=1
	jmp	LBB4_47
LBB4_63:
	addq	$192, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.align	4, 0x90
_cheney:                                ## @cheney
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp15:
	.cfi_def_cfa_offset 16
Ltmp16:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp17:
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	_rootstack_begin@GOTPCREL(%rip), %rax
	movq	_free_ptr@GOTPCREL(%rip), %rcx
	movq	%rdi, -8(%rbp)
	movq	_tospace_begin(%rip), %rdi
	movq	%rdi, -16(%rbp)
	movq	_tospace_begin(%rip), %rdi
	movq	%rdi, (%rcx)
	movq	(%rax), %rax
	movq	%rax, -24(%rbp)
LBB5_1:                                 ## =>This Inner Loop Header: Depth=1
	movq	-24(%rbp), %rax
	cmpq	-8(%rbp), %rax
	je	LBB5_4
## BB#2:                                ##   in Loop: Header=BB5_1 Depth=1
	movq	-24(%rbp), %rdi
	callq	_copy_vector
## BB#3:                                ##   in Loop: Header=BB5_1 Depth=1
	movq	-24(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -24(%rbp)
	jmp	LBB5_1
LBB5_4:
	jmp	LBB5_5
LBB5_5:                                 ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB5_7 Depth 2
	movq	_free_ptr@GOTPCREL(%rip), %rax
	movq	-16(%rbp), %rcx
	cmpq	(%rax), %rcx
	je	LBB5_12
## BB#6:                                ##   in Loop: Header=BB5_5 Depth=1
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	_get_length
	movl	%eax, -36(%rbp)
	movq	-16(%rbp), %rdi
	movslq	-36(%rbp), %rcx
	shlq	$3, %rcx
	addq	%rcx, %rdi
	addq	$8, %rdi
	movq	%rdi, -48(%rbp)
	movq	-32(%rbp), %rdi
	callq	_get_ptr_bitfield
	movq	%rax, -56(%rbp)
	movq	-16(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -16(%rbp)
LBB5_7:                                 ##   Parent Loop BB5_5 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movq	-16(%rbp), %rax
	cmpq	-48(%rbp), %rax
	je	LBB5_11
## BB#8:                                ##   in Loop: Header=BB5_7 Depth=2
	movq	-56(%rbp), %rax
	andq	$1, %rax
	cmpq	$1, %rax
	jne	LBB5_10
## BB#9:                                ##   in Loop: Header=BB5_7 Depth=2
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	callq	_copy_vector
LBB5_10:                                ##   in Loop: Header=BB5_7 Depth=2
	movq	-56(%rbp), %rax
	sarq	$1, %rax
	movq	%rax, -56(%rbp)
	movq	-16(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -16(%rbp)
	jmp	LBB5_7
LBB5_11:                                ##   in Loop: Header=BB5_5 Depth=1
	jmp	LBB5_5
LBB5_12:
	movq	_fromspace_end@GOTPCREL(%rip), %rax
	movq	_fromspace_begin@GOTPCREL(%rip), %rcx
	movq	_tospace_begin(%rip), %rdx
	movq	%rdx, -64(%rbp)
	movq	_tospace_end(%rip), %rdx
	movq	%rdx, -72(%rbp)
	movq	(%rcx), %rdx
	movq	%rdx, _tospace_begin(%rip)
	movq	(%rax), %rdx
	movq	%rdx, _tospace_end(%rip)
	movq	-64(%rbp), %rdx
	movq	%rdx, (%rcx)
	movq	-72(%rbp), %rcx
	movq	%rcx, (%rax)
	addq	$80, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.align	4, 0x90
_get_length:                            ## @get_length
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp18:
	.cfi_def_cfa_offset 16
Ltmp19:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp20:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	andq	$126, %rdi
	sarq	$1, %rdi
	movl	%edi, %eax
	popq	%rbp
	retq
	.cfi_endproc

	.align	4, 0x90
_get_ptr_bitfield:                      ## @get_ptr_bitfield
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp21:
	.cfi_def_cfa_offset 16
Ltmp22:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp23:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	sarq	$7, %rdi
	movq	%rdi, %rax
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_read_int
	.align	4, 0x90
_read_int:                              ## @read_int
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp24:
	.cfi_def_cfa_offset 16
Ltmp25:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp26:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.16(%rip), %rdi
	leaq	-8(%rbp), %rsi
	movb	$0, %al
	callq	_scanf
	movq	-8(%rbp), %rsi
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	movq	%rsi, %rax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_int
	.align	4, 0x90
_print_int:                             ## @print_int
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp27:
	.cfi_def_cfa_offset 16
Ltmp28:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp29:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.16(%rip), %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	movq	%rax, %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_bool
	.align	4, 0x90
_print_bool:                            ## @print_bool
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp30:
	.cfi_def_cfa_offset 16
Ltmp31:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp32:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	cmpq	$0, -8(%rbp)
	je	LBB10_2
## BB#1:
	leaq	L_.str.17(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	jmp	LBB10_3
LBB10_2:
	leaq	L_.str.18(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -16(%rbp)         ## 4-byte Spill
LBB10_3:
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_void
	.align	4, 0x90
_print_void:                            ## @print_void
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp33:
	.cfi_def_cfa_offset 16
Ltmp34:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp35:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.19(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_vecbegin
	.align	4, 0x90
_print_vecbegin:                        ## @print_vecbegin
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp36:
	.cfi_def_cfa_offset 16
Ltmp37:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp38:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.20(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_space
	.align	4, 0x90
_print_space:                           ## @print_space
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp39:
	.cfi_def_cfa_offset 16
Ltmp40:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp41:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.21(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_vecend
	.align	4, 0x90
_print_vecend:                          ## @print_vecend
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp42:
	.cfi_def_cfa_offset 16
Ltmp43:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp44:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.22(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_ellipsis
	.align	4, 0x90
_print_ellipsis:                        ## @print_ellipsis
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp45:
	.cfi_def_cfa_offset 16
Ltmp46:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp47:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str.23(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_print_any
	.align	4, 0x90
_print_any:                             ## @print_any
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp48:
	.cfi_def_cfa_offset 16
Ltmp49:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp50:
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -8(%rbp)
	callq	_any_tag
	decl	%eax
	movl	%eax, %edi
	subl	$4, %eax
	movq	%rdi, -40(%rbp)         ## 8-byte Spill
	movl	%eax, -44(%rbp)         ## 4-byte Spill
	ja	LBB16_13
## BB#15:
	leaq	LJTI16_0(%rip), %rax
	movq	-40(%rbp), %rcx         ## 8-byte Reload
	movslq	(%rax,%rcx,4), %rdx
	addq	%rax, %rdx
	jmpq	*%rdx
LBB16_1:
	leaq	L_.str.16(%rip), %rdi
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	movq	%rax, %rsi
	movb	$0, %al
	callq	_printf
	movl	%eax, -48(%rbp)         ## 4-byte Spill
	jmp	LBB16_14
LBB16_2:
	movq	-8(%rbp), %rax
	sarq	$3, %rax
	cmpq	$0, %rax
	je	LBB16_4
## BB#3:
	leaq	L_.str.17(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -52(%rbp)         ## 4-byte Spill
	jmp	LBB16_5
LBB16_4:
	leaq	L_.str.18(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -56(%rbp)         ## 4-byte Spill
LBB16_5:
	jmp	LBB16_14
LBB16_6:
	movq	-8(%rbp), %rax
	andq	$-8, %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	_get_length
	leaq	L_.str.20(%rip), %rdi
	movb	%al, %cl
	movb	%cl, -25(%rbp)
	movb	$0, %al
	callq	_printf
	movl	$0, -32(%rbp)
	movl	%eax, -60(%rbp)         ## 4-byte Spill
LBB16_7:                                ## =>This Inner Loop Header: Depth=1
	movl	-32(%rbp), %eax
	movzbl	-25(%rbp), %ecx
	cmpl	%ecx, %eax
	je	LBB16_10
## BB#8:                                ##   in Loop: Header=BB16_7 Depth=1
	movl	-32(%rbp), %eax
	addl	$1, %eax
	movslq	%eax, %rcx
	movq	-16(%rbp), %rdx
	movq	(%rdx,%rcx,8), %rdi
	callq	_print_any
## BB#9:                                ##   in Loop: Header=BB16_7 Depth=1
	movl	-32(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -32(%rbp)
	jmp	LBB16_7
LBB16_10:
	leaq	L_.str.22(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -64(%rbp)         ## 4-byte Spill
	jmp	LBB16_14
LBB16_11:
	leaq	L_.str.24(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -68(%rbp)         ## 4-byte Spill
	jmp	LBB16_14
LBB16_12:
	leaq	L_.str.19(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -72(%rbp)         ## 4-byte Spill
	jmp	LBB16_14
LBB16_13:
	leaq	L_.str.25(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	$4294967295, %edi       ## imm = 0xFFFFFFFF
	movl	%eax, -76(%rbp)         ## 4-byte Spill
	callq	_exit
LBB16_14:
	addq	$80, %rsp
	popq	%rbp
	retq
	.cfi_endproc
	.align	2, 0x90
L16_0_set_1 = LBB16_1-LJTI16_0
L16_0_set_6 = LBB16_6-LJTI16_0
L16_0_set_11 = LBB16_11-LJTI16_0
L16_0_set_2 = LBB16_2-LJTI16_0
L16_0_set_12 = LBB16_12-LJTI16_0
LJTI16_0:
	.long	L16_0_set_1
	.long	L16_0_set_6
	.long	L16_0_set_11
	.long	L16_0_set_2
	.long	L16_0_set_12

	.align	4, 0x90
_copy_vector:                           ## @copy_vector
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp51:
	.cfi_def_cfa_offset 16
Ltmp52:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp53:
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	callq	_any_tag
	movl	%eax, -20(%rbp)
	movq	-16(%rbp), %rdi
	callq	_is_ptr
	cmpl	$0, %eax
	jne	LBB17_2
## BB#1:
	jmp	LBB17_9
LBB17_2:
	movq	-16(%rbp), %rdi
	callq	_to_ptr
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	_is_forwarding
	cmpl	$0, %eax
	je	LBB17_4
## BB#3:
	movq	-32(%rbp), %rax
	movslq	-20(%rbp), %rcx
	orq	%rcx, %rax
	movq	-8(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	LBB17_9
LBB17_4:
	movq	-32(%rbp), %rdi
	callq	_get_length
	movq	_free_ptr@GOTPCREL(%rip), %rdi
	movl	%eax, -36(%rbp)
	movq	(%rdi), %rdi
	movq	%rdi, -48(%rbp)
	movl	$0, -52(%rbp)
LBB17_5:                                ## =>This Inner Loop Header: Depth=1
	movl	-52(%rbp), %eax
	movl	-36(%rbp), %ecx
	addl	$1, %ecx
	cmpl	%ecx, %eax
	je	LBB17_8
## BB#6:                                ##   in Loop: Header=BB17_5 Depth=1
	movslq	-52(%rbp), %rax
	movq	-16(%rbp), %rcx
	movq	(%rcx,%rax,8), %rax
	movslq	-52(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movq	%rax, (%rdx,%rcx,8)
## BB#7:                                ##   in Loop: Header=BB17_5 Depth=1
	movl	-52(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -52(%rbp)
	jmp	LBB17_5
LBB17_8:
	movq	_free_ptr@GOTPCREL(%rip), %rax
	movq	(%rax), %rcx
	movslq	-36(%rbp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rcx
	addq	$8, %rcx
	movq	%rcx, (%rax)
	movq	-48(%rbp), %rax
	movq	-16(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-48(%rbp), %rax
	movslq	-20(%rbp), %rcx
	orq	%rcx, %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	-8(%rbp), %rcx
	movq	%rax, (%rcx)
LBB17_9:
	addq	$64, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.align	4, 0x90
_is_forwarding:                         ## @is_forwarding
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp54:
	.cfi_def_cfa_offset 16
Ltmp55:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp56:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	andq	$1, %rdi
	cmpq	$0, %rdi
	setne	%al
	xorb	$-1, %al
	andb	$1, %al
	movzbl	%al, %eax
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L___func__.initialize:                  ## @__func__.initialize
	.asciz	"initialize"

L_.str:                                 ## @.str
	.asciz	"runtime.c"

L_.str.1:                               ## @.str.1
	.asciz	"(heap_size % sizeof(int64_t)) == 0"

L_.str.2:                               ## @.str.2
	.asciz	"(rootstack_size % sizeof(int64_t)) == 0"

	.comm	_fromspace_begin,8,3    ## @fromspace_begin
L_.str.3:                               ## @.str.3
	.asciz	"Failed to malloc %llu byte fromspace\n"

.zerofill __DATA,__bss,_tospace_begin,8,3 ## @tospace_begin
L_.str.4:                               ## @.str.4
	.asciz	"Failed to malloc %llu byte tospace\n"

	.comm	_rootstack_begin,8,3    ## @rootstack_begin
L_.str.5:                               ## @.str.5
	.asciz	"Failed to malloc %llu byte rootstack"

	.comm	_fromspace_end,8,3      ## @fromspace_end
.zerofill __DATA,__bss,_tospace_end,8,3 ## @tospace_end
	.comm	_rootstack_end,8,3      ## @rootstack_end
	.comm	_free_ptr,8,3           ## @free_ptr
.zerofill __DATA,__bss,_initialized,4,2 ## @initialized
L___func__.collect:                     ## @__func__.collect
	.asciz	"collect"

L_.str.6:                               ## @.str.6
	.asciz	"initialized"

L_.str.7:                               ## @.str.7
	.asciz	"rootstack_ptr >= rootstack_begin"

L_.str.8:                               ## @.str.8
	.asciz	"rootstack_ptr < rootstack_end"

L_.str.9:                               ## @.str.9
	.asciz	"fromspace_begin <= a_root && a_root < fromspace_end"

L_.str.10:                              ## @.str.10
	.asciz	"failed to malloc %ld byte fromspace"

L_.str.11:                              ## @.str.11
	.asciz	"failed to malloc %ld byte tospace"

L_.str.12:                              ## @.str.12
	.asciz	"free_ptr < fromspace_end"

L_.str.13:                              ## @.str.13
	.asciz	"free_ptr >= fromspace_begin"

L_.str.14:                              ## @.str.14
	.asciz	"real_ptr < fromspace_end"

L_.str.15:                              ## @.str.15
	.asciz	"real_ptr >= fromspace_begin"

L_.str.16:                              ## @.str.16
	.asciz	"%lld"

L_.str.17:                              ## @.str.17
	.asciz	"#t"

L_.str.18:                              ## @.str.18
	.asciz	"#f"

L_.str.19:                              ## @.str.19
	.asciz	"#<void>"

L_.str.20:                              ## @.str.20
	.asciz	"#("

L_.str.21:                              ## @.str.21
	.asciz	" "

L_.str.22:                              ## @.str.22
	.asciz	")"

L_.str.23:                              ## @.str.23
	.asciz	"#(...)"

L_.str.24:                              ## @.str.24
	.asciz	"#<procedure>"

L_.str.25:                              ## @.str.25
	.asciz	"unrecognized!"


.subsections_via_symbols
