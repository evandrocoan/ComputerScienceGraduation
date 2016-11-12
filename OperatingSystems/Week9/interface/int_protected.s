	.file	"int_protected.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"Abstraction::operation1()\n"
.text
	.align 4
.globl operation1__11Abstraction
	.type	 operation1__11Abstraction,@function
operation1__11Abstraction:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC0
	pushl	$cout
	call	__ls__7ostreamPCc
	movl	$1, %eax
	leave
	ret
.Lfe1:
	.size	 operation1__11Abstraction,.Lfe1-operation1__11Abstraction
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	")\n"
.LC2:
	.string	"Abstraction::operation2("
.text
	.align 4
.globl operation2__11Abstractioni
	.type	 operation2__11Abstractioni,@function
operation2__11Abstractioni:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC1
	subl	$12, %esp
	pushl	12(%ebp)
	subl	$12, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreami
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe2:
	.size	 operation2__11Abstractioni,.Lfe2-operation2__11Abstractioni
	.section	.rodata.str1.1,"ams",@progbits,1
.LC3:
	.string	"\n"
.LC4:
	.string	"Size of interface: "
.LC5:
	.string	"Size of implementation: "
.LC6:
	.string	"Size of instance: "
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	pushl	$8
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	operation1__11Abstraction
	addl	$8, %esp
	pushl	$1
	pushl	%ebx
	call	operation2__11Abstractioni
	addl	$8, %esp
	pushl	$.LC3
	subl	$12, %esp
	pushl	$1
	subl	$12, %esp
	pushl	$.LC4
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamUi
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$.LC3
	subl	$12, %esp
	pushl	$8
	subl	$12, %esp
	pushl	$.LC5
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamUi
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$.LC3
	subl	$12, %esp
	pushl	$8
	subl	$12, %esp
	pushl	$.LC6
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamUi
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	movl	%ebx, (%esp)
	call	__builtin_delete
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe3:
	.size	 main,.Lfe3-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
