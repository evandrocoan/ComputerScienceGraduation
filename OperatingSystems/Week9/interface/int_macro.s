	.file	"int_macro.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"\n"
.LC1:
	.string	"Size of interface: "
.LC2:
	.string	"Size of implementation: "
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC0
	subl	$12, %esp
	pushl	$1
	subl	$12, %esp
	pushl	$.LC1
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamUi
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$.LC0
	subl	$12, %esp
	pushl	$8
	subl	$12, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamUi
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	movl	$0, %eax
	leave
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
