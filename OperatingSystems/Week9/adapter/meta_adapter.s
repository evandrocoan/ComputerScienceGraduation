	.file	"meta_adapter.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"1"
.LC1:
	.string	"\n"
.LC2:
	.string	"n = "
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	pushl	$1
	call	__builtin_new
	movl	%eax, %ebx
	addl	$16, %esp
	pushl	$0
	pushl	$10
	pushl	$0
	pushl	$.LC0
	call	__strtol_internal
	addl	$8, %esp
	pushl	$.LC1
	subl	$12, %esp
	pushl	%eax
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
	movl	%ebx, (%esp)
	call	__builtin_delete
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
