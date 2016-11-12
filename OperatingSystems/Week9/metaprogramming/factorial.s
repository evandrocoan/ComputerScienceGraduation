	.file	"factorial.cc"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$10
	subl	$12, %esp
	pushl	$5040
	pushl	$cout
	call	__ls__7ostreami
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamc
	movl	$0, %eax
	leave
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
