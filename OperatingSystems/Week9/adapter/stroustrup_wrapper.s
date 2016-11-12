	.file	"stroustrup_wrapper.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"enter()\n"
.text
	.align 4
.globl enter__Fv
	.type	 enter__Fv,@function
enter__Fv:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC0
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe1:
	.size	 enter__Fv,.Lfe1-enter__Fv
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	"leave()\n"
.text
	.align 4
.globl leave__Fv
	.type	 leave__Fv,@function
leave__Fv:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC1
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe2:
	.size	 leave__Fv,.Lfe2-leave__Fv
	.section	.rodata.str1.1,"ams",@progbits,1
.LC2:
	.string	"operation1()\n"
.LC3:
	.string	")\n"
.LC4:
	.string	"operation2("
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$20, %esp
	pushl	$1
	call	__builtin_new
	addl	$16, %esp
	call	enter__Fv
	subl	$8, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	call	leave__Fv
	call	enter__Fv
	subl	$8, %esp
	pushl	$.LC3
	subl	$12, %esp
	pushl	$2
	subl	$12, %esp
	pushl	$.LC4
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreami
	addl	$20, %esp
	pushl	%eax
	call	__ls__7ostreamPCc
	addl	$16, %esp
	call	leave__Fv
	movl	$0, %eax
	leave
	ret
.Lfe3:
	.size	 main,.Lfe3-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
