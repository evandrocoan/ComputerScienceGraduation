	.file	"incremental.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"Member_A()\n"
.text
	.align 4
.globl __8Member_A
	.type	 __8Member_A,@function
__8Member_A:
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
	.size	 __8Member_A,.Lfe1-__8Member_A
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	"~Member_A()\n"
.text
	.align 4
.globl _._8Member_A
	.type	 _._8Member_A,@function
_._8Member_A:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$12, %esp
	movl	12(%ebp), %ebx
	pushl	$.LC1
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	testl	$1, %ebx
	je	.L9
	subl	$12, %esp
	pushl	8(%ebp)
	call	__builtin_delete
	addl	$16, %esp
.L9:
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe2:
	.size	 _._8Member_A,.Lfe2-_._8Member_A
	.section	.rodata.str1.1,"ams",@progbits,1
.LC2:
	.string	"Member_A::operation1()\n"
.text
	.align 4
.globl operation1__8Member_A
	.type	 operation1__8Member_A,@function
operation1__8Member_A:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	movl	$1, %eax
	leave
	ret
.Lfe3:
	.size	 operation1__8Member_A,.Lfe3-operation1__8Member_A
	.section	.rodata.str1.1,"ams",@progbits,1
.LC3:
	.string	")\n"
.LC4:
	.string	"Member_B::operation2("
.text
	.align 4
.globl operation2__8Member_Bi
	.type	 operation2__8Member_Bi,@function
operation2__8Member_Bi:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC3
	subl	$12, %esp
	pushl	12(%ebp)
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
	leave
	ret
.Lfe4:
	.size	 operation2__8Member_Bi,.Lfe4-operation2__8Member_Bi
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
	call	__8Member_A
	movl	%ebx, (%esp)
	call	operation1__8Member_A
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L16
	subl	$8, %esp
	pushl	$3
	pushl	%ebx
	call	_._8Member_A
	addl	$16, %esp
.L16:
	subl	$12, %esp
	pushl	$12
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_A
	movl	%ebx, (%esp)
	call	operation1__8Member_A
	addl	$8, %esp
	pushl	%eax
	pushl	%ebx
	call	operation2__8Member_Bi
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L20
	subl	$8, %esp
	pushl	$0
	pushl	%ebx
	call	_._8Member_A
	movl	%ebx, (%esp)
	call	__builtin_delete
	addl	$16, %esp
.L20:
	subl	$12, %esp
	pushl	$12
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_A
	movl	%ebx, (%esp)
	call	operation1__8Member_A
	addl	$8, %esp
	pushl	%eax
	pushl	%ebx
	call	operation2__8Member_Bi
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L28
	subl	$8, %esp
	pushl	$0
	pushl	%ebx
	call	_._8Member_A
	movl	%ebx, (%esp)
	call	__builtin_delete
	addl	$16, %esp
.L28:
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe5:
	.size	 main,.Lfe5-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
