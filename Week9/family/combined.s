	.file	"combined.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"Member_A()\n"
.text
	.align 4
.globl __8Member_Ai
	.type	 __8Member_Ai,@function
__8Member_Ai:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	8(%ebp), %edx
	cmpl	$0, 12(%ebp)
	je	.L3
	leal	8(%edx), %eax
	movl	%eax, (%edx)
.L3:
	subl	$8, %esp
	pushl	$.LC0
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe1:
	.size	 __8Member_Ai,.Lfe1-__8Member_Ai
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
	je	.L12
	subl	$12, %esp
	pushl	8(%ebp)
	call	__builtin_delete
	addl	$16, %esp
.L12:
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
	.string	"Member_B()\n"
.text
	.align 4
.globl __8Member_Bi
	.type	 __8Member_Bi,@function
__8Member_Bi:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	movl	8(%ebp), %ebx
	cmpl	$0, 12(%ebp)
	je	.L16
	leal	8(%ebx), %eax
	movl	%eax, (%ebx)
.L16:
	subl	$8, %esp
	pushl	$.LC3
	pushl	$cout
	call	__ls__7ostreamPCc
	movl	(%ebx), %eax
	movl	$1, (%eax)
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe4:
	.size	 __8Member_Bi,.Lfe4-__8Member_Bi
	.section	.rodata.str1.1,"ams",@progbits,1
.LC4:
	.string	"~Member_B()\n"
.text
	.align 4
.globl _._8Member_B
	.type	 _._8Member_B,@function
_._8Member_B:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$12, %esp
	movl	12(%ebp), %ebx
	pushl	$.LC4
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	testl	$1, %ebx
	je	.L25
	subl	$12, %esp
	pushl	8(%ebp)
	call	__builtin_delete
	addl	$16, %esp
.L25:
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe5:
	.size	 _._8Member_B,.Lfe5-_._8Member_B
	.section	.rodata.str1.1,"ams",@progbits,1
.LC5:
	.string	")\n"
.LC6:
	.string	"Member_B::operation2("
.text
	.align 4
.globl operation2__8Member_Bi
	.type	 operation2__8Member_Bi,@function
operation2__8Member_Bi:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC5
	subl	$12, %esp
	pushl	12(%ebp)
	subl	$12, %esp
	pushl	$.LC6
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
.Lfe6:
	.size	 operation2__8Member_Bi,.Lfe6-operation2__8Member_Bi
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	subl	$8, %esp
	pushl	$1
	pushl	$12
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_Ai
	movl	%ebx, (%esp)
	call	operation1__8Member_A
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L30
	subl	$8, %esp
	pushl	$3
	pushl	%ebx
	call	_._8Member_A
	addl	$16, %esp
.L30:
	subl	$8, %esp
	pushl	$1
	pushl	$12
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_Bi
	addl	$8, %esp
	pushl	$1
	pushl	%ebx
	call	operation2__8Member_Bi
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L32
	subl	$8, %esp
	pushl	$3
	pushl	%ebx
	call	_._8Member_B
	addl	$16, %esp
.L32:
	subl	$12, %esp
	pushl	$20
	call	__builtin_new
	addl	$8, %esp
	movl	%eax, %ebx
	leal	16(%ebx), %eax
	movl	%eax, 8(%ebx)
	movl	%eax, (%ebx)
	pushl	$0
	pushl	%ebx
	call	__8Member_Ai
	addl	$8, %esp
	pushl	$0
	leal	8(%ebx), %esi
	pushl	%esi
	call	__8Member_Bi
	movl	%ebx, (%esp)
	call	operation1__8Member_A
	addl	$8, %esp
	pushl	%eax
	pushl	%esi
	call	operation2__8Member_Bi
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L40
	subl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._8Member_B
	addl	$8, %esp
	pushl	$0
	pushl	%ebx
	call	_._8Member_A
	movl	%ebx, (%esp)
	call	__builtin_delete
	addl	$16, %esp
.L40:
	movl	$0, %eax
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe7:
	.size	 main,.Lfe7-main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
