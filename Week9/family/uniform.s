	.file	"uniform.cc"
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
	movl	8(%ebp), %eax
	movl	$__vt_8Member_A, 8(%eax)
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
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_8Member_A, 8(%esi)
	subl	$8, %esp
	pushl	$.LC1
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	testl	$1, %ebx
	je	.L9
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L9:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
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
	.string	"Member_A::operation2("
.text
	.align 4
.globl operation2__8Member_Ai
	.type	 operation2__8Member_Ai,@function
operation2__8Member_Ai:
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
	.size	 operation2__8Member_Ai,.Lfe4-operation2__8Member_Ai
	.section	.rodata.str1.1,"ams",@progbits,1
.LC5:
	.string	"Member_B()\n"
.text
	.align 4
.globl __8Member_B
	.type	 __8Member_B,@function
__8Member_B:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	movl	8(%ebp), %ebx
	pushl	%ebx
	call	__8Member_A
	movl	$__vt_8Member_B, 8(%ebx)
	addl	$8, %esp
	pushl	$.LC5
	pushl	$cout
	call	__ls__7ostreamPCc
	movl	$1, (%ebx)
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe5:
	.size	 __8Member_B,.Lfe5-__8Member_B
	.section	.rodata.str1.1,"ams",@progbits,1
.LC6:
	.string	"~Member_B()\n"
.text
	.align 4
.globl _._8Member_B
	.type	 _._8Member_B,@function
_._8Member_B:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_8Member_B, 8(%esi)
	subl	$8, %esp
	pushl	$.LC6
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._8Member_A
	addl	$16, %esp
	testl	$1, %ebx
	je	.L19
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L19:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe6:
	.size	 _._8Member_B,.Lfe6-_._8Member_B
	.section	.rodata.str1.1,"ams",@progbits,1
.LC7:
	.string	"Member_B::operation1()\n"
.text
	.align 4
.globl operation1__8Member_B
	.type	 operation1__8Member_B,@function
operation1__8Member_B:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC7
	pushl	$cout
	call	__ls__7ostreamPCc
	movl	$1, %eax
	leave
	ret
.Lfe7:
	.size	 operation1__8Member_B,.Lfe7-operation1__8Member_B
	.section	.rodata.str1.1,"ams",@progbits,1
.LC8:
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
	pushl	$.LC8
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
.Lfe8:
	.size	 operation2__8Member_Bi,.Lfe8-operation2__8Member_Bi
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	subl	$12, %esp
	pushl	$12
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_A
	movl	%ebx, %esi
	movl	8(%esi), %ebx
	movl	%esi, (%esp)
	call	*12(%ebx)
	addl	$8, %esp
	pushl	%eax
	pushl	%esi
	call	*16(%ebx)
	addl	$16, %esp
	testl	%esi, %esi
	je	.L26
	subl	$8, %esp
	movl	8(%esi), %eax
	pushl	$3
	pushl	%esi
	call	*8(%eax)
	addl	$16, %esp
.L26:
	subl	$12, %esp
	pushl	$16
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__8Member_B
	movl	%ebx, %esi
	movl	8(%esi), %ebx
	movl	%esi, (%esp)
	call	*12(%ebx)
	addl	$8, %esp
	pushl	%eax
	pushl	%esi
	call	*16(%ebx)
	addl	$16, %esp
	testl	%esi, %esi
	je	.L28
	subl	$8, %esp
	movl	8(%esi), %eax
	pushl	$3
	pushl	%esi
	call	*8(%eax)
	addl	$16, %esp
.L28:
	movl	$0, %eax
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe9:
	.size	 main,.Lfe9-main
	.weak	__vt_8Member_B
	.section	.gnu.linkonce.d.__vt_8Member_B,"aw",@progbits
	.align 8
	.type	 __vt_8Member_B,@object
	.size	 __vt_8Member_B,24
__vt_8Member_B:
	.long	0
	.long	__pure_virtual
	.long	_._8Member_B
	.long	operation1__8Member_B
	.long	operation2__8Member_Bi
	.zero	4
	.weak	__vt_8Member_A
	.section	.gnu.linkonce.d.__vt_8Member_A,"aw",@progbits
	.align 8
	.type	 __vt_8Member_A,@object
	.size	 __vt_8Member_A,24
__vt_8Member_A:
	.long	0
	.long	__pure_virtual
	.long	_._8Member_A
	.long	operation1__8Member_A
	.long	operation2__8Member_Ai
	.zero	4
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
