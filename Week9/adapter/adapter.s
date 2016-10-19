	.file	"adapter.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"1"
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
	addl	$8, %esp
	movl	%eax, %ebx
	movl	$__vt_7Adapter, (%ebx)
	pushl	$.LC0
	pushl	%ebx
	call	*__vt_7Adapter+8
	movl	%ebx, (%esp)
	call	__builtin_delete
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.weak	__vt_7Adapter
	.section	.gnu.linkonce.d.__vt_7Adapter,"aw",@progbits
	.align 8
	.type	 __vt_7Adapter,@object
	.size	 __vt_7Adapter,16
__vt_7Adapter:
	.long	0
	.long	__pure_virtual
	.long	operation__7AdapterPCc
	.zero	4
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	"\n"
.LC2:
	.string	"n = "
	.section	.gnu.linkonce.t.operation__7AdapterPCc,"ax",@progbits
	.align 4
	.weak	operation__7AdapterPCc
	.type	 operation__7AdapterPCc,@function
operation__7AdapterPCc:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	pushl	$0
	pushl	$10
	pushl	$0
	pushl	12(%ebp)
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
	addl	$16, %esp
	leave
	ret
.Lfe2:
	.size	 operation__7AdapterPCc,.Lfe2-operation__7AdapterPCc
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
