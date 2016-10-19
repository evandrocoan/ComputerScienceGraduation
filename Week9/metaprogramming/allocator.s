	.file	"allocator.cc"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$92, %esp
	movl	$0, %ebx
	leal	-104(%ebp), %esi
	movl	$buffer_allocator, %edi
	.p2align 2
.L15:
	leal	0(,%ebx,4), %ecx
	movl	$0, %eax
	cmpb	$0, buffer_allocator
	je	.L7
	movl	$buffer_allocator, %edx
	.p2align 2
.L8:
	incl	%eax
	cmpl	$15, %eax
	jg	.L7
	cmpb	$0, (%eax,%edx)
	jne	.L8
.L7:
	cmpl	$16, %eax
	je	.L12
	movb	$1, (%eax,%edi)
	sall	$8, %eax
	addl	$buffer_allocator+16, %eax
	jmp	.L14
	.p2align 2
.L12:
	movl	$0, %eax
.L14:
	movl	%eax, (%ecx,%esi)
	leal	0(,%ebx,4), %eax
	incl	%ebx
	cmpl	$0, (%eax,%esi)
	jne	.L15
	testl	%ebx, %ebx
	jle	.L29
	leal	-104(%ebp), %esi
	movl	$buffer_allocator, %edi
	.p2align 2
.L18:
	decl	%ebx
	movl	(%esi,%ebx,4), %ecx
	movl	$0, %edx
	movl	$buffer_allocator+16, %eax
	cmpl	%ecx, %eax
	je	.L20
	.p2align 2
.L21:
	incl	%edx
	cmpl	$15, %edx
	jg	.L20
	movl	%edx, %eax
	sall	$8, %eax
	addl	$buffer_allocator+16, %eax
	cmpl	%ecx, %eax
	jne	.L21
.L20:
	cmpl	$16, %edx
	je	.L16
	movb	$0, (%edi,%edx)
.L16:
	testl	%ebx, %ebx
	jg	.L18
.L29:
	movl	$0, %eax
	addl	$92, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe1:
	.size	 main,.Lfe1-main
.globl buffer_allocator
.bss
	.align 32
	.type	 buffer_allocator,@object
	.size	 buffer_allocator,4112
buffer_allocator:
	.zero	4112
.text
	.align 4
	.type	 __static_initialization_and_destruction_0,@function
__static_initialization_and_destruction_0:
	pushl	%ebp
	movl	%esp, %ebp
	cmpl	$65535, 12(%ebp)
	jne	.L39
	cmpl	$1, 8(%ebp)
	jne	.L39
	movl	$0, %eax
	movl	$buffer_allocator, %edx
	.p2align 2
.L35:
	movb	$0, (%edx,%eax)
	incl	%eax
	cmpl	$15, %eax
	jle	.L35
.L39:
	popl	%ebp
	ret
.Lfe2:
	.size	 __static_initialization_and_destruction_0,.Lfe2-__static_initialization_and_destruction_0
	.align 4
	.type	 _GLOBAL_.I.main,@function
_GLOBAL_.I.main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$65535
	pushl	$1
	call	__static_initialization_and_destruction_0
	addl	$16, %esp
	leave
	ret
.Lfe3:
	.size	 _GLOBAL_.I.main,.Lfe3-_GLOBAL_.I.main
		.section	.ctors,"aw"
	.long	 _GLOBAL_.I.main
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
