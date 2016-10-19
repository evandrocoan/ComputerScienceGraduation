	.file	"bad_polymorphic.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"Synchronizer()\n"
.text
	.align 4
.globl __12Synchronizer
	.type	 __12Synchronizer,@function
__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	movl	8(%ebp), %eax
	movl	$__vt_12Synchronizer, 8(%eax)
	pushl	$.LC0
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe1:
	.size	 __12Synchronizer,.Lfe1-__12Synchronizer
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	"~Synchronizer()\n"
.text
	.align 4
.globl _._12Synchronizer
	.type	 _._12Synchronizer,@function
_._12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_12Synchronizer, 8(%esi)
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
	.size	 _._12Synchronizer,.Lfe2-_._12Synchronizer
	.section	.rodata.str1.1,"ams",@progbits,1
.LC2:
	.string	"Error: Synchronizer::p()\n"
.text
	.align 4
.globl p__12Synchronizer
	.type	 p__12Synchronizer,@function
p__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe3:
	.size	 p__12Synchronizer,.Lfe3-p__12Synchronizer
	.section	.rodata.str1.1,"ams",@progbits,1
.LC3:
	.string	"Error: Synchronizer::v()\n"
.text
	.align 4
.globl v__12Synchronizer
	.type	 v__12Synchronizer,@function
v__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC3
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe4:
	.size	 v__12Synchronizer,.Lfe4-v__12Synchronizer
	.section	.rodata.str1.1,"ams",@progbits,1
.LC4:
	.string	"Error: Synchronizer::wait()\n"
.text
	.align 4
.globl wait__12Synchronizer
	.type	 wait__12Synchronizer,@function
wait__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC4
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe5:
	.size	 wait__12Synchronizer,.Lfe5-wait__12Synchronizer
	.section	.rodata.str1.32,"ams",@progbits,1
	.align 32
.LC5:
	.string	"Error: Synchronizer::signal()\n"
.text
	.align 4
.globl signal__12Synchronizer
	.type	 signal__12Synchronizer,@function
signal__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC5
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe6:
	.size	 signal__12Synchronizer,.Lfe6-signal__12Synchronizer
	.section	.rodata.str1.32,"ams",@progbits,1
	.align 32
.LC6:
	.string	"Error: Synchronizer::broadcast()\n"
.text
	.align 4
.globl broadcast__12Synchronizer
	.type	 broadcast__12Synchronizer,@function
broadcast__12Synchronizer:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC6
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe7:
	.size	 broadcast__12Synchronizer,.Lfe7-broadcast__12Synchronizer
	.section	.rodata.str1.1,"ams",@progbits,1
.LC7:
	.string	"Semaphore()\n"
.text
	.align 4
.globl __9Semaphorei
	.type	 __9Semaphorei,@function
__9Semaphorei:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	movl	8(%ebp), %ebx
	pushl	%ebx
	call	__12Synchronizer
	movl	$__vt_9Semaphore, 8(%ebx)
	addl	$8, %esp
	pushl	$.LC7
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe8:
	.size	 __9Semaphorei,.Lfe8-__9Semaphorei
	.section	.rodata.str1.1,"ams",@progbits,1
.LC8:
	.string	"~Semaphore()\n"
.text
	.align 4
.globl _._9Semaphore
	.type	 _._9Semaphore,@function
_._9Semaphore:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_9Semaphore, 8(%esi)
	subl	$8, %esp
	pushl	$.LC8
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._12Synchronizer
	addl	$16, %esp
	testl	$1, %ebx
	je	.L25
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L25:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe9:
	.size	 _._9Semaphore,.Lfe9-_._9Semaphore
	.section	.rodata.str1.1,"ams",@progbits,1
.LC9:
	.string	"Semaphore::p()\n"
.text
	.align 4
.globl p__9Semaphore
	.type	 p__9Semaphore,@function
p__9Semaphore:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC9
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe10:
	.size	 p__9Semaphore,.Lfe10-p__9Semaphore
	.section	.rodata.str1.1,"ams",@progbits,1
.LC10:
	.string	"Semaphore::v()\n"
.text
	.align 4
.globl v__9Semaphore
	.type	 v__9Semaphore,@function
v__9Semaphore:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC10
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe11:
	.size	 v__9Semaphore,.Lfe11-v__9Semaphore
	.section	.rodata.str1.1,"ams",@progbits,1
.LC11:
	.string	"Condition()\n"
.text
	.align 4
.globl __9Condition
	.type	 __9Condition,@function
__9Condition:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	movl	8(%ebp), %ebx
	pushl	%ebx
	call	__12Synchronizer
	movl	$__vt_9Condition, 8(%ebx)
	addl	$8, %esp
	pushl	$.LC11
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe12:
	.size	 __9Condition,.Lfe12-__9Condition
	.section	.rodata.str1.1,"ams",@progbits,1
.LC12:
	.string	"~Condition()\n"
.text
	.align 4
.globl _._9Condition
	.type	 _._9Condition,@function
_._9Condition:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_9Condition, 8(%esi)
	subl	$8, %esp
	pushl	$.LC12
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._12Synchronizer
	addl	$16, %esp
	testl	$1, %ebx
	je	.L35
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L35:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe13:
	.size	 _._9Condition,.Lfe13-_._9Condition
	.section	.rodata.str1.1,"ams",@progbits,1
.LC13:
	.string	"Condition::wait()\n"
.text
	.align 4
.globl wait__9Condition
	.type	 wait__9Condition,@function
wait__9Condition:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC13
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe14:
	.size	 wait__9Condition,.Lfe14-wait__9Condition
	.section	.rodata.str1.1,"ams",@progbits,1
.LC14:
	.string	"Condition::signal()\n"
.text
	.align 4
.globl signal__9Condition
	.type	 signal__9Condition,@function
signal__9Condition:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC14
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe15:
	.size	 signal__9Condition,.Lfe15-signal__9Condition
	.section	.rodata.str1.1,"ams",@progbits,1
.LC15:
	.string	"Condition::broadcast()\n"
.text
	.align 4
.globl broadcast__9Condition
	.type	 broadcast__9Condition,@function
broadcast__9Condition:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC15
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe16:
	.size	 broadcast__9Condition,.Lfe16-broadcast__9Condition
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	pushl	$16
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__9Condition
	movl	8(%ebx), %eax
	movl	%ebx, (%esp)
	call	*20(%eax)
	movl	8(%ebx), %eax
	movl	%ebx, (%esp)
	call	*12(%eax)
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L44
	subl	$8, %esp
	movl	8(%ebx), %eax
	pushl	$3
	pushl	%ebx
	call	*8(%eax)
	addl	$16, %esp
.L44:
	subl	$8, %esp
	pushl	$1
	pushl	$16
	call	__builtin_new
	movl	%eax, %ebx
	movl	%ebx, (%esp)
	call	__9Semaphorei
	movl	8(%ebx), %eax
	movl	%ebx, (%esp)
	call	*12(%eax)
	movl	8(%ebx), %eax
	movl	%ebx, (%esp)
	call	*20(%eax)
	addl	$16, %esp
	testl	%ebx, %ebx
	je	.L46
	subl	$8, %esp
	movl	8(%ebx), %eax
	pushl	$3
	pushl	%ebx
	call	*8(%eax)
	addl	$16, %esp
.L46:
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe17:
	.size	 main,.Lfe17-main
	.weak	__vt_9Condition
	.section	.gnu.linkonce.d.__vt_9Condition,"aw",@progbits
	.align 32
	.type	 __vt_9Condition,@object
	.size	 __vt_9Condition,36
__vt_9Condition:
	.long	0
	.long	__pure_virtual
	.long	_._9Condition
	.long	p__12Synchronizer
	.long	v__12Synchronizer
	.long	wait__9Condition
	.long	signal__9Condition
	.long	broadcast__9Condition
	.zero	4
	.weak	__vt_9Semaphore
	.section	.gnu.linkonce.d.__vt_9Semaphore,"aw",@progbits
	.align 32
	.type	 __vt_9Semaphore,@object
	.size	 __vt_9Semaphore,36
__vt_9Semaphore:
	.long	0
	.long	__pure_virtual
	.long	_._9Semaphore
	.long	p__9Semaphore
	.long	v__9Semaphore
	.long	wait__12Synchronizer
	.long	signal__12Synchronizer
	.long	broadcast__12Synchronizer
	.zero	4
	.weak	__vt_12Synchronizer
	.section	.gnu.linkonce.d.__vt_12Synchronizer,"aw",@progbits
	.align 32
	.type	 __vt_12Synchronizer,@object
	.size	 __vt_12Synchronizer,36
__vt_12Synchronizer:
	.long	0
	.long	__pure_virtual
	.long	_._12Synchronizer
	.long	p__12Synchronizer
	.long	v__12Synchronizer
	.long	wait__12Synchronizer
	.long	signal__12Synchronizer
	.long	broadcast__12Synchronizer
	.zero	4
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
