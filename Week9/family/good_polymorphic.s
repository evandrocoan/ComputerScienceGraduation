	.file	"good_polymorphic.cc"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"Network()\n"
.text
	.align 4
.globl __7Network
	.type	 __7Network,@function
__7Network:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	movl	8(%ebp), %eax
	movl	$__vt_7Network, (%eax)
	pushl	$.LC0
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe1:
	.size	 __7Network,.Lfe1-__7Network
	.section	.rodata.str1.1,"ams",@progbits,1
.LC1:
	.string	"~Network()\n"
.text
	.align 4
.globl _._7Network
	.type	 _._7Network,@function
_._7Network:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_7Network, (%esi)
	subl	$8, %esp
	pushl	$.LC1
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	testl	$1, %ebx
	je	.L7
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L7:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe2:
	.size	 _._7Network,.Lfe2-_._7Network
	.section	.rodata.str1.1,"ams",@progbits,1
.LC2:
	.string	"Ethernet()\n"
.text
	.align 4
.globl __8Ethernet
	.type	 __8Ethernet,@function
__8Ethernet:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	movl	8(%ebp), %ebx
	pushl	%ebx
	call	__7Network
	movl	$__vt_8Ethernet, (%ebx)
	addl	$8, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe3:
	.size	 __8Ethernet,.Lfe3-__8Ethernet
	.section	.rodata.str1.1,"ams",@progbits,1
.LC3:
	.string	"~Ethernet()\n"
.text
	.align 4
.globl _._8Ethernet
	.type	 _._8Ethernet,@function
_._8Ethernet:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_8Ethernet, (%esi)
	subl	$8, %esp
	pushl	$.LC3
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._7Network
	addl	$16, %esp
	testl	$1, %ebx
	je	.L13
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L13:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe4:
	.size	 _._8Ethernet,.Lfe4-_._8Ethernet
	.section	.rodata.str1.1,"ams",@progbits,1
.LC4:
	.string	"Ethernet::send()\n"
.text
	.align 4
.globl send__8EthernetiPCvUi
	.type	 send__8EthernetiPCvUi,@function
send__8EthernetiPCvUi:
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
	.size	 send__8EthernetiPCvUi,.Lfe5-send__8EthernetiPCvUi
	.section	.rodata.str1.1,"ams",@progbits,1
.LC5:
	.string	"Ethernet::receive()\n"
.text
	.align 4
.globl receive__8EthernetPiPCvPUi
	.type	 receive__8EthernetPiPCvPUi,@function
receive__8EthernetPiPCvPUi:
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
	.size	 receive__8EthernetPiPCvPUi,.Lfe6-receive__8EthernetPiPCvPUi
	.section	.rodata.str1.1,"ams",@progbits,1
.LC6:
	.string	"Myrinet()\n"
.text
	.align 4
.globl __7Myrinet
	.type	 __7Myrinet,@function
__7Myrinet:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$16, %esp
	movl	8(%ebp), %ebx
	pushl	%ebx
	call	__7Network
	movl	$__vt_7Myrinet, (%ebx)
	addl	$8, %esp
	pushl	$.LC6
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe7:
	.size	 __7Myrinet,.Lfe7-__7Myrinet
	.section	.rodata.str1.1,"ams",@progbits,1
.LC7:
	.string	"~Myrinet()\n"
.text
	.align 4
.globl _._7Myrinet
	.type	 _._7Myrinet,@function
_._7Myrinet:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %ebx
	movl	$__vt_7Myrinet, (%esi)
	subl	$8, %esp
	pushl	$.LC7
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$8, %esp
	pushl	$0
	pushl	%esi
	call	_._7Network
	addl	$16, %esp
	testl	$1, %ebx
	je	.L23
	subl	$12, %esp
	pushl	%esi
	call	__builtin_delete
	addl	$16, %esp
.L23:
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe8:
	.size	 _._7Myrinet,.Lfe8-_._7Myrinet
	.section	.rodata.str1.1,"ams",@progbits,1
.LC8:
	.string	"Myrinet::send()\n"
.text
	.align 4
.globl send__7MyrinetiPCvUi
	.type	 send__7MyrinetiPCvUi,@function
send__7MyrinetiPCvUi:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC8
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe9:
	.size	 send__7MyrinetiPCvUi,.Lfe9-send__7MyrinetiPCvUi
	.section	.rodata.str1.1,"ams",@progbits,1
.LC9:
	.string	"Myrinet::receive()\n"
.text
	.align 4
.globl receive__7MyrinetPiPCvPUi
	.type	 receive__7MyrinetPiPCvPUi,@function
receive__7MyrinetPiPCvPUi:
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
	.size	 receive__7MyrinetPiPCvPUi,.Lfe10-receive__7MyrinetPiPCvPUi
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	pushl	%ebx
	subl	$44, %esp
	leal	-24(%ebp), %esi
	pushl	%esi
	call	__8Ethernet
	leal	-40(%ebp), %ebx
	movl	%ebx, (%esp)
	call	__7Myrinet
	addl	$16, %esp
	movl	-24(%ebp), %eax
	pushl	$0
	pushl	$0
	pushl	$1
	pushl	%esi
	call	*12(%eax)
	movl	-40(%ebp), %eax
	pushl	$0
	pushl	$0
	pushl	$1
	pushl	%ebx
	call	*12(%eax)
	addl	$24, %esp
	pushl	$2
	pushl	%ebx
	call	_._7Myrinet
	addl	$8, %esp
	pushl	$2
	pushl	%esi
	call	_._8Ethernet
	addl	$16, %esp
	movl	$0, %eax
	leal	-8(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%ebp
	ret
.Lfe11:
	.size	 main,.Lfe11-main
	.weak	__vt_7Myrinet
	.section	.gnu.linkonce.d.__vt_7Myrinet,"aw",@progbits
	.align 8
	.type	 __vt_7Myrinet,@object
	.size	 __vt_7Myrinet,24
__vt_7Myrinet:
	.long	0
	.long	__pure_virtual
	.long	_._7Myrinet
	.long	send__7MyrinetiPCvUi
	.long	receive__7MyrinetPiPCvPUi
	.zero	4
	.weak	__vt_8Ethernet
	.section	.gnu.linkonce.d.__vt_8Ethernet,"aw",@progbits
	.align 8
	.type	 __vt_8Ethernet,@object
	.size	 __vt_8Ethernet,24
__vt_8Ethernet:
	.long	0
	.long	__pure_virtual
	.long	_._8Ethernet
	.long	send__8EthernetiPCvUi
	.long	receive__8EthernetPiPCvPUi
	.zero	4
	.weak	__vt_7Network
	.section	.gnu.linkonce.d.__vt_7Network,"aw",@progbits
	.align 8
	.type	 __vt_7Network,@object
	.size	 __vt_7Network,24
__vt_7Network:
	.long	0
	.long	__pure_virtual
	.long	_._7Network
	.long	__pure_virtual
	.long	__pure_virtual
	.zero	4
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
