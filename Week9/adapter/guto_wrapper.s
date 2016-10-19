	.file	"guto_wrapper.cc"
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
	movw	$0, %ax
	orl	$-65536, %eax
	movl	$operation1__7A_Class, %edx
	addl	$12, %esp
	pushl	%edx
	pushl	%eax
	pushl	%ebx
	call	invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11
	movw	$0, %ax
	orl	$-65536, %eax
	movl	$operation2__7A_Classi, %edx
	pushl	$1
	pushl	%edx
	pushl	%eax
	pushl	%ebx
	call	invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11
	addl	$20, %esp
	pushl	%ebx
	call	__builtin_delete
	movl	$0, %eax
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe3:
	.size	 main,.Lfe3-main
	.section	.gnu.linkonce.t.invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11,"ax",@progbits
	.align 4
	.weak	invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11
	.type	 invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11,@function
invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$12, %esp
	movl	8(%ebp), %edi
	movl	12(%ebp), %ebx
	movl	16(%ebp), %esi
	call	enter__Fv
	subl	$12, %esp
	movl	%ebx, %eax
	movl	%eax, %edx
	sarl	$16, %edx
	js	.L9
	movswl	%si,%eax
	movl	(%eax,%edi), %eax
	movl	-4(%eax,%edx,4), %edx
	jmp	.L10
	.p2align 2
.L9:
	movl	%esi, %edx
.L10:
	movswl	%bx,%eax
	leal	(%eax,%edi), %eax
	pushl	%eax
	call	*%edx
	movl	%eax, %ebx
	call	leave__Fv
	movl	%ebx, %eax
	leal	-12(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe4:
	.size	 invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11,.Lfe4-invoke__H2Z7A_ClassZi_PX01PMX01FPX01_X11_X11
	.section	.gnu.linkonce.t.invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11,"ax",@progbits
	.align 4
	.weak	invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11
	.type	 invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11,@function
invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$12, %esp
	movl	8(%ebp), %edi
	movl	12(%ebp), %ebx
	movl	16(%ebp), %esi
	call	enter__Fv
	subl	$8, %esp
	movl	%ebx, %eax
	movl	%eax, %edx
	sarl	$16, %edx
	js	.L13
	movswl	%si,%eax
	movl	(%eax,%edi), %eax
	movl	-4(%eax,%edx,4), %edx
	jmp	.L14
	.p2align 2
.L13:
	movl	%esi, %edx
.L14:
	pushl	20(%ebp)
	movswl	%bx,%eax
	leal	(%eax,%edi), %eax
	pushl	%eax
	call	*%edx
	movl	%eax, %ebx
	call	leave__Fv
	movl	%ebx, %eax
	leal	-12(%ebp), %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe5:
	.size	 invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11,.Lfe5-invoke__H3Z7A_ClassZiZi_PX01PMX01FPX01X21_X11X21_X11
	.section	.rodata.str1.1,"ams",@progbits,1
.LC2:
	.string	"operation1()\n"
.LC3:
	.string	")\n"
.LC4:
	.string	"operation2("
	.section	.gnu.linkonce.t.operation1__7A_Class,"ax",@progbits
	.align 4
	.weak	operation1__7A_Class
	.type	 operation1__7A_Class,@function
operation1__7A_Class:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	pushl	$.LC2
	pushl	$cout
	call	__ls__7ostreamPCc
	addl	$16, %esp
	leave
	ret
.Lfe6:
	.size	 operation1__7A_Class,.Lfe6-operation1__7A_Class
	.section	.gnu.linkonce.t.operation2__7A_Classi,"ax",@progbits
	.align 4
	.weak	operation2__7A_Classi
	.type	 operation2__7A_Classi,@function
operation2__7A_Classi:
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
.Lfe7:
	.size	 operation2__7A_Classi,.Lfe7-operation2__7A_Classi
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
