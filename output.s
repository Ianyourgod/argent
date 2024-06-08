	.globl main
main:
 	mov $1, %rax
	push %rax
	mov $2, %rax
	pop %rcx
	add %rcx, %rax
	# no moving needed as return value should be in rax
	ret