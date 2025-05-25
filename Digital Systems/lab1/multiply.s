@ lab1/multiply.s

        .syntax unified
        .global func

        .text
        .thumb_func
func:
@ ----------------
@ Two parameters are in registers r0 and r1
	movs r2, r0

mult:
	cmp r1, #1 @ if r1 is 0 don't need to multiply by any more
	beq done

	@ else check if r1 is odd or not
	movs r3, #1
	tst r1, r3
	bne isOdd @ if is odd need to add by 1 

	@ if is not odd can just divide multiplier by 2 
	lsls r0, r0, #1 @ multiply x by 2
	lsrs r1, r1, #1 @ divide y by 2 
	b mult 
	
isOdd:
	adds r0, r0, r2  @ add an r3 to r0
	subs r1, r1, #1 @ subtract one from r1
	b mult

@ multiplies value in r0 by 10
@multiply10:
	@movs r0, r1
	@lsls r0, r1, #3 @ multiply by 8
	@adds r0, r0, r1 @ add 2 more times => multiply by 10
	@adds r0, r0, r1

@ Result is now in register r0
@ ----------------
done:
        bx lr                   @ Return to the caller

