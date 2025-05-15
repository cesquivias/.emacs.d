    .text

    .global str0len
str0len:
    stp x29, x30, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
