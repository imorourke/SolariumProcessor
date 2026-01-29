;; Hello World Program

; Define the hard-reset vector location
.loadloc start

; Define the soft-reset vector location
.loadloc start

; Define the starting location
.oper 0x1000
:start
ldn $sp:u16
.u16 0x2000
.align

; Load the string location into memory
ldn 14:u32
.loadloc str_hello_world

; Load the function location into memory
ldn 15:u32
.loadloc func_print_str

:loop
copy $arg 14
call 15
jmpri loop

.oper 0x1100
:str_hello_world
.text "hello, world!"

.oper 0x1200
:func_print_str
    ldn 13:u32
    .u32 0xFFFFA000
    .align

    ; Mark the location to write serial values to
    ldi 15:u16 5
    add 15:u32 15 13

    ; Load the argument value
    copy 6 $arg
    ldi 8:u16 1

    :func_print_str_loop
        ld 7:u8 6
        tz 7
            jmpri func_print_str_end
            sav 15:u8 7
        add 6:u32 8 6
        jmpri func_print_str_loop

    :func_print_str_end
    ldi 7:u16 10
    sav 15:u8 7
    ret
