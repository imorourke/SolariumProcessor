;; Threading Example

.loadloc program_start
.loadloc program_start

.oper #0
.loadloc hw_int_0

.oper 0x1000
:global_num_threads
.u32 0
:global_current_thread
.u32 0
:global_thread_pc_stack

.oper 0x1200
:stack_pointer_base

.oper 0x1400
:thread_1_sp_base

.oper 0x1600
:thread_2_sp_base

.oper 0x3000

:init_thread
; args (start_loc, stack_ptr_loc)
    ; Load word size into array
    ldi 7:u16 4
    ldi 10:u16 1

    ; Stack Size
    ldi 11:u16 32
    mul 11:u32 11 7

    ; Load the main start location into register 8
    ld 8:u32 $arg

    ; Increment and load the stack location into register 9
    add $arg:u32 $arg 7
    ld 9:u32 $arg

    ; Copy the base SP into a register with the ending
    ; location by adding stack size
    add 11:u32 9 11

    ; Save the stack location to the global PC stack
    ldn 13:u32
    .loadloc global_num_threads
    ld 14:u32 13
    mul 15:u32 14 7

    ldn 12:u32
    .loadloc global_thread_pc_stack
    add 12:u32 12 15
    sav 12:u32 11

    ; Add one to the global thread count
    add 14:u32 14 10
    sav 13:u32 14

    ; Push PC (R0) to the new stack
    sav 9:u32 8
    add 9:u32 9 7

    ; Push Flags (R1) to the new stack
    sav 9:u32 $stat
    add 9:u32 9 7

    ; Push SP (R2) to the new stack
    sav 9:u32 11
    add 9:u32 9 7

    ; Push SPB (R3) to the new stack
    add 9:u32 9 7

    ; Push return and argument registers (R4, R5)
    ldi 10:u16 0
    sav 9:u32 10
    add 9:u32 9 7
    sav 9:u32 10

    ret

.oper 0x3200
:hw_int_0
    intoff
    ; Write Switch
    ldn 9:u32
    .loadloc msg_switch
    ldn 10:u32
    .loadloc func_print_str

    copy $arg $sp
    push 9
    call 10
    pop

    ; Load constants
    ldn 9:u32 ; Current Thread Loc
    .loadloc global_current_thread
    ld 10:u32 9 ; Current Thread Value
    ldn 11:u32
    .loadloc global_num_threads
    ld 11:u32 11 ; Global Thread Count
    ldn 12:u32 ; Global Thread Stack
    .loadloc global_thread_pc_stack

    ; Save the current stack
    ldi 14:u16 4
    mul 15:u32 10 14 ; Compute the correct location to save the new stack value to
    add 13:u32 12 15
    sav 13:u32 $sp

    ; Add the new current thread value to get the new selected thread
    ldi 7:u16 1
    add 10:u32 10 7
    rem 10:u32 10 11
    sav 9:u32 10

    ; Increment to the next stack
    mul 15:u32 10 14
    add 13:u32 12 15
    ld $sp:u32 13

    ; Resume the next thread
    inton
    retint

.oper 0x3400
; Define the location for the serial device
:dev_serial_loc
    .u32 0xA000

:dev_clock_loc
    .u32 0xA020

:msg_init
.text "Starting Program..."

:msg_switch
.text "Switching Task"

:msg_task_a
.text "TSK_A"

:msg_task_b
.text "TSK_B"

.oper 0x3500
; print_str(start_addr)
:func_print_str
    ldn 13:u32
    .loadloc dev_serial_loc
    ld 13:u32 13

    ; Mark the location to write serial values to
    ldi 15:u16 5
    add 15:u32 15 13

    ; Load the argument value
    ld 6:u32 $arg

    ; Load the bytes-per-address value
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

.oper 0x4000
:program_start
    ldn $sp:u32
    .loadloc stack_pointer_base

    ; Start to init thread 1
    ldn 10:u32
    .loadloc thread_1_main
    ldn 11:u32
    .loadloc thread_1_sp_base
    ldn 12:u32
    .loadloc init_thread

    copy $arg $sp
    push 10
    push 11
    call 12
    pop
    pop

    ; Init thread 2
    ldn 10:u32
    .loadloc thread_2_main
    ldn 11:u32
    .loadloc thread_2_sp_base

    push 10
    push 11
    call 12
    pop
    pop

    ldn 10:u32
    .loadloc msg_init
    ldn 11:u32
    .loadloc func_print_str
    push 10
    call 11
    pop

    ; Setup the IRQ Clock
    ldn 10:u32
    .loadloc dev_clock_loc
    ld 10:u32 10

    ldi 11:u16 1000 ; Set the clock interval to 1000 cycles
    sav 10:u32 11

    ldi 12:u16 4 ; Set the interrupt to interrupt 0
    add 10:u32 10 12
    add 10:u32 10 12
    ldi 11:u16 0
    sav 10:u32 11

    ; Set the stack pointer and exit the main loop
    ldn $sp:u32
    .loadloc global_thread_pc_stack
    ld $sp:u32 $sp
    retint

.oper 0x6000
:thread_1_main
    inton
    :thread_1_loop
        ldn 10:u32
        .u32 50
        ldi 11:u16 1
        ldn 12:u32
        .loadloc func_print_str
        ldn 13:u32
        .loadloc msg_task_a

        :thread_1_loop_loop
            sub 10:u32 10 11
            tz 10
            jmpri thread_1_loop_loop_end
            jmpri thread_1_loop_loop
        :thread_1_loop_loop_end

        intoff
        copy $arg $sp
        push 13
        call 12
        pop
        inton

        jmpri thread_1_loop

.oper 0x8000
:thread_2_main
    inton
    :thread_2_loop
        ldn 10:u32
        .u32 50
        ldi 11:u16 1
        ldn 12:u32
        .loadloc func_print_str
        ldn 13:u32
        .loadloc msg_task_b

        :thread_2_loop_loop
            sub 10:u32 10 11
            tz 10
            jmpri thread_2_loop_loop_end
            jmpri thread_2_loop_loop
        :thread_2_loop_loop_end

        intoff
        copy $arg $sp
        push 13
        call 12
        pop
        inton

        jmpri thread_2_loop
