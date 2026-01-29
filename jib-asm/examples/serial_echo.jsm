;; Serial Echoer

; Define the hard-reset vector location
.loadloc program_start

; Define the soft-reset vector location
.loadloc program_start

.oper 0x4000
:program_start
ldn $sp:u32
.u32 0x1000
ldn 13:u32
.u32 0xFFFFA000

; Mark the location to read input values from
ldi 14:u16 3
add 14:u32 14 13

; Mark the location to write serial values to
ldi 15:u16 5
add 15:u32 15 13

; Mark the location to check the queue size from
ldi 12:u16 2
add 12:u32 12 13

:main_loop
ld 6:u8 12
tz 6
jmpri main_loop_end

ld 7:u8 14
sav 15:u8 7

jmpri main_loop

:main_loop_end
jmpri main_loop
