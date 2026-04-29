;; Infinite Counter Program

; Define the starting location
.loadloc start

; Move to the starting location
.oper 0x2000
:start
ldn $sp:u32
.u32 0x1000

; Load initial values
ldi 6:u16 1
ldi 7:u16 0

:loop ; define the main loop
add 7:u32 7 6
jmpri loop
