;; Counter Program

; Define the hard-reset vector location
.loadloc start

; Define the soft-reset vector location
.loadloc start

.oper 0x2000
:functest
ldi $ret:i16 97
ret

; Define the starting location
.oper 0x4000
:start
ldn $sp:u16
.u16 0x1000

; Setup the counter variables
ldi 6:i16 1
ldi 7:i16 0
ldi 13:i16 0
ldi 15:i16 100

; Setup the target value
jmpri load_data

:target_value
.u16 64 ; 0x7FFF

.align
:load_data
ldri 8:u16 target_value

; Load the function call test
ldri 15:u32 functest_loc
jmpri post_load
:functest_loc
.loadloc functest
:post_load
call 15
copy $ret 12

; Perform the addition and check for reaching the target
:loop
add 7:i32 6 7
int 0
sub 9:i32 8 7

; Jump back to the add instruction if the
; target minus current is greater than zero
tl 10:i32 7 8
tz 10
jmpri endloc
jmpri loop

; Otherwise, enter an infinite loop as program completion
:endloc
jmpri endloc
