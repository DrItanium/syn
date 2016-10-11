; Since I've never ever done something like this before, I used the picolisp
; reference as a base in defining the cell layout. Unlike picoLisp, this
; machine is really simple but is a good place to start for the time being.
; There is no floating point support nor symbols at this time.
; The machine is defined as having a 24-bit flat memory space internally but
; only 8 megawords are used as user memory with the other half as system and
; internal book keeping. We will call these halves 'zones'. A memory address
; denotes which zone it refers to by looking at bit 23 of an address. 
; zone 0 is system and zone 1 is memory
; An address refering to zone 1 refers to a 21 bit "cell" address because
; each cell is comprised of 4 words or 64-bits in size. Thus the lowest two
; bits are used for basic tagging information. If bit zero is set to one then
; it has been marked for garbage collection. If bit 1 is set to 1 then the car
; is a 30-bit integer. If bit 1 is not set then the cell refers to a list and
; then upper most 8 bits of the half cell refers to extra tagging information. 
; 
; Since pico lisp is so simple with respect to garbage collection, it makes it
; a perfect target for this machine. Thus we have to setup the 21 bit "free list"
; with the first step being to generate routines we need to manipulate lists
; There are also two stacks, a call stack and another being a value stack
;
; 
; addr - free list
; value, r8, r7, r6 - args0-3 respectively
; value, r4, r5 - return value / temporaries
; r9 - unused 
; r10 - parameter stack
; r0, r1, r2, r3 - extra temporaries
; Divide memory into 2 separate "zones",
; 0 - System, Free List, Stacks
; 1 - User Memory
; The zone bit is defined as bit 23 in a memory address
(defglobal MAIN
           ?*arg0* = value
           ?*arg1* = r8
           ?*arg2* = r7
           ?*arg3* = r6
           ?*ret0* = value
           ?*ret1* = r4
           ?*ret2* = r5
           ?*t0* = r4
           ?*t1* = r5
           ?*t2* = r0
           ?*t3* = r1
           ?*t4* = r2
           ?*t5* = r3
           ?*free* = addr
           ?*param-stack* = r10
           ?*cell-size* = 0x4)
(code (memory-location 0x00000000)
      (comment (assign32 sp 
                         stackBottom)
               "setup the stack pointer")
      (comment (assign32 r0
                         0x00800000)
               "set the start of memory")
      (comment (assign32 r1
                         0x00FFFFFC)
               "set the end of memory")
      (comment (assign32 ?*free*
                         NIL)
               "initialize the free list by pointing to NIL, we're going to be using this like a stack")
      (comment (assign32 ?*param-stack*
                         paramBottom)
               "Setup the parameter stack to be used internally")
      (comment "Setup the machine by first constructing the free list")
      (scope memorySetupLoop
             (comment (compare > None r0 r1)
                      "have we gone past the last cell?")
             (branch-cond immediate
                          memortyHasBeenSetup)
             (comment (push 0m1111
                            r0
                            addr)
                      "Add the current memory location to the free list")
             (comment (add immediate
                           r0
                           ?*cell-size*)
                      "next address cell")
             (branch immediate
                     memorySetupLoop))




            
                
                
