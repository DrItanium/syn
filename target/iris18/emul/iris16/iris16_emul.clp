(defglobal MAIN
           ?*true* = 0x1
           ?*false* = 0x0
           ?*execute* = 0x0
           ?*advanceIp* = 0x1
           ?*pc* = 0xFF
           ?*sp* = 0xFE
           ?*lr* = 0xFD
           )
(deffunction set-address
             (?value)
             (assign32 addr
                       ?value))
(deffunction jump-table
             (?title $?locations)
             (scope ?title
                    (map dword
                         (expand$ ?locations))))

(output t
        (deflabel Iris16Startup)
        (deflabel Iris16Loop)
        (todo "implement the loop body")
        (deflabel Iris16Shutdown)
        (terminate)
        (scope BadGroup
               (todo "Print out an error message")
               (branch-call immediate
                            Iris16Shutdown))

        (defunc PushOntoStack
                (use-register value
                              (branch-call immediate
                                           IncrementStackPointer))
                (branch-call immediate
                             StoreStackTop))
        (defunc PopFromStack
                (branch-call immediate
                             LoadStackTop)
                (use-register value
                              (branch-call immediate
                                           DecrementStackPointer)))
        (defunc LoadBody
                (scope LoadStackTop
                       (branch-call immediate
                                    LoadStackPointer)
                       (set-address Iris16Stack)
                       (branch immediate
                               Load16bitValue))
                (scope LoadDataAddress
                       (set-address Iris16Data)
                       (branch immediate
                               Load16bitValue))
                (scope LoadStackPointer
                       (assign8 value
                                ?*sp*)
                       (branch immediate
                               LoadRegister))
                (scope LoadLinkRegister
                       (assign8 value
                                ?*lr*)
                       (branch immediate
                               LoadRegister))
                (scope LoadInstructionPointer
                       (assign8 value
                                ?*pc*))
                (scope LoadRegister
                       (set-address Iris16Registers))
                (scope Load16bitValue
                       (add FALSE
                            addr
                            value)
                       (load16 0x0)))
        (defunc StoreBody
                (scope StoreStackTop
                       (use-register value
                                     (branch-call immediate
                                                  LoadStackPointer)
                                     (copy r8
                                           value))
                       (set-address Iris16Stack)
                       (branch immediate
                               Store16bitValue))
                (scope StoreDataAddress
                       (assign32 addr
                                 Iris16Data)
                       (branch immediate
                               Store16bitValue))
                (scope StoreStackPointer
                       (assign8 r8
                                ?*sp*)
                       (branch immediate
                               StoreRegister))
                (scope StoreLinkRegister
                       (assign8 r8
                                ?*lr*)
                       (branch immediate
                               StoreRegister))
                (scope StoreInstructionPointer
                       (assign8 r8
                                ?*pc*))
                (scope StoreRegister
                       (set-address Iris16Registers))
                (scope Store16bitValue
                       (add FALSE
                            addr
                            r8)
                       (store16 0x0)))
        (defunc InstructionLoads
                (scope LoadInstruction
                       (branch-call immediate
                                    LoadInstructionPointer)
                       (shift left
                              immediate
                              value
                              0x1))
                (scope LoadCodeAddress
                       (set-address Iris16Code))
                (scope Load32bitValue
                       (add FALSE
                            addr
                            value)
                       (load32 0x0)))
        (defunc InstructionStores
                (scope StoreInstruction
                       (comment "shift left by one to take the 32bit instruction nature")
                       (comment "into account and then exploit the fallthrough")
                       (shift left
                              immediate
                              r8
                              0x1))
                (scope StoreCodeAddress
                       (describe-arg r8
                                     "the offset of the address")
                       (describe-arg value
                                     "what to store")
                       (set-address Iris16Code))
                (scope Store32bitValue
                       (add FALSE
                            addr
                            r8)
                       (store32 0x0)))
        (defunc TryAdvanceIp
                (set-address Iris16_Fields)
                (load16 ?*advanceIp*)
                (compare-op !=
                            none
                            immediate
                            value
                            ?*false*)
                (branch-cond immediate
                             TryAdvanceIpDone)
                (comment "else")
                (branch-call immediate
                             IncrementInstructionPointer)
                (assign16 value
                          ?*true*)
                (store16 ?*advanceIp*)
                (deflabel TryAdvanceIpDone)))
(deffunction register-manipulation-code
             (?title)
             (bind ?loader
                   (sym-cat Load
                            ?title))
             (bind ?load-register
                   (branch-call immediate
                                (sym-cat ?title)))

             (bind ?commit
                   (sym-cat Commit
                            ?title))
             (defunc (sym-cat ?title
                              Manipulation)
                     (comment "increment the given register by one as seen by iris16")
                     (comment "the shift to convert it to iris18 is not needed")
                     (scope (sym-cat Increment
                                     ?title)
                            ?load-register
                            (increment value)
                            (branch immediate
                                    ?commit))
                     (scope (sym-cat Decrement
                                     ?title)
                            ?load-register
                            (decrement value))
                     (scope ?commit
                            (branch-call immediate
                                         (sym-cat Store
                                                  ?title)))))
(output t
        (register-manipulation-code InstructionPointer)
        (register-manipulation-code StackPointer)
        (register-manipulation-code LinkRegister)
        (defunc DecodeOperations
                (scope DecodeDestination
                       (assign32 mask
                                 0x0000FF00)
                       (assign8 shift_width
                                8)
                       (bind ?branch-to-base-decode
                             (branch immediate
                                     (bind ?base-decode
                                           BaseDecode))))
                (scope DecodeSource0
                       (assign32 mask
                                 0x00FF0000)
                       (assign8 shift_width
                                16)
                       ?branch-to-base-decode)
                (scope DecodeHalfImmediate
                       (scope DecodeSource1
                              (assign32 mask
                                        0xFF000000)
                              (assign8 shift_width
                                       24)
                              ?branch-to-base-decode))
                (scope DecodeImmediate
                       (assign32 mask
                                 0xFFFF0000)
                       (assign8 shift_width
                                16)
                       ?branch-to-base-decode)
                (scope DecodeOperation
                       (assign32 mask
                                 0x000000F8)
                       (assign8 shift_width
                                3)
                       ?branch-to-base-decode)
                (scope DecodeGroup
                       (assign32 mask
                                 0x00000007)
                       (zero shift_width))
                (scope ?base-decode
                       "complex encoding decode"))
        (todo "port over the rest of the tables and function dispatches"))
(deffunction imm-and-non-imm
             (?value)
             (create$ ?value
                      (sym-cat ?value
                               Imm)))
(deffunction and-link-form
             (?value)
             (create$ ?value
                      (sym-cat ?value
                               Link)))
(deffunction boolean-forms
             (?value)
             (create$ (sym-cat ?value
                               True)
                      (sym-cat ?value
                               False)))

(deffunction imm-reg-form
             (?value)
             (create$ (and-link-form (sym-cat ?value
                                              Immediate))
                      (and-link-form (sym-cat ?value
                                              Register))))
(deffunction todo-implement
             ()
             (todo "Implement this function"))

(output t
        (defunc Move
                (todo-implement))
        (defunc Swap
                (todo-implement))
        (defunc Load
                (todo-implement))
        (defunc LoadImmediate
                (todo-implement))
        (defunc Store
                (todo-implement))
        (defunc Memset
                (todo-implement))
        (defunc Push
                (todo-implement))
        (defunc PushImmediate
                (todo-implement))
        (defunc Pop
                (todo-implement))
        (defunc LoadCode
                (todo-implement))
        (defunc StoreCode
                (todo-implement)))

(output t
        (jump-table GroupBase
                    ArithmeticOp
                    MoveOp
                    JumpOp
                    CompareOp
                    MiscOp
                    BadGroup
                    BadGroup
                    BadGroup)
        (jump-table BaseTableAddresses
                    ArithmeticBase
                    MoveBase
                    JumpBase
                    CompareBase
                    MiscBase
                    BadGroup
                    BadGroup
                    BadGroup)
        (jump-table ArithmeticBase
                    Add
                    Sub
                    Mul
                    Div
                    Rem
                    ShiftLeft
                    ShiftRight
                    BinaryAnd
                    BinaryOr
                    BinaryNot
                    BinaryXor
                    AddImmediate
                    SubImmediate
                    DivImmediate
                    RemImmediate
                    ShiftLeftImmediate
                    ShiftRightImmediate)
        (jump-table MoveBase
                    Move
                    Set
                    Swap
                    Load
                    LoadImmediate
                    Store
                    Memset
                    Push
                    PushImmediate
                    Pop
                    LoadCode
                    StoreCode)
        (jump-table JumpBase
                    ; Compressed entries
                    (imm-reg-form Unconditional)
                    (imm-reg-form ConditionalTrue)
                    (imm-reg-form ConditionalFalse)
                    (boolean-forms IfThenElseNormalPred)
                    (boolean-forms IfThenElseLinkPred))
        (jump-table CompareBase
                    (imm-and-non-imm Eq)
                    (imm-and-non-imm Neq)
                    (imm-and-non-imm LessThan)
                    (imm-and-non-imm GreaterThan)
                    (imm-and-non-imm LessThanOrEqualTo)
                    (imm-and-non-imm GreaterThanOrEqualTo))
        (jump-table MiscBase
                    SystemCall))
(output t
        (at-memory-location 0x00FB0000
                            (deflabel Iris16Code))
        (at-memory-location 0x00FD0000
                            (deflabel Iris16Data))
        (at-memory-location 0x00FE0000
                            (deflabel Iris16Stack))
        (at-memory-location 0x00FF0000
                            (deflabel Iris16_Fields)
                            (scope Iris16_Execute
                                   (word 0x0001))
                            (scope Iris16_AdvanceIp
                                   (word 0x0001)))
        (at-memory-location 0x00FFFF00
                            (deflabel Iris16Registers))
        )

