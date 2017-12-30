;------------------------------------------------------------------------------
; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; RegisterFile.clp - A simple memory block with 256 entries
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* order.clp)
(batch* SimpleServer.clp)
(defglobal MAIN
           ?*register-file-capacity* = 256)
(definstances MAIN::register-file
              (gpr of memory-block 
                   (capacity ?*register-file-capacity*)))
;----------------------------------------------------------------
; commands are: add, sub, mul, div, rem, shift-left/left-shift, shift-right/right-shift, shutdown, list-commands
;----------------------------------------------------------------


(defrule MAIN::load
         (stage (current dispatch))
         ?f <- (action load ?address callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                read
                                                ?address)))))

(defrule MAIN::store
         (stage (current dispatch))
         ?f <- (action store ?address ?value callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                write
                                                ?address
                                                ?value)))))
(defrule MAIN::swap
         (stage (current dispatch))
         ?f <- (action swap ?a ?b callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                swap
                                                ?a
                                                ?b)))))
(defrule MAIN::move
         (stage (current dispatch))
         ?f <- (action move ?from ?to callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                move
                                                ?from
                                                ?to)))))
(defrule MAIN::increment
         (stage (current dispatch))
         ?f <- (action increment|incr|++ ?address callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                increment
                                                ?address)))))

(defrule MAIN::decrement
         (stage (current dispatch))
         ?f <- (action decrement|decr|-- ?address callback ?callback)
         ?gpr <- (object (is-a memory-block)
                         (name [gpr]))
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (send ?gpr
                                                decrement
                                                ?address)))))
(defrule MAIN::length
         (stage (current dispatch))
         ?f <- (action length callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command ?*register-file-capacity*))))



(defmethod MAIN::get-command-list
  ()
  (create$ load
           store
           swap
           move
           increment incr ++
           decrement decr --
           length))
