(defclass MAIN::io-device
  (is-a USER)
  (slot index
        (type INTEGER)
        (range 0 ?VARIABLE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot length
        (type INTEGER)
        (range 1 ?VARIABLE)
        (storage local)
        (visibility public)
        (default-dynamic 1))
  (message-handler responds-to primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::io-device responds-to primary
                    (?address)
                    (and (<= ?self:index
                             ?address)
                         (< ?address
                            (+ ?self:index
                               (dynamic-get length)))))

(defmessage-handler MAIN::io-device read primary
                    (?address)
                    0)

(defmessage-handler MAIN::io-device write primary
                    (?address ?value))

(defclass MAIN::native-io-device
  (is-a io-device)
  (slot native-reference
        (type EXTERNAL-ADDRESS)
        (storage local)
        (visibility public))
  (slot native-type
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (default PLEASE-REIMPLEMENT-THIS))
  (message-handler get-native-construction-args primary)
  (message-handler init after))

(defmessage-handler MAIN::native-io-device init after
                    ()
                    (bind ?self:native-reference
                          (new (dynamic-get native-type)
                               (expand$ (send ?self
                                              get-native-construction-args)))))

(defmessage-handler MAIN::native-io-device get-native-construction-args primary
                    ()
                    (create$))


(defclass MAIN::stdin/out-device
  (is-a io-device)
  (message-handler read primary)
  (message-handler write primary))

(defmessage-handler MAIN::stdin/out-device read primary
                    (?address)
                    (get-char))

(defmessage-handler MAIN::stdin/out-device write primary
                    (?address ?value)
                    (put-char ?value))
(definstances MAIN::default-non-native-devices
              ([keyboard] of stdin/out-device
                          (index 2)))

(defclass MAIN::random-number-generator
  (is-a native-io-device)
  (slot native-type
        (source composite)
        (default random-number-generator:uint16))
  (slot length
        (source composite)
        (storage shared)
        (default 3))
  (message-handler read primary)
  (message-handler write primary))

(defmessage-handler MAIN::random-number-generator read primary
                    (?address)
                    (bind ?adjusted-address
                          (- ?address
                             ?self:index))
                    (if (= ?adjusted-address
                           1) then
                      (call ?self:native-reference
                            next)
                      else
                      0))
(defmessage-handler MAIN::random-number-generator write primary
                    (?address ?value)
                    (bind ?adjusted-address
                          (- ?address
                             ?self:index))
                    (switch ?adjusted-address
                            (case 0 then
                              (call ?self:native-reference
                                    seed
                                    ?value))
                            (case 2 then
                              (call ?self:native-reference
                                    skip))
                            (default 0)))
(defclass MAIN::memory
          "Concept of memory used to store data in it."
          (is-a native-io-device)
          (slot native-type
                (source composite)
                (default memory-space))
          (slot length
                (source composite)
                (default ?NONE))
          (message-handler get-native-construction-args after)
          (message-handler read primary)
          (message-handler write primary))

(defmessage-handler MAIN::memory get-native-construction-args primary
                    ()
                    (create$ (dynamic-get length)))

(defmessage-handler MAIN::memory read primary
                    (?address)
                    (call ?self:native-reference
                          get
                          ?address))

(defmessage-handler MAIN::memory write primary
                    (?address ?value)
                    (call ?self:native-reference
                          set
                          ?address
                          ?value))

(defclass MAIN::unconnected-memory
  "Memory which will not respond to io requests directly!"
  (is-a memory)
  (slot index
        (source composite)
        (default 0))
  (message-handler responds-to primary))

(defmessage-handler MAIN::unconnected-memory responds-to primary
                    (?address)
                    FALSE)

(definstances MAIN::native-io-devices
              ([rng0] of random-number-generator
                      (index 3)))

(defglobal MAIN
           ?*result* = 0)

(defrule MAIN::perform-read-operation
         ?f <- (read ?address)
         ?q <- (object (is-a io-device))
         (test (send ?q
                     responds-to
                     ?address))
         =>
         (retract ?f)
         (bind ?*result*
               (send ?q
                     read
                     ?address)))

(defrule MAIN::perform-write-operation
         ?f <- (write ?address
                      ?value)
         ?q <- (object (is-a io-device))
         (test (send ?q
                     responds-to
                     ?address))
         =>
         (retract ?f)
         (send ?q
               write
               ?address
               ?value))

(defrule MAIN::no-read-match
         (declare (salience -1))
         ?f <- (read ?address)
         =>
         (retract ?f)
         (printout werror
                   "Can't read from " ?address crlf)
         (funcall illegal-read-operation))

(defrule MAIN::no-write-match
         (declare (salience -1))
         ?f <- (write ?address
                      ?)
         =>
         (retract ?f)
         (printout werror
                   "Can't write to " ?address crlf)
         (funcall illegal-write-operation))

(deffunction MAIN::process-io-event
             ()
             (run)
             ?*result*)

(deffunction read-from-io-address
             (?address)
             (assert (read ?address))
             (process-io-event))

(deffunction write-to-io-address
             (?address ?value)
             (assert (write ?address
                            ?value))
             (process-io-event))


(reset)
