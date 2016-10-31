(deffunction alloc-word16u
             (?capacity)
             (new word16u
                  ?capacity))
(deffunction to-word16u
             (?value)
             (add-word16u ?value
                          0))
(deffunction zero-memory
             (?mem)
             (call ?mem
                   clear))
(deffunction load-memory
             (?mem ?address)
             (call ?mem
                   get
                   ?address))
(deffunction store-memory
             (?mem ?address ?value)
             (call ?mem
                   set
                   ?address
                   ?value))

(defclass iris16-cpu
  (is-a USER)
  (slot registers
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot code
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot stack
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot code-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot stack-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler init after))
(defmessage-handler iris16-cpu init after
                    ()
                    (bind ?self:registers
                          (alloc-word16u 256))
                    (bind ?self:data
                          (alloc-word16u (to-word16u ?self:data-capacity)))
                    (bind ?self:stack
                          (alloc-word16u (to-word16u ?self:stack-capacity)))
                    (bind ?self:code
                          (alloc-word16u (to-word16u ?self:code-capacity))))

(deffunction new-iris16-cpu
             (?data-cap ?code-cap ?stack-cap)
             (make-instance of iris16-cpu
                            (data-capacity ?data-cap)
                            (code-capacity ?code-cap)
                            (stack-capacity ?stack-cap)))
