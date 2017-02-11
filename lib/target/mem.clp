(deffunction assign-memory-to-single-value
             (?memory ?value)
             (call ?memory
                   populate
                   ?value))

(deffunction randomize-memory 
             (?memory) 
             (bind ?size
                   (- (call ?memory
                            size)
                      1))
             (loop-for-count (?addr 0 ?size) do 
                             (call ?memory 
                                   set 
                                   ?addr 
                                   (random -128 
                                           128))))
(deffunction clear-memory
             (?memory)
             (call ?memory
                   clear))

(deffunction number-of-items 
             (?count ?item) 
             (bind ?output 
                   (create$)) 
             (loop-for-count ?count do 
                             (bind ?output 
                                   ?output 
                                   ?item))
             ?output)

(deffunction make-1m-page 
             (?type) 
             (new ?type 
                  (hex->int 0xFFFFF)))
(defglobal MAIN
           ?*memory* = (map make-1m-page 
                            (expand$ (number-of-items (hex->int 0x4FF) 
                                                      word32u))))
