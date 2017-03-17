(deffunction write-char
             (?value)
             (put-char ?value))
(deffunction read-char
             ()
             (get-char))
(deffunction read-from-io-address
             (?address)
             (if (= ?address 2) then
                 (read-char)
                 else
                 0))

(deffunction write-to-io-address
             (?address ?value)
             (if (= ?address 2) then
                 (write-char ?value)
                 else
                 0))


