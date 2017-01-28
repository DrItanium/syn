(defclass lisp->intermediary::register
  (is-a node)
  (slot alias-to
        (type SYMBOL
              INSTANCE)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE))
  (message-handler get-title primary))


(defmessage-handler lisp->intermediary::register get-title primary
                    ()
                    (if (instancep ?self:alias-to) then
                        (send ?self:alias-to
                              get-title)
                        else
                        (instance-name-to-symbol (instance-name ?self))))
(definstances lisp->intermediary::registers
              (r0 of register
                  (parent FALSE))
              (r1 of register
                  (parent FALSE))
              (r2 of register
                  (parent FALSE))
              (r3 of register
                  (parent FALSE))
              (r4 of register
                  (parent FALSE))
              (r5 of register
                  (parent FALSE))
              (r6 of register
                  (parent FALSE))
              (r7 of register
                  (parent FALSE))
              (r8 of register
                  (parent FALSE))
              (r9 of register
                  (parent FALSE))
              (r10 of register
                   (parent FALSE))
              (r11 of register
                   (parent FALSE))
              (r12 of register
                   (parent FALSE))
              (r13 of register
                   (parent FALSE))
              (r14 of register
                   (parent FALSE))
              (r15 of register
                   (parent FALSE))
              (r16 of register
                   (parent FALSE))
              (r17 of register
                   (parent FALSE))
              (r18 of register
                   (parent FALSE))
              (r19 of register
                   (parent FALSE))
              (r20 of register
                   (parent FALSE))
              (r21 of register
                   (parent FALSE))
              (r22 of register
                   (parent FALSE))
              (r23 of register
                   (parent FALSE))
              (r24 of register
                   (parent FALSE))
              (r25 of register
                   (parent FALSE))
              (r26 of register
                   (parent FALSE))
              (r27 of register
                   (parent FALSE))
              (r28 of register
                   (parent FALSE))
              (r29 of register
                   (parent FALSE))
              (r30 of register
                   (parent FALSE))
              (r31 of register
                   (parent FALSE))
              (r32 of register
                   (parent FALSE))
              (r33 of register
                   (parent FALSE))
              (r34 of register
                   (parent FALSE))
              (r35 of register
                   (parent FALSE))
              (r36 of register
                   (parent FALSE))
              (r37 of register
                   (parent FALSE))
              (r38 of register
                   (parent FALSE))
              (r39 of register
                   (parent FALSE))
              (r40 of register
                   (parent FALSE))
              (r41 of register
                   (parent FALSE))
              (r42 of register
                   (parent FALSE))
              (r43 of register
                   (parent FALSE))
              (r44 of register
                   (parent FALSE))
              (r45 of register
                   (parent FALSE))
              (r46 of register
                   (parent FALSE))
              (r47 of register
                   (parent FALSE))
              (r48 of register
                   (parent FALSE))
              (r49 of register
                   (parent FALSE))
              (r50 of register
                   (parent FALSE))
              (r51 of register
                   (parent FALSE))
              (r52 of register
                   (parent FALSE))
              (r53 of register
                   (parent FALSE))
              (r54 of register
                   (parent FALSE))
              (r55 of register
                   (parent FALSE))
              (r56 of register
                   (parent FALSE))
              (r57 of register
                   (parent FALSE))
              (r58 of register
                   (parent FALSE))
              (r59 of register
                   (parent FALSE))
              (r60 of register
                   (parent FALSE))
              (r61 of register
                   (parent FALSE))
              (r62 of register
                   (parent FALSE))
              (r63 of register
                   (parent FALSE))
              (r64 of register
                   (parent FALSE))
              (r65 of register
                   (parent FALSE))
              (r66 of register
                   (parent FALSE))
              (r67 of register
                   (parent FALSE))
              (r68 of register
                   (parent FALSE))
              (r69 of register
                   (parent FALSE))
              (r70 of register
                   (parent FALSE))
              (r71 of register
                   (parent FALSE))
              (r72 of register
                   (parent FALSE))
              (r73 of register
                   (parent FALSE))
              (r74 of register
                   (parent FALSE))
              (r75 of register
                   (parent FALSE))
              (r76 of register
                   (parent FALSE))
              (r77 of register
                   (parent FALSE))
              (r78 of register
                   (parent FALSE))
              (r79 of register
                   (parent FALSE))
              (r80 of register
                   (parent FALSE))
              (r81 of register
                   (parent FALSE))
              (r82 of register
                   (parent FALSE))
              (r83 of register
                   (parent FALSE))
              (r84 of register
                   (parent FALSE))
              (r85 of register
                   (parent FALSE))
              (r86 of register
                   (parent FALSE))
              (r87 of register
                   (parent FALSE))
              (r88 of register
                   (parent FALSE))
              (r89 of register
                   (parent FALSE))
              (r90 of register
                   (parent FALSE))
              (r91 of register
                   (parent FALSE))
              (r92 of register
                   (parent FALSE))
              (r93 of register
                   (parent FALSE))
              (r94 of register
                   (parent FALSE))
              (r95 of register
                   (parent FALSE))
              (r96 of register
                   (parent FALSE))
              (r97 of register
                   (parent FALSE))
              (r98 of register
                   (parent FALSE))
              (r99 of register
                   (parent FALSE))
              (r100 of register
                    (parent FALSE))
              (r101 of register
                    (parent FALSE))
              (r102 of register
                    (parent FALSE))
              (r103 of register
                    (parent FALSE))
              (r104 of register
                    (parent FALSE))
              (r105 of register
                    (parent FALSE))
              (r106 of register
                    (parent FALSE))
              (r107 of register
                    (parent FALSE))
              (r108 of register
                    (parent FALSE))
              (r109 of register
                    (parent FALSE))
              (r110 of register
                    (parent FALSE))
              (r111 of register
                    (parent FALSE))
              (r112 of register
                    (parent FALSE))
              (r113 of register
                    (parent FALSE))
              (r114 of register
                    (parent FALSE))
              (r115 of register
                    (parent FALSE))
              (r116 of register
                    (parent FALSE))
              (r117 of register
                    (parent FALSE))
              (r118 of register
                    (parent FALSE))
              (r119 of register
                    (parent FALSE))
              (r120 of register
                    (parent FALSE))
              (r121 of register
                    (parent FALSE))
              (r122 of register
                    (parent FALSE))
              (r123 of register
                    (parent FALSE))
              (r124 of register
                    (parent FALSE))
              (r125 of register
                    (parent FALSE))
              (r126 of register
                    (parent FALSE))
              (r127 of register
                    (parent FALSE))
              (r128 of register
                    (parent FALSE))
              (r129 of register
                    (parent FALSE))
              (r130 of register
                    (parent FALSE))
              (r131 of register
                    (parent FALSE))
              (r132 of register
                    (parent FALSE))
              (r133 of register
                    (parent FALSE))
              (r134 of register
                    (parent FALSE))
              (r135 of register
                    (parent FALSE))
              (r136 of register
                    (parent FALSE))
              (r137 of register
                    (parent FALSE))
              (r138 of register
                    (parent FALSE))
              (r139 of register
                    (parent FALSE))
              (r140 of register
                    (parent FALSE))
              (r141 of register
                    (parent FALSE))
              (r142 of register
                    (parent FALSE))
              (r143 of register
                    (parent FALSE))
              (r144 of register
                    (parent FALSE))
              (r145 of register
                    (parent FALSE))
              (r146 of register
                    (parent FALSE))
              (r147 of register
                    (parent FALSE))
              (r148 of register
                    (parent FALSE))
              (r149 of register
                    (parent FALSE))
              (r150 of register
                    (parent FALSE))
              (r151 of register
                    (parent FALSE))
              (r152 of register
                    (parent FALSE))
              (r153 of register
                    (parent FALSE))
              (r154 of register
                    (parent FALSE))
              (r155 of register
                    (parent FALSE))
              (r156 of register
                    (parent FALSE))
              (r157 of register
                    (parent FALSE))
              (r158 of register
                    (parent FALSE))
              (r159 of register
                    (parent FALSE))
              (r160 of register
                    (parent FALSE))
              (r161 of register
                    (parent FALSE))
              (r162 of register
                    (parent FALSE))
              (r163 of register
                    (parent FALSE))
              (r164 of register
                    (parent FALSE))
              (r165 of register
                    (parent FALSE))
              (r166 of register
                    (parent FALSE))
              (r167 of register
                    (parent FALSE))
              (r168 of register
                    (parent FALSE))
              (r169 of register
                    (parent FALSE))
              (r170 of register
                    (parent FALSE))
              (r171 of register
                    (parent FALSE))
              (r172 of register
                    (parent FALSE))
              (r173 of register
                    (parent FALSE))
              (r174 of register
                    (parent FALSE))
              (r175 of register
                    (parent FALSE))
              (r176 of register
                    (parent FALSE))
              (r177 of register
                    (parent FALSE))
              (r178 of register
                    (parent FALSE))
              (r179 of register
                    (parent FALSE))
              (r180 of register
                    (parent FALSE))
              (r181 of register
                    (parent FALSE))
              (r182 of register
                    (parent FALSE))
              (r183 of register
                    (parent FALSE))
              (r184 of register
                    (parent FALSE))
              (r185 of register
                    (parent FALSE))
              (r186 of register
                    (parent FALSE))
              (r187 of register
                    (parent FALSE))
              (r188 of register
                    (parent FALSE))
              (r189 of register
                    (parent FALSE))
              (r190 of register
                    (parent FALSE))
              (r191 of register
                    (parent FALSE))
              (r192 of register
                    (parent FALSE))
              (r193 of register
                    (parent FALSE))
              (r194 of register
                    (parent FALSE))
              (r195 of register
                    (parent FALSE))
              (r196 of register
                    (parent FALSE))
              (r197 of register
                    (parent FALSE))
              (r198 of register
                    (parent FALSE))
              (r199 of register
                    (parent FALSE))
              (r200 of register
                    (parent FALSE))
              (r201 of register
                    (parent FALSE))
              (r202 of register
                    (parent FALSE))
              (r203 of register
                    (parent FALSE))
              (r204 of register
                    (parent FALSE))
              (r205 of register
                    (parent FALSE))
              (r206 of register
                    (parent FALSE))
              (r207 of register
                    (parent FALSE))
              (r208 of register
                    (parent FALSE))
              (r209 of register
                    (parent FALSE))
              (r210 of register
                    (parent FALSE))
              (r211 of register
                    (parent FALSE))
              (r212 of register
                    (parent FALSE))
              (r213 of register
                    (parent FALSE))
              (r214 of register
                    (parent FALSE))
              (r215 of register
                    (parent FALSE))
              (r216 of register
                    (parent FALSE))
              (r217 of register
                    (parent FALSE))
              (r218 of register
                    (parent FALSE))
              (r219 of register
                    (parent FALSE))
              (r220 of register
                    (parent FALSE))
              (r221 of register
                    (parent FALSE))
              (r222 of register
                    (parent FALSE))
              (r223 of register
                    (parent FALSE))
              (r224 of register
                    (parent FALSE))
              (r225 of register
                    (parent FALSE))
              (r226 of register
                    (parent FALSE))
              (r227 of register
                    (parent FALSE))
              (r228 of register
                    (parent FALSE))
              (r229 of register
                    (parent FALSE))
              (r230 of register
                    (parent FALSE))
              (r231 of register
                    (parent FALSE))
              (r232 of register
                    (parent FALSE))
              (r233 of register
                    (parent FALSE))
              (r234 of register
                    (parent FALSE))
              (r235 of register
                    (parent FALSE))
              (r236 of register
                    (parent FALSE))
              (r237 of register
                    (parent FALSE))
              (r238 of register
                    (parent FALSE))
              (r239 of register
                    (parent FALSE))
              (r240 of register
                    (parent FALSE))
              (r241 of register
                    (parent FALSE))
              (r242 of register
                    (parent FALSE))
              (r243 of register
                    (parent FALSE))
              (r244 of register
                    (parent FALSE))
              (r245 of register
                    (parent FALSE))
              (r246 of register
                    (parent FALSE))
              (r247 of register
                    (parent FALSE))
              (r248 of register
                    (parent FALSE))
              (r249 of register
                    (parent FALSE))
              (r250 of register
                    (parent FALSE))
              (r251 of register
                    (parent FALSE))
              (r252 of register
                    (parent FALSE))
              (r253 of register
                    (parent FALSE))
              (r254 of register
                    (parent FALSE))
              (r255 of register
                    (parent FALSE)))
(defclass lisp->intermediary::section
  (is-a node
        has-body)
  (slot section
        (type SYMBOL)
        (allowed-symbols code
                         data)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::label
  (is-a node
        has-title
        has-body))
(defclass lisp->intermediary::org
  (is-a node
        has-body)
  (slot address
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lisp->intermediary::word
          (is-a node)
          (slot value
                (visibility public)
                (storage local)
                (default ?NONE)))

(defclass lisp->intermediary::instruction
  (is-a node
        has-title)
  (message-handler resolve primary))

(defclass lisp->intermediary::has-destination-register
   (is-a USER)
   (slot destination-register
         (visibility public)
         (storage local)
         (default ?NONE)))
(defclass lisp->intermediary::has-full-immediate
  (is-a USER)
  (slot full-immediate
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::has-source-register0
  (is-a USER)
  (slot source-register0
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::has-source-register1
  (is-a USER)
  (slot source-register1
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::two-argument-instruction
  (is-a instruction
        has-destination-register
        has-source-register0))

(defclass lisp->intermediary::set-instruction
  (is-a instruction
        has-destination-register
        has-full-immediate))


(defrule lisp->intermediary::parse-push-operation-style0
         ?f <- (object (is-a list)
                       (contents push
                                 ?target
                                 onto
                                 ?stack)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title push)
                        (destination-register ?stack)
                        (source-register0 ?target)))

(defrule lisp->intermediary::parse-pop-operation-style0
         ?f <- (object (is-a list)
                       (contents pop
                                 ?stack
                                 into
                                 ?target)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title pop)
                        (destination-register ?target)
                        (source-register0 ?stack)))

(defrule lisp->intermediary::parse-set-operation-style0
         ?f <- (object (is-a list)
                       (contents set
                                 ?target
                                 to
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of set-instruction
                        (parent ?p)
                        (title set)
                        (destination-register ?target)
                        (full-immediate ?value)))

(defrule lisp->intermediary::construct-alias
         ?f <- (object (is-a list)
                       (contents alias
                                 ?target
                                 as
                                 ?alias&:(symbolp ?alias))
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?alias of register
                        (parent ?parent)
                        (alias-to (if (symbolp ?target) then
                                    (symbol-to-instance-name ?target)
                                    else
                                    ?target))))

(defrule lisp->intermediary::mark-register
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents $?a
                                 ?register&:(symbolp ?register)
                                 $?b))
         (object (is-a register)
                 (name =(symbol-to-instance-name ?register)))
         =>
         (modify-instance ?f
                          (contents ?a
                                    (symbol-to-instance-name ?register)
                                    ?b)))

(defrule lisp->intermediary::construct-section
         ?f <- (object (is-a list)
                       (contents section
                                 ?section&:(not (neq ?section
                                                     code
                                                     data))
                                 $?body)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of section
                        (body ?body)
                        (section ?section)
                        (parent ?p)))

(defrule lisp->intermediary::make-org
         ?f <- (object (is-a list)
                       (contents org
                                 ?address
                                 $?body)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of org
                        (address ?address)
                        (parent ?p)
                        (body ?body)))
(defrule lisp->intermediary::make-label
         ?f <- (object (is-a list)
                       (contents label
                                 ?title
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of label
                        (parent ?p)
                        (title ?title)
                        (body ?body)))



(defrule lisp->intermediary::make-word
         ?f <- (object (is-a list)
                       (contents word
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of word
                        (parent ?p)
                        (value ?value)))
