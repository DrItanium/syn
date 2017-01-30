(defmessage-handler NUMBER resolve primary
                    ()
                    ?self)
(defmessage-handler LEXEME resolve primary
                    ()
                    ?self)
(deffunction lower::mk-list
             (?parent $?contents)
             (make-instance of list
                            (parent ?parent)
                            (contents ?contents)))
(defclass lower::register
  (is-a node)
  (slot alias-to
        (type SYMBOL
              INSTANCE)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE))
  (message-handler resolve primary))


(defmessage-handler lower::register resolve primary
                    ()
                    (if (instancep ?self:alias-to) then
                      (send ?self:alias-to
                            resolve)
                      else
                      (instance-name-to-symbol (instance-name ?self))))
(definstances lower::registers
              (p0 of register
                  (parent FALSE))
              (p1 of register
                  (parent FALSE))
              (p2 of register
                  (parent FALSE))
              (p3 of register
                  (parent FALSE))
              (p4 of register
                  (parent FALSE))
              (p5 of register
                  (parent FALSE))
              (p6 of register
                  (parent FALSE))
              (p7 of register
                  (parent FALSE))
              (p8 of register
                  (parent FALSE))
              (p9 of register
                  (parent FALSE))
              (p10 of register
                   (parent FALSE))
              (p11 of register
                   (parent FALSE))
              (p12 of register
                   (parent FALSE))
              (p13 of register
                   (parent FALSE))
              (p14 of register
                   (parent FALSE))
              (p15 of register
                   (parent FALSE))
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
(definstances lower::predefined-statements
              (of list
                  (parent FALSE)
                  (contents alias 
                            r239
                            as
                            iv0))
              (of list
                  (parent FALSE)
                  (contents alias
                            r238
                            as
                            sp))
              (of list
                  (parent FALSE)
                  (contents alias
                            r237
                            as
                            iv1)))



(defclass lower::section
  (is-a node
        has-body)
  (slot section
        (type SYMBOL)
        (allowed-symbols code
                         data)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::section resolve primary
                    ()
                    (bind ?output
                          (format nil
                                  ".%s"
                                  ?self:section))
                    (progn$ (?b ?self:body)
                            (bind ?output
                                  ?output
                                  (send ?b 
                                        resolve)))
                    ?output)


(defclass lower::label
  (is-a node
        has-title
        has-body)
  (message-handler resolve primary))

(defmessage-handler lower::label resolve primary
                    ()
                    (bind ?output
                          (format nil
                                  ".label %s"
                                  (dynamic-get title)))
                    (progn$ (?b ?self:body)
                            (bind ?output
                                  ?output
                                  (send ?b
                                        resolve)))
                    ?output)
(defclass lower::org
  (is-a node
        has-body)
  (slot address
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::org resolve primary
                    ()
                    (bind ?output
                          (format nil
                                  ".org %s"
                                  (dynamic-get address)))
                    (progn$ (?b ?self:body)
                            (bind ?output
                                  ?output
                                  (send ?b
                                        resolve)))
                    ?output)

(defclass lower::word
  (is-a node)
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::word resolve primary
                    ()
                    (format nil
                            ".word %s"
                            (dynamic-get value)))

(defclass lower::instruction
  (is-a node
        has-title)
  (message-handler resolve-arguments primary)
  (message-handler resolve primary))

(defmessage-handler lower::instruction resolve primary
                    ()
                    (format nil 
                            "%s %s"
                            (dynamic-get title)
                            (send ?self 
                                  resolve-arguments)))
(defmessage-handler lower::instruction resolve-arguments primary
                    ()
                    "")

(defclass lower::zero-argument-instruction
  (is-a instruction))

(defclass lower::instruction-with-destination
  (is-a instruction)
  (slot destination-register
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination resolve-arguments primary
                    ()
                    (format nil
                            "%s"
                            (send ?self:destination-register
                                  resolve)))

(defclass lower::one-argument-instruction
  (is-a instruction-with-destination))

(defclass lower::instruction-with-destination-and-source0
  (is-a instruction-with-destination)
  (slot source-register0
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination-and-source0 resolve-arguments primary
                    ()
                    (format nil
                            "%s %s"
                            (call-next-handler)
                            (send ?self:source-register0
                                  resolve)))

(defclass lower::two-argument-instruction
  (is-a instruction-with-destination-and-source0))

(defclass lower::instruction-with-destination-source0-and-source1
  (is-a instruction-with-destination-and-source0)
  (slot source-register1
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination-source0-and-source1 resolve-arguments primary
                    ()
                    (format nil
                            "%s %s"
                            (call-next-handler)
                            (str-cat (send ?self:source-register1 
                                           resolve))))

(defclass lower::three-argument-instruction
  (is-a instruction-with-destination-source0-and-source1))

(defclass lower::instruction-with-destination-source0-source1-and-source2
  (is-a instruction-with-destination-source0-and-source1)
  (slot source-register2
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lower::four-argument-instruction
  (is-a instruction-with-destination-source0-source1-and-source2))

(defclass lower::simple-container
  (is-a node
        has-body))

(deffunction lower::mk-container
             (?name ?parent $?body)
             (make-instance ?name of simple-container
                            (parent ?parent)
                            (body ?body)))

(defmessage-handler lower::simple-container resolve primary
                    ()
                    (bind ?output
                          (create$))
                    (progn$ (?b ?self:body)
                            (bind ?output
                                  ?output
                                  (send ?b 
                                        resolve)))
                    ?output)

(defrule lower::construct-alias
         (declare (salience ?*priority:first*))
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

(defrule lower::mark-register
         (declare (salience 100))
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

(defrule lower::construct-section
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

(defrule lower::make-org
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
(defrule lower::make-label
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



(defrule lower::make-word
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

(defrule lower::construct-output-string
         (declare (salience -1000))
         (object (is-a file)
                 (name ?file))
         ?f <- (object (is-a section)
                       (parent ?file))
         =>
         (progn$ (?l (send ?f resolve))
                 (printout t ?l crlf)))

(deffacts lower::register-operations
          (four-register-operation eq)
          (four-register-operation eqi)
          (four-register-operation ne)
          (four-register-operation nei)
          (four-register-operation le)
          (four-register-operation lei)
          (four-register-operation ge)
          (four-register-operation gei)
          (four-register-operation lt)
          (four-register-operation lti)
          (four-register-operation gt)
          (four-register-operation gti)
          (four-register-operation crxor)
          (four-register-operation crand)
          (four-register-operation cror)
          (four-register-operation crnand)
          (four-register-operation crnor)
          (three-register-operation add)
          (three-register-operation sub)
          (three-register-operation mul)
          (three-register-operation div)
          (three-register-operation rem)
          (three-register-operation shl)
          (three-register-operation shr)
          (three-register-operation and)
          (three-register-operation or)
          (three-register-operation xor)
          (three-register-operation nand)
          (three-register-operation nor)
          (three-register-operation min)
          (three-register-operation max)
          (three-register-operation addi)
          (three-register-operation subi)
          (three-register-operation muli)
          (three-register-operation divi)
          (three-register-operation remi)
          (three-register-operation shli)
          (three-register-operation shri)
          (three-register-operation ldc)
          (three-register-operation stc)
          (three-register-operation ldwo)
          (three-register-operation stwo)
          (three-register-operation ldiwo)
          (three-register-operation stiwo)
          (three-register-operation if)
          (three-register-operation ifl)
          (three-register-operation crnot)
          (two-register-operation ldio)
          (two-register-operation stio)
          (two-register-operation sti)
          (two-register-operation memset)
          (two-register-operation ldi)
          (two-register-operation ldm)
          (two-register-operation not)
          (two-register-operation set)
          (two-register-operation swap)
          (two-register-operation move)
          (two-register-operation ld)
          (two-register-operation st)
          (two-register-operation push)
          (two-register-operation pushi)
          (two-register-operation pop)
          (two-register-operation bc)
          (two-register-operation bcl)
          (two-register-operation bic)
          (two-register-operation bicl)
          (two-register-operation crswap)
          (two-register-operation crmove)
          (one-register-operation mtip)
          (one-register-operation mfip)
          (one-register-operation mtlr)
          (one-register-operation mflr)
          (one-register-operation svcr)
          (one-register-operation recr)
          (one-register-operation b)
          (one-register-operation bl)
          (one-register-operation bi)
          (one-register-operation bil)
          (one-register-operation blrc)
          (one-register-operation blrcl)
          (zero-register-operation blr)
          (zero-register-operation blrl)
          )

(defrule lower::construct-one-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination)
                       (name ?n)
                       (parent ?p))
         (one-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction 
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)))
(defrule lower::construct-zero-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation)
                       (name ?n)
                       (parent ?p))
         (zero-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of zero-argument-instruction 
                        (parent ?p)
                        (title ?operation)))

(defrule lower::construct-two-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0)
                       (name ?n)
                       (parent ?p))
         (two-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)))

(defrule lower::construct-three-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1)
                       (name ?n)
                       (parent ?p))
         (three-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of three-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)
                        (source-register1 ?source1)))

(defrule lower::construct-four-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1
                                 ?source2)
                       (name ?n)
                       (parent ?p))
         (four-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of four-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)
                        (source-register1 ?source1)
                        (source-register2 ?source2)))
;---------------------------------------------------------
; macros and shorthand operations
;---------------------------------------------------------

(defrule lower::parse-set-operation-alternate-style
         ?f <- (object (is-a list)
                       (contents set
                                 ?target
                                 to
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title set)
                        (destination-register ?target)
                        (source-register0 ?value)))

(defrule lower::parse-call-unconditional-operation-register-version
         ?f <- (object (is-a list)
                       (contents call
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f 
                          (contents bl
                                    ?register)))

(defrule lower::parse-call-unconditional-operation-immediate-version
         ?f <- (object (is-a list)
                       (contents call
                                 ?target&:(or (symbolp ?target)
                                              (numberp ?target))))
         =>
         (modify-instance ?f 
                          (contents bil
                                    ?target)))

(defrule lower::parse-return-instruction
         ?f <- (object (is-a list)
                       (contents ret))
         =>
         (modify-instance ?f
                          (contents blr)))

(defrule lower::parse-call-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents call lr))
         =>
         (modify-instance ?f
                          (contents blrl)))

(defrule lower::parse-move-to-lr-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 lr
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mtlr 
                                    ?register)))
(defrule lower::parse-move-from-lr-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 lr))
         =>
         (modify-instance ?f
                          (contents mflr
                                    ?register)))

(defrule lower::parse-move-to-ip-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ip
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mtip 
                                    ?register)))
(defrule lower::parse-move-from-ip-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 ip))
         =>
         (modify-instance ?f
                          (contents mfip
                                    ?register)))

(deffunction lower::make-swap-lr
             (?parent ?reg)
             (create$ (make-instance of list
                                     (parent ?parent)
                                     (body move
                                           iv0
                                           lr))
                      (make-instance of list
                                     (parent ?parent)
                                     (body swap
                                           iv0
                                           ?reg))
                      (make-instance of list
                                     (parent ?parent)
                                     (body move
                                           lr
                                           iv0))))
(defrule lower::make-swap-lr-register-lr-first
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents swap
                                 lr
                                 ?register)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (unmake-instance ?f)
         (make-instance ?name of simple-container
                        (parent ?p)
                        (body (make-swap-lr ?name
                                            ?register))))

(defrule lower::make-swap-lr-register-lr-second
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (contents swap
                                 ?register
                                 lr)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f 
                          (contents swap
                                    lr
                                    ?register)))
(deffacts lower::nary-register-macros
          (nary-register-macro add)
          (nary-register-macro sub)
          (nary-register-macro mul)
          (nary-register-macro div)
          (nary-register-macro rem)
          (nary-register-macro shl)
          (nary-register-macro shr)
          (nary-register-macro and)
          (nary-register-macro or)
          (nary-register-macro xor)
          (nary-register-macro nand)
          (nary-register-macro nor)
          (nary-register-macro min)
          (nary-register-macro max))

(defrule lower::parse-nary-argument-macro
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1
                                 $?rest
                                 ?sourceN)
                       (name ?name)
                       (parent ?p))
         (nary-register-macro ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?name of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?name)
                                             (contents ?operation
                                                       iv0
                                                       ?source0
                                                       ?source1
                                                       $?rest))
                              (make-instance of list
                                             (parent ?name)
                                             (contents ?operation
                                                       ?destination
                                                       iv0
                                                       ?sourceN)))))

(defrule lower::parse-increment-macro
         ?f <- (object (is-a list)
                       (contents incr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents addi
                                    ?destination
                                    ?register
                                    1)))

(defrule lower::parse-decrement-macro
         ?f <- (object (is-a list)
                       (contents decr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents subi
                                    ?destination
                                    ?register
                                    1)))
(defrule lower::parse-double-macro
         ?f <- (object (is-a list)
                       (contents double
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents add
                                    ?destination
                                    ?register
                                    ?register)))

(defrule lower::parse-triple-macro
         ?f <- (object (is-a list)
                       (contents triple 
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents muli
                                    ?destination
                                    ?register
                                    3)))

(defrule lower::parse-halve-macro
         ?f <- (object (is-a list)
                       (contents halve
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents divi
                                    ?destination
                                    ?register
                                    2)))
(defrule lower::parse-square-macro
         ?f <- (object (is-a list)
                       (contents square
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register)))
(defrule lower::parse-cube-macro
         ?f <- (object (is-a list)
                       (contents cube
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register
                                    ?register)))


(defrule lower::parse-using-block
         ?f <- (object (is-a list)
                       (contents using
                                 ?stack-info
                                 ?registers-to-preserve
                                 $?body)
                       (name ?n)
                       (parent ?p))
         ?f2 <- (object (is-a list)
                        (name ?stack-info)
                        (contents save-to 
                                  ?stack))
         ?f3 <- (object (is-a list)
                        (name ?registers-to-preserve)
                        (contents $?registers))
         =>
         (unmake-instance ?f 
                          ?f2 
                          ?f3)
         (bind ?pre
               (create$))
         (bind ?post
               (create$))
         (progn$ (?a $?registers)
                 (bind ?pre
                       ?pre
                       (mk-list ?n
                                push
                                ?stack
                                ?a))
                 (bind ?post
                       (mk-list ?n
                                pop
                                ?a
                                ?stack)
                       ?post))
         (mk-container ?n
                       ?p
                       ?pre
                       ?body
                       ?post))



(defrule lower::move-predicates-to-register
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 predicates))
         =>
         (modify-instance ?f
                          (contents svcr
                                    ?register)))

(defrule lower::move-register-to-predicates
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 predicates
                                 ?register))
         =>
         (modify-instance ?f
                          (contents recr 
                                    ?register)))


(defrule lower::store-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents st
                                 ?address
                                 predicates)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?n)
                                             (contents move
                                                       iv0
                                                       predicates))
                              (make-instance of list
                                             (parent ?n)
                                             (contents st
                                                       ?address
                                                       iv0)))))


(defrule lower::load-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents ld
                                 predicates
                                 ?address)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                ld
                                iv0
                                ?address)
                       (mk-list ?n
                                move
                                predicates
                                iv0)))



(defrule lower::memswap-data
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents memswap
                                 ?register0
                                 ?register1)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n 
                       ?p
                       (mk-list ?n 
                                ld 
                                iv0 
                                ?register0)
                       (mk-list ?n 
                                ld 
                                iv1 
                                ?register1)
                       (mk-list ?n
                                st 
                                ?register1 
                                iv0)
                       (mk-list ?n 
                                st 
                                ?register0 
                                iv1)))

(defrule lower::terminate
         ?f <- (object (is-a list)
                       (contents terminate)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0000)
                       (mk-list ?n
                                stio
                                iv0
                                iv0)))

(defrule lower::putc
         ?f <- (object (is-a list)
                       (contents putc
                                 ?register)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0001)
                       (mk-list ?n
                                stio
                                iv0
                                ?register)))

(defrule lower::getc
         ?f <- (object (is-a list)
                       (contents getc
                                 ?register)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0001)
                       (mk-list ?n
                                ldio
                                ?register
                                iv0)))


(defrule lower::parse-func
         ?f <- (object (is-a list)
                       (contents func
                                 ?name
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of label
                        (parent ?p)
                        (title ?name)
                        (body ?body
                              (mk-list ?n
                                       ret))))
