(defmodule windowing
           (import cortex
                   ?ALL)
           (export ?ALL))

(defgeneric windowing::make-window)
(defmethod windowing::make-window
  ((?width INTEGER
           (> ?current-argument 0))
   (?height INTEGER
            (> ?current-argument 0))
   (?title LEXEME))
  (new window
       ?width
       ?height
       ?title))
(defmethod windowing::make-window
  ((?width INTEGER
           (> ?current-argument 0))
   (?height INTEGER
            (> ?current-argument 0))
   (?title LEXEME)
   (?bit-depth INTEGER
               (> ?current-argument 
                  0)))
  (new window
       ?width
       ?height
       ?title
       ?bit-depth))
(deffunction windowing::close-window
             (?ptr)
             (call ?ptr
                   close))
(deffunction windowing::window-openp
             (?ptr)
             (call ?ptr 
                   openp))
(deffunction windowing::window-display
             (?ptr)
             (call ?ptr
                   display))


(defclass windowing::window
  (is-a thing)
  (slot width
        (type INTEGER)
        ;(range 1 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot height
        (type INTEGER)
        ;(range 1 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot title 
        (type LEXEME)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot bit-depth
        (type INTEGER)
        (range 1 ?VARIABLE)
        (storage local)
        (visibility public)
        (default-dynamic 32))
  (slot base-pointer
        (type EXTERNAL-ADDRESS)
        (visibility public)
        (storage local))
  (message-handler init after)
  (message-handler delete before))

(defmessage-handler windowing::window delete before
                    ()
                    (if (window-openp ?self:base-pointer) then
                      (close-window ?self:base-pointer)))
(defmessage-handler windowing::window init after
                    ()
                    (bind ?self:base-pointer
                          (make-window ?self:width
                                       ?self:height
                                       ?self:title
                                       ?self:bit-depth)))

(deffunction windowing::windowp
             (?ext)
             (eq (external-address-type ?ext)
                 window))
; originally, I overrode clear but that seems to cause CLIPS to crash, so for the time being I'm doing clear-window
(defgeneric windowing::clear-window)
(defmethod windowing::clear-window
  ((?window window))
  (clear-window (send ?window
                      get-base-pointer)))

(defmethod windowing::clear-window
  ((?window EXTERNAL-ADDRESS
            (windowp ?current-argument)))
  (call ?window
        clear))


