;;;; brainfuck.asd

(asdf:defsystem #:brainfuck
  :description "Brainfuck interpreter."
  :author "Mateusz Malisz <maliszmat at gmail dot com>"
  :license "WTFPL"
  :serial t
  :components ((:file "package")
               (:file "brainfuck")))
