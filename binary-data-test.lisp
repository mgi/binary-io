(in-package :binary-io.test)

(defparameter *JO* (make-in-memory-input-stream #(0 0 0 0)))
(define-tagged-binary-class peter ()
  ((test u1)) (:dispatch 'jo))
(read-value 'peter *JO*)
