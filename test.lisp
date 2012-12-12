(cl:in-package :sharp-bq.internal)
(in-readtable :sharp-bq)


(def-suite sharp-bq)

(in-suite sharp-bq)

(test sharp-bq
  (is (equal 
       '#`(send stream ',(:clear-input :clear-output))
       '(PROGN (SEND STREAM ':CLEAR-INPUT) (SEND STREAM ':CLEAR-OUTPUT))))
  (is (equal 
       '#`(send stream ,(:clear-input :clear-output))
       '(PROGN (SEND STREAM :CLEAR-INPUT) (SEND STREAM :CLEAR-OUTPUT))))
  (is (equal 
       '#`(a b ,((c d) (e f)))
       '(PROGN (A B (C D)) (A B (E F)))))
  (is (equal 
       '#`(a b ,@((c d) (e f)))
       '(PROGN (A B C D) (A B E F))))
  (is (equal 
       '#`#`(print (* ,(5 7) ,,(11. 13.)))
       '(PROGN
         (PROGN (PRINT (* 5 11)) (PRINT (* 7 11)))
         (PROGN (PRINT (* 5 13)) (PRINT (* 7 13))))))
  (is (equal 
       '#`#`(a0 (b1 ,(c2 d2) ,,(e3 f3)))
       '(PROGN
         (PROGN (A0 (B1 C2 E3)) (A0 (B1 D2 E3)))
         (PROGN (A0 (B1 C2 F3)) (A0 (B1 D2 F3)))))))

;;; eof

