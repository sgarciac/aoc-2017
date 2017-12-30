(LET ((A 0) (B 0) (C 0) (D 0) (E 0) (F 0) (G 0) (H 0))
  (DECLARE (TYPE FIXNUM A B C D E F G H))
  (SETQ A 1)
  (TAGBODY
   0
     (SETQ B 93)
   1
     (SETQ C B)
   4
     (SETQ B (* B 100))
   5
     (SETQ B (SB-IMPL::XSUBTRACT -100000 B))
   6
     (SETQ C B)
   7
     (SETQ C (SB-IMPL::XSUBTRACT -17000 C))
   8
     (loop for k from B upto C by 17
        do (progn
             (SETQ F 1)
             (loop for j from 2 upto B
                do (loop for i from 2 upto B
                      do (progn   (IF (- (* i j) B)
                                      (SETQ F 0)))))
             (when (ZEROP F)
               (SETQ H (SB-IMPL::XSUBTRACT -1 H))
               (print h)
               )
             ))
   32)
  (LIST A B C D E F G H))
