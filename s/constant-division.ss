;; Compute the signed multiply-high multiplier and shift for division by d.
;; d is a signed width-bit integer other than 0 or +/-1. All arithmetic here
;; is exact, including the magnitude of the most-negative signed word.
;; This is the signed division algorithm from Hacker's Delight, section 10-3.
(define (signed-division-magic d width)
  (let* ([half (expt 2 (- width 1))]
         [ad (abs d)]
         [t (+ half (if (< d 0) 1 0))]
         [anc (- t 1 (modulo t ad))])
    (let loop ([p (- width 1)]
               [q1 (quotient half anc)] [r1 (modulo half anc)]
               [q2 (quotient half ad)] [r2 (modulo half ad)])
      (let* ([p (+ p 1)]
             [r1 (* r1 2)]
             [q1 (+ (* q1 2) (if (>= r1 anc) 1 0))]
             [r1 (if (>= r1 anc) (- r1 anc) r1)]
             [r2 (* r2 2)]
             [q2 (+ (* q2 2) (if (>= r2 ad) 1 0))]
             [r2 (if (>= r2 ad) (- r2 ad) r2)]
             [delta (- ad r2)])
        (if (or (< q1 delta) (and (= q1 delta) (= r1 0)))
            (loop p q1 r1 q2 r2)
            (let ([m (* (+ q2 1) (if (< d 0) -1 1))])
              ;; Interpret the multiplier as a signed target word.
              (values (- (modulo (+ m half) (* half 2)) half)
                      (- p width))))))))
