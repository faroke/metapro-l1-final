#lang racket
(require "ressource.rkt")
(require "exceptions.rkt")

(define (len l)
  (define (le l n)
    (if (pair? l)
        (le (cdr l) (+ n 1))
        n))
  (le l 0))

(define (fct)
  (let ((l (read)))
    (let ((n (len l)))
      (if (or (< n 3) (> n 4))
          (write "RTFM!")
          (if (or (equal? (caddr l) 'l) (equal? (caddr l) 'l1) (equal? (cadr l) 'l))
              (if (pair? (cdddr l))
                  (if (and (equal? (cadr l) '+) (number? (cadddr l)))
                      (make_ls (car l) (cadr l) (caddr l) (cadddr l) 0)
                      (if (and (equal? (cadr l) '*) (number? (cadddr l)))
                          (make_ls (car l) (cadr l) (caddr l) (cadddr l) '=)
                          (if (and (equal? (cadr l) '+) (equal? (cadddr l) '>))
                              (make_ls (car l) (cadr l) (caddr l) 1 (cadddr l))
                              (if (or (equal? (cadr l) '>) (equal? (cadr l) '<))
                                  (make_ptgr (car l) (cadr l) (caddr l) (cadddr l))
                                  (write "RTFM!")))))
                  (if (equal? (cadr l) '+)
                      (make_ls (car l) (cadr l) (caddr l) 1 0)
                      (if (equal? (cadr l) '*)
                          (make_ls (car l) (cadr l) (caddr l) 1 '=)
                          (if (or (equal? (cadr l) '<) (equal? (cadr l) '>))
                              (make_ptgr (car l) (cadr l) (caddr l) 0)
                              (if (and (or (equal? (cadr l) 'l) (equal? (cadr l) 'l1)) (number? (caddr l)))
                                  (make_ton (car l) (cadr l) (caddr l))
                                  (make_nieme (car l) (cadr l) (caddr l)))))))
             (if (pair? (cdddr l))
                 (if (number? (cdddr l))
                     (fct_s (car l) (cadr l) (caddr l) (cadddr l))
                     (if (or (equal? (cadr l) '+) (equal? (cadr l) '-))
                         (make_add_ss (car l) (cadr l) (caddr l) (cadddr l))
                         (if (or (equal? (cadr l) '*) (equal? (cadr l) '/))
                             (make_mult_div (car l) (cadr l) (caddr l) (cadddr l))
                             (if (equal? (cadr l) '^)
                                 (puiss)
                                 (display "RTFM!")))))
                 (if (or (equal? (cadr l) '*) (equal? (cadr l) '+))
                     (fct_s (car l) (cadr l) (caddr l) 1)
                     (display "RTFM!"))))))))

(define (fct_excep)
   (let ((l (read)))
    (let ((n (len l)))
      (if (= n 5)
          (make_reste (car l) (cadr l) (caddr l))
          (if (= n 4)
              (if (equal? (cadr l) '/)
                  (make_pgcd (car l) (cadr l) (caddr l))
                  (if (equal? (cadr l) '*)
                      (make_ppcm (car l) (cadr l) (caddr l))
                      (if (equal? (cadddr l) '?)
                          (make_mediantf (car l) (cadr l) (caddr l))
                          (display "RTFM!"))))
              (if (= n 3)
                  (if (equal? (caddr l) '?)
                      (make_pair (car l) (cadr l))
                      (if (and (equal? (cadr l) 'l) (equal? (caddr l) 'n))
                          (make_autant (car l) (cadr l) (caddr l))
                          (if (equal? (cadr l) 'm)
                              (make_mirroir (car l) (cadr l))
                              (make_concat (car l) (cadr l) (caddr l)))))
                  (if (= n 2)
                      (if (equal? (cadr l) 'l)
                          (make_median (car l) (cadr l))
                          (make_fib (car l) (cadr l)))
                      (display "RTFM!"))))))))















 

