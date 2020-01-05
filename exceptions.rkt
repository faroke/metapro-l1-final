#lang racket
(require "ressource.rkt")
(provide make_reste) 
(provide make_pgcd)
(provide make_ppcm)
(provide make_fib)
(provide make_pair)
(provide make_concat)
(provide make_mirroir)
(provide make_autant)
(provide make_mediantf)
(provide make_median)
(provide make_racine)


;make reste
(define (make_reste nom param1 param2)
  (list 'define (list nom param1 param2)
        `(if (< ,param1 0)
             (+ ,param1 ,param2)
             (,nom (- ,param1 ,param2) ,param2))))
;make pgcd
(define (make_pgcd nom param1 param2)
  (list 'define (list nom param1 param2)
        `(if (= ,param1 ,param2) ,param1
             (if (> ,param1 ,param2)
                 (,nom (- ,param1 ,param2) ,param2)
                 (,nom ,param1 (- ,param2 ,param1))))))
;make ppcm
(define (make_ppcm nom param1 param2)
  (list 'define (list nom param1 param2)
        (make_pgcd 'pgcd param1 param2)
        `(/ (* ,param1 ,param2) (pgcd ,param1 ,param2))))
;pair / impair
(define (make_pair nom param)
  (list 'define (list nom param)
        `(if (= ,param 0) #t
             (if (> ,param 0)
                 (,nom (- ,param 2))
                 #f))))
;racine!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(define (make_racine nom param) (display ":'("))
;fib
(define (make_fib nom param)
  (list 'define (list nom param)
        (list 'define `(fib_aux ,param z x)
              `(if (= ,param 0)
                   (if (< z x) x z)
                   (if (< z x)
                       (fib_aux (- ,param 1) (+ z x) x)
                       (fib_aux (- ,param 1) z (+ x z)))))
        `(fib_aux ,param 1 0)))
;median #t #f
(define (make_mediantf nom param_list param)
  (list 'define (list nom param_list param)
        (make_ptgr 'ppt '< 'z 'x)
        (make_ptgr 'plgr '> 'z 'x)
        (make_autant 'autant 'l 'n)
        (list 'define `(medi_aux p g ,param_list ,param)
             `(if (pair? ,param_list)
                  (if (autant ,param_list ,param)
                      #t
                      (if (= p (+ g 1))
                          #t
                          (if (= (+ p 1) g)
                              #t
                              #f)))
                  #f))
        `(medi_aux (ppt ,param_list ,param) (plgr ,param_list ,param) ,param_list ,param)))
;element median
(define (make_median nom param_list)
  (list 'define (list nom param_list)
        (make_mediantf 'median? param_list 'n)
        (make_nieme 'nieme param_list 'n)
        (list 'define `(med_aux ,param_list n)
              `(if (pair? ,param_list)
                   (if (median? ,param_list n)
                       n
                       (med_aux ,param_list (nieme ,param_list (+ 1 n))))
                   n))
        `(med_aux ,param_list 1)))
;concat
(define (make_concat nom param1 param2)
  (list 'define (list nom param1 param2)
        (make_mirroir 'mirroir param1)
        (list 'define `(conc_aux ,param1 ,param2 l3)
              `(if (pair? ,param1)
                   (conc_aux (cdr ,param1) ,param2 (cons (car ,param1) l3))
                   (if (pair? ,param2)
                       (conc_aux ,param1 (cdr ,param2) (cons (car ,param2) l3))
                       l3)))
        `(mirroir (conc_aux ,param1 ,param2 '()))))
;mirroir
(define (make_mirroir nom param)
  (list 'define (list nom param)
        (list 'define `(mir_aux ,param i1)
              `(if (pair? ,param)
                   (mir_aux (cdr ,param) (cons (car ,param) i1))
                   i1))
        `(mir_aux ,param '())))
;autant
(define (make_autant nom param_list param)
  (list 'define (list nom param_list param)
        (make_ptgr 'ppt '< 'z 'x)
        (make_ptgr 'plgr '> 'z 'x)
        `(if (pair? ,param_list)
             (if (= (plgr ,param_list ,param) (ppt ,param_list ,param))
                 #t
                 #f)
             #t)))


