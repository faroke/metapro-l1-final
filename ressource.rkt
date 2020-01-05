#lang racket
(provide make_ls)
(provide make_ptgr)
(provide make_add_ss)
(provide fct_s)
(provide make_mult_div)
(provide make_ton)
(provide make_nieme)
(provide puiss)

(define (puiss)
  (list 'define `(puiss p n)
               (list 'define `(puiss_aux p n stock)
                     `(if (= p 0)
                          stock
                          (puiss_aux (- p 1) n (* stock n))))
               `(puiss_aux p n 1)))
(define (make_nbele)
  (list 'define `(nbele l)
        (list 'define `(nb_aux l n)
              `(if (pair? l)
                   (nb_aux (cdr l) (+ n 1))
                   n))
        `(nb_aux l 0)))

(define (fct_s nom oper para p) ;FCT somme/produit fcts num
  (list 'define (list nom para)
        (puiss)
        (list 'define (list `aux para `c)
              `(if (= ,para 0)
                   c
                   ,(list `aux `(- ,para 1) `(,oper ,(list `puiss p para) c))))
	(if (equal? oper '+)
        `(aux ,para 0)
	`(aux ,para 1))))

(define (make_ls nom oper param p sym) ;FCT somme produit listes
  (list 'define (list nom param)
        (puiss)
        (list 'define (list `ps_aux param `c)
              (list 'if (list 'pair? param)
                    (if (equal? sym '0)
                        `(ps_aux (cdr ,param) (,oper c (puiss ,p (car ,param))))
                        (make_sp oper param sym p))
                    `c))
        (fin oper param)))
(define (fin oper param) ;pour somme produit (fin de la fct) listes
  (if (equal? oper '*)
      (list 'ps_aux param 1)
      (list 'ps_aux param 0)))
(define (make_sp oper param sym p) ;pour produit / somme non nul listes
  (list 'if `(,sym 0 (car ,param))
        `(ps_aux (cdr ,param) c)
        `(ps_aux (cdr ,param) (,oper (puiss ,p (car ,param)) c))))

(define (make_p param1 oper) ;pour ppt normal etc
  (list 'if (list oper `c `(car ,param1))
        (list 'ptgr_aux `(cdr ,param1) `c)
        (list 'ptgr_aux `(cdr ,param1) `(car ,param1))))
(define (make_p1 param1 param2 oper) ;pour ppt que n etc
  (list 'if (list oper `(car ,param1) param2)
        (list 'ptgr_aux `(cdr ,param1) param2 `(+ c 1))
        (list 'ptgr_aux `(cdr ,param1) param2 `c)))
(define (par nom param1 param2) ;pour ppt (soit ppt soit ppt que n)
  (if (number? param2)
      `,(list nom param1)
      `,(list nom param1 param2)))
(define (par2 nom param1 param2) ;pour ppt (soit ppt soit ppt que n)
  (if (number? param2)
      `,(list `ptgr_aux param1 `c)
      `,(list `ptgr_aux param1 param2 `c)))
(define (make_ptgr nom oper param1 param2)  ;FCT ppt plgr
  (list 'define (par nom param1 param2)
        (list 'define (par2 nom param1 param2)
              (list 'if (list 'pair? param1)
                    (if (number? param2) 
                        (make_p param1 oper)
                        (make_p1 param1 param2 oper))
                    `c))
        (if (number? param2) 
            (list 'ptgr_aux param1 `(car ,param1))
            (list 'ptgr_aux param1 param2 0))))

(define (make_add_ss nom oper para1 para2) ;FCT addition/soustraction +1 -1
  (list 'define (list nom para1 para2)
        `(if (= ,para2 0) ,para1
             ,(list nom `(,oper ,para1 1) `(- ,para2 1)))))

(define (make_mult_div nom oper param1 param2 born sym) ;FCT multiplication / division + et - num
  (let ((inv (if (equal? oper '*)
                 1
                 param2))
        (inv2 (if (equal? oper '*)
                  param2
                  1)))
    (list 'define (list nom param1 param2)
    (list 'define (list `aux param1 param2 `c)
             `(if (,sym ,param1 ,born)
                  c
                  ,(list `aux `(- ,param1 ,inv) param2 `(+ c ,inv2))))
          `(aux ,param1 ,param2 0))))

(define (make_ton nom param_list nb) ;FCT singl/double/triple TON listes
  (list 'define (list nom param_list)
        (make_nbele)
        `(if (= (nbele ,param_list) ,nb)
             #t
             #f)))

(define (make_nieme nom param_list nb) ;FCT nieme element de la liste
  (list 'define (list nom param_list nb)
        `(if (pair? l)
             (if (= ,nb 1)
                 (car ,param_list)
                 (,nom (cdr ,param_list) (- ,nb 1)))
             '())))
