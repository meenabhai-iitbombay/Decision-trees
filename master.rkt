#lang racket

(require 2htdp/batch-io)
(require "decision_functions.rkt")
(define (my_display l)
  (if (null? l)
      (begin (newline))
      (begin ;(display (car l)) (newline)
             (my_display (cdr l)))))
;input dataset
(define (str->list-of-list str)
  (map
   (lambda (str)
      (string-split str ","))
   (cdr (string-split str))))
(define (read_my file_path)
  (call-with-input-file file_path
    (lambda (x)
      (str->list-of-list (port->string  x)))))

(provide toytrain)
(define toytrain "../data/toy_train.csv") ;formely it was .../data/toy_train.csv

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree1.dot")
(define titout "../output/titanic-dicison-tree1.dot")
(define musout "../output/mushroom-dicision-tree1.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw
  (read_my toytrain))

(provide titanic-raw)
  (define (helper old acc-new)
    (cond [(null? old) (reverse acc-new)]
          [(= (length (car old)) 9) (helper (cdr old) (cons (cdar old) acc-new))]
          [else (helper (cdr old) acc-new)]))
(define titanic-raw

  (let ([old  (read_my titanictrain)])
    (helper old '())))

(provide mushroom-raw)
(define mushroom-raw
  (read_my mushroomtrain))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (let ([new_data (map string->number data)])
  (cons (cdr new_data) (car new_data))))

;list of (features . result)
(provide toy)
(define toy
  (map format toy-raw))

(provide titanic)
(define titanic
  (map format titanic-raw))

(provide mushroom)
(define mushroom
  (map format mushroom-raw))

;=======================================================================================================
;=======================================================================================================
;=======================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (define (helper data acc)
    (if (null? data)
        acc
        (if (= (cdar data) 1)
            (helper (cdr data) (+ 1 acc))
            (helper (cdr data) acc))))
  (/ (helper data 0) (length data))
  )

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
 (let* ([p-#t (get-leaf-prob data)]
        [p-#f (- 1 p-#t)])
    (if (or (= p-#t 1) (= p-#t 0))
     0
   (-
   (+ (* p-#t (log p-#t 2))
      (* p-#f (log p-#f 2)))))
  ))

;find the difference in entropy achieved
;by applying a decision function f to the data
;every attribute has descrete valuees from 0 to something
;new_pair will have cdr as class and car as descrete value
;i will use pair for every descrete value which has car as tag of descrete value and 
;cadr as no of positive class and last as no of negative class
(provide entropy-diff)
(define (entropy-diff f data)

  ;(my_display data) (newline)
  (define (add-child child child_list acc)
  (if (null? child_list)
        
        (cond [(= (cdr child) 1) (cons (list (car child) 1 0) acc)]
              [else              (cons (list (car child) 0 1) acc)])
        
        (match (cons (car child_list) child)
          [(cons (list tag p n) (cons tag class))
           (cond [(= class 1) (append acc (cdr child_list) (list (list tag (+ p 1) n)))]
                 [else        (append acc (cdr child_list) (list (list tag p (+ n 1))))])]
          [_ (add-child child (cdr child_list) (cons (car child_list) acc))])))
  
  (define (get-child_list data child_list)
  (if (null? data)
      child_list
  (let* ([x (car data)]
         [child (cons (f (car x)) (cdr x))]);child=(tag class) ;child_list_member=(tag p n)
    (get-child_list (cdr data) (add-child child child_list '())))))
  
  (define (myget_S achild)
     (let* ([val-#t (cadr achild)]
            [val-#f (last achild)]
            [p-#t (/ val-#t (+ val-#t val-#f))]
            [p-#f (- 1 p-#t)])
       (if (or (= p-#t 1) (= p-#t 0))
           0
   (-
   (+ (* p-#t (log p-#t 2))
      (* p-#f (log p-#f 2)))))
  ))
  (define (get-WS child_list len acc)
  (if (null? child_list)
      acc
  (let* ([child (car child_list)]
        [WS    (match child
                     [(list tag p n ) (* (/ (+ p n) len) (myget_S child))])])
    (get-WS (cdr child_list) len (+ acc WS)))))
  
    (define (weighted_sum l term acc)
    (if (null? l) acc
        (weighted_sum (cdr l) term (+ (term (car l))acc))))
  (let*([child_list   (get-child_list data '())]
        [parent_S     (begin ;(my_display child_list)
                             (get-entropy data))]
        [child_S (get-WS child_list (length data) 0)]
        [entropy_diff (- parent_S child_S)])
    (begin ;(display f) (newline)
           ;(display entropy_diff) (newline) (newline)
           entropy_diff)
  )
)
;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (define (optimal-f f-with-S acc)
    (if (null? f-with-S)
        (begin ; (newline) (display "Optimal function is   ") (display (car acc))
               (car acc))
       (let ([new_acc  (if (> (cdar f-with-S) (cdr acc))
                           (car f-with-S)
                           acc)])
            (optimal-f (cdr f-with-S) new_acc))))
  (if (null? candidates) '()
    (let ([f-with-S (map (lambda (f) (cons f (entropy-diff (cdr f) data))) candidates)])
         (optimal-f (cdr f-with-S) (car f-with-S)))
  ))

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (add-remaining kids i acc)
  (match kids
    ['() (reverse acc)]
    [(cons (cons a b) l)
     (cond [(= b i) (add-remaining l      (+ i 1) (cons (cons a i)     acc))]
           [else    (add-remaining kids   (+ i 1) (cons (cons '() i)   acc))])]))
(define (add-kid kid kids acc)
  (if (null? kids)
      (cons  (cons (list (car kid)) (cdr kid)) acc)
      (match (cons kid (car kids))
        [(cons (cons 1data tag) (cons ldata tag))
         (append acc (cdr kids) (list (cons (cons 1data ldata) tag)))]
        [_ (add-kid kid (cdr kids ) (cons (car kids) acc))])))
(define (get-kids f data kids)
  (if (null? data)
      (add-remaining (sort kids (lambda (x y) (< (cdr x) (cdr y)))) 0 '())
      (let* ([x (car data)]
            [kid (cons x (f (car x)))])
        ;(display kid) (newline)
        (get-kids f (cdr data) (add-kid kid kids '())))))
            
(define (build-tree candidates data depth)
  (if (null? data)
      (DTree (string-append "0  " "this isn't path ") 'no-pathway '())
  (if (or (equal? depth 0) (null? candidates))
      ;(let * ([func (choose-f candidates data)])
      (DTree  (number->string (get-leaf-prob data))  'leaf '())
  (let* ([func (choose-f candidates data)]
         [new_candidates (remove func candidates)]
         [kids (get-kids (cdr func) data '())])
 ;kids=list of (ldata . tag) and kid=(1data . tag)
  (DTree
   (string-append  (car func) "    " (number->string (get-leaf-prob data)))
   (cons (get-leaf-prob data) (cdr func))
   (map (lambda (x) (build-tree new_candidates (car x) (- depth 1))) kids)
  )))))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  (let  ([f (DTree-func tree)])
    (cond [(equal? f 'no-pathway)  ;"Think Later-No pathway for given data"
           0]
          [(equal? f 'leaf) (begin ;(display "HI")
                                   (string->number (DTree-desc tree)))]
          [else
  (let*  ([ftest (begin ;(display "HI" )(display (car f) )
                        ((cdr f) test))]
          [lkids (DTree-kids tree)]
          [len (length lkids)])
    (cond [(>= ftest len) ;"Think Later-No pathway for given data"
              ;(car f)
                     0 ]
          [else (let ([kidtree (begin ;(display "hi"
                                       ;  )
                                      ;(display len)
                                      ;(display lkids)
                                      ;(display ftest)
                                 (list-ref lkids ftest)
                     )])
                  (if (equal? (DTree-func kidtree) 'no-pathway)
                      (begin ;(display "HI")
                             ;(car f)
                        0)
                     (make-decision kidtree test)))]))]))                  
  )
;=========================================================================================================
;=========================================================================================================
;=========================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;=========================================================================================================
;=========================================================================================================
;=========================================================================================================
