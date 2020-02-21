;;; adbmal adbmals adbmal* alet alet* --- define-macro

(define-macro (adbmal . args)
  (let ((f (gensym)))
    `(lambda (,f) (,f ,@args))))
(define-macro (adbmals . args)
  (let ((f (gensym)))
    `(lambda (,f) (apply ,f ,@args))))
(define (adbmal* . args) (lambda (f) (apply f args)))

(define-macro (alet-and binds tmpvars . body)
  (define (helper bindings tmps body)
    (if (null? bindings)
        `(begin ,@body)
        (let ((bind (car bindings)))
          (let ((tmp (car tmps))
                (var (car bind))
                (val (cadr bind))
                (opt (cddr bind)))
            (if (null? opt)
                `(let ((,tmp ,val))
                   (and ,tmp ,(helper (cdr bindings) (cdr tmps) body)))
                `(let ((,tmp ,val))
                   (and (let ((,var ,tmp)) ,(car opt))
                        ,(helper (cdr bindings) (cdr tmps) body))))))))
  (helper binds tmpvars body))

(define-macro (alet-and* bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      (let ((bind (car bindings)))
        (let ((var (car bind))
              (val (cadr bind))
              (opt (cddr bind)))
          (if (null? opt)
              `(let ((,var ,val))
                 (and ,var (alet-and* ,(cdr bindings) ,@body)))
              `(let ((,var ,val))
                 (and ,(car opt) (alet-and* ,(cdr bindings) ,@body))))))))

(define-macro (wow-opt n v . r)
  (cond
   ((null? r) v)
   ((or (null? (cdr r)) (null? (cddr r)))
    (let ((t (car r))
          (ts (if (null? (cdr r)) n (cadr r))))
      `(let ((,n ,v)) (if ,t ,ts (error 'alet* "bad argument" ,n ',n ',t)))))
   (else
    (let ((t (car r)) (ts (cadr r)) (fs (caddr r)))
      `(let ((,n ,v)) (if ,t ,ts ,fs))))))

(define-macro (wow-cat! z n d . r)
  (cond
   ((null? r)
    `(let ((,n (car ,z))) (set! ,z (cdr ,z)) ,n))
   ((or (null? (cdr r)) (null? (cddr r)))
    (let ((t (car r))
          (ts (if (null? (cdr r)) n (cadr r)))
          (head (gensym))
          (tail (gensym)))
      `(let ((,n (car ,z)))
         (if ,t
             (begin (set! ,z (cdr ,z)) ,ts)
             (let lp ((,head (list ,n)) (,tail (cdr ,z)))
               (if (null? ,tail)
                   ,d
                   (let ((,n (car ,tail)))
                     (if ,t
                         (begin (set! ,z (append (reverse ,head) (cdr ,tail)))
                                ,ts)
                         (lp (cons ,n ,head) (cdr ,tail))))))))))
   (else
    (let ((t (car r)) (ts (cadr r)) (fs (caddr r)))
      `(let ((,n (car ,z)))
         (set! ,z (cdr ,z))
         (if ,t ,ts ,fs))))))

(define (wow-key z k d)
  (let ((x (car z))
        (y (cdr z)))
    (if (null? y)
        (cons d z)
        (if (eq? k x)
            y
            (let lp ((head (list x (car y))) (tail (cdr y)))
              (if (null? tail)
                  (cons d z)
                  (let ((x (car tail))
                        (y (cdr tail)))
                    (if (null? y)
                        (cons d z)
                        (if (eq? k x)
                            (cons (car y) (append head (cdr y)))
                            (lp (cons x (cons (car y) head)) (cdr y)))))))))))
(define-macro (wow-key! z m nkf d . r)
  (let ((n (car nkf))
        (k (cadr nkf))
        (f (if (null? (cddr nkf)) 'eq? (caddr nkf)))
        (x (gensym))
        (y (gensym))
        (head (gensym))
        (tail (gensym)))
    (if (null? r)
        (if (eqv? 2 m)
            ;; two in a time
            `(let ((,x (car ,z))
                   (,y (cdr ,z)))
               (if (null? ,y)
                   ,d
                   (if (,f ,k ,x)
                       (begin (set! ,z (cdr ,y)) (car ,y))
                       (let lp ((,head (list (car ,y) ,x)) (,tail (cdr ,y)))
                         (if (null? ,tail)
                             ,d
                             (let ((,x (car ,tail))
                                   (,y (cdr ,tail)))
                               (if (null? ,y)
                                   ,d
                                   (if (,f ,k ,x)
                                       (begin
                                         (set! ,z (append (reverse ,head)
                                                          (cdr ,y)))
                                         (car ,y))
                                       (lp (cons (car ,y) (cons ,x ,head))
                                           (cdr ,y))))))))))
            ;; one in a time
            `(let ((,x (car ,z))
                   (,y (cdr ,z)))
               (if (null? ,y)
                   ,d
                   (if (,f ,k ,x)
                       (begin (set! ,z (cdr ,y)) (car ,y))
                       (let lp ((,head (list ,x)) (,tail ,y))
                         (let ((,x (car ,tail))
                               (,y (cdr ,tail)))
                           (if (null? ,y)
                               ,d
                               (if (,f ,k ,x)
                                   (begin
                                     (set! ,z (append (reverse ,head) (cdr ,y)))
                                     (car ,y))
                                   (lp (cons ,x ,head) ,y)))))))))
        (let* ((t (car r))
               (ts (if (null? (cdr r)) n (cadr r)))
               (fs (if (or (null? (cdr r)) (null? (cddr r)))
                       `(error 'alet* "bad argument" ,n ',n ',t)
                       (caddr r))))
          (if (eqv? 2 m)
              ;; two in a time
              `(let ((,x (car ,z))
                     (,y (cdr ,z)))
                 (if (null? ,y)
                     ,d
                     (if (,f ,k ,x)
                         (let ((,n (car ,y)))
                           (set! ,z (cdr ,y))
                           (if ,t ,ts ,fs))
                         (let lp ((,head (list (car ,y) ,x)) (,tail (cdr ,y)))
                           (if (null? ,tail)
                               ,d
                               (let ((,x (car ,tail))
                                     (,y (cdr ,tail)))
                                 (if (null? ,y)
                                     ,d
                                     (if (,f ,k ,x)
                                         (let ((,n (car ,y)))
                                           (set! ,z (append (reverse ,head)
                                                            (cdr ,y)))
                                           (if ,t ,ts ,fs))
                                         (lp (cons (car ,y) (cons ,x ,head))
                                             (cdr ,y))))))))))
              ;; one in a time
              `(let ((,x (car ,z))
                     (,y (cdr ,z)))
                 (if (null? ,y)
                     ,d
                     (if (,f ,k ,x)
                         (let ((,n (car ,y)))
                           (set! ,z (cdr ,y))
                           (if ,t ,ts ,fs))
                         (let lp ((,head (list ,x)) (,tail ,y))
                           (let ((,x (car ,tail))
                                 (,y (cdr ,tail)))
                             (if (null? ,y)
                                 ,d
                                 (if (,f ,k ,x)
                                     (let ((,n (car ,y)))
                                       (set! ,z (append (reverse ,head) (cdr ,y)))
                                       (if ,t ,ts ,fs))
                                     (lp (cons ,x ,head) ,y)))))))))))))

;; (define (split-at-dot pair)          ;(a b . c) => (c a b)
;;   (let loop ((tail pair)             ;(a b c)   => (() a b c)
;;           (head '()))
;;     (if (pair? tail)
;;      (loop (cdr tail) (cons (car tail) head))
;;      (cons tail (reverse head)))))

(define (split-at-dot pair)             ;(a b . c) => (c a b)
  (let loop ((tail pair)                ;(a b c)   => (c a b)
             (head '()))
    (if (pair? tail)
        (if (null? (cdr tail))
            (cons (car tail) (reverse head))
            (loop (cdr tail) (cons (car tail) head)))
        (cons tail (reverse head)))))

(define (split-at-last pair)            ;(a b c) => (c a b)
  (let loop ((tail pair)
             (head '()))
    (if (null? (cdr tail))
        (cons (car tail) (reverse head))
        (loop (cdr tail) (cons (car tail) head)))))

(define (pair->gensym-pair pair) ;(a b . c) => (g1 g2 . g3)
  (if (pair? pair)
      (cons (gensym) (pair->gensym-pair (cdr pair)))
      (if (null? pair)
          '()
          (gensym))))

(define (pair->list pair)                ;(a b . c) => (a b c)
  (if (pair? pair)
      (cons (car pair) (pair->list (cdr pair)))
      (if (null? pair)
          '()
          (list pair))))

(define-macro (lambda* args . body)
  (cond
   ((pair? args)
    (let ((gen-args (pair->gensym-pair args)))
      `(lambda ,gen-args
         (let* ,(map (lambda (v g) `(,v ,g))
                     (pair->list args) (pair->list gen-args))
           ,@body))))
   ((null? args) `(lambda () ,@body))
   ((symbol? args) `(lambda ,args ,@body))))

(define (%opt-helper z clauses parent varlist pattern body)
  (cond
   ((symbol? clauses)
    (let ((t (gensym)))
      `(let ((,t ,z))
         ,(%alet-helper parent (append varlist (list (list clauses t)))
                        pattern body))))
   ((null? clauses)
    `(if (null? ,z)
         ,(%alet-helper parent varlist pattern body)
         (error 'alet* "too many arguments" ,z)))
   (else
    (let ((cl (car clauses)))
      (if (pair? cl)
          (let ((var (car cl))
                (def (cadr cl)))
            (cond
             ((eq? 'unquote var)
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               #f
                               (wow-cat! ,z ,def #f))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list def t)))
                                 pattern body))))
             ((eq? 'quote var)
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               #f
                               (wow-key! ,z 2 (,def ,cl eq?) #f))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list def t)))
                                 pattern body))))
             ((eq? 'quasiquote var)
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               #f
                               (wow-key! ,z 1 (,def ,cl eq?) #f))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list def t)))
                                 pattern body))))
             ((and (symbol? var) (null? (cddr cl)))
              (let ((y (gensym))
                    (t (gensym)))
                `(let ((,t (if (null? ,z)
                               ,def
                               (car ,z)))
                       (,y (if (null? ,z) ,z (cdr ,z))))
                   ,(%opt-helper y (cdr clauses) parent
                                 (append varlist (list (list var t)))
                                 pattern body))))
             ((symbol? var)
              (let ((y (gensym))
                    (t (gensym)))
                `(let ((,t (if (null? ,z)
                               ,def
                               (wow-opt ,var (car ,z) ,@(cddr cl))))
                       (,y (if (null? ,z) ,z (cdr ,z))))
                   ,(%opt-helper y (cdr clauses) parent
                                 (append varlist (list (list var t)))
                                 pattern body))))
             ((eq? 'unquote (car var))
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               ,def
                               (wow-cat! ,z ,(cadr var) ,@(cdr cl)))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list (cadr var) t)))
                                 pattern body))))
             ((eq? 'quote (car var))
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               ,def
                               (wow-key! ,z 2 (,(cadr var) ,var eq?)
                                         ,@(cdr cl)))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list (cadr var) t)))
                                 pattern body))))
             ((eq? 'quasiquote (car var))
              (let ((t (gensym)))
                `(let ((,t (if (null? ,z)
                               ,def
                               (wow-key! ,z 1 (,(cadr var) ,var eq?)
                                         ,@(cdr cl)))))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list (cadr var) t)))
                                 pattern body))))
             ((symbol? (car var))
              (let ((t (gensym)))
                `(let* ((,z (if (null? ,z)
                                (cons ,def ,z)
                                (wow-key ,z ,(cadr var) ,def)))
                        (,t (car ,z))
                        (,z (cdr ,z)))
                   ,(%opt-helper z (cdr clauses) parent
                                 (append varlist (list (list (car var) t)))
                                 pattern body))))
             (else
              (let ((naked-v (cadr (car var)))
                    (quote-s (car (car var)))
                    (t (gensym)))
                (cond
                 ((eq? 'quote quote-s)
                  `(let ((,t (if (null? ,z)
                                 ,def
                                 (wow-key! ,z 2 (,naked-v ,@(cdr var))
                                           ,def ,@(cddr cl)))))
                     ,(%opt-helper z (cdr clauses) parent
                                   (append varlist (list (list naked-v t)))
                                   pattern body)))
                 ((eq? 'quasiquote quote-s)
                  `(let ((,t (if (null? ,z)
                                 ,def
                                 (wow-key! ,z 1 (,naked-v ,@(cdr var))
                                           ,def ,@(cddr cl)))))
                     ,(%opt-helper z (cdr clauses) parent
                                   (append varlist (list (list naked-v t)))
                                   pattern body))))))))
          (let ((y (gensym))
                (t (gensym)))
            `(let ((,t (if (null? ,z) #f (cdr ,z)))
                   (,y (if (null? ,z) ,z (cdr ,z))))
               ,(%opt-helper y (cdr clauses) parent
                             (append varlist (list (list cl t)))
                             pattern body))))))))

(define (%alet-helper parent varlist pattern body)
  (if (null? pattern)
      (cond
       ((null? parent)
        `((lambda ,(map car varlist) ,@body) ,@(map cadr varlist)))
       ((null? (cdr parent))
        `((letrec ((,(car parent) (lambda ,(map car varlist) ,@body)))
            ,(car parent)) ,@(map cadr varlist)))
       (else
        (let ((t (gensym)))
          `((letrec ((,t
                      (lambda ,(map cadr varlist)
                      ,(%alet-helper (cadr parent)
                                     (append (caddr parent) varlist
                                             (list (list (car parent) t)))
                                     (cadddr parent)
                                     body))))
            ,t) ,@(map cadr varlist)))))
      (let ((vars (car pattern)))
        (if (list? vars)
            (if (pair? (car vars))
                (if (null? (cddr vars))
                    (cond
                     ((null? (cdar vars))
                      (let ((t (gensym)))
                        `((lambda (,t)
                            ,(%alet-helper parent
                                           (append varlist
                                                   (list (list (caar vars) t)))
                                           (cdr pattern) body))
                          ,(cadr vars))))
                     ((eq? (caar vars) 'adbmal)
                      (let ((gen-args (pair->gensym-pair (cdar vars))))
                        `(,(cadr vars)
                          (lambda ,gen-args
                            ,(%alet-helper parent
                                           (append varlist
                                                   (map (lambda (v g) `(,v ,g))
                                                        (pair->list (cdar vars))
                                                        (pair->list gen-args)))
                                           (cdr pattern) body)))))
                     ((eq? (caar vars) 'values)
                      (let ((gen-args (pair->gensym-pair (cdar vars))))
                        ;; `(receive ,gen-args ,(cadr vars)
                        ;;    ,(%alet-helper parent
                        ;;                (append varlist
                        ;;                        (map (lambda (v g) `(,v ,g))
                        ;;                             (pair->list (cdar vars))
                        ;;                             (pair->list gen-args)))
                        ;;                (cdr pattern) body))))
                        `(call-with-values (lambda () ,(cadr vars))
                           (lambda ,gen-args
                             ,(%alet-helper parent
                                            (append varlist
                                                    (map (lambda (v g) `(,v ,g))
                                                         (pair->list (cdar vars))
                                                         (pair->list gen-args)))
                                            (cdr pattern) body)))))
                     ((and (eq? (caar vars) 'cons) (= 3 (length (car vars))))
                      (let ((gen-args (pair->gensym-pair (cdar vars)))
                            (val (gensym)))
                        `(let ((,val ,(cadr vars)))
                           ((lambda ,gen-args
                              ,(%alet-helper parent
                                             (append varlist
                                                     (map (lambda (v g) `(,v ,g))
                                                          (cdar vars) gen-args))
                                             (cdr pattern) body))
                            (car ,val) (cdr ,val)))))
                     (else
                      (let ((gen-args (pair->gensym-pair (car vars))))
                        `(apply
                          (lambda ,gen-args
                            ,(%alet-helper parent
                                           (append varlist
                                                   (map (lambda (v g) `(,v ,g))
                                                        (pair->list (car vars))
                                                        (pair->list gen-args)))
                                           (cdr pattern) body))
                          ,(cadr vars)))))
                    (let ((gen-args (pair->gensym-pair (car vars))))
                      `((lambda ,gen-args
                          ,(%alet-helper parent
                                         (append varlist
                                                 (map (lambda (v g) `(,v ,g))
                                                      (pair->list (car vars))
                                                      (pair->list gen-args)))
                                         (cdr pattern) body))
                        ,@(cdr vars))))
                (if (null? (cdr vars))
                    (let ((t (gensym)))
                      `(call-with-current-continuation
                        (lambda (,t)
                          ,(%alet-helper parent
                                         (append varlist
                                                 (list (list (car vars) t)))
                                         (cdr pattern) body))))
                    (cond
                     ((eq? (car vars) 'and)
                      (let* ((vlist (map car (cdr vars)))
                             (glist (pair->gensym-pair vlist)))
                        `(alet-and ,(cdr vars) ,glist
                                   ,(%alet-helper parent
                                                  (append varlist
                                                          (map (lambda (v g) `(,v ,g))
                                                               vlist glist))
                                                  (cdr pattern) body))))
                     ((eq? (car vars) 'rec)
                      (let* ((vlist (map car (cdr vars)))
                             (glist (pair->gensym-pair vlist)))
                        `((letrec ,(cdr vars) (adbmal ,@vlist))
                          (lambda ,glist
                            ,(%alet-helper parent
                                           (append varlist
                                                   (map (lambda (v g) `(,v ,g))
                                                        vlist glist))
                                           (cdr pattern) body)))))
                     ((null? (cddr vars))
                      (let ((t (gensym)))
                        `((lambda (,t)
                            ,(%alet-helper parent
                                           (append varlist
                                                   (list (list (car vars) t)))
                                           (cdr pattern) body))
                          ,(cadr vars))))
                     ((eq? (car vars) 'opt)
                      (let ((y (gensym)))
                        `(let ((,y ,(cadr vars)))
                           ,(%opt-helper y (cddr vars)
                                         parent varlist (cdr pattern) body))))
                     ((eq? (car vars) 'adbmal)
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp))
                             (gen (pair->gensym-pair var)))
                        `(,(car tmp)
                          (lambda ,gen
                            ,(%alet-helper parent
                                           (append varlist
                                                   (map (lambda (v g) `(,v ,g))
                                                        var gen))
                                           (cdr pattern) body)))))
                     ((eq? (car vars) 'values)
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp))
                             (gen (pair->gensym-pair var)))
                        ;; `(receive ,gen ,(car tmp)
                        ;;    ,(%alet-helper parent
                        ;;                (append varlist
                        ;;                        (map (lambda (v g) `(,v ,g))
                        ;;                             var gen))
                        ;;                (cdr pattern) body))))
                        `(call-with-values (lambda () ,(car tmp))
                           (lambda ,gen
                             ,(%alet-helper parent
                                            (append varlist
                                                    (map (lambda (v g) `(,v ,g))
                                                         var gen))
                                            (cdr pattern) body)))))
                     ((and (eq? (car vars) 'cons) (= 4 (length vars)))
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp))
                             (gen (pair->gensym-pair var))
                             (val (gensym)))
                        `(let ((,val ,(car tmp)))
                           ((lambda ,gen
                              ,(%alet-helper parent
                                             (append varlist
                                                     (map (lambda (v g) `(,v ,g))
                                                          var gen))
                                             (cdr pattern) body))
                            (car ,val) (cdr ,val)))))
                     (else
                      (let* ((tmp (split-at-last vars))
                             (var (cdr tmp))
                             (gen (pair->gensym-pair var)))
                        `(apply (lambda ,gen
                                  ,(%alet-helper parent
                                                 (append varlist
                                                         (map (lambda (v g) `(,v ,g))
                                                              var gen))
                                                 (cdr pattern) body))
                                ,(car tmp)))))))
            (if (symbol? vars)
                (%alet-helper (list vars parent varlist (cdr pattern))
                              '() '() body)
                (if (and (eq? (car vars) 'opt) (pair? (cdr vars)))
                    (let ((y (gensym)))
                      `(let ((,y ,(cadr vars)))
                         ,(%opt-helper y (cddr vars)
                                       parent varlist (cdr pattern) body)))
                    (let ((tmp (split-at-dot vars)))
                      (%alet-helper (list (car tmp) parent varlist (cdr pattern))
                                    '() (cdr tmp) body))))))))

(define-macro (alet pattern . body)
  (if (symbol? pattern)
      (%alet-helper (list pattern) '() (car body) (cdr body))
      (%alet-helper '() '() pattern body)))

(define (%opt-helper* z clauses parent varlist pattern body)
  (cond
   ((symbol? clauses)
    `(let ((,clauses ,z))
       ,(%alet-helper* parent (append varlist (list clauses)) pattern body)))
   ((null? clauses)
    `(if (null? ,z)
         ,(%alet-helper* parent varlist pattern body)
         (error 'alet* "too many arguments" ,z)))
   (else
    (let ((cl (car clauses)))
      (if (pair? cl)
          (let ((var (car cl))
                (def (cadr cl)))
            (cond
             ((eq? 'unquote var)
              `(let ((,def (if (null? ,z)
                               #f
                               (wow-cat! ,z ,def #f))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list def))
                                pattern body)))
             ((eq? 'quote var)
              `(let ((,def (if (null? ,z)
                               #f
                               (wow-key! ,z 2 (,def ,cl eq?) #f))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list def))
                                pattern body)))
             ((eq? 'quasiquote var)
              `(let ((,def (if (null? ,z)
                               #f
                               (wow-key! ,z 1 (,def ,cl eq?) #f))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list def))
                                pattern body)))
             ((and (symbol? var) (null? (cddr cl)))
              (let ((y (gensym)))
                `(let ((,var (if (null? ,z)
                                 ,def
                                 (car ,z)))
                       (,y (if (null? ,z) ,z (cdr ,z))))
                   ,(%opt-helper* y (cdr clauses) parent
                                  (append varlist (list var))
                                  pattern body))))
             ((symbol? var)
              (let ((y (gensym)))
                `(let ((,var (if (null? ,z)
                                 ,def
                                 (wow-opt ,var (car ,z) ,@(cddr cl))))
                       (,y (if (null? ,z) ,z (cdr ,z))))
                   ,(%opt-helper* y (cdr clauses) parent
                                  (append varlist (list var))
                                  pattern body))))
             ((eq? 'unquote (car var))
              `(let ((,(cadr var) (if (null? ,z)
                                      ,def
                                      (wow-cat! ,z ,(cadr var) ,@(cdr cl)))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list (cadr var)))
                                pattern body)))
             ((eq? 'quote (car var))
              `(let ((,(cadr var) (if (null? ,z)
                                      ,def
                                      (wow-key! ,z 2 (,(cadr var) ,var eq?)
                                                ,@(cdr cl)))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list (cadr var)))
                                pattern body)))
             ((eq? 'quasiquote (car var))
              `(let ((,(cadr var) (if (null? ,z)
                                      ,def
                                      (wow-key! ,z 1 (,(cadr var) ,var eq?)
                                                ,@(cdr cl)))))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list (cadr var)))
                                pattern body)))
             ((symbol? (car var))
              `(let* ((,z (if (null? ,z)
                              (cons ,def ,z)
                              (wow-key ,z ,(cadr var) ,def)))
                      (,(car var) (car ,z))
                      (,z (cdr ,z)))
                 ,(%opt-helper* z (cdr clauses) parent
                                (append varlist (list (car var)))
                                pattern body)))
             (else
              (let ((naked-v (cadr (car var)))
                    (quote-s (car (car var))))
                (cond
                 ((eq? 'quote quote-s)
                  `(let ((,naked-v (if (null? ,z)
                                       ,def
                                       (wow-key! ,z 2 (,naked-v ,@(cdr var))
                                                 ,def ,@(cddr cl)))))
                     ,(%opt-helper* z (cdr clauses) parent
                                    (append varlist (list naked-v))
                                    pattern body)))
                 ((eq? 'quasiquote quote-s)
                  `(let ((,naked-v (if (null? ,z)
                                       ,def
                                       (wow-key! ,z 1 (,naked-v ,@(cdr var))
                                                 ,def ,@(cddr cl)))))
                     ,(%opt-helper* z (cdr clauses) parent
                                    (append varlist (list naked-v))
                                    pattern body))))))))
          (let ((y (gensym)))
            `(let ((,cl (if (null? ,z) #f (car ,z)))
                   (,y (if (null? ,z) ,z (cdr ,z))))
               ,(%opt-helper* y (cdr clauses) parent
                              (append varlist (list cl))
                              pattern body))))))))

(define (%alet-helper* parent varlist pattern body)
  (if (null? pattern)
      (cond
       ((null? parent) `((lambda () ,@body)))
       ((null? (cdr parent))
        `((letrec ((,(car parent) (lambda* ,varlist ,@body))) ;for duplication
            ,(car parent)) ,@varlist))
       (else
        `((letrec ((,(car parent)
                    (lambda* ,varlist   ;for duplication
                      ,(%alet-helper* (cadr parent)
                                      (append (caddr parent) varlist
                                              (list (car parent)))
                                      (cadddr parent)
                                      body))))
            ,(car parent)) ,@varlist)))
      (let ((vars (car pattern)))
        (if (list? vars)
            (if (pair? (car vars))
                (if (null? (cddr vars))
                    (cond
                     ((null? (cdar vars))
                      `((lambda ,(car vars)
                          ,(%alet-helper* parent
                                          (append varlist (car vars))
                                          (cdr pattern) body))
                        ,(cadr vars)))
                     ((eq? (caar vars) 'adbmal)
                      `(,(cadr vars)
                        (lambda ,(cdar vars)
                          ,(%alet-helper* parent
                                          (append varlist
                                                  (pair->list (cdar vars)))
                                          (cdr pattern) body))))
                     ((eq? (caar vars) 'values)
                      ;; `(receive ,(cdar vars) ,(cadr vars)
                      ;;         ,(%alet-helper* parent
                      ;;                         (append varlist
                      ;;                                 (pair->list (cdar vars)))
                      ;;                         (cdr pattern) body)))
                      `(call-with-values (lambda () ,(cadr vars))
                         (lambda ,(cdar vars)
                           ,(%alet-helper* parent
                                           (append varlist
                                                   (pair->list (cdar vars)))
                                           (cdr pattern) body))))
                     ((and (eq? (caar vars) 'cons) (= 3 (length (car vars))))
                      (let ((val (gensym)))
                        `(let ((,val ,(cadr vars)))
                           ((lambda ,(cdar vars)
                              ,(%alet-helper* parent
                                              (append varlist (cdar vars))
                                              (cdr pattern) body))
                            (car ,val) (cdr ,val)))))
                     (else
                      `(apply (lambda ,(car vars)
                                ,(%alet-helper* parent
                                                (append varlist
                                                        (pair->list (car vars)))
                                                (cdr pattern) body))
                              ,(cadr vars))))
                    `((lambda ,(car vars)
                        ,(%alet-helper* parent
                                        (append varlist (pair->list (car vars)))
                                        (cdr pattern) body))
                      ,@(cdr vars)))
                (if (null? (cdr vars))
                    `(call-with-current-continuation
                      (lambda (,(car vars))
                        ,(%alet-helper* parent
                                        (append varlist vars)
                                        (cdr pattern) body)))
                    (cond
                     ((eq? (car vars) 'and)
                      `(alet-and* ,(cdr vars)
                                  ,(%alet-helper* parent
                                                  (append varlist
                                                          (map car (cdr vars)))
                                                  (cdr pattern) body)))
                     ((eq? (car vars) 'rec)
                      `(letrec* ,(cdr vars)
                                ,(%alet-helper* parent
                                                (append varlist
                                                        (map car (cdr vars)))
                                                (cdr pattern) body)))
                     ((null? (cddr vars))
                      `((lambda (,(car vars))
                          ,(%alet-helper* parent
                                          (append varlist (list (car vars)))
                                          (cdr pattern) body))
                        ,(cadr vars)))
                     ((eq? (car vars) 'opt)
                      (let ((y (gensym)))
                        `(let ((,y ,(cadr vars)))
                           ,(%opt-helper* y (cddr vars)
                                          parent varlist (cdr pattern) body))))
                     ((eq? (car vars) 'adbmal)
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp)))
                        `(,(car tmp) (lambda ,var
                                       ,(%alet-helper* parent
                                                       (append varlist var)
                                                       (cdr pattern) body)))))
                     ((eq? (car vars) 'values)
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp)))
                        ;; `(receive ,var ,(car tmp)
                        ;;    ,(%alet-helper* parent
                        ;;                 (append varlist var)
                        ;;                 (cdr pattern) body))))
                        `(call-with-values (lambda () ,(car tmp))
                           (lambda ,var
                             ,(%alet-helper* parent
                                             (append varlist var)
                                             (cdr pattern) body)))))
                     ((and (eq? (car vars) 'cons) (= 4 (length vars)))
                      (let* ((tmp (split-at-last (cdr vars)))
                             (var (cdr tmp))
                             (val (gensym)))
                        `(let ((,val ,(car tmp)))
                           ((lambda ,var
                              ,(%alet-helper* parent
                                              (append varlist var)
                                              (cdr pattern) body))
                            (car ,val) (cdr ,val)))))
                     (else
                      (let* ((tmp (split-at-last vars))
                             (var (cdr tmp)))
                        `(apply (lambda ,var
                                  ,(%alet-helper* parent
                                                  (append varlist var)
                                                  (cdr pattern) body))
                                ,(car tmp)))))))
            (if (symbol? vars)
                (%alet-helper* (list vars parent varlist (cdr pattern))
                               '() '() body)
                (if (and (eq? (car vars) 'opt) (pair? (cdr vars)))
                    (let ((y (gensym)))
                      `(let ((,y ,(cadr vars)))
                         ,(%opt-helper* y (cddr vars)
                                        parent varlist (cdr pattern) body)))
                    (let ((tmp (split-at-dot vars)))
                      (%alet-helper* (list (car tmp) parent varlist (cdr pattern))
                                     '() (cdr tmp) body))))))))

(define-macro (alet* pattern . body)
  (if (symbol? pattern)
      (%alet-helper* (list pattern) '() (car body) (cdr body))
      (%alet-helper* '() '() pattern body)))

;;; eof