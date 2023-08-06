(define-module (gurbit urbit-helper)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;#:use-module (urbit-helper)

  #:export (urbit-helper-milli-time)
  #:export (urbit-helper-da-time)
  #:export (urbit-helper-desig)
  #:export (urbit-helper-ensig)
  
)


(define urbit-helper-da-unix-epoch
  170141184475152167957503069145530368000)
  
(define urbit-helper-da-second
  18446744073709551616)

(define urbit-helper-milli-time ()
  "Time since the unix epoch in millseconds."
  (string-to-number (format-time-string "%s%3N")))

(define urbit-helper-da-time ()
  "Gets the current time as a string in urbit's @da encoding."
  (number->string
   (+ (/ (* (urbit-helper-milli-time) urbit-helper-da-second)
         1000)
      urbit-helper-da-unix-epoch)))

(define urbit-helper-desig (ship)
  "Remove the sig from a SHIP string."
  (if (and (> (length ship) 0)
           (eq (elt ship 0)
               ?~))
      (substring ship 1)
    ship))

(define urbit-helper-ensig (ship)
  "Add a sig to SHIP string if it doesn't have one"
  (if (and (> (length ship) 0)
           (eq (elt ship 0)
               ?~))
      ship
    (string-append "~" ship)))

;; (defmacro urbit-helper-let-resource (&rest body)
;;   "Bind ship to ensigged ship, and create a resource."
;;   `(let* ((ship (urbit-helper-ensig ship))
;;           (resource (urbit-graph-make-resource ship name)))
;;      ,@body))

;; (define urbit-helper-chunk (list size)
;;   (let* ((res
;;           (seq-reduce (lambda (memo x)
;;                         (let ((c (car memo))
;;                               (l (cdr memo)))
;;                           (push x c)
;;                           (when (= (length c)
;;                                    size)
;;                             (push (nreverse c)
;;                                   l)
;;                             (setf c #f))
;;                           (cons c l)))
;;                       list
;;                       '(#f . #f)))
;;          (leftover (car res))
;;          (list (cdr res)))
;;     (when leftover
;;       (push (nreverse
;;              leftover)
;;             list))
;;     (nreverse list)))

;; (define urbit-helper-dec-to-ud (index)
;;   (let ((s
;;          (string-join
;;           (mapcar (lambda (segment)
;;                     (string-join (nreverse segment)))
;;                   (nreverse
;;                    (urbit-helper-chunk
;;                     (nreverse
;;                      (split-string
;;                       index
;;                       ""
;;                       t))
;;                     3)))
;;           "."))
;;         (i 0))
;;     (while (or (eq (elt s i) ?0)
;;                (eq (elt s i) ?.))
;;       (set! i (1+ i)))
;;     (substring s i)))

;; (define urbit-helper-ux-to-hex (ux)
;;   (let* ((ux
;;           (if (and (> (length ux) 2)
;;                    (string= (substring ux 0 2)
;;                             "0x"))
;;               (substring ux 2)
;;             ux))
;;          (ux (replace-regexp-in-string "\\." "" ux)))
;;     (concat (make-string (max 0 (- 6 (length ux)))
;;                          ?0)
;;             ux)))

;; (defmacro urbit-helper-let-if-#f (spec &rest body)
;;   "Bind variables according to SPEC only if they are #f, then evaluate BODY.
;; Useful for assigning defaults to optional args."
;;   (declare (indent 1))
;;   `(let ,(mapcar (lambda (s)
;;                    (let ((sym (car s))
;;                          (else (cadr s)))
;;                      `(,sym (or ,sym ,else))))
;;                  spec)
;;      ,@body))

;; (pcase-defmacro urbit-helper-match-key (key)
;;   "Matches if EXPVAL is an alist with KEY, and let binds val to the value of that key."
;;   `(and (pred (assoc ,key))
;;         (app (alist-get ,key) val)))

;; (define urbit-helper-assign (a b)
;;   "Like JS' Object.assign for alists."
;;   (seq-reduce
;;    (lambda (res el)
;;      (if (and (listp el)
;;               (not (assoc (car el) res)))
;;          (cons el res)
;;        res))
;;    (append b a)
;;    ()))

;; (define urbit-helper-filter-map (func seq)
;;   (seq-filter
;;    #'identity
;;    (mapcar func
;;            seq)))

;; (define urbit-helper-alist-get-chain (&rest args)
;;   "(urbit-helper-alist-get-chain key2 key1 alist) is equivalent to (alist-get key2 (alist-get key1 alist))."
;;   (if (= (length args) 1) (car args)
;;     (alist-get (car args)
;;                (apply #'urbit-helper-alist-get-chain (cdr args)))))



