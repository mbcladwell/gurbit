#! /gnu/store/kphp5d85rrb3q1rdc2lfqc1mdklwh3qp-guile-3.0.9/bin/guile \
-e main -s
!#

 (add-to-load-path "/home/mbc/test")
 (add-to-load-path "/home/mbc/.guix-profile/share/guile/site/3.0")
 (add-to-load-path "/home/mbc/.guix-profile/share/2.2")
(add-to-load-path "/home/mbc/projects/bookstore")

(use-modules (web client)
	     (srfi srfi-19)   ;; date time
	     (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization
	     (ice-9 popen)
	     (ice-9 regex) ;;list-matches
	     (ice-9 receive)	     
	     (ice-9 string-fun)  ;;string-replace-substring
	     (ice-9 pretty-print)
	     (ice-9 textual-ports)
	     (ice-9 binary-ports)
	     (ice-9 textual-ports)
	     (ice-9 match)
	     (json)
	     (hashing sha-2)
	     (rnrs bytevectors)
	     (fibers)
	     (fibers channels)

	     )

;; Paste this into your guile interpreter!


(define (server in out)
  (let lp ()
    (match (pk 'server-received (get-message in))
      ('ping! (put-message out 'pong!))
      ('sup   (put-message out 'not-much-u))
      (msg    (put-message out (cons 'wat msg))))
    (lp)))

(define (client in out)
  (for-each (lambda (msg)
              (put-message out msg)
              (pk 'client-received (get-message in)))
            '(ping! sup)))

(run-fibers
 (lambda ()
   (let ((c2s (make-channel))
         (s2c (make-channel)))
     (spawn-fiber (lambda () (server c2s s2c)))
     (client s2c c2s))))

