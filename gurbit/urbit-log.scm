(define-module (gurbit urbit-log)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;;#:use-module (gurbit sse-curl)

 ; #:export ()
  
)

(define urbit-log-buffer "*urbit-log*"
  "Buffer for urbit log messages.")

(define urbit-log (&rest msg-args)
  "Log to `urbit-log-buffer'.  MSG-ARGS are passed to `format'."
  (with-current-buffer (get-buffer-create urbit-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format msg-args))
    (insert "\n\n")))

