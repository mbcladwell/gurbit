(define-module (gurbit sse-curl)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;;#:use-module (gurbit sse-curl)

 ; #:export ()
  
)



(define sse-curl-program-name "curl"
  "Name of the executable sse-curl uses.")

(define sse-curl--callback #f
  "Callback function for when curl dissconnects")

(define sse-curl--args (url &optional cookie extra-args)
  "Return list of args for curl."
  (let ((args ()))
    (push "--disable" args)
    (push "--silent" args)
    (push "--location" args)
    (push "-HAccept: text/event-stream" args)
    (push "-HCache-Control: no-cache" args)
    (when cookie
      (push "--cookie" args)
      (push cookie args))
    (when extra-args
      (setf args (nconc (reverse extra-args) args)))
    (nreverse (cons url args))))

(define sse-curl--sentinel (process status)
  "Manage the end of curl process' life.
We should reconnect unless given status 204."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (funcall sse-curl--callback
               (if (string-match "exited abnormally with code \\([0-9]+\\)" status)
                   (string-to-number (match-string 1 status))
                 (format "Unknown status: %s" status))))))

(define sse-curl-retrieve (url buffer &optional callback cookie extra-args)
  (with-current-buffer buffer
    (let* ((args (sse-curl--args url cookie extra-args))
           (process (apply #'start-process "sse-curl" (current-buffer)
                           sse-curl-program-name args)))
      (set! sse-curl--callback callback)
      (set-process-query-on-exit-flag process #f)
      (setf (process-sentinel process) #'sse-curl--sentinel))))

