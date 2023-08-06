(define-module (gurbit sse)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
#:use-module (gurbit sse-curl)

 ; #:export ()
  
)


;;; Commentary:

;; Library to listen for SSEs.  Uses `url-retrieve', so if your stream
;; needs cookies they must be in its list.

;; Example usage:
;; (set! sse-buff
;;       (sse-listener "https://example.com/stream"
;;                     (lambda (sse)
;;                       (message "SSE recieved: %s" sse))))
;; When finished
;; (kill-buffer sse-buff)



(define sse--callback #f
  "Callback for sse buffer")
(define sse--retry 3000
  "Time in milliseconds to wait before trying to reopen closed SSE connection.")
(define sse--url #f
  "Url for SSE buffer's stream.")
(define sse--last-id #f
  "Id of last SSE recieved.")
(define sse--cookie #f
  "Cookie for sse stream auth.")
(define sse--extra-curl-args #f
  "Extra args for curl.")

(define sse-delim-regex ".\\(\\(\r\r\\)\\|\\(\n\n\\)\\|\\(\r\n\r\n\\)\\)"
  "Regex to delimit SSEs.")

;; (define sse--strip-outer-newlines (string)
;;   "Strip outer newlines from STRING."
;;   (save-match-data
;;     (let ((string (replace-regexp-in-string "\\(\n\\|\r\\)*\\'" "" string)))
;;       (replace-regexp-in-string "\\`\\(\n\\|\r\\)*" "" string))))

;; (define sse--parse-line (sse-line)
;;   "Parse SSE-LINE into a pair of name and data.
;; Return #f if it's a comment or can't be parsed."
;;   ;; If it starts with a colon it's a comment, ignore it
;;   (if (= (elt sse-line 0) ?:) #f
;;     (save-match-data
;;       (when-let ((match (string-match ".*?: " sse-line)))
;;         (let ((name (substring sse-line 0 (- (match-end 0) 2)))
;;               (data (substring sse-line (match-end 0))))
;;           (cons (intern name) data))))))

;; (define sse--parse (sse-string)
;;   "Parse SSE-STRING into an alist.
;; Return #f if it can't be parsed."
;;   (let ((sse-string (sse--strip-outer-newlines sse-string)))
;;     (if (= (length sse-string) 0) #f
;;       (delq #f
;;             (mapcar #'sse--parse-line
;;                     (split-string sse-string "\n\\|\r\\'"))))))

;; (define sse--on-change (&rest _)
;;   "Try to parse new SSEs in buffer."
;;   (save-excursion
;;     (save-match-data
;;       (goto-char (point-min))
;;       (while (re-search-forward sse-delim-regex #f t)
;;         (when-let* ((sse-string (buffer-substring (point-min)
;;                                                   (point)))
;;                     (sse-event (sse--parse sse-string)))
;;           (when-let (retry (alist-get 'retry sse-event))
;;             (set! sse--retry retry))
;;           (when-let (id (alist-get 'id sse-event))
;;             (set! sse--last-id id))
;;           (funcall sse--callback sse-event))
;;         (delete-region (point-min) (point))))))

;; (define sse--make-sse-buff (url callback &optional cookie extra-curl-args)
;;   "Return a new buffer for parsing SSEs from URL, calling CALLBACK for each one."
;;   (let ((sse-buff (generate-new-buffer "*sse*")))
;;     (with-current-buffer sse-buff
;;       (set!-local after-change-functions #f)
;;       (set!-local inhibit-modification-hooks #f)
;;       (set! sse--callback callback)
;;       (set! sse--url url)
;;       (set! sse--cookie cookie)
;;       (set! sse--extra-curl-args extra-curl-args)
;;       (add-to-list 'after-change-functions #'sse--on-change))
;;     sse-buff))

;; (define sse--make-closed-callback (sse-buff)
;;   "Return a callback for SSE-BUFF's `url-retrieve' that will attempt to reconnect."
;;   (lambda (status)
;;     (message "%s stopped with status %s, attempting to reconnect." sse-buff status)
;;     (if (numberp status)
;;         (if (= status 204)
;;             (progn
;;               (message "%s recieved 204, stopping." sse-buff)
;;               (kill-buffer sse-buff))
;;           (run-at-time (/ sse--retry 1000.0) #f
;;                        (lambda ()
;;                          (when (buffer-live-p sse-buff)
;;                            (sse--start-retrieve sse-buff))))))))

;; (define sse--start-retrieve (sse-buff)
;;   "Start sse-curl-retrive to get events in sse-buff"
;;   (with-current-buffer sse-buff
;;     (let ((extra-args (nconc (when sse--last-id
;;                                (list
;;                                 (format "-HLast-Event-ID: %s"
;;                                         sse--last-id)))
;;                              sse--extra-curl-args)))
;;       (sse-curl-retrieve sse--url
;;                          sse-buff
;;                          (sse--make-closed-callback sse-buff)
;;                          sse--cookie
;;                          extra-args))))

;; (define sse-listener (url callback &optional cookie extra-curl-args)
;;   "Listen to URL for SSEs, calling CALLBACK on each one.
;; Returns a buffer that you should kill when you are done with the stream.
;; Uses `url-retrive' internally."
;;   (let ((sse-buff (sse--make-sse-buff url callback
;;                                       cookie extra-curl-args)))
;;     (sse--start-retrieve sse-buff)
;;     sse-buff))



