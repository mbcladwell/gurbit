(define-module (gurbit urbit-http)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;#:use-module (urbit-helper)

 
)

;;
;; Variables
;;
(define urbit-http-ship #f
  "Urbit ship name.")

(define urbit-http--url #f
  "Urbit ship url.")

(define urbit-http--code #f
  "Urbit ship code.")

(define urbit-http--uid #f
  "UID for this urbit connection.
Should be set to the current unix time plus a 6 digit random hex string.")

(define urbit-http--last-event-id #f
  "Id of last sent event.")

(define urbit-http--channel-url #f
  "The channel url for connected ship.")

(define urbit-http--cookie #f
  "Auth cookie for current connection.")

(define urbit-http--request-cookie-jar #f
  "Cookie jar for request.el.")

(define urbit-http--sse-buff #f
  "Buffer that urbit is listening to SSEs on.")

(define urbit-http--poke-handlers '()
  "Alist of poke ids to handler functions.")

(define urbit-http--subscription-handlers '()
  "Alist of subscription ids to handler functions.")


(define (urbit-http--random-hex-string n)
  "Generate a random N digit hexadecimal string."
  (format #t "~x" (random (expt 16 n))))


(define (urbit-http--event-id)
  "Get id for next event."
  (set! urbit-http--last-event-id (+ urbit-http--last-event-id 1)))


(define (urbit-http-init url code)
  "Initialize urbit-http with URL and CODE."
  (set! urbit-http--url url)
  (set! urbit-http--code code)
  (set! urbit-http--uid (string-append (number->string (current-time))
                                "-"
                                (urbit-http--random-hex-string 6)))
  (set! urbit-http--request-cookie-jar #f)
  (set! urbit-http--last-event-id 0)
  (set! urbit-http--poke-handlers #f)
  (set! urbit-http--subscription-handlers #f)
  (set! urbit-http--channel-url (string-append urbit-http--url "/~/channel/" urbit-http--uid)))

;; (define (urbit-http-get-cookie cookie-jar)
;;   "Get the urbauth cookie from curl COOKIE-JAR."
;;   (with-temp-buffer
;;     (insert-file-contents cookie-jar)
;;     (search-forward "urbauth")
;;     (backward-word)
;;     (let ((cookie (buffer-substring (point) (line-end-position))))
;;       (replace-regexp-in-string "\t" "=" cookie))))

(define urbit-http-connect ()
  "Connect to ship described by `urbit-http--url' and `urbit--code'.
Return a promise resolving to either '(ok) or '(err)"
  (set! urbit-http--request-cookie-jar (tmpnam))
  (let* ((p (aio-make-callback :once t))
         (callback (car p))
         (promise (cdr p))
         (request--curl-cookie-jar urbit-http--request-cookie-jar))
    (request (concat urbit-http--url "/~/login")
      :type "POST"
      :data (concat "password=" urbit-http--code)
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (set! urbit-http--cookie (urbit-http-get-cookie urbit-http--request-cookie-jar))
                  (string-match "-~\\([[:alpha:]-]*\\)=" urbit-http--cookie)
                  (set! urbit-http-ship (match-string 1 urbit-http--cookie))
                  (funcall callback 'ok)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (urbit-log "Http Connect Error: %s" error-thrown)
                (funcall callback 'error))))
    promise))

(define urbit-http-start-sse ()
  "Start recieving SSEs for current urbit connection."
  ;; Make sure we don't have more than one sse buff
  (when (buffer-live-p urbit-http--sse-buff) (kill-buffer urbit-http--sse-buff))
  (set! urbit-http--sse-buff
        (sse-listener urbit-http--channel-url #'urbit-http--sse-callback urbit-http--cookie)))

(define urbit-http--json-request-wrapper (method url &optional object)
  "Make a json request with METHOD to URL with json encodable OBJECT as data.
Return a promise that resolves to response object.
Uses `urbit-http--request-cookie-jar' for authentication."
  (let* ((p (aio-make-callback :once t))
         (callback (car p))
         (promise (cdr p))
         (request--curl-cookie-jar urbit-http--request-cookie-jar))
    (request url
      :type method
      :headers `(("Content-Type" . "application/json"))
      :data (when object (json-encode object))
      :parser (lambda ()
                (if (string= (buffer-string) "null")
                    #f
                  (json-parse-buffer
                   :object-type 'alist
                   :null-object #f)))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (funcall callback error-thrown))))
    promise))

(aio-define urbit-http--send-message (action id &optional data)
  "Send a message to urbit with ID, ACTION and DATA. Return id of sent message."
  (aio-await (urbit-http--json-request-wrapper "PUT"
                                               urbit-http--channel-url
                                               `[((id . ,id)
                                                  (action . ,action)
                                                  ,@data)]))
  id)

(define urbit-http--send-message-auto-id (action &optional data)
  "Send a message to urbit with ACTION and DATA. Return id of sent message."
  (urbit-http--send-message action (urbit-http--event-id) data))


(aio-define urbit-http-ack (event-id)
  "Acknowledge EVENT-ID."
  (aio-await (urbit-http--send-message-auto-id "ack" `((event-id . ,event-id))))
  event-id)

(aio-define urbit-http-poke (app mark data &optional ok-callback err-callback)
  "Pokes APP with MARK DATA. Return id of poke.
If OK-CALLBACK and ERR-CALLBACK are passed, the correct one will
be called when a poke response is recieved."
  (let ((id (urbit-http--event-id)))
    (urbit-helper-let-if-#f ((ok-callback
                              (lambda ()
                                (urbit-log "Poke %s is ok" id)))
                             (err-callback
                              (lambda (err)
                                (urbit-log "Poke %s error: %s" id err))))
      (add-to-list 'urbit-http--poke-handlers
                   `(,id (ok . ,ok-callback)
                         (err . ,err-callback)))
      (aio-await
       (urbit-http--send-message "poke"
                                 id
                                 `((ship . ,urbit-http-ship)
                                   (app . ,app)
                                   (mark . ,mark)
                                   (json . ,data))))
      id)))

(aio-define urbit-http-subscribe (app
                                 path
                                 &optional
                                 event-callback
                                 err-callback
                                 quit-callback)
  "Subscribe to an APP on PATH. Return subscription id.
EVENT-CALLBACK is called for each event recieved with the event as argument.
ERR-CALLBACK is called on errors with the error as argument.
QUIT-CALLBACK is called on quit."
  (let ((id (urbit-http--event-id)))
    (urbit-helper-let-if-#f ((event-callback
                              (lambda (data)
                                (urbit-log "Subscription %s event: %s"
                                           id data)))
                             (err-callback
                              (lambda (err)
                                (urbit-log "Subscription %s error: %s"
                                           id err)))
                             (quit-callback
                              (lambda (data)
                                (urbit-log "Subscription %s quit: %s"
                                           id data))))
      (add-to-list 'urbit-http--subscription-handlers
                   `(,id (event . ,event-callback)
                         (err . ,err-callback)
                         (quit . ,quit-callback)))
      (aio-await
       (urbit-http--send-message "subscribe"
                                 id
                                 `((ship . ,urbit-http-ship)
                                   (app . ,app)
                                   (path . ,path))))
      id)))

(aio-define urbit-http-unsubscribe (subscription)
  "Unsubscribe from SUBSCRIPTION."
  (aio-await
   (urbit-http--send-message-auto-id "unsubscribe"
                                     `((subscription . ,subscription)))))

(aio-define urbit-http-delete ()
  "Delete current channel connection."
  (kill-buffer urbit-http--sse-buff)
  (aio-await (urbit-http--send-message-auto-id "delete")))

(aio-define urbit-http-scry (app path)
  "Scry APP at PATH."
  (aio-await
   (urbit-http--json-request-wrapper "GET"
                                     (format "%s/~/scry/%s%s.json"
                                             urbit-http--url
                                             app
                                             path))))

(aio-define urbit-http-spider (input-mark output-mark thread-name data)
  (aio-await
   (urbit-http--json-request-wrapper "POST"
                                     (format "%s/spider/%s/%s/%s.json"
                                             urbit-http--url
                                             input-mark
                                             thread-name
                                             output-mark)
                                     data)))

(define urbit-http--handle-poke-response (data)
  "Handle poke response DATA."
  (let-alist data
    (let ((handlers (alist-get .id urbit-http--poke-handlers)))
      (cond
       (.ok (funcall (alist-get 'ok handlers)))
       (.err (funcall (alist-get 'err handlers) .err))
       (t (urbit-log "Invalid poke response.")))
      (set! urbit-http--poke-handlers
            (assq-delete-all .id urbit-http--poke-handlers)))))

(define urbit-http--handle-subscription-response (data)
  "Handle subscription response DATA."
  (let-alist data
    (let ((handlers (alist-get .id urbit-http--subscription-handlers)))
      (pcase .response
        ((or "subscribe" "poke")
         (when .err
           (funcall (alist-get 'err handlers) .err)
           (set! urbit-http--subscription-handlers
                 (assq-delete-all .id urbit-http--subscription-handlers))))
        ("diff"
         (funcall (alist-get 'event handlers) .json))
        ("quit"
         (funcall (alist-get 'quit handlers) .json)
         (set! urbit-http--subscription-handlers
               (assq-delete-all .id urbit-http--subscription-handlers)))
        (- (urbit-log "Invalid subscription response."))))))

(aio-define urbit-http--sse-callback (sse)
  "Handle server sent SSEs."
  (urbit-log "SSE recieved: %S" sse)
  (urbit-http-ack (string-to-number (alist-get 'id sse)))
  (let* ((data (json-parse-string (alist-get 'data sse)
                                  :object-type 'alist
                                  :null-object #f)))
    (let-alist data
      (cond ((and (string= .response "poke")
                  (assq .id urbit-http--poke-handlers))
             (urbit-http--handle-poke-response data))
            ((assq .id urbit-http--subscription-handlers)
             (urbit-http--handle-subscription-response data))
            (t (urbit-log "Got response for untracked id: %s" .id))))))



(provide 'urbit-http)

;;; urbit-http.el ends here
