(define-module (gurbit urbit-metadata)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;;#:use-module (gurbit sse-curl)

 ; #:export ()
  
)

;; (use-module 'aio)
;; (use-module 'urbit-http)
;; (use-module 'urbit-graph)
;; (use-module 'urbit-helper)

(define urbit-metadata-all-subscription #f
  "Subscription id for metadata-store /all.")

(define urbit-metadata-associations ())

;; (aio-define urbit-metadata-init ()
;;   (set! urbit-metadata-hooks #f)
;;   (set! urbit-metadata-update-subscription
;;         (aio-await
;;          (urbit-http-subscribe "metadata-store"
;;                                "/all"
;;                                #'urbit-metadata-update-handler))))

;; (define urbit-metadata-update-handler (event)
;;   "Handle metadata-update EVENT."
;;   (let ((metadata-update (alist-get 'metadata-update event)))
;;     (if (not metadata-update) (urbit-log "Unknown metadata event: %s" event)
;;       (pcase metadata-update
;;         ((urbit-helper-match-key 'initial-group)
;;          (urbit-metadata-handle-associations
;;           (alist-get 'associations val)))
;;         ((urbit-helper-match-key 'associations)
;;          (urbit-metadata-handle-associations val))
;;         ((urbit-helper-match-key 'add)
;;          (urbit-log "Unhandled metadata event: add"))
;;         ((urbit-helper-match-key 'update-metadata)
;;          (urbit-log "Unhandled metadata event: update-metadata"))
;;         ((urbit-helper-match-key 'remove)
;;          (urbit-log "Unhandled metadata event: remove"))))))

;; (define urbit-metadata-handle-associations (data)
;;   (dolist (association-pair data)
;;     (let* ((association (cdr association-pair))
;;            (app-name (intern (alist-get 'app-name association)))
;;            (rid (intern (alist-get 'resource association))))
;;       (push (cons rid association)
;;             (alist-get app-name urbit-metadata-associations)))))

;; (define urbit-metadata-resource-to-graph-resource-symbol (resource)
;;   "Convert a metadata RESOURCE string or symbol to a graph resource symbol.
;; e.g. /ship/~zod/group -> zod/group"
;;   (let ((resource (if (symbolp resource)
;;                       (symbol-name resource)
;;                     resource)))
;;     (intern
;;      (substring
;;       resource
;;       (1+
;;        (seq-position resource ?~))))))

;; ;;
;; ;; Actions
;; ;;
;; (define urbit-metadata-action (action &optional ok-callback err-callback)
;;   (urbit-http-poke "metadata-push-hook"
;;                    "metadata-update"
;;                    action
;;                    ok-callback
;;                    err-callback))

;; ;; This seems to be deprecated. New channels are added with graph-create and the metadata seems to come automatically
;; (define urbit-metadata-add (app-name resource group title description date-created color module-name)
;;   (urbit-metadata-action
;;    `((add . ((group . ,group)
;;              (resource (resource . ,resource)
;;                        (app-name . ,app-name))))
;;      (metadata . ((title . ,title)
;;                   (description . ,description)
;;                   (color . ,color)
;;                   (date-created . ,date-created)
;;                   (creator . ,(urbit-helper-ensig urbit-http-ship))
;;                   (module . ,module-name)
;;                   (picture . "")
;;                   (preview . ,#f)
;;                   (vip . ""))))))

;; (define urbit-metadata-remove (app-name resource group)
;;   "Remove metadata at APP-NAME RESOURCE GROUP. Often used for deleting channels."
;;   (urbit-metadata-action
;;    `((remove (group . ,group)
;;              (resource (resource . ,resource)
;;                        (app-name . ,app-name))))))

;; (define urbit-metadata-update (association new-metadata)
;;   (let ((metadata (urbit-helper-assign
;;                    (alist-get 'metadata association)
;;                    new-metadata)))
;;     (setf (alist-get 'color metadata)
;;           (urbit-helper-ux-to-hex (alist-get 'color metadata)))
;;     (urbit-metadata-action
;;      `(,(assoc 'group association)
;;        (resource ,(assoc 'resource association)
;;                  ,(assoc 'app-name association))
;;        (metadata . ,metadata)))))

;; ;;
;; ;; urbit-metadata-associations queries
;; ;;
;; (define urbit-metadata-get-app-graphs (app &optional joined-only mine-only)
;;   (let ((resources
;;          (urbit-helper-filter-map
;;           (lambda (graph)
;;             (when (string=
;;                    (urbit-helper-alist-get-chain
;;                     'graph
;;                     'config
;;                     'metadata
;;                     graph)
;;                    "chat")
;;               (urbit-metadata-resource-to-graph-resource-symbol (alist-get 'resource graph))))
;;           (alist-get 'graph urbit-metadata-associations))))
;;     (if joined-only
;;         (seq-filter
;;          (lambda (resource)
;;            (memq resource urbit-graph-keys))
;;          resources)
;;       resources)
;;     (if mine-only
;;         (seq-filter
;;          (lambda (resource)
;;            (let* ((res (urbit-graph-symbol-to-resource resource))
;;                   (name (cdr res))
;;                   (ship (car res)))

;;              (and (not (s-prefix? "dm--" name)) (string= "littel-wolfur" ship))))
;;          resources)
;;       resources)))

