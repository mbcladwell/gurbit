(define-module (gurbit urbit-graph)

;#:use-module (request)
;#:use-module (aio)
;#:use-module (sse)
;#:use-module (cl-macs)
;#:use-module (urbit-log)
;;#:use-module (gurbit sse-curl)

 ; #:export ()
  
)

;; (use-module 'request)
;; (use-module 'aio)
;; (use-module 'urbit-http)
;; (use-module 'urbit-log)
;; (use-module 'urbit-helper)



;;
;; Variables
;;
(define urbit-graph-update-subscription #f
  "Urbit-http graph-store /update subscription")

(define urbit-graph-hooks '()
  "Alist of resource symbols to hook objects for watching graphs.")

(define urbit-graph-keys '()
  "List of graph resource symbols.")

;;
;; Functinos
;;
;; (aio-define urbit-graph-init ()
;;   (set! urbit-graph-hooks #f)
;;   (let ((subscription-promise
;;          (urbit-http-subscribe "graph-store"
;;                                "/updates"
;;                                #'urbit-graph-update-handler))
;;         (keys-promise (urbit-graph-get-keys)))
;;     (set! urbit-graph-update-subscription
;;           (aio-await subscription-promise))
;;     (set! urbit-graph-keys (aio-await keys-promise))))

;; (define urbit-graph-index-symbol-to-list (symbol)
;;   (mapcar #'string-to-number
;;           (split-string (symbol-name symbol) "/" t)))

;; (define urbit-graph-resource-to-symbol (resource)
;;   "Turn a RESOURCE object into a symbol."
;;   (intern (concat (alist-get 'ship resource)
;;                   "/"
;;                   (alist-get 'name resource))))

;; (define urbit-graph-symbol-to-resource (symbol)
;;   (let ((split (split-string (symbol-name symbol) "/")))
;;     (cons (car split)
;;           (cadr split))))

;; (define urbit-graph-fix-indexes (nodes)
;;   "Fix indexes of add-nodes node list by stripping the slashes and converting to numbers."
;;   (dolist (node nodes)
;;     (setf (car node)
;;           (car (last (urbit-graph-index-symbol-to-list (car node)))))
;;     (let ((children (alist-get 'children (cdr node))))
;;       (setf children (urbit-graph-fix-indexes children)))))

;; (define urbit-graph-index-to-ud (index)
;;   "Convert all nums in INDEX to UDs."
;;   (string-join
;;    (mapcar (lambda (x)
;;              (if (= (length x) 0) x
;;                (urbit-helper-dec-to-ud
;;                 x)))
;;            (split-string index
;;                          "/"))
;;    "/"))
;; ;;
;; ;; Event handling
;; ;;
;; (define urbit-graph-update-handler (event)
;;   "Handle graph-update EVENT."
;;   (let ((graph-update (alist-get 'graph-update event)))
;;     (if (not graph-update) (urbit-log "Unknown graph event: %s" event)
;;       (let* ((update-type (caar graph-update))
;;              (update (cdar graph-update)))
;;           (pcase update-type
;;             ((or 'add-nodes 'remove-nodes)
;;              (urbit-graph-update-nodes-handler update-type update))
;;             ('add-graph
;;              (push (urbit-graph-resource-to-symbol (alist-get 'resource update))
;;                    urbit-graph-keys))
;;             ('remove-graph
;;              (set! urbit-graph-keys
;;                    (remq (urbit-graph-resource-to-symbol
;;                           update)
;;                          urbit-graph-keys)))
;;             (- (urbit-log "Ignoring graph-update %s" update-type)))))))

;; (define urbit-graph-update-nodes-handler (update-type update)
;;   (let-alist update
;;     (let* ((resource-symbol (urbit-graph-resource-to-symbol .resource))
;;            (hook (urbit-helper-alist-get-chain update-type
;;                                                resource-symbol
;;                                                urbit-graph-hooks)))
;;       (funcall
;;        hook
;;        (pcase update-type
;;          ('add-nodes
;;           (let ((nodes (alist-get 'nodes update)))
;;             (urbit-graph-fix-indexes nodes)
;;             nodes))
;;          ('remove-nodes
;;           (append (alist-get 'indices update) #f)))))))

;; (define urbit-graph-watch (ship name add-callback &optional remove-callback)
;;   "Watch graph at SHIP NAME. When an add-nodes event is recieved, ADD-CALLBACK will be called with a list of nodes.
;; When a remove-nodes event is recieved, REMOVE-CALLBACK will be called with a list of indexes.
;; Return a resource symbol you can use with `urbit-graph-stop-watch'."
;;   (let ((resource (urbit-graph-resource-to-symbol `((ship . ,ship)
;;                                                     (name . ,name)))))
;;     (urbit-helper-let-if-#f ((remove-callback (lambda (indexes)
;;                                                  (urbit-log "Got remove-nodes for indexes: %s" indexes))))
;;       (add-to-list 'urbit-graph-hooks
;;                    `(,resource (add-nodes . ,add-callback)
;;                                (remove-nodes . ,remove-callback))))

;;     resource))

;; (define urbit-graph-stop-watch (resource)
;;   "Stop watching RESOURCE symbol."
;;   (set! urbit-graph-hooks
;;         (assq-delete-all resource urbit-graph-hooks)))

;; ;;
;; ;; Constructors
;; ;;
;; (define urbit-graph-make-post (contents &optional parent-index index)
;;   "Create a new post with CONTENTS.
;; CONTENTS is a vector or list of content objects.
;; PARENT-INDEX is the index of its parent node
;; INDEX is the index of this node. If not passed, it will be auto-generated."
;;   (let ((contents (if (vectorp contents)
;;                       contents
;;                     (vconcat contents))))
;;     `((index . ,(concat (or parent-index "")
;;                         (or index
;;                             (concat "/" (urbit-helper-da-time)))))
;;       (author . ,(urbit-helper-ensig urbit-http-ship))
;;       (time-sent . ,(urbit-helper-milli-time))
;;       (signatures . [])
;;       (contents . ,contents)
;;       (hash . #f))))

;; (define urbit-graph-index-children (nodes)
;;   "Convert all children nodes of a node into pairs of their index and node."
;;   (mapcar
;;    (lambda (node)
;;      (urbit-log "node: %s" node)
;;      (setf (alist-get 'children node)
;;            (urbit-graph-index-children (alist-get 'children node)))
;;      (cons (car (last (split-string (urbit-helper-alist-get-chain 'index
;;                                                                   'post
;;                                                                   node)
;;                                     "/")))
;;            node))
;;    nodes))

;; (define urbit-graph-make-node (post &optional children)
;;   "Make an urbit graph node with POST and CHILDREN."
;;   `((post . ,post)
;;     (children . ,(urbit-graph-index-children children))))

;; (define urbit-graph-make-resource (ship name)
;;   "Make a resource object from SHIP NAME."
;;   `(resource (ship . ,ship)
;;              (name . ,name)))

;; ;;
;; ;; Actions
;; ;;
;; (define urbit-graph-store-action (action &optional ok-callback err-callback)
;;   (urbit-http-poke "graph-store"
;;                    "graph-update-2"
;;                    action
;;                    ok-callback
;;                    err-callback))

;; (define urbit-graph-view-action (thread-name action)
;;   (urbit-http-spider "graph-view-action"
;;                      "json"
;;                      thread-name
;;                      action))

;; (define urbit-graph-hook-action (action &optional ok-callback err-callback)
;;   (urbit-http-poke "graph-push-hook"
;;                    "graph-update-2"
;;                    action
;;                    ok-callback
;;                    err-callback))

;; ;;
;; ;; View Actions
;; ;;
;; (define urbit-graph-join (ship name)
;;   "Join graph at SHIP NAME."
;;   (urbit-helper-let-resource
;;    (urbit-graph-view-action "graph-join"
;;                             `((join ,resource
;;                                     (ship . ,ship))))))

;; (define urbit-graph-delete (name)
;;   "Delete graph at NAME from your ship."
;;   (let ((resource
;;          (urbit-graph-make-resource (urbit-helper-ensig urbit-http-ship)
;;                                     name)))
;;     (urbit-graph-view-action "graph-delete"
;;                              `((delete ,resource)))))

;; (define urbit-graph-leave (ship name)
;;   "Leave graph at SHIP NAME."
;;   (urbit-helper-let-resource
;;    (urbit-graph-view-action "graph-leave"
;;                             `((leave ,resource)))))


;; ;; TODO: what is to
;; (define urbit-graph-groupify (ship name to-path)
;;   (urbit-helper-let-resource
;;    (urbit-graph-view-action "graph-groupify"
;;                             `((groupify ,resource (to . to))))))

;; ;;
;; ;; Store Actions
;; ;;
;; (define urbit-graph-add (ship name graph mark)
;;   "At SHIP NAME, add GRAPH with MARK."
;;   (urbit-helper-let-resource
;;    (urbit-graph-store-action
;;     `((add-graph ,resource (graph . ,graph) (mark . ,mark))))))

;; ;;
;; ;; Hook Actions
;; ;;
;; (define urbit-graph-add-nodes (ship name nodes)
;;   "To graph at SHIP NAME, add NODES."
;;   (urbit-log "adding nodes: %s" nodes)
;;   (urbit-helper-let-resource
;;    (urbit-graph-hook-action
;;     `((add-nodes ,resource (nodes . ,nodes))))))

;; (define urbit-graph-add-node (ship name node)
;;   "To graph at SHIP NAME, add NODE."
;;   (urbit-graph-add-nodes ship name
;;                          (let ((index (intern
;;                                        (alist-get 'index
;;                                                   (alist-get 'post node)))))
;;                            `((,index . ,node)))))

;; (define urbit-graph-remove-nodes (ship name indices)
;;   "INDICES is a list of indexes to be removed from graph at SHIP NAME."
;;   (urbit-helper-let-resource
;;    (urbit-graph-hook-action `((remove-nodes ,resource (indices . indices))))))

;; ;;
;; ;; Fetching
;; ;;
;; (aio-define urbit-graph-get-wrapper (path)
;;   "Scries graph-store at PATH, and feeds the result to `urbit-graph-update-handler'.
;; Returns a list of nodes"
;;   (let ((result (car
;;                  (aio-await
;;                   (urbit-http-scry "graph-store" path)))))
;;     (let* ((graph-update (cdar result))
;;            (update-type (caar graph-update))
;;            (update (cdar graph-update)))
;;       (let ((nodes 
;;              (pcase update-type
;;                ('add-nodes
;;                 (alist-get 'nodes update))
;;                ('add-graph
;;                 (alist-get 'graph update))
;;                (- (urbit-log "Get wrapper unkown update type %s" update-type)
;;                   '()))))
;;         (urbit-graph-fix-indexes nodes)
;;         nodes))))

;; (aio-define urbit-graph-get-keys ()
;;   "Get a list of all graph store keys as resource symbols."
;;   (let ((keys
;;          (aio-await
;;           (urbit-http-scry "graph-store" "/keys"))))
;;     ;; Return a list of resource symbols
;;     (mapcar #'urbit-graph-resource-to-symbol
;;             (cdar (cdaar keys)))))

;; (define urbit-graph-get (ship name)
;;   "Get a graph at SHIP NAME."
;;   (urbit-graph-get-wrapper
;;    (format "/graph/%s/%s"
;;            (urbit-helper-ensig ship)
;;            name)))

;; (define urbit-graph-get-newest (ship name count &optional index)
;;   (urbit-helper-let-if-#f ((index ""))
;;     (urbit-graph-get-wrapper
;;      (format "/graph/%s/%s/node/siblings/newest/lone/%s%s"
;;              (urbit-helper-ensig ship)
;;              name
;;              count
;;              index))))

;; (define urbit-graph-get-older-siblings (ship name count &optional index)
;;   (urbit-helper-let-if-#f ((index ""))
;;     (urbit-graph-get-wrapper
;;      (format "/graph/%s/%s/node/siblings/older/lone/%s%s"
;;              (urbit-helper-ensig ship)
;;              name
;;              count
;;              (urbit-graph-index-to-ud index)))))

;; (define urbit-graph-get-younger-siblings (ship name count &optional index)
;;   (urbit-helper-let-if-#f ((index ""))
;;     (urbit-graph-get-wrapper
;;      (format "/graph/%s/%s/node/siblings/younger/lone/%s%s"
;;              (urbit-helper-ensig ship)
;;              name
;;              count
;;              (urbit-graph-index-to-ud index)))))

;; (define urbit-graph-get-subset (ship name start end)
;;   (urbit-graph-get-wrapper
;;    (format "/graph-subset/%s/%s/%s/%s"
;;            ship
;;            name
;;            end
;;            start)))

;; (define urbit-graph-get-node (ship name index)
;;   (urbit-graph-get-wrapper

;;    (format "/graph/%s/%s/node/index/kith%s"
;;            (urbit-helper-ensig ship)
;;            name
;;            (urbit-graph-index-to-ud index))))


