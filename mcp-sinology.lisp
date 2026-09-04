;; -*- coding: utf-8 -*-
;; MCP-сервер для Synology NAS (финальная версия с dispatcher'ами)

(require :asdf)

(asdf:load-system :hunchentoot)
(asdf:load-system :cl-json)
(asdf:load-system :dexador)
(asdf:load-system :quri)


(defpackage #:mcp-sinology

  (:use #:cl #:hunchentoot #:cl-json)
  (:export #:main #:start-server #:stop-server  #:test-connection))

(in-package #:mcp-sinology)

;; ============================================
;; Конфигурация
;; ============================================

(defvar *config* (make-hash-table :test #'equal))
(defvar *default-config*
  `(("synology-url" . "https://127.0.0.1:5001")
    ("synology-username" . "user")
    ("synology-password" . "pass")
    ("server-port" . 8080)))

(defun save-config (&optional (filename "config-mcp.lisp"))
  (with-open-file (out filename :direction :output :if-exists :supersede :external-format :utf-8)
    (let ((*print-readably* t) (*print-pretty* t) (*print-right-margin* 120))
      (let ((alist (loop for key being the hash-keys of *config*
                         collect (cons key (gethash key *config*)))))
        (pprint alist out)
        (terpri out)))))

(defun load-config (&optional (filename "config-mcp.lisp"))
  (clrhash *config*)
  (dolist (pair *default-config*)
    (setf (gethash (car pair) *config*) (cdr pair)))
  (when (probe-file filename)
    (with-open-file (in filename :direction :input :external-format :utf-8)
      (let ((alist (read in)))
        (dolist (pair alist)
          (setf (gethash (car pair) *config*) (cdr pair)))))
    (format t "Конфигурация загружена из ~A~%" filename))
  (save-config filename)
  *config*)

(defun config-value (key &optional (default nil)) (gethash key *config* default))

;; ============================================
;; Параметры сервера
;; ============================================

(defvar *synology-url* nil)
(defvar *synology-username* nil)
(defvar *synology-password* nil)
(defvar *verify-ssl* nil)
(defvar *server-port* nil)

;; ============================================
;; SID и авторизация
;; ============================================

(defvar *sid-lock* (sb-thread:make-mutex))
(defmacro with-lock-held ((lock) &body body)
  `(sb-thread:with-mutex (,lock) ,@body))
(defvar *sid* nil)

(defun login ()
  (let* ((url (config-value "synology-url"))
         (user (config-value "synology-username"))
         (pass (config-value "synology-password")))
    (format t "~&[DEBUG] login: url=~S, user=~S, pass=~S~%" url user pass)
    (unless (and url user pass)
      (error "Synology credentials not set. Check config file."))
    (let ((params `(("api" . "SYNO.API.Auth")
                    ("version" . "3")
                    ("method" . "login")
                    ("account" . ,user)
                    ("passwd" . ,pass)
                    ("session" . "FileStation")
                    ("format" . "cookie"))))
      (let ((body (with-output-to-string (s)
                    (loop for (key . val) in params
                          for i from 0
                          do (unless (zerop i) (write-char #\& s))
                          (write-string key s)
                          (write-char #\= s)
                          (write-string (quri:url-encode (princ-to-string val)) s)))))
        (multiple-value-bind (response status headers)
            (dex:post (format nil "~a/webapi/auth.cgi" url)
                      :content body
                      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                      :insecure t)   ; <-- правильный ключ
          (declare (ignore status headers))
          (let ((json (cl-json:decode-json-from-string response)))
            (if (cdr (assoc :success json))
                (cdr (assoc :sid (cdr (assoc :data json))))
                (error "Login failed: ~a" json))))))))


; (defun login ()
;   (let* ((url (config-value "synology-url"))
;          (user (config-value "synology-username"))
;          (pass (config-value "synology-password")))
;     (format t "~&[DEBUG] login: url=~S, user=~S, pass=~S~%" url user pass)
;     (unless (and url user pass)
;       (error "Synology credentials not set. Check config file."))
;     (let ((params `(("api" . "SYNO.API.Auth")
;                     ("version" . "3")
;                     ("method" . "login")
;                     ("account" . ,user)
;                     ("passwd" . ,pass)
;                     ("session" . "FileStation")
;                     ("format" . "cookie"))))
;       (let ((body (with-output-to-string (s)
;                     (loop for (key . val) in params
;                           for i from 0
;                           do (unless (zerop i) (write-char #\& s))
;                           (write-string key s)
;                           (write-char #\= s)
;                           (write-string (quri:url-encode (princ-to-string val)) s)))))
;         (multiple-value-bind (response status headers)
;             (dex:post (format nil "~a/webapi/auth.cgi" url)
;                       :content body
;                       :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
;                       :insecure t)   ; <-- правильный ключ
;           (declare (ignore status headers))
;           (let ((json (cl-json:decode-json-from-string response)))
;             (if (cdr (assoc :success json))
;                 (cdr (assoc :sid (cdr (assoc :data json))))
;                 (error "Login failed: ~a" json))))))))



; (defun login ()
;   (let ((url (format nil "~a/webapi/auth.cgi" *synology-url*))
;         (params `(("api" . "SYNO.API.Auth")
;                   ("version" . "3")
;                   ("method" . "login")
;                   ("account" . ,*synology-username*)
;                   ("passwd" . ,*synology-password*)
;                   ("session" . "FileStation")
;                   ("format" . "cookie"))))
;     (multiple-value-bind (body status headers)
;         (dex:post url 
;                   :content (with-output-to-string (s)
;                              (loop for (key . val) in params
;                                    for i from 0
;                                    do (unless (zerop i) (write-char #\& s))
;                                    (write-string key s)
;                                    (write-char #\= s)
;                                    (write-string (quri:url-encode (princ-to-string val) :utf-8) s)))
;                   :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
;                   :want-string t)
;       (declare (ignore status headers))
;       (let ((json (cl-json:decode-json-from-string body)))
;         (if (and (gethash "success" json) (gethash "success" json))
;             (gethash "sid" (gethash "data" json))
;             (error "Ошибка входа: ~a" json))))))

(defun ensure-sid ()
  (with-lock-held (*sid-lock*)
    (unless *sid* (setf *sid* (login)))
    *sid*))

(defun refresh-sid ()
  (with-lock-held (*sid-lock*)
    (setf *sid* (login))))

;; ============================================
;; API вызовы
;; ============================================



;;;dickpic
; (defun call-filestation-api (api method &rest additional-params)
;   (flet ((do-call (sid)
;            (let* ((url (config-value "synology-url"))
;                   (params (append `(("api" . ,api)
;                                     ("version" . "2")
;                                     ("method" . ,method)
;                                     ("_sid" . ,sid))
;                                   (loop for (key value) on additional-params by #'cddr
;                                         collect (cons key value))))
;                   (query (with-output-to-string (s)
;                            (loop for (key . val) in params
;                                  for i from 0
;                                  do (unless (zerop i) (write-char #\& s))
;                                  (write-string key s)
;                                  (write-char #\= s)
;                                  (write-string (quri:url-encode (princ-to-string val)) s))))
;                   (full-url (format nil "~a/webapi/entry.cgi?~a" url query)))
;              (multiple-value-bind (body status)
;                  (dex:get full-url :insecure t)   ; только URL и :insecure
;                (if (= status 200)
;                    (let ((json (cl-json:decode-json-from-string body)))
;                      (if (cdr (assoc :success json))
;                          (cdr (assoc :data json))
;                          (if (and (cdr (assoc :error json))
;                                   (equal (cdr (assoc :code (cdr (assoc :error json)))) 401))
;                              (throw 'need-relogin nil)
;                              (error "API error ~a: ~a" api json))))
;                    (error "HTTP error ~a calling ~a" status api))))))
;     (catch 'need-relogin
;       (let ((sid (ensure-sid)))
;         (return-from call-filestation-api (do-call sid))))
;     (refresh-sid)
;     (do-call *sid*)))
;;;;;;;;;;;;;;;


(defun call-filestation-api (api method &rest additional-params)
  (flet ((build-query-string (params)
           (with-output-to-string (s)
             (loop for (key . val) in params
                   for i from 0
                   do (unless (zerop i) (write-char #\& s))
                   (write-string (quri:url-encode key) s)
                   (write-char #\= s)
                   (write-string (quri:url-encode (princ-to-string val)) s))))
         (do-call (sid)
           (let* ((base-url (format nil "~a/webapi/entry.cgi" *synology-url*))
                  (params (append `(("api" . ,api)
                                    ("version" . "2")
                                    ("method" . ,method)
                                    ("_sid" . ,sid))
                                  (loop for (key value) on additional-params by #'cddr
                                        collect (cons key value)))))
             (let ((url (format nil "~a?~a" base-url (build-query-string params))))
               (multiple-value-bind (body status)
                   (dex:get url :want-string t :insecure t)
                 (if (= status 200)
                     (let ((json (cl-json:decode-json-from-string body)))
                       (if (and (gethash "success" json) (gethash "success" json))
                           (gethash "data" json)
                           (if (and (gethash "error" json)
                                    (equal (gethash "code" (gethash "error" json)) 401))
                               (throw 'need-relogin nil)
                               (error "Ошибка API ~a: ~a" api json))))
                   (error "HTTP ошибка ~a при вызове ~a" status api)))))))
    (catch 'need-relogin
      (let ((sid (ensure-sid)))
        (return-from call-filestation-api (do-call sid))))
    (refresh-sid)
    (do-call *sid*)))









;; ============================================
;; Инструменты MCP
;; ============================================

(defun list-files (path)
  (call-filestation-api "SYNO.FileStation.List" "list" "folder_path" path))

(defun get-file-info (path)
  (call-filestation-api "SYNO.FileStation.List" "list"
                        "folder_path" path
                        "additional" "[\"real_path\",\"size\",\"owner\",\"time\",\"perm\"]"))
;;;;;;;;;;;dick pick;;;;;;;;;;;;
; (defun read-file (path)
;   (let* ((sid (ensure-sid))
;          (url (config-value "synology-url"))
;          (params `(("api" . "SYNO.FileStation.Download")
;                    ("version" . "2")
;                    ("method" . "download")
;                    ("_sid" . ,sid)
;                    ("path" . ,path)
;                    ("mode" . "open")))
;          (query (with-output-to-string (s)
;                   (loop for (key . val) in params
;                         for i from 0
;                         do (unless (zerop i) (write-char #\& s))
;                         (write-string key s)
;                         (write-char #\= s)
;                         (write-string (quri:url-encode (princ-to-string val)) s))))
;          (full-url (format nil "~a/webapi/entry.cgi?~a" url query)))
;     (multiple-value-bind (body status)
;         (dex:get full-url :insecure t)
;       (if (= status 200) body (error "Read file error: HTTP ~a" status)))))
;;;;;;;;;;;;;;;;;;;;;;



(defun read-file (path)
  (let ((sid (ensure-sid)))
    (let* ((base-url (format nil "~a/webapi/entry.cgi" *synology-url*))
           (params `(("api" . "SYNO.FileStation.Download")
                     ("version" . "2")
                     ("method" . "download")
                     ("_sid" . ,sid)
                     ("path" . ,path)
                     ("mode" . "open")))
           (url (with-output-to-string (s)
                  (write-string base-url s)
                  (write-char #\? s)
                  (loop for (key . val) in params
                        for i from 0
                        do (unless (zerop i) (write-char #\& s))
                        (write-string (quri:url-encode key) s)
                        (write-char #\= s)
                        (write-string (quri:url-encode (princ-to-string val)) s)))))
      (multiple-value-bind (body status)
          (dex:get url :want-string t :insecure t)
        (if (= status 200) body (error "Ошибка чтения файла: HTTP ~a" status))))))



(defun search-files (path pattern)
  (call-filestation-api "SYNO.FileStation.Search" "start"
                        "folder_path" path "pattern" pattern "recursive" "true"))

;; ============================================
;; JSON-RPC 2.0 (ключи — символы)
;; ============================================

(defun alist->json (alist)
  (cl-json:encode-json-to-string alist))

(defun send-json-response (id result)
  (cl-json:encode-json-to-string
   `((:jsonrpc . "2.0") (:id . ,id) (:result . ,result))))

(defun send-json-error (id code message)
  (cl-json:encode-json-to-string
   `((:jsonrpc . "2.0") (:id . ,id)
     (:error . ((:code . ,code) (:message . ,message))))))

(defun make-tool-result (text)
  `((:content . ((:type . "text") (:text . ,text)))))

(defun handle-tools-list (id)
  (let ((tools
         (list
          (list :name "list_files"
                :description "Показать содержимое папки на Synology NAS"
                :inputSchema `((:type . "object")
                               (:properties . ((:path . ((:type . "string")
                                                         (:description . "Путь к папке")))))
                               (:required . "path")))
          (list :name "get_file_info"
                :description "Получить метаданные файла/папки"
                :inputSchema `((:type . "object")
                               (:properties . ((:path . ((:type . "string")
                                                         (:description . "Путь")))))
                               (:required . "path")))
          (list :name "read_file"
                :description "Прочитать содержимое текстового файла"
                :inputSchema `((:type . "object")
                               (:properties . ((:path . ((:type . "string")
                                                         (:description . "Путь к файлу")))))
                               (:required . "path")))
          (list :name "search_files"
                :description "Поиск файлов по шаблону (рекурсивно)"
                :inputSchema `((:type . "object")
                               (:properties . ((:path . ((:type . "string")
                                                         (:description . "Папка")))
                                               (:pattern . ((:type . "string")
                                                            (:description . "Шаблон")))))
                               (:required . ("path" "pattern")))))))
    (send-json-response id `((:tools . ,tools)))))


(defun process-json-request (json)
  (let* ((method (cdr (assoc :method json)))
         (id (cdr (assoc :id json)))
         (params (cdr (assoc :params json))))
    (cond
      ((string= method "tools/list") (handle-tools-list id))
      ((string= method "tools/call")
       (let ((tool-name (cdr (assoc :name params)))
             (args (cdr (assoc :arguments params))))
         (handle-tools-call id tool-name (or args nil))))
      (t (send-json-error id -32601 (format nil "Unknown method: ~a" method))))))

(defun handle-tools-call (id name arguments)
  (handler-case
      (let ((result
             (cond
               ((string= name "list_files")
                (make-tool-result (alist->json (list-files (cdr (assoc :path arguments))))))
               ((string= name "get_file_info")
                (make-tool-result (alist->json (get-file-info (cdr (assoc :path arguments))))))
               ((string= name "read_file")
                (make-tool-result (read-file (cdr (assoc :path arguments)))))
               ((string= name "search_files")
                (make-tool-result (alist->json (search-files (cdr (assoc :path arguments))
                                                             (cdr (assoc :pattern arguments))))))
               (t (send-json-error id -32601 "Method not found")))))
        (send-json-response id result))
    (error (e) (send-json-error id -32000 (format nil "Error: ~a" e)))))

;; ============================================
;; Обработчики HTTP (обычные функции с одним аргументом)
;; ============================================

(define-easy-handler (hello-handler :uri "/hello") ()
  (setf (return-code*) 200
        (content-type*) "text/plain")
  "hello")


(defun set-config-vars ()
  (dolist (pair '((:synology-url . "synology-url")
                  (:synology-username . "synology-username")
                  (:synology-password . "synology-password")))
    (let* ((key (car pair))
           (var-name-str (cdr pair))
           (symbol (intern (string-upcase (format nil "*~A*" var-name-str)) :mcp-sinology))
           (value (config-value key)))
      (setf (symbol-value symbol) value))))



(defun test-connection ()
  (load-config)
  ;; Устанавливаем глобальные переменные из конфига
  (setf *synology-url* (config-value "synology-url")
        *synology-username* (config-value "synology-username")
        *synology-password* (config-value "synology-password"))
  ;; Отладочный вывод
  (format t "~&[DEBUG] test-connection: url=~S, user=~S, pass=~S~%" 
          *synology-url* *synology-username* *synology-password*)
  (let ((sid (login)))
    (format t "~&Login successful, SID: ~A~%" sid)
    (let ((result (list-files "/")))
      (format t "~&List files result: ~A~%" result)))
  (values))

(define-easy-handler (mcp-handler :uri "/mcp") ()
  (handler-case
      (if (string= (request-method*) "POST")
          (let* ((body (raw-post-data :force-text t))
                 (json (cl-json:decode-json-from-string body)))  ; без :json-symbols
            (format t "~&JSON: ~S" json)
            (format t "~&JSON keys: ~S" (mapcar #'car json))
            (let ((response (process-json-request json)))
              (setf (return-code*) 200
                    (content-type*) "application/json")
              response))
          (progn
            (setf (return-code*) 405)
            "Method Not Allowed"))
    (error (e)
      (format t "~&!!! Error: ~A" e)
      #+sbcl (sb-debug:backtrace 20)
      (setf (return-code*) 500)
      (format nil "Internal error: ~a" e))))
;; ============================================
;; Запуск / остановка
;; ============================================

(defvar *server* nil)

(defun start-server (&key (port *server-port*))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor 
                                 :port port
                                 :read-timeout 300
                                 :write-timeout 300)))
    (hunchentoot:start acceptor)
    (setf *server* acceptor)
    (format t "~&MCP-сервер Synology запущен на порту ~a~%" port)
    (format t "Эндпоинт: http://localhost:~a/mcp~%" port)
    acceptor))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Сервер остановлен~%")))

(defun main ()
  (load-config)
  (setf *synology-url* (config-value "synology-url"))
  (setf *synology-username* (config-value "synology-username"))
  (setf *synology-password* (config-value "synology-password"))
  (setf *server-port* (parse-integer (format nil "~a" (config-value "server-port"))))
  (start-server :port *server-port*)
  (loop (sleep 10)))

  ;;; sbcl --noinform --disable-debugger --load mcp-sinology.lisp --eval "(mcp-sinology:main)"
  ;;; sbcl --load mcp-sinology.lisp    --eval "(sb-ext:save-lisp-and-die \"mcp-sinology\" :toplevel #'mcp-sinology:main :executable t :purify t)"