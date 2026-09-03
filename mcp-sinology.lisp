(require :asdf)

(asdf:load-system :hunchentoot)
(asdf:load-system :cl-json)
(asdf:load-system :dexador)

(defpackage #:mcp-sinology
  (:use #:cl #:hunchentoot #:cl-json)
  (:export      #:main                      #:start-server              #:stop-server)
)

(in-package     #:mcp-sinology)


(defvar *config* (make-hash-table :test #'equal)
  "Таблица с настройками сервера.")

(defvar *default-config*
  `(("synology-url" . "https://127.0.0.1:5001")
    ("synology-username" . "user")
    ("synology-password" . "pass")
    ("verify-ssl" . "false")
    ("server-port" . 8080))
  "Список пар (ключ . значение) по умолчанию.")

(defun save-config (&optional (filename "config-mcp.lisp"))
  "Сохраняет текущую конфигурацию в FILENAME, форматируя каждую пару на отдельной строке."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* t)
            (*print-pretty* t)
            (*print-right-margin* 120))
        (let ((alist (loop for key being the hash-keys of *config*
                           collect (cons key (gethash key *config*)))))
          (pprint alist out)
          (terpri out))))))

(defun load-config (&optional (filename "config-mcp.lisp"))
  "Загружает конфигурацию из FILENAME, если файл существует.
   Если файла нет, используется *DEFAULT-CONFIG* и создаётся новый файл."
  ;; Сначала заполняем таблицу значениями по умолчанию
  (clrhash *config*)
  (dolist (pair *default-config*)
    (setf (gethash (car pair) *config*) (cdr pair)))
  
  ;; Если файл существует, читаем его и обновляем таблицу
  (when (probe-file filename)
    (with-open-file (in filename
                        :direction :input
                        :external-format :utf-8)
      (with-standard-io-syntax
        (let ((alist (read in)))
          (dolist (pair alist)
            (setf (gethash (car pair) *config*) (cdr pair))))))
    (format t "Конфигурация загружена из ~A~%" filename))
  
  ;; Сохраняем текущую таблицу в файл (если его не было, он создастся)
  (save-config filename)
  *config*)

(defun reload-config ()
  "Перезагружает конфигурацию из файла (сбрасывая изменения)."
  (load-config))

;; Удобные функции доступа к настройкам
(defun config-value (key &optional (default nil))
  (gethash key *config* default))

(defun (setf config-value) (value key)
  (setf (gethash key *config*) value))

;; ============================================
;; Инициализация конфигурации при загрузке
;; ============================================


(defvar *synology-url* nil)
(defvar *synology-username* nil)
(defvar *synology-password* nil)
(defvar *verify-ssl* nil)
(defvar *server-port* nil)

;; ============================================
;; Потокобезопасный SID (используем sb-thread mutex)
;; ============================================

(defvar *sid-lock* (sb-thread:make-mutex))
(defmacro with-lock-held ((lock) &body body)
  `(sb-thread:with-mutex (,lock) ,@body))

(defvar *sid* nil)

(defun login ()
  (let ((url (format nil "~a/webapi/auth.cgi" *synology-url*))
        (params `(("api" . "SYNO.API.Auth")
                  ("version" . "3")
                  ("method" . "login")
                  ("account" . ,*synology-username*)
                  ("passwd" . ,*synology-password*)
                  ("session" . "FileStation")
                  ("format" . "cookie"))))
    (multiple-value-bind (body status headers)
        (dexador:post url :form params :ssl-verify *verify-ssl*)
      (declare (ignore status headers))
      (let ((json (cl-json:decode-json-from-string body)))
        (if (and (gethash "success" json) (gethash "success" json))
            (gethash "sid" (gethash "data" json))
            (error "Ошибка входа: ~a" json))))))

(defun ensure-sid ()
  (with-lock-held (*sid-lock*)
    (unless *sid* (setf *sid* (login)))
    *sid*))

(defun refresh-sid ()
  (with-lock-held (*sid-lock*)
    (setf *sid* (login))))

;; ============================================
;; Вызов API FileStation
;; ============================================

(defun call-filestation-api (api method &rest additional-params)
  (flet ((do-call (sid)
           (let* ((url (format nil "~a/webapi/entry.cgi" *synology-url*))
                  (params (append `(("api" . ,api)
                                    ("version" . "2")
                                    ("method" . ,method)
                                    ("_sid" . ,sid))
                                  (loop for (key value) on additional-params by #'cddr
                                        collect (cons key value)))))
             (multiple-value-bind (body status)
                 (dexador:get url :query params :ssl-verify *verify-ssl*)
               (if (= status 200)
                   (let ((json (cl-json:decode-json-from-string body)))
                     (if (and (gethash "success" json) (gethash "success" json))
                         (gethash "data" json)
                         (if (and (gethash "error" json)
                                  (equal (gethash "code" (gethash "error" json)) 401))
                             (throw 'need-relogin nil)
                             (error "Ошибка API ~a: ~a" api json))))
                   (error "HTTP ошибка ~a при вызове ~a" status api))))))
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

(defun read-file (path)
  (let ((sid (ensure-sid)))
    (let ((url (format nil "~a/webapi/entry.cgi" *synology-url*))
          (params `(("api" . "SYNO.FileStation.Download")
                    ("version" . "2")
                    ("method" . "download")
                    ("_sid" . ,sid)
                    ("path" . ,path)
                    ("mode" . "open"))))
      (multiple-value-bind (body status)
          (dexador:get url :query params :ssl-verify *verify-ssl*)
        (if (= status 200) body (error "Ошибка чтения файла: HTTP ~a" status))))))

(defun search-files (path pattern)
  (call-filestation-api "SYNO.FileStation.Search" "start"
                        "folder_path" path "pattern" pattern "recursive" "true"))

;; ============================================
;; JSON‑RPC 2.0 обработчики
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

(defun handle-tools-call (id name arguments)
  (handler-case
      (let ((result
             (cond
               ((string= name "list_files")
                (make-tool-result (alist->json (list-files (gethash "path" arguments)))))
               ((string= name "get_file_info")
                (make-tool-result (alist->json (get-file-info (gethash "path" arguments)))))
               ((string= name "read_file")
                (make-tool-result (read-file (gethash "path" arguments))))
               ((string= name "search_files")
                (make-tool-result (alist->json (search-files (gethash "path" arguments)
                                                             (gethash "pattern" arguments)))))
               (t (send-json-error id -32601 "Метод не найден")))))
        (send-json-response id result))
    (error (e) (send-json-error id -32000 (format nil "Ошибка: ~a" e)))))

(defun handle-request (json)
  (let* ((method (gethash "method" json))
         (id (gethash "id" json))
         (params (gethash "params" json)))
    (cond
      ((string= method "tools/list") (handle-tools-list id))
      ((string= method "tools/call")
       (let ((tool-name (gethash "name" params))
             (args (gethash "arguments" params)))
         (handle-tools-call id tool-name (or args (make-hash-table)))))
      (t (send-json-error id -32601 (format nil "Неизвестный метод: ~a" method))))))

;; ============================================
;; Обработчик Hunchentoot
;; ============================================

(defun mcp-handler ()
  (cond
    ((string= (hunchentoot:request-method*) "POST")
     (let* ((body (hunchentoot:raw-post-data :force-text t))
            (json (cl-json:decode-json-from-string body))
            (response (handle-request json)))
       (setf (hunchentoot:content-type*) "application/json")
       response))
    (t (setf (hunchentoot:return-code*) 405) "Method Not Allowed")))

(push (hunchentoot:create-regex-dispatcher "^/mcp$" #'mcp-handler)
      hunchentoot:*dispatch-table*)

;; ============================================
;; Запуск / остановка сервера
;; ============================================

(defvar *server* nil)

(defun start-server (&key (port *server-port*))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port port)))
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

;; ============================================
;; Функция main – загружает конфиг и запускает сервер
;; ============================================

(defun main ()
  ;; 1. Загружаем конфигурацию из файла (или создаём по умолчанию)
  (load-config)   ; теперь это вызывается при каждом запуске бинарника
  ;; 2. Устанавливаем параметры сервера из конфига
  (setf *synology-url* (config-value "synology-url"))
  (setf *synology-username* (config-value "synology-username"))
  (setf *synology-password* (config-value "synology-password"))
  (setf *verify-ssl* (not (string= (config-value "verify-ssl") "false")))
  (setf *server-port* (parse-integer (format nil "~a" (config-value "server-port"))))
  ;; 3. Запускаем сервер
  (start-server :port *server-port*)
  ;; 4. Бесконечный цикл (сервер работает в фоновых потоках)
  (loop (sleep 10)))

  ;;; sbcl --load mcp-sinology.lisp    --eval "(sb-ext:save-lisp-and-die \"mcp-sinology\" :toplevel #'mcp-sinology:main :executable t :purify t)"