;;; text-translator.el --- Text Translator

;; Copyright (C) 2007-2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Translates character strings on Emacs.
;; This package use the text translation service that exists on the internet.

;; Read README.en (English) or README.ja (Japanese).

;;; Code:

(require 'text-translator-vars)
(require 'text-translator-window)


(defun text-translator (arg &optional last engine-or-func)
  "The function which does text translation.
Use Excite, Google and so translation site.
1. Mark is active
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate by type which you selected.
 - Prefix was not supplied.
   Translate range of region that you selected by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)
2. Mark is deactive
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate value which you input from minibuffer by type you selected.
 - Prefix was not supplied.
   Translate value which you input from minibuffer by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)"
  (interactive "P")
  (let ((engine (text-translator-check-valid-translation-engine
                 engine-or-func
                 (cond
                  ((and text-translator-all-history
                        (car text-translator-all-history))
                   (nth 0 (caar text-translator-all-history)))
                  (t
                   text-translator-default-engine))))
        str)
    ;; If prefix-arg is non-nil, change translation type.
    (when (or arg last)
      (let ((minibuffer-history
             (let (engines)
               (dolist (i text-translator-all-history)
                 (dolist (j i)
                   (setq engines (cons (nth 0 j) engines))))
               (nreverse engines))))
        (setq engine (completing-read
                      (format "Select translation engine (default %s): "
                              engine)
                      text-translator-site-data-alist nil t nil nil engine))))
    (text-translator-proc-clear)
    ;; Initialize (init global variable, delete running processess etc...).
    (setq str
          (cond
           (last
            (nth 1 (caar text-translator-all-history)))
           (t
            (text-translator-region-or-read-string
             (format "Enter string translated by %s: " engine))))
          text-translator-all-site-number   1
          text-translator-all-results       nil
          text-translator-processes-alist   nil
          text-translator-all-before-string nil)
    (cond
     ;; The translating engine and traslanting string is same of last
     ;; translation. So, Text-translator display last result.
     ;; Todo: To check whether or not the result string has
     ;; "TRANSLATION: (TIMEOUT|FAILED)".
     ((and (= text-translator-all-site-number
              (length (car text-translator-all-history)))
           (string= str (nth 1 (caar text-translator-all-history)))
           (string= engine (nth 0 (caar text-translator-all-history))))
      (setq text-translator-all-results
            (list (cons (concat text-translator-buffer engine)
                        (nth 2 (assoc engine
                                      (car text-translator-all-history))))))
      (funcall text-translator-display-function))
     ;; Newly translating
     (t
      (text-translator-client
     (text-translator-check-valid-translation-engine
      (and (not arg)
           (functionp engine-or-func)
           (funcall engine-or-func engine str))
      engine)
     str)
    (text-translator-timeout-start)))))

(defun text-translator-translate-by-auto-selection (arg)
  "Function that translates by auto selection of translation
engine.  Function that select automatically is value of
`text-translator-auto-selection-func'."
  (interactive "P")
  (text-translator arg nil text-translator-auto-selection-func))

(defun text-translator-translate-by-auto-selection-enja (engine str)
  "Automatic selection function for English to Japanese(or Japanese to
English) translation.
If alphabet ratio is over 40%, select engine which is translating from
English to Japanese.  Otherwise, from Japanese to English."
  (let ((str (or str ""))
        (engine (text-translator-get-engine-type-or-site engine t))
        (site   (text-translator-get-engine-type-or-site engine))
        (percentage 40))
    (cond
     ((member site '("enja" "jaen" ""))
      (cond
       ((> (/ (* (length (replace-regexp-in-string "[^A-Za-z 0-9]+" "" str))
                   100)
                (length str))
             percentage)
        (format "%s_enja" engine))
       (t
        (format "%s_jaen" engine))))
     (t
      (message (concat "Selected engine is not enja or jaen. "
                       "So use selected engine : %s")
               engine)
      engine))))

(defun text-translator-region-or-read-string (&optional prompt)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (cond
   (mark-active
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (let ((minibuffer-history
           (mapcar #'(lambda (x)
                       (nth 1 (car x)))
                   text-translator-all-history)))
      (read-string (or prompt "Enter string: "))))))

(defun text-translator-all (arg &optional key str)
  "The function to translate in all of translate sites that matches
the selected type."
  (interactive "P")
  (let ((hash text-translator-sitedata-hash)
        (str (or str
                 (text-translator-region-or-read-string)))
        keys)
    ;; Initalize global variable.
    (text-translator-proc-clear)
    (setq text-translator-all-results       nil
          text-translator-all-site-number   nil
          text-translator-processes-alist   nil
          text-translator-all-before-string nil)
    ;; confirm and update translation site data.
    (when (or (null hash) arg)
      (setq text-translator-sitedata-hash (text-translator-update-hashtable)
            hash text-translator-sitedata-hash))
    (maphash '(lambda (x y)
                (setq keys (cons x keys)))
             hash)
    (when (setq key (or key
                        (completing-read "Select type: " keys nil t)))
      (let ((sites (gethash key hash)))
        (setq text-translator-all-site-number (length sites))
        (cond
         ;; The translating engine and traslanting string is same of last
         ;; translation. So, Text-translator display last result.
         ;; Todo: To check whether or not the result string has
         ;; "TRANSLATION: (TIMEOUT|FAILED)".
         ((and (= (length (car text-translator-all-history))
                  text-translator-all-site-number)
               (string= (nth 1 (caar text-translator-all-history)) str)
               (member (nth 0 (caar text-translator-all-history)) sites))
          (dolist (i sites)
            (setq text-translator-all-results
                  (cons (cons (concat text-translator-buffer i)
                              (nth 2
                                   (assoc i
                                          (car text-translator-all-history))))
                        text-translator-all-results)))
          (funcall text-translator-display-function))
         ;; Newly translating
         (t
          (dolist  (i sites)
            (text-translator-client i str t))
          (text-translator-timeout-start)))))))

(defun text-translator-all-by-auto-selection (arg)
  "The function to translate in all of translate sites, whose
translation engine is selected automatically.  The selection function
is the value of `text-translator-auto-selection-func'."
  (interactive "P")
  (let ((str (text-translator-region-or-read-string)))
    (text-translator-all
     arg
     (substring (funcall text-translator-auto-selection-func "" str) 1)
     str)))

(defun text-translator-client (engine str &optional all sync)
  "Function that throws out words and phrases that want to translate into
specified site, and receives translation result."
  (let* ((history-delete-duplicates t)
         (work-buf (concat " " text-translator-buffer engine))
         (str (text-translator-replace-string
               str
               (cond
                ((not text-translator-do-fill-region)
                 text-translator-pre-string-replace-alist)
                ;; For example, if engine is "excite.co.jp_enja",
                ;; this code returns "en".
                ((member (substring
                          (text-translator-get-engine-type-or-site engine) 0 2)
                         text-translator-space-division-languages)
                 ;; replace "\n" to " ".
                 (append '(("\n" . " ") ("\r" . ""))
                         text-translator-pre-string-replace-alist))
                (t
                 ;; replace "\n" to "".
                 (append '(("\n" . "") ("\r" . ""))
                         text-translator-pre-string-replace-alist)))))
         (type (assoc engine text-translator-site-data-alist))
         (proc (open-network-stream (concat text-translator-buffer engine)
                                    work-buf
                                    (or text-translator-proxy-server
                                        (nth 1 type))
                                    (or (and text-translator-proxy-server
                                             text-translator-proxy-port)
                                        80)))
         (process-connection-type nil)
         (enc-str (text-translator-url-encode-string str (nth 4 type)))
         (post-str (if (nth 3 type) (format (nth 3 type) enc-str) nil))
         (truncate-partial-width-windows nil)
         (text-translator-display-function
          (if sync 'ignore text-translator-display-function)))
    ;; Initalize temporary variables
    (setq text-translator-all-before-string
          (cons (list (process-name proc) engine str)
                text-translator-all-before-string)
          text-translator-processes-alist
          (cons (cons (process-name proc) nil)
                text-translator-processes-alist)
          text-translator-all-results
          (cons (cons (process-name proc) nil)
                text-translator-all-results))
    (with-current-buffer (get-buffer-create work-buf)
      (erase-buffer)
      (set-process-coding-system proc (nth 4 type) 'binary)
      (set-process-filter proc 'text-translator-client-filter)
      (process-send-string
       proc
       (setq text-translator-send-string
             (concat
              (cond
               (post-str
                ;; use POST method
                (concat "POST "
                        (if text-translator-proxy-server
                            (concat "http://" (nth 1 type) (nth 2 type))
                          (nth 2 type))
                        "\r\n"))
               (t
                ;; use GET method
                (concat "GET "
                        (format
                         (if text-translator-proxy-server
                             (concat "http://" (nth 1 type) (nth 2 type))
                           (nth 2 type))
                         enc-str)
                        "\r\n")))
              (and text-translator-proxy-server
                   text-translator-proxy-user
                   text-translator-proxy-password
                   (format "Proxy-Authorization: Basic %s \r\n"
                           (base64-encode-string
                            (concat text-translator-proxy-user ":"
                                    text-translator-proxy-password))))
              "HOST: " (nth 1 type) "\r\n"
              "User-Agent: " text-translator-user-agent "\r\n"
              "Accept-Encoding: " text-translator-accept-encoding "\r\n"
              "Accept-Charset: " text-translator-accept-charset "\r\n"
              "Keep-Alive: " text-translator-keep-alive "\r\n"
              "Connection: " text-translator-connection "\r\n"
              (when post-str
                (concat
                 "Content-Type: application/x-www-form-urlencoded\r\n"
                 "Content-Length: "
                 (number-to-string (string-bytes post-str)) "\r\n"
                 "\r\n"
                 post-str "\r\n"))
              "\r\n")))
      ;; Display only once (Countermesure for text-translator-all).
      (when (and (not sync)
                 (= 1 (length text-translator-processes-alist)))
        (message "Translating..."))
      ;; If `sync' was `t', Synchronize a output.
      (when (and sync (not all))
        (while (not (cdar text-translator-all-results))
          (sit-for 0.1))
        (cdar text-translator-all-results)))))

(defun text-translator-client-filter (proc str)
  (let (buf-name buf-bytes buf-string content-len chunk-len all parse-method)
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (insert str)
      (set-marker (process-mark proc) (point))
      ;; Initialize a variable
      (setq buf-name     (buffer-name)
            all          (> text-translator-all-site-number 1)
            content-len  (cadr (assoc "Content-Length"
                                      (text-translator-proc-header-get
                                       (process-name proc))))
            buf-bytes    (if content-len (string-bytes (buffer-string)))
            parse-method (nth 5
                              (assoc
                               (substring buf-name
                                          ;; Plus a 1 space " " length.
                                          (1+ (length text-translator-buffer))
                                          (length buf-name))
                               text-translator-site-data-alist)))
      ;; Header line or chunk-length
      (cond
       ((null (text-translator-proc-header-get (process-name proc)))
        (setq chunk-len (text-translator-proc-header-parse
                         proc buf-name text-translator-debug)))
       (t
        (when (null content-len)
          (setq chunk-len (text-translator-proc-chunk-get
                           buf-name text-translator-debug))
          (when text-translator-debug
            (message ";; chunk-len: %s" chunk-len)))))
      (setq buf-string (buffer-string))
      ;; Extract a translated string.
      (with-temp-buffer
        (insert (text-translator-decode-string buf-string (process-name proc)))
        (setq str (text-translator-replace-string
                   (or (cond
                        ((functionp parse-method)
                         (funcall parse-method))
                        ((re-search-backward parse-method nil t)
                         (match-string 1)))
                       "")
                   text-translator-post-string-replace-alist))))
    ;; Clean up
    (when (or (not (string= "" str))
              (and content-len
                   (> 100 (abs (- buf-bytes (string-to-number content-len)))))
              (and chunk-len (string= "0" chunk-len)))
      (delete-process proc)
      (unless text-translator-debug
        ;; Todo: Should I save a log before killing a buffer?
        (kill-buffer buf-name))
      (when (string= "" str)
        (setq str "TRANSLATION: FAILED")))
    ;; Display translated string
    (when (not (string= "" str))
      (when text-translator-all-results
        (when (assoc (process-name proc) text-translator-all-results)
          (setcdr (assoc (process-name proc) text-translator-all-results) str))
        (text-translator-display all)))))

(defun text-translator-display (all)
  (ding)
  (message "Translating...done")
  (cond
   (all
    (when (not (member 'nil (mapcar 'cdr text-translator-all-results)))
      (text-translator-timeout-stop)
      (text-translator-add-history)
      (cond
       (text-translator-display-function
        (funcall text-translator-display-function))
       (t
        (text-translator-window-display)))))
   (t
    (text-translator-timeout-stop)
    (text-translator-add-history)
    (cond
     (text-translator-display-function
      (funcall text-translator-display-function))
     (t
      (text-translator-window-display))))))

(defun text-translator-add-history ()
  (let ((make-history
         #'(lambda ()
             (mapcar
              #'(lambda (i)
                  (let ((pname (car i)) (tstring (cdr i)) match)
                    ;; match is '(engine before-string).
                    (when (setq match
                                (assoc pname
                                       text-translator-all-before-string))
                      (when (cdr match)
                        (nreverse (cons tstring
                                        (nreverse (cdr match))))))))
              text-translator-all-results))))
    (cond
     (text-translator-all-history
      (add-to-history 'text-translator-all-history (funcall make-history)))
     (t
      (setq text-translator-all-history
            (cons (funcall make-history) text-translator-all-history))))))

(defun text-translator-update-hashtable ()
  (let ((hash (make-hash-table :test 'equal)))
    (mapc #'(lambda (x)
	      (let ((matched (replace-regexp-in-string "\\([^_]*\\)_"
						       ""
						       (car x))))
		(unless (or (string= (car x) matched)
			    (eq ?* (aref matched 0)))
		  (cond
		   ((gethash matched hash)
		    (puthash matched
			     (cons (car x) (gethash matched hash))
			     hash))
		   (t
		    (puthash matched (list (car x)) hash))))))
          text-translator-site-data-alist)
    hash))

(defun text-translator-proc-clear ()
  (let (proc buf)
    (when text-translator-processes-alist
      (dolist (i text-translator-processes-alist)
        (setq proc (get-process (nth 0 i))
              buf  (get-buffer  (nth 0 i)))
        (when (processp proc)
          (delete-process proc))
        (when (bufferp buf)
          (kill-buffer buf))))))

(defun text-translator-proc-header-get (proc-name &optional alist)
  (cdr (assoc proc-name (or alist text-translator-processes-alist))))

(defun text-translator-proc-header-set (proc-name value &optional alist)
  (when (assoc proc-name (or alist text-translator-processes-alist))
    (setcdr (assoc proc-name (or alist text-translator-processes-alist))
            value)))

(defun text-translator-proc-header-parse (proc buf-name
                                               &optional not-delete-header)
  (with-current-buffer (get-buffer buf-name)
    (goto-char (point-min))
    (when (re-search-forward
           "^\\([\n\r]\\([0-9a-fA-F]+\\)?[ \t]*\\([\n\r]\\)?\\)"
           nil t)
      (let ((header (buffer-substring (point-min) (match-beginning 1)))
            (chunk-len (match-string 2)))
        (unless not-delete-header
          (delete-region (point-min) (match-end 0)))
        (text-translator-proc-header-set
         (process-name proc)
         (let (lis)
           (dolist (i (split-string header "\n" t))
             (setq lis (cons (split-string i ": ") lis)))
           (nreverse lis)))
        (when text-translator-debug
          (message ";; text-translator-proc-header-parse: chunk-len %s"
                   chunk-len))
        chunk-len))))

(defun text-translator-proc-chunk-get (buf-name &optional not-delete-header)
  (with-current-buffer (get-buffer buf-name)
    (goto-char (point-max))
    (cond
     ((re-search-backward "\\([\n\r]\\([0-9a-f]+\\)[ \t]*[\n\r]\\)" nil t)
      (let ((chunk-len (match-string 2)))
        (unless not-delete-header
          (delete-region (match-beginning 0) (match-end 0)))
        (when text-translator-debug
          (message ";; text-translator-proc-chunk-get: t"))
        chunk-len))
     (t
      ;; Dispite Transfer-Encoding is chunked, size did not exist.
      ;; Wait the chunk coming.
      (when text-translator-debug
        (message ";; text-translator-proc-chunk-get: t"))))))

(defun text-translator-timeout-start ()
  (when text-translator-timeout-interval
    (or text-translator-timeout
        (setq text-translator-timeout
              (run-with-timer text-translator-timeout-interval
                              text-translator-timeout-interval
                              'text-translator-timeout)))))

(defun text-translator-timeout ()
  (when text-translator-timeout-interval
    (condition-case err
        (progn
          (text-translator-proc-clear)
          (text-translator-timeout-stop)
          ;; Insert a timeout message.
          (dolist (i text-translator-all-results)
            (when (null (cdr i))
              (setcdr i "TRANSLATION: TIMEOUT")))
          ;; Displaying translated string.
          (text-translator-display (> text-translator-all-site-number 1)))
      (error (message "Error: %S: text-translator-timeout." err)))))

(defun text-translator-timeout-stop ()
  (when text-translator-timeout
    (cancel-timer text-translator-timeout)
    (setq text-translator-timeout nil)))

(defun text-translator-decode-string (str name)
  (let* ((header (text-translator-proc-header-get name))
         (content-type (if header (assoc "Content-Type" header)))
         (http-charset (when (and content-type
                                  (string-match "charset=\\([^ ]*\\)$"
                                                (cadr content-type)))
                         (match-string 1 (cadr content-type))))
         (charset (when http-charset
                    (cdr (assoc http-charset
                                text-translator-charset-alist)))))
    (cond
     (charset
      (when text-translator-debug
        (message ";; charset %s (%s)" http-charset charset))
      (decode-coding-string str charset))
     (t
      str))))

(defun text-translator-replace-string (str replace)
  "Function that converts character string specified for argument STR
according to rule REPLACE."
  (with-temp-buffer
    (insert str)
    ;; convert unusable string
    (format-replace-strings replace)
    (buffer-string)))

(defun text-translator-extract-tag-exclusion-string (regex &optional dont-convert-br)
  (when (re-search-backward regex nil t)
    ;; first: convert <br> tag to '\n' (when variable dont-convert-br is nil)
    ;; second: convert any another tags to empty string.
    (let ((matchstr (match-string 1)))
      (setq matchstr
            (text-translator-replace-string
             matchstr
             text-translator-post-string-replace-alist))
      (replace-regexp-in-string
       "<.*?>" "" (if dont-convert-br
                      matchstr
                    (replace-regexp-in-string
                     "<[bB][rR]\\( /\\)?>" "\n" matchstr))))))

(defun text-translator-check-valid-translation-engine (engine valid-engine)
  "Check ENGINE that is registered in `text-translator-site-data-alist'.
Return ENGINE if it is already registered, otherwise return VALID-ENGINE."
  (or (car (member engine (mapcar 'car text-translator-site-data-alist)))
      valid-engine))

(defun text-translator-get-engine-type-or-site (engine &optional get-site)
  "Get a translation engine type or site name.
If optional argument GET-SITE is nil, return a translation engine type.
Otherwise return a translation site name."
  (let ((val (nth (if get-site 0 1) (split-string engine "_"))))
    (if val val "")))

;; by google2.el
(defun text-translator-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)               ; newline
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             ((char-equal ch ?\x20)     ; space
              "+")
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                  nil))))

;; initialization function
(defun text-translator-site-data-init ()
  ;; initialize
  (setq text-translator-site-data-alist nil)
  (setq text-translator-site-data-alist
        text-translator-site-data-minimum-alist)
  (dolist (site text-translator-site-data-template-alist)
    (let ((tt-convert-name '(lambda (lang)
                            (let ((match-lang (assoc lang
                                                     (nth 7 site))))
                              (if match-lang
                                  (cdr match-lang)
                                lang))))
        (tt-replace-string '(lambda (pstr olang tlang)
                              (when olang
                                (setq pstr
                                    (replace-regexp-in-string "%o"
                                                              olang
                                                              pstr)))
                              (when tlang
                                (setq pstr
                                    (replace-regexp-in-string "%t"
                                                              tlang
                                                              pstr))
                                pstr)))
        tt-alist)
    (dolist (i (nth 6 site))
      (add-to-list 'text-translator-site-data-alist
                   (list (format "%s"
                                 (concat (nth 0 site)
                                         "_"
                                         (funcall tt-convert-name (car i))
                                         (funcall tt-convert-name (cdr i))))
                         (nth 1 site)
                         (nth 2 site)
                         (funcall tt-replace-string
                                  (nth 3 site) (car i) (cdr i))
                         (nth 4 site)
                         (nth 5 site)))))))
(text-translator-site-data-init)        ; init

(provide 'text-translator)
;;; text-translator.el ends here

;; Local Variables:
;; Coding: utf-8
;; End:
