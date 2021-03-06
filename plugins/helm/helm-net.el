;;; helm-net.el --- helm browse url and search web.

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)
(require 'url)
(require 'xml)


;;; Google Suggestions
;;
;;
;; Internal
(defvar helm-ggs-max-length-real-flag 0)
(defvar helm-ggs-max-length-num-flag 0)

(defun helm-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (setq helm-ggs-max-length-real-flag 0
        helm-ggs-max-length-num-flag 0)
  (let ((request (concat helm-c-google-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                   with result-alist = (xml-get-children
                                        (car (xml-parse-region
                                              (point-min) (point-max)))
                                        'CompleteSuggestion)
                   for i in result-alist
                   for data = (cdr (caadr (assoc 'suggestion i)))
                   for nqueries = (cdr (caadr (assoc 'num_queries i)))
                   for lqueries = (length (helm-c-ggs-set-number-result
                                           nqueries))
                   for ldata = (length data)
                   do
                   (progn
                     (when (> ldata helm-ggs-max-length-real-flag)
                       (setq helm-ggs-max-length-real-flag ldata))
                     (when (> lqueries helm-ggs-max-length-num-flag)
                       (setq helm-ggs-max-length-num-flag lqueries)))
                   collect (cons data nqueries) into cont
                   finally return cont)))
      (if helm-google-suggest-use-curl-p
          (with-temp-buffer
            (call-process "curl" nil t nil request)
            (fetch))
          (with-current-buffer
              (url-retrieve-synchronously request)
            (fetch))))))

(defun helm-c-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of google results found."
  (let ((suggestions
         (loop with suggested-results = (helm-c-google-suggest-fetch
                                         (or (and request-prefix
                                                  (concat request-prefix
                                                          " " helm-pattern))
                                             helm-pattern))
               for (real . numresult) in suggested-results
               ;; Prepare number of results with ","
               for fnumresult = (helm-c-ggs-set-number-result numresult)
               ;; Calculate number of spaces to add before fnumresult
               ;; if it is smaller than longest result
               ;; `helm-ggs-max-length-num-flag'.
               ;; e.g 1,234,567
               ;;       345,678
               ;; To be sure it is aligned properly.
               for nspaces = (if (< (length fnumresult)
                                    helm-ggs-max-length-num-flag)
                                 (- helm-ggs-max-length-num-flag
                                    (length fnumresult))
                                 0)
               ;; Add now the spaces before fnumresult.
               for align-fnumresult = (concat (make-string nspaces ? )
                                              fnumresult)
               for interval = (- helm-ggs-max-length-real-flag
                                 (length real))
               for spaces   = (make-string (+ 2 interval) ? )
               for display = (format "%s%s(%s results)"
                                     real spaces align-fnumresult)
               collect (cons display real))))
    (if (loop for (disp . dat) in suggestions
              thereis (equal dat helm-pattern))
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Google item to the list
        (append
         suggestions
         (list (cons (concat "Search for " "'" helm-input "'" " on Google")
                     helm-input))))))

(defun helm-c-ggs-set-number-result (num)
  (if num
      (progn
        (and (numberp num) (setq num (number-to-string num)))
        (loop for i in (reverse (split-string num "" t))
              for count from 1
              append (list i) into C
              when (= count 3)
              append (list ",") into C
              and do (setq count 0)
              finally return
              (replace-regexp-in-string
               "^," "" (mapconcat 'identity (reverse C) ""))))
      "?"))

(defvar helm-c-google-suggest-default-browser-function nil
  "*The browse url function you prefer to use with google suggest.
When nil, use the first browser function available
See `helm-browse-url-default-browser-alist'.")

(defun helm-c-google-suggest-action (candidate)
  "Default action to jump to a google suggested candidate."
  (let ((arg (concat helm-c-google-suggest-search-url
                     (url-hexify-string candidate))))
    (helm-aif helm-c-google-suggest-default-browser-function
        (funcall it arg)
      (helm-c-browse-url arg))))

(defvar helm-c-google-suggest-default-function
  'helm-c-google-suggest-set-candidates
  "Default function to use in helm google suggest.")

(defvar helm-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . (lambda ()
                    (funcall helm-c-google-suggest-default-function)))
    (action . (("Google Search" . helm-c-google-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defun helm-c-google-suggest-emacs-lisp ()
  "Try to emacs lisp complete with google suggestions."
  (helm-c-google-suggest-set-candidates "emacs lisp"))


;;; Yahoo suggestions
;;
;;
(defun helm-c-yahoo-suggest-fetch (input)
  "Fetch Yahoo suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat helm-c-yahoo-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                   with result-alist = (xml-get-children
                                        (car (xml-parse-region
                                              (point-min) (point-max)))
                                        'Result)
                   for i in result-alist
                   collect (caddr i))))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (fetch)))))

(defun helm-c-yahoo-suggest-set-candidates ()
  "Set candidates with Yahoo results found."
  (let ((suggestions (helm-c-yahoo-suggest-fetch helm-input)))
    (or suggestions
        (append
         suggestions
         (list (cons (concat "Search for " "'" helm-input "'" " on Yahoo")
                     helm-input))))))

(defun helm-c-yahoo-suggest-action (candidate)
  "Default action to jump to a Yahoo suggested candidate."
  (helm-c-browse-url (concat helm-c-yahoo-suggest-search-url
                             (url-hexify-string candidate))))

(defvar helm-c-source-yahoo-suggest
  '((name . "Yahoo Suggest")
    (candidates . helm-c-yahoo-suggest-set-candidates)
    (action . (("Yahoo Search" . helm-c-yahoo-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))


;;; Web browser functions.
;;
;;
(require 'browse-url)
;; If default setting of `w3m-command' is not
;; what you want you and you modify it, you will have to reeval
;; also `helm-browse-url-default-browser-alist'.
(defvar w3m-command "/usr/bin/w3m")
(defvar helm-c-home-url "http://www.google.fr"
  "*Default url to use as home url.")

(defvar helm-browse-url-chromium-program "chromium-browser")
(defvar helm-browse-url-uzbl-program "uzbl-browser")
(defvar helm-browse-url-default-browser-alist
  `((,w3m-command . w3m-browse-url)
    (,browse-url-firefox-program . browse-url-firefox)
    (,helm-browse-url-chromium-program . helm-browse-url-chromium)
    (,helm-browse-url-uzbl-program . helm-browse-url-uzbl)
    (,browse-url-kde-program . browse-url-kde)
    (,browse-url-gnome-moz-program . browse-url-gnome-moz)
    (,browse-url-mozilla-program . browse-url-mozilla)
    (,browse-url-galeon-program . browse-url-galeon)
    (,browse-url-netscape-program . browse-url-netscape)
    (,browse-url-mosaic-program . browse-url-mosaic)
    (,browse-url-xterm-program . browse-url-text-xterm))
  "*Alist of \(executable . function\) to try to find a suitable url browser.")

(defun* helm-c-generic-browser (url name &rest args)
  "Browse URL with NAME browser."
  (let ((proc (concat name " " url)))
    (message "Starting %s..." name)
    (apply 'start-process proc nil name
           (append args (list url)))
    (set-process-sentinel
     (get-process proc)
     #'(lambda (process event)
         (when (string= event "finished\n")
           (message "%s process %s" process event))))))

(defun helm-browse-url-chromium (url)
  "Browse URL with google chrome browser."
  (interactive "sURL: ")
  (helm-c-generic-browser
   url helm-browse-url-chromium-program))

(defun helm-browse-url-uzbl (url &optional ignore)
  "Browse URL with uzbl browser."
  (interactive "sURL: ")
  (helm-c-generic-browser url helm-browse-url-uzbl-program "-u"))

(defun helm-browse-url-default-browser (url &rest args)
  "Find the first available browser and ask it to load URL."
  (let ((default-browser-fn
         (loop for (exe . fn) in helm-browse-url-default-browser-alist
               thereis (and exe (executable-find exe) fn))))
    (if default-browser-fn
        (apply default-browser-fn url args)
        (error "No usable browser found"))))

(defun helm-c-browse-url (url &rest args)
  "Default command to browse URL."
  (if browse-url-browser-function
      (browse-url url args)
      (helm-browse-url-default-browser url args)))


;;; Surfraw
;;
;; Need external program surfraw.
;; <http://surfraw.alioth.debian.org/>

(defvar helm-surfraw-default-browser-function nil
  "*The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'.")

;; Internal
(defvar helm-surfraw-engines-history nil)
(defvar helm-surfraw-input-history nil)

(defun helm-c-build-elvi-list ()
  "Return list of all engines and descriptions handled by surfraw."
  (cdr
   (with-temp-buffer
     (call-process "surfraw" nil t nil
                   "-elvi")
     (split-string (buffer-string) "\n"))))

;;;###autoload
(defun helm-surfraw (pattern engine)
  "Preconfigured `helm' to search PATTERN with search ENGINE."
  (interactive (list (read-string "SearchFor: "
                                  nil 'helm-surfraw-input-history)
                     (helm-comp-read
                      "Engine: "
                      (helm-c-build-elvi-list)
                      :must-match t
                      :name "Surfraw Search Engines"
                      :history helm-surfraw-engines-history)))
  (let* ((engine-nodesc (car (split-string engine)))
         (url (with-temp-buffer
                (apply 'call-process "surfraw" nil t nil
                       ;;JAVE
                       (append  (list engine-nodesc "-p") (split-string pattern)))
                (replace-regexp-in-string
                 "\n" "" (buffer-string))))
         (browse-url-browser-function (or helm-surfraw-default-browser-function
                                          browse-url-browser-function)))
    (if (string= engine-nodesc "W")
        (helm-c-browse-url helm-c-home-url)
        (helm-c-browse-url url)
        (setq helm-surfraw-engines-history
              (cons engine (delete engine helm-surfraw-engines-history))))))

;;;###autoload
(defun helm-google-suggest ()
  "Preconfigured `helm' for google search with google suggest."
  (interactive)
  (helm-other-buffer 'helm-c-source-google-suggest "*helm google*"))

;;;###autoload
(defun helm-yahoo-suggest ()
  "Preconfigured `helm' for Yahoo searching with Yahoo suggest."
  (interactive)
  (helm-other-buffer 'helm-c-source-yahoo-suggest "*helm yahoo*"))


(provide 'helm-net)

;;; helm-net.el ends here
