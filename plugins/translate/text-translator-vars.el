;;; text-translator-vars.el --- Text Translator

;; Copyright (C) 2007-2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Variables for text-translator

;;; Code:

(require 'text-translator-sites)


;; Variables:

(defconst text-translator-version "1.0.50"
  "version numbers of this version of text-translator")

(defconst text-translator-buffer "*translated*"
  "Buffer name that displays translation result.")

(defconst text-translator-window-mode-name "Translator"
  "Major mode name for displaying to mode line.")

(defconst text-translator-work-buffer (concat " " text-translator-buffer)
  "Output Buffer name from translation site.")

(defgroup text-translator nil
  "Text Translator"
  :tag "Text Translator"
  :group 'text-translator)

(defcustom text-translator-window-prefix-key "\C-c"
  "*Prefix key for text-translator commands."
  :tag "Prefix Key of text-translator"
  :type '(string :size 10)
  :group 'text-translator)

(defcustom text-translator-auto-window-adjust t
  "*Whether or not you adjust height of window displayed by dividing."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-window-min-height 4
  "*Specify minimum height of the translation result display buffer."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-leave-string nil
  "*Whether or not you leave the character string before the translating."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-pre-string-replace-alist
  '(("+" . "＋") ("&#8211;" . "-")  ("&#8226;" . "・"))
  "*Rule that converts character string that wants to translate."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-post-string-replace-alist
  '(("\r" . "") ("&#39;" . "'") ("&quot;" . "\"")
    ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&#8211;" . "-")
    ("&#264;" . "Ĉ") ("&#265;" . "ĉ") ("&#284;" . "Ĝ") ("&#285;" . "ĝ")
    ("&#292;" . "Ĥ") ("&#293;" . "ĥ") ("&#308;" . "Ĵ") ("&#309;" . "ĵ")
    ("&#348;" . "Ŝ") ("&#349;" . "ŝ") ("&#364;" . "Ŭ") ("&#365;" . "ŭ"))
  "*Rule that converts character string after the translation."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-proxy-server
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
         (match-string 2 proxy)))
  "*Proxy server used."
  :type  '(choice (string :tag "specify proxy")
                  (const :tag "not use proxy" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-port
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (or (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
             (string-to-number (match-string 3 proxy)))
        8080))
  "*Proxy port number used."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-proxy-user nil
  "*Basic proxy authorization user name."
  :type  '(choice (string :tag "Basic proxy authorization user name")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-password nil
  "*Basic proxy authorization password."
  :type  '(choice (string :tag "Basic proxy authorization password")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-default-engine "google.com_enja"
  "*Translation engine used by default."
  :type  (cons 'radio
               (mapcar
                (lambda (x)
                  (list 'const (car x)))
                text-translator-site-data-alist))
  :group 'text-translator)

(defcustom text-translator-user-agent
  (concat "Mozilla/5.0 Text-translator/" text-translator-version)
  "*text-translator's User Agent."
  :type  'string
  :group 'text-translator)

(defcustom text-translator-accept-encoding "identity"
  "*The data of Accept-Encoding request header."
  :type 'string
  :group 'text-translator)

(defcustom text-translator-accept-charset "utf-8"
  "The data of Accept-charset request header."
  :type 'string
  :group 'text-translator)

(defcustom text-translator-keep-alive "300"
  "The data of Keep-alive request header."
  :type 'stringp
  :group 'text-translator)

(defcustom text-translator-connection "keep-alive"
  "The data of connection request header."
  :type 'stringp
  :group 'text-translator)

(defcustom text-translator-mode-hook nil
  "*Hook run at the end of function `text-translator-mode'."
  :type 'hook
  :group 'text-translator)

(defcustom text-translator-auto-selection-func nil
  "*Value is function that select translation engine automatic.
this value is function for `text-translator-translate-by-auto-selection'."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-display-popup nil
  "*Non-nil means translated message is displayed by using popup-tip.
To use this option, you have to require popup.el.
popup.el URL: http://github.com/m2ym/auto-complete"
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-do-fill-region nil
  "*If this value was non-nil, The all translation texta are done
`fill-region'. The default value is nil."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-space-division-languages
  '("en" "es" "fr" "de" "it" "pt" "ru" "nl" "el" "no")
  "*List of language that word is delimited by blank."
  :type '(repeat (string :tag "language(2char)"))
  :group 'text-translator)

(defcustom text-translator-display-function nil
  "*The function that shows translation results.
If this value was `nil', The translation results was showed by
`text-translator-window-display'.
The default value is `nil'."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-timeout-interval 3.00
  "*The translation timeout seconds."
  :type 'integer
  :group 'text-translator)

(defcustom text-translator-debug nil
  "*Non-nil means showing debug messages."
  :type 'symbol
  :group 'text-translator)

(defvar text-translator-charset-alist
  '(("Shift_JIS"   . sjis)
    ("ISO-2022-JP" . iso-2022-7bit)
    ("EUC-JP"      . euc-jp)
    ("ISO-8859-6"  . iso-8859-6)
    ("KOI8-R"      . koi8-r)
    ("EUC-KR"      . euc-kr)
    ("GB2312"      . cn-gb-2312)
    ("Big5"        . big5)
    ("UTF-8"       . utf-8))
  "The alist of HTTP charset and emacs charset.")

(defvar text-translator-all-history nil
  "The value has history of all translation.
This value is list of\(engine before_string after_string\).")

(defvar text-translator-all-results nil
  "The internal variable of `text-translator'.
This value is list of \(buffer_name after_string\).")

(defvar text-translator-all-site-number nil
  "The internal variable of `text-translator'.
This value stores the number of translation.
This variables was used to distinguish `text-translator-all' and
`text-translator'.")

(defvar text-translator-all-before-string nil
  "The internal variable of `tex-translator'.
This value is list of \(buffer_name engine before_string
after_string\).")

(defvar text-translator-processes-alist nil
  "The internal variable of `text-translator'.
This values stores detail of the process which worked in last
translation.")

(defvar text-translator-sitedata-hash nil
  "The internal variable of `text-translator'.
This value has hash table of translation site data.")

(defvar text-translator-timeout nil
  "The internal variable of `text-translator'.
This variable used by the translation process monitoring timer.")

(defvar text-translator-send-string nil
  "The debug purpose variable of `text-translator'.
This variable has send string of last request.")

(provide 'text-translator-vars)

;;; text-translator-vars.el ends here

;; Local Variables:
;; Coding: utf-8
;; End:
