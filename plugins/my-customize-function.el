;; -*-Emacs-Lisp-*-

;;;==============================自定义函数==============================
;; 有关自定义函数添加(interactive)的提示
;; 如果没有(interactive),则这个函数无法交互调用,即无法通过M-x 函数名称
;; 或快捷键的方式调用,但是可以直接在代码内调用,例如(函数名)或M-:快捷键

;; (require 'textmate)
;; (setq tm/backspace-delete-column t)
;; (add-to-list 'tm/non-insert-alist '(css-mode . '(?\{)))

(require 'thingatpt)
(require 'imenu)
(require 'idle-highlight-mode)
(require 'sequential-command)
(require 'auto-indent-mode)
(setq auto-indent-backward-delete-char-behavior 'nil)

(autoload 'auto-indent-yank "auto-indent-mode" "" t)
(autoload 'auto-indent-yank-pop "auto-indent-mode" "" t)
(define-key global-map [remap yank] 'auto-indent-yank)
(define-key global-map [remap yank-pop] 'auto-indent-yank-pop)
(autoload 'auto-indent-delete-char "auto-indent-mode" "" t)
(define-key global-map [remap delete-char] 'auto-indent-delete-char)
(autoload 'auto-indent-kill-line "auto-indent-mode" "" t)
(define-key global-map [remap kill-line] 'auto-indent-kill-line)
;; (auto-indent-global-mode)
(require 'autopair)
(autoload 'paredit-mode "paredit" "" t)

;; ------------------------------X下最大化函数------------------------------
;; X下全屏
(defun x-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
;; X下最大化
(defun x-new-frame-maximize (&optional frame)
  (run-with-idle-timer 0 nil 'x-send-client-message
                       nil 0 nil "_NET_WM_STATE" 32
                       '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (run-with-idle-timer 0 nil 'x-send-client-message
                       nil 0 nil "_NET_WM_STATE" 32
                       '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;; WIN32下最大化
(defun w32-new-frame-maximize (&optional frame)
  (run-with-idle-timer 0 nil 'w32-send-sys-command 61488))

(when (equal window-system 'x)
  (x-new-frame-maximize)
  (add-hook 'after-make-frame-functions 'x-new-frame-maximize))

(when (equal window-system 'w32)
  (setq-default ispell-program-name (concat plugins "aspell/bin/aspell.exe")) ;WIN32下，aspell路径设置
  (run-with-idle-timer 0 nil 'w32-send-sys-command 61488)
  ;; 注意,传送给aftar-make-frame-function的函数必须有且只能有一个参数用来表示新建立的frame.
  (add-hook 'after-make-frame-functions 'w32-new-frame-maximize))

(add-hook 'server-visit-hook 'x-new-frame-maximize) ;开启deamon，打开文件最大化

;; ------------------------------函数定义------------------------------
(defun insert-date () ;定义函数，用于插入当前日期
  "Insert date at point."
  (interactive)
  ;; (insert (format-time-string"Author: Billy(zw963) mailto:zw963@163.com\r"))
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" ))
  (move-end-of-line 1))

(defun view-man-help () ;定义函数，调用UNIX系统的man命令
  "Opening man help page..."
  (interactive)
  (manual-entry (current-word)))

(defun help-go-back-other-window ()
  "View previous help buffer other window."
  (interactive)
  (other-window 1)
  (help-go-back)
  (other-window 1))
(defun help-go-forward-other-window ()
  "View next help buffer other window."
  (interactive)
  (other-window 1)
  (help-go-forward)
  (other-window 1))

;; 有关大小写p的区别.
;; 小写的p, 总是将任意的参数转换为一个有意义的数字.即使不指定参数, 即参数为nil, 默认代表1.
;; 大写的p, 除非显式的通过C-u或其他方式指定参数, 否则所有的nil, 当作无参数处理.
(defun window-move-up (&optional arg)
  "Current window move-up 2 lines."
  (interactive "P")
  (if (region-active-p)
      (next-line nil)
    (if arg
        (scroll-up arg)
      (scroll-up 2))))
(defun window-move-down (&optional arg)
  "Current window move-down 3 lines."
  (interactive "P")
  (if (region-active-p)
      (previous-line nil)
    (if arg
        (scroll-down arg)
      (scroll-down 3))))

(defun other-window-move-up (&optional arg)
  "Other window move-up 1 lines."
  (interactive "p")
  (if (region-active-p)
      (next-line nil)
    (scroll-other-window arg)))
(defun other-window-move-down (&optional arg)
  "Other window move-down 2 lines."
  (interactive "P")
  (if (region-active-p)
      (previous-line nil)
    (if arg
        (scroll-other-window-down arg)
      (scroll-other-window-down 2))))

;; (defun Meta-R-or-previous-error (&optional arg)
;;   (interactive "p")
;;   (cond
;;    ((get-buffer-window "*ruby*") (previous-error arg))
;;    ((get-buffer-window "*compilation*") (previous-error arg))
;;    ((get-buffer-window "*rake*") (previous-error arg))
;;    (t (move-to-window-line-top-bottom))))

;; (defun Meta-F-or-next-error (arg char)
;;   ;; (interactive "p")
;;   (interactive "p\ncZap to char: ")
;;   (cond
;;    ((get-buffer-window "*ruby*") (next-error arg))
;;    ((get-buffer-window "*compilation*") (next-error arg))
;;    ((get-buffer-window "*rake*") (next-error arg))
;;    (t (zap-to-char arg char))))

(defun save-buffer-and-recompile ()
  (interactive)
  (save-buffer)
  (recompile))

(defun mark-line (&optional arg allow-extend)
  "Put point at beginning of this line, mark at end.
The line marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following line, so that
the number of lines marked equals ARG.

If ARG is negative, point is put at end of this line, mark is put
at beginning of this or a previous line.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next ARG lines after
the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero lines"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (forward-line arg)
         (push-mark nil t t)
         (previous-line arg))))

(defun mark-lines-next-line ()
  (interactive)
  (if (region-active-p)
      (next-line nil)
    (mark-lines-previous-line nil)))

(defun copy-line (&optional arg)
  "Kills a line, not including leading indentation"
  (interactive "p")
  (mark-line arg)
  (kill-ring-save (point) (mark)))

;; (defun backward-upcase-word (&optional arg)
;;   (interactive "p")
;;   (upcase-word (- (or arg 1))))

(defun backward-upcase-word ()
  (interactive)
  (upcase-word (- (1+ (seq-count)))))

(defun backward-capitalize-word ()
  (interactive)
  (capitalize-word (- (1+ (seq-count)))))

(defun backward-downcase-word ()
  (interactive)
  (downcase-word (- (1+ (seq-count)))))

(defun backward-mark-word ()
  (interactive)
  (mark-word (- (1+ (seq-count)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun isearch-yank-word-backward ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (backward-word 1) (point))))

(defun delete-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun google-translate-this-word ()
  (interactive)
  (if (region-active-p)
      (text-translator-translate-by-auto-selection nil)
    (progn
      (mark-word nil)
      (text-translator-translate-by-auto-selection nil))))

(defun kill-whole-line-or-kill-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line nil)))

(defun kill-line-or-kill-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-line nil)))

(defun down-list-or-delete-region (&optional arg)
  (interactive "^p")
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (down-list arg)))

(defun apend-killed-line ()
  (interactive)
  (append-next-kill)
  (kill-whole-line-or-kill-region))

(defun kill-current-line-and-indent ()
  "Kill all character in current line."
  (interactive)
  (kill-whole-line 0)
  (indent-for-tab-command))

(defun copy-whole-line-or-copy-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (mark-lines-previous-line nil)
      (kill-ring-save (region-beginning) (region-end)))))

(defun html-mark-sexp ()
  (interactive)
  (push-mark nil t t)
  (sgml-skip-tag-forward 1))
(defun html-kill-sexp ()
  (interactive)
  (push-mark nil t t)
  (sgml-skip-tag-forward 1)
  (kill-region (region-beginning) (region-end)))

(defun set-this-point-as-register-1 ()
  (interactive)
  (point-to-register 49 nil))

(defun show-existing-help-buffer ()
  "Show helper buffer."
  (interactive)
  (display-buffer "*Help*" nil))

(defun transpose-lines-backward (&optional arg)
  "Move current lines up."
  (interactive "p")
  (transpose-lines -1))

(defun transpose-words-backward (&optional arg)
  "Move current word left."
  (interactive "p")
  (transpose-words -1))

(defun transpose-chars-backward (&optional arg)
  "Move current word left."
  (interactive "p")
  (transpose-chars -1))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun format-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun format-paragraph ()
  "Perform a bunch of operations on the whitespace content of a paragraph."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-paragraph))
    (indent-region (region-beginning) (region-end))
    (untabify (region-beginning) (region-end))
    ;; (flush-lines "^\\s-*$" (region-beginning) (region-end))
    (whitespace-cleanup-region (region-beginning) (region-end)))
  (deactivate-mark)
  (indent-for-tab-command))

(defun lisp-format-defun ()
  "Perform a bunch of operations on the whitespace content of a lisp function."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-defun))
    (indent-region (region-beginning) (region-end))
    (untabify (region-beginning) (region-end))
    (whitespace-cleanup-region (region-beginning) (region-end)))
  (deactivate-mark)
  (delete-blank-lines)
  (indent-for-tab-command))

(defun ruby-format-defun ()
  "Perform a bunch of operations on the whitespace content of a ruby function."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (ruby-mark-defun))
    (indent-region (region-beginning) (region-end))
    (untabify (region-beginning) (region-end))
    (whitespace-cleanup-region (region-beginning) (region-end)))
  (deactivate-mark)
  (delete-blank-lines)
  (indent-for-tab-command))

(defun turn-on-format-paragraph ()
  (local-set-key [(control c) (control c)] 'format-paragraph))
(defun turn-on-format-buffer ()
  (local-set-key [(control c) (control c)] 'format-buffer))
(defun turn-on-format-lisp-function ()
  (local-set-key [(control c) (control c)] 'lisp-format-defun))
(defun turn-on-format-ruby-function ()
  (local-set-key [(control c) (control c)] 'ruby-format-defun))

(defun switch-to-mode-compile-window (&optional arg)
  (interactive "P")
  (mode-compile arg)
  (switch-to-buffer-other-window "*compilation*"))

(defun turn-on-mode-compile ()
  (local-set-key [(control c) (return)] 'switch-to-mode-compile-window)
  (local-set-key [(control c) (?\r)] 'switch-to-mode-compile-window))

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  ;; (setq fill-column 80)
  (auto-fill-mode t))

(defun local-yas/triggers-in-field ()
  (make-local-variable 'yas/triggers-in-field)
  (setq yas/triggers-in-field t))

(defun save-all-buffer-and-kill-terminal (&optional arg)
  (interactive "P")
  (save-some-buffers)
  (save-buffers-kill-terminal arg))

(global-set-key [(control x) (control c)] 'save-all-buffer-and-kill-terminal)

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; Cosmetic 化妆??真幽默.....
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun insert-my-comment (arg)
(interactive "*P")
(comment-normalize-vars)
(if comment-insert-comment-function
    (funcall comment-insert-comment-function)
  (let ((add (comment-add arg)))
    ;; Some modes insist on keeping column 0 comment in column 0
    ;; so we need to move away from it before inserting the comment.
    (indent-according-to-mode)
    (insert (comment-padright comment-start add))
    (save-excursion
      (unless (string= "" comment-end)
        (insert (comment-padleft comment-end add)))
      (indent-according-to-mode)))))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8) (hl-line-mode t)))
(defun turn-on-idle-highlight ()
  (idle-highlight-mode t))
(defun point-auto-end-line ()
  (setq line-move-visual nil)
  (setq track-eol t))
(defun turn-on-linum-mode ()
  (linum-mode t))
(defun turn-on-autopair-mode()
  (autopair-mode t))
(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-paredit ()
  (paredit-mode t))
(defun turn-on-auto-fill ()
  (auto-fill-mode t)
  (local-set-key [(control c) (control c)] 'fill-paragraph))
(defun turn-on-newline-and-indent ()
  (local-set-key [(return)] 'newline-and-indent)
  (local-set-key [(?\r)] 'newline-and-indent))
(defun tm/open-next-line()
  "Function to open and goto indented next line"
  (interactive)
  ;; (indent-according-to-mode)
  (end-of-line)
  (reindent-then-newline-and-indent))
(defun tm/open-next-line-and-semi-colon()
  "Function to open and goto indented next line and first insert a semi-colon"
  (interactive)
  (end-of-line)
  (unless (looking-back "^\\s-*")
    (unless (looking-back "; *")
      (unless (looking-back "} *")
        (insert ";"))))
  (reindent-then-newline-and-indent))
(defun tm/backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char-untabify))))))
(defun turn-on-textmate-like-open-next-line ()
  (local-set-key [(return)] 'tm/open-next-line)
  (local-set-key [(?\r)] 'tm/open-next-line))
(defun next-line-with-semi-colon ()
  (local-set-key [(return)] 'tm/open-next-line-and-semi-colon)
  (local-set-key [(?\r)] 'tm/open-next-line-and-semi-colon))
;; (defun turn-on-textmate-minor-mode ()
;;   (setq skeleton-pair t)
;;   (tm/set-keymap)
;;   (tm/minor-mode-auto-on))
(defun turn-on-textmate-like-backspace ()
  (local-set-key [(backspace)] 'tm/backward-delete-whitespace-to-column)
  (local-set-key [(?\d)] 'tm/backward-delete-whitespace-to-column))
;; (defun turn-auto-complete-minor-mode ()
;;   (define-globalized-minor-mode real-global-auto-complete-mode
;;     auto-complete-mode (lambda ()
;;                          (auto-complete-mode 1)))
;;   (real-global-auto-complete-mode t))
;; (defun ac-html-mode-setup ())
;; (add-hook 'html-mode-hook 'ac-html-mode-setup)
;; (global-auto-complete-mode t)
(add-hook 'coding-hook
          '(lambda ()
             (local-comment-auto-fill)
             (add-watchwords)
             (pretty-lambdas)
             (turn-on-hl-line-mode)
             (turn-on-idle-highlight)
             (point-auto-end-line)
             (turn-on-linum-mode)
             (turn-on-textmate-like-open-next-line)
             (turn-on-textmate-like-backspace)
             (turn-on-format-buffer)
             (turn-on-mode-compile)
             (turn-on-autopair-mode)
             ))

(add-hook 'text-editing-hook
          '(lambda ()
             (local-set-key [(control c) (return)] 'org-ctrl-c-ret)
             (local-set-key [(control c) (?\r)] 'org-ctrl-c-ret)
             (local-set-key [(control j)] 'org-return-indent)
             ;; (turn-on-auto-fill)
             ;; (turn-on-flyspell)
             ;; (turn-on-orgtbl)  ;开启org表格支持
             ))

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun run-text-editing-hook ()
  "Enable things that are convenient across all text editing buffers."
  (run-hooks 'text-editing-hook))

(require 'flymake-shell)
(add-hook 'sh-mode-hook (lambda () (run-coding-hook)
                          (flymake-shell-load)))
(add-hook 'c-mode-hook (lambda () (run-coding-hook)
                         (next-line-with-semi-colon)))
(add-hook 'emacs-lisp-mode-hook (lambda () (run-coding-hook)
                                  (turn-on-format-lisp-function)))
(add-hook 'ruby-mode-hook (lambda () (run-coding-hook)
                            (turn-on-format-ruby-function)))
(add-hook 'html-mode-hook (lambda () ;; (run-coding-hook)
                            (local-yas/triggers-in-field)
                            (turn-on-format-buffer)))
(add-hook 'js-mode-hook (lambda () (run-coding-hook)))
(add-hook 'css-mode-hook (lambda () (run-coding-hook)
                           (next-line-with-semi-colon)
                           (local-set-key [(control c) (control c)] 'format-buffer)))
(add-hook 'snippet-mode-hook (lambda () (turn-on-whitespace)
                               (turn-off-auto-fill)
                               (local-set-key [(control c) (control c)] 'delete-trailing-whitespace)
                               (local-set-key [(control meta g)] 'yas/load-snippet-buffer)))


(add-hook 'markdown-mode-hook (lambda () (run-text-editing-hook)
                                (local-set-key [(control c) (control c)] 'format-paragraph)))
(add-hook 'message-mode-hook (lambda () (run-text-editing-hook)))
;; (add-hook 'edit-server-start-hook (lambda () (run-text-editing-hook)))
(add-hook 'text-mode-hook (lambda () (run-text-editing-hook)))
(add-hook 'org-mode-hook (lambda () (turn-off-auto-fill)
                           (turn-on-textmate-like-backspace)))
(defun message-point ()
  "Show current point's value in minibuffer."
  (interactive)
  (message "%s" (point)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(remove-dos-eol)

(defun dired-filter-by-name(filter-regexp)
  (interactive "s(only show matched):")
  (let ((dired-marker-char 16)
        (files (directory-files default-directory t)))
    ;;(dired-unmark-all-files dired-marker-char)
    (save-excursion
      (dolist (file files)
        (when (and (dired-goto-file  (expand-file-name file))
                   (not (string= "" filter-regexp))
                   (string-match filter-regexp (file-name-nondirectory file)))
          (dired-mark 1)
          )))
    (dired-toggle-marks)
    (dired-do-kill-lines nil (concat "Filter:'" filter-regexp "' omitted %d line%s"))
    (dired-move-to-filename)))

(if (not (fboundp 'time-less-p))
                                        ; definition from xemacs21
    (defun time-less-p (t1 t2)
      "Say whether time value T1 is less than time value T2."
      (or (< (car t1) (car t2))
          (and (= (car t1) (car t2))
               (< (nth 1 t1) (nth 1 t2))))))

;; ;; 自动恢复autosave文件.
;; (defun csm-check-recover ()
;;   (interactive)
;;   (let ((buffer-mtime) (autosave-mtime))
;;     (progn
;;       (setq buffer-mtime (nth 5 (file-attributes (buffer-file-name))))
;;       (setq autosave-mtime (nth 5 (file-attributes (make-auto-save-file-name))))
;;       (if (time-less-p buffer-mtime autosave-mtime)
;;           (if (file-exists-p (make-auto-save-file-name))
;;               (if (not (local-variable-p 'csm-check-recover-loop-block
;;                                          (current-buffer)))
;;                   (progn
;;                     (make-local-variable 'csm-check-recover-loop-block)
;;                     (if (y-or-n-p (concat "file " (buffer-file-name) " has autosavedata. Recover? "))
;;                         (recover-file (buffer-file-name))
;;                       (kill-local-variable 'csm-check-recover-loop-block)))))))))

;; (add-hook 'find-file-hooks 'csm-check-recover)

;; 在Emacs daemon启动方式下,取消启动提示Autosave文件.
(defadvice desktop-restore-file-buffer
  (around my-desktop-restore-file-buffer-advice)
  "Be non-interactive while starting a daemon."
  (if (and (daemonp)
           (not server-process))
      (let ((noninteractive t))
        ad-do-it)
    ad-do-it))
(ad-activate 'desktop-restore-file-buffer)

;; 使用ediff比较当前buffer与auto save文件.
(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))
(defun ediff-auto-save ()
  "Ediff current file and its auto-save pendant."
  (interactive)
  (let ((auto-file-name (make-auto-save-file-name))
        (file-major-mode major-mode))
    (ediff-files buffer-file-name auto-file-name)
    (switch-to-buffer-other-window (file-name-nondirectory auto-file-name))
    (apply file-major-mode '())
    (other-window 1)
    )) ;; back to ediff panel

;; ;; 如果存在Auto save文件,更改Emacs背景.
;;  (defface recover-this-file
;;   '((t :background "orange"))
;;   "Face for buffers visiting files with auto save data."
;;   :group 'files)
;; (defvar recover-this-file nil
;;   "If non-nil, an overlay indicating that the visited file has auto save data.")
;; (defun recover-this-file-find-file-hook ()
;;   ;; see after-find-file
;;   (let ((warn (not buffer-read-only)))
;;     (when (and warn
;;                ;; No need to warn if buffer is auto-saved
;;                ;; under the name of the visited file.
;;                (not (and buffer-file-name
;;                          auto-save-visited-file-name))
;;                (file-newer-than-file-p (or buffer-auto-save-file-name
;;                                            (make-auto-save-file-name))
;;                                        buffer-file-name))
;;       (set (make-local-variable 'recover-this-file)
;;            (make-overlay (point-min) (point-max)))
;;       (overlay-put recover-this-file 'face 'recover-this-file))))
;; (add-hook 'find-file-hook 'recover-this-file-find-file-hook)

;;;==============================宏定义==============================

(fset '\previous-file-buffer ;定义宏，前一个文件缓冲区
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 98 117 102 102 101 114 32 109 101 110 117 13 134217848 66 117 102 102 101 114 32 109 101 110 117 32 116 111 103 103 108 101 32 102 105 108 101 115 32 111 110 108 121 13 134217848 101 110 100 32 111 102 32 98 117 102 102 101 114 13 134217848 112 114 101 118 105 111 117 115 32 108 105 110 101 13 13 134217848 107 105 108 108 32 98 117 102 102 101 114 13 42 66 117 102 102 101 114 32 76 105 115 116 42 13] 0 "%d")) arg)))

(fset '\next-file-buffer ;定义宏，下一个文件缓冲区
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 98 117 102 102 101 114 32 109 101 110 117 13 134217848 66 117 102 102 101 114 32 109 101 110 117 32 116 111 103 103 108 101 32 102 105 108 101 115 32 111 110 108 121 13 134217848 98 101 103 105 110 110 105 110 103 32 111 102 32 98 117 102 102 101 114 13 134217848 66 117 102 102 101 114 32 109 101 110 117 32 98 117 114 121 13 13 134217848 107 105 108 108 32 98 117 102 102 101 114 13 42 66 117 102 102 101 114 32 76 105 115 116 42 13] 0 "%d")) arg)))

(provide 'my-customize-function)

;;; my-customize-function.el ends here


