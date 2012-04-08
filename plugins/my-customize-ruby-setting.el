;; -*-Emacs-Lisp-*-
;; ------------------------------ruby-mode设置------------------------------
(setq ruby-path (expand-file-name "~/.emacs.d/plugins/ruby-mode/"))
(setq rails-path (expand-file-name "~/.emacs.d/plugins/ruby-mode/rails/"))
(add-to-list 'load-path ruby-path) ;
(add-to-list 'load-path rails-path)
(require 'ruby-mode)
;; (add-to-list 'load-path (concat ruby-path "ruby-mode-1.9.2"))
;; (setq enh-ruby-program "/root/.rvm/rubies/ruby-1.9.2-p290/bin/ruby")
;; (require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; ------------------------------info-ruby设置------------------------------
(require 'inf-ruby)
(add-to-list 'inf-ruby-implementations '("ruby" . "irb --inf-ruby-mode -r irb/completion"))
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-keys))
;; ------------------------------RVM设置------------------------------
(require 'rvm)
(setq rvm-executable "~/.rvm/bin/rvm")
(setq-default rvm--gemset-default "rails3.2.1")
(rvm-use-default)

(global-set-key [(shift f5)] '(lambda () (interactive) (rvm-use "rbx-1.2.4-20110705" "test")))
(global-set-key [(shift f6)] '(lambda () (interactive) (rvm-use "ruby-1.8.6-p420" "test")))
(global-set-key [(shift f7)] '(lambda () (interactive) (rvm-use "ruby-1.8.7-p352" "rails3.2.1")))
(global-set-key [(shift f8)] '(lambda () (interactive) (rvm-use "ruby-1.9.2-p318" "rails3.2.1")))
;; (global-set-key [(shift f9)] '(lambda () (interactive) (rvm-use "ruby-1.9.2-p290" "test"))))

;; ------------------------------ruby-debug设置------------------------------
(add-to-list 'load-path (concat ruby-path "ruby-debug"))
;; (autoload 'test-unit "test-unit" "test unit??" t)
;; (autoload 'dbgr-rdebug "dbgr" "dbgr??" t)

;; (require 'rubydb)

(require 'test-unit)
;; (require 'load-relative)
;; (require 'loc-changes)
(require 'dbgr)
;; (global-set-key [(f5)] ')
;; (global-set-key [(f6)] ')
;; (global-set-key [(f7)] ')
;; (global-set-key [(f8)] ')
;; (global-set-key [(f9)] ')

;; ------------------------------Ruby ri------------------------------
;; (autoload 'ri "ri-ruby" nil t)
;; (setq ri-ruby-program "/usr/bin/ruby")
;; (setq ri-ruby-script (concat ruby-path "ri-emacs.rb"))
(require 'yari)
;; ------------------------------rails-mode------------------------------
;; (add-to-list 'load-path (concat rails-path "emacs-rails-mode"))
;; (require 'rails)
(add-to-list 'load-path (concat rails-path "rinari"))
;; (require 'ruby-compilation)
(require 'rinari)
(global-set-key [(control f) (\;)] 'rinari-find-by-context)
(global-set-key [(control f) (F)] 'rinari-find-features)
(global-set-key [(control f) (M)] 'rinari-find-mailer)
(global-set-key [(control f) (S)] 'rinari-find-steps)
(global-set-key [(control f) (Y)] 'rinari-find-sass)
(global-set-key [(control f) (a)] 'rinari-find-application)
(global-set-key [(control f) (c)] 'rinari-find-controller)
(global-set-key [(control f) (e)] 'rinari-find-environment)
(global-set-key [(control f) (f)] 'rinari-find-file-in-project)
(global-set-key [(control f) (h)] 'rinari-find-helper)
(global-set-key [(control f) (i)] 'rinari-find-migration)
(global-set-key [(control f) (j)] 'rinari-find-javascript)
(global-set-key [(control f) (l)] 'rinari-find-lib)
(global-set-key [(control f) (m)] 'rinari-find-model)
(global-set-key [(control f) (n)] 'rinari-find-configuration)
(global-set-key [(control f) (o)] 'rinari-find-log)
(global-set-key [(control f) (p)] 'rinari-find-public)
(global-set-key [(control f) (s)] 'rinari-find-script)
(global-set-key [(control f) (t)] 'rinari-find-test)
(global-set-key [(control f) (u)] 'rinari-find-plugin)
(global-set-key [(control f) (v)] 'rinari-find-view)
;; (global-set-key [(control f) (control v)] 'rinari-find-view)
(global-set-key [(control f) (w)] 'rinari-find-worker)
(global-set-key [(control f) (control r)] 'rinari-rake)
(global-set-key [(control f) (r)] 'rinari-find-rspec)
(global-set-key [(control f) (x)] 'rinari-find-fixture)
(global-set-key [(control f) (y)] 'rinari-find-stylesheet)
(global-set-key [(control f) (z)] 'rinari-find-rspec-fixture)
;; (require 'rinari-merb)
(add-hook 'rinari-minor-mode-hook
          '(lambda ()
             (define-key rinari-minor-mode-map [(control c) (i)] 'rinari-run-inf-ruby)
             ))
;; ------------------------------html ------------------------------
(add-to-list 'load-path (concat rails-path "rhtml-mode"))
(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(require 'haml-mode)
;; ------------------------------yaml模式设置------------------------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; (add-hook 'yaml-mode-hook
;;           '(lambda ()
;;              (define-key yaml-mode-map "\C-m" 'reindent-then-newline-and-indent)))
;; ------------------------------mode-compile 设置------------------------------
;; (setq ruby-command "ruby")
;; (setq ruby-dbg-flags "-w")
;; ------------------------------respec模式设置------------------------------
(add-to-list 'load-path (concat ruby-path "rspec"))
(require 'rspec-mode)
(setq rspec-use-rvm t)
(add-to-list 'load-path (concat ruby-path "rspec/feature-mode"))
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; ------------------------------其他模块设置------------------------------
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; (require 'ruby-end)
;; (require 'ruby-block)
;; (ruby-block-mode t)
;; (require 'ruby-test-mode)

;;; ==============================快捷键以及hook设置==============================
(global-set-key [(control c) (i)] 'run-inf-ruby)
(global-set-key [(meta c) (return)] 'ruby-run-file)
(global-set-key [(meta c) (?\r)] 'ruby-run-file)
(global-set-key [(control h) (r)] 'yari)
(global-set-key [(control c) (control d)] 'dbgr-rdebug)
(global-set-key [(control c) (control v)] 'ruby-run-definition)
;; (global-set-key [(control c) (control f)] 'ruby-run-file) ; 实际快捷键为C-c C-l

(defun show-existing-compilation-buffer ()
  (interactive)
  (if (get-buffer "*compilation*")
      (progn
        (display-buffer "*compilation*")
        (other-window -1))
    (progn
      (display-buffer "*ruby")
      (other-wndow -1))))

(defun run-inf-ruby ()
  (interactive)
  (if (get-buffer "*ruby*")
      (progn
        (kill-buffer "*ruby*")
        (inf-ruby)
        (other-window -1))
    (progn
      (inf-ruby)
      (other-window -1))))

(defun rinari-run-inf-ruby ()
  (interactive)
  (if (get-buffer "*ruby*")
      (progn
        (kill-buffer "*ruby*")
        (rinari-console)
        (other-window -1))
    (progn
      (rinari-console)
      (other-window -1))))

(defun ruby-run-definition ()
  (interactive)
  (if (get-buffer "*ruby*")
      (if (region-active-p)
          (progn
            (deactivate-mark)
            (ruby-send-region-and-go (region-beginning) (region-end)))
        (ruby-send-definition-and-go))
    (progn
      (inf-ruby)
      (other-window -1)
      (if (region-active-p)
          (progn
           (deactivate-mark)
           (ruby-send-region-and-go (region-beginning) (region-end)))
        (ruby-send-definition-and-go)))))

(defun ruby-run-last-sexp ()
  (interactive)
  (if (get-buffer "*ruby*")
      (progn
        (display-buffer "*ruby*")
        (ruby-send-region (save-excursion (backward-sexp) (point)) (point)))
    (progn
      (inf-ruby)
      (other-window -1)
      (ruby-send-region (save-excursion (backward-sexp) (point)) (point)))))

(defun ruby-run-file ()
  (interactive)
  (save-buffer)
  (if (get-buffer "*ruby*")
      (progn
        (display-buffer "*ruby*")
        (ruby-load-file buffer-file-name)
        (other-window -1))
    (progn
      (inf-ruby)
      (other-window -1)
      (ruby-load-file buffer-file-name)
      (other-window -1))))

;; (defun ruby-mark-block ()
;;   (interactive)
;;   (next-line nil)
;;   (ruby-beginning-of-block)
;;   (push-mark nil t t)
;;   (ruby-forward-sexp))

(defun ruby-mark-sexp ()
  (interactive)
  (push-mark nil t t)
  (ruby-forward-sexp))

(defun ruby-kill-sexp ()
  (interactive)
  (push-mark nil t t)
  (ruby-forward-sexp)
  (kill-region (region-beginning) (region-end)))

;; (defun ruby-mark-defun ()
;;   (interactive)
;;   (ruby-beginning-of-defun nil)
;;   (push-mark nil t t)
;;   (ruby-end-of-defun))

;; (defun ruby-kill-defun ()
;;   (interactive)
;;   (ruby-mark-defun)
;;   (kill-region (region-beginning) (region-end)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map [(control meta h)] 'ruby-mark-defun)
            (define-key ruby-mode-map [(control meta k)] 'ruby-kill-sexp)
            (define-key ruby-mode-map [(control meta ?\s)] 'ruby-mark-sexp)
            (define-key ruby-mode-map [(control x) (control e)] 'ruby-run-last-sexp)
            (define-key ruby-mode-map [(control f) (control r)] 'save-buffer-and-recompile)
            (local-set-key [(f1)] 'show-existing-compilation-buffer)
            (setq compile-command "rake -X ")
            ;; Yasnippet相关设置.
            (set-default 'yas/buffer-local-condition
                         '(cons 'require-snippet-condition 'always))
            (define-key ruby-mode-map [(meta ?\.)] 'find-tag)
            ;; (make-local-variable 'skeleton-pair-alist)
            ;; (add-to-list 'skeleton-pair-alist '(?\` _ ?\`))
            ))

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (unless (or (file-exists-p "Rakefile")
;;                          (file-exists-p "rakefile"))
;;                (set (make-local-variable 'compile-command)
;;                     (concat "rake "
;;                             (file-name-sans-extension buffer-file-name)))
;;                )
;;              ))

(add-hook 'inf-ruby-mode-hook
          '(lambda ()
             (define-key inf-ruby-mode-map [(control x) (control e)] 'ruby-run-last-sexp)
             (define-key inf-ruby-mode-map [(meta n)] 'window-move-up)
             (define-key inf-ruby-mode-map [(meta p)] 'window-move-down)
             (define-key inf-ruby-mode-map [(control n)] 'comint-next-input)
             (define-key inf-ruby-mode-map [(control p)] 'comint-previous-input)
             (define-key inf-ruby-mode-map [(meta \8)] 'delete-window)
             (define-key inf-ruby-mode-map [(meta r)] 'previous-error)
             (turn-on-autopair-mode)
             (turn-on-format-ruby-function)
             ))

(provide 'my-customize-ruby-setting)

;;; my-customize-ruby-setting.el ends here
