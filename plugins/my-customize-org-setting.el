;; -*-Emacs-Lisp-*-

;;;==============================ORG模式==============================
(add-to-list 'load-path (concat plugins "org-mode"))
(require 'org-install)
(require 'org-table)
(setq org-directory "~/.emacs.d/org/")
(setq GTD-directory "~/.emacs.d/org/gtd/")
(setq org-agenda-files (list GTD-directory))
(setq org-ctrl-c-ctrl-c-final-hook '(fill-paragraph))

;; (setq org-hide-leading-stars t) ;隐藏刚开始的*符号
;; (setq org-default-notes-file (concat org-directory "home.org"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-show-inherited-tags nil);不显示继承的tag属性
(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;通过SHIFT,修改TODO状态时，跳过输入记录
(setq org-log-done 'time) ;TODO状态改变为DONE时，记录时间戳

;; 自动设定列表分类小标题
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))
;; 使用外部程序打开特定格式文档
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))
;; ------------------------------函数------------------------------
;;当所有子TODO项目切换为DONE，父项目自动切换
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun bh/org-todo ()
  (interactive)
  (org-narrow-to-subtree)
  ;; (org-show-todo-tree nil)
  )

(defun bh/widen ()
  (interactive)
  (widen)
  (org-reveal))

(defun bh/save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

;; archive操作相关函数
(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (daynr (string-to-int (format-time-string "%d" (current-time))))
         (a-month-ago (* 60 60 24 (+ daynr 1)))
         (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
         (this-month (format-time-string "%Y-%m-" (current-time)))
         (subtree-is-current (save-excursion
                               (forward-line 1)
                               (and (< (point) subtree-end)
                                    (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
    (if subtree-is-current
        subtree-end ; Has a date in this month or last month, skip it
      ;; Make sure the ARCHIVE property is set for this task
      ;; Create one at the parent task if necessary
      (save-excursion
        (save-restriction
          (widen)
          (let ((archive-prop (org-entry-get nil "ARCHIVE" 'inherit))
                (parent-task))
            (org-up-heading-safe)
            (setq parent-task (nth 4 (org-heading-components)))
            (unless archive-prop
              (setq archive-prop (org-entry-put nil "ARCHIVE" (concat "%s_archive::* " parent-task)))))))
      nil)))

;; Project有关的函数
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    has-subtask))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      subtree-end)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        subtree-end
      nil)))

(defun org-kill-line-or-kill-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (progn
      (org-kill-line arg))))

;;------------------------------快捷键设置------------------------------
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key [(f11)] 'org-capture)
(global-set-key [(f12)] 'org-agenda)

;; (global-set-key "\C-cs" 'org-store-link)
;; (global-set-key [(control c) (i)] 'org-insert-link)

(add-hook 'org-mode-hook
          '(lambda ()
             ;; (define-key org-mode-map [(return)] 'org-return-indent)
             (define-key org-mode-map [(control x) (return)] 'org-meta-return)
             (define-key org-mode-map [(control x) (?\r)] 'org-meta-return)
             (define-key org-mode-map [(control c) (t)] 'org-todo)
             (define-key org-mode-map [(meta ?\;)] 'org-set-tags-command)
             (define-key org-mode-map [(control c) (g)] 'org-goto)
             (define-key org-mode-map [(meta \1)] 'org-beginning-of-line)
             (define-key org-mode-map [(meta \3)] 'org-end-of-line)
             (define-key org-mode-map [(tab)] 'yas/expand)
             (define-key org-mode-map [(control c) (control F)] 'org-ido-switchb)
             (define-key org-mode-map [(meta c) (n)] 'bh/org-todo)
             (define-key org-mode-map [(meta c) (N)] 'bh/widen)
             (define-key org-mode-map [(control a)] 'downcase-word)
             (define-key org-mode-map [(control e)] 'upcase-word)
             (define-key org-mode-map [(control k)] 'org-kill-line-or-kill-region)
             (define-key org-mode-map [(control c) (control F)] 'org-iswitchb)
             (define-key org-mode-map [(control c) (p)] 'bh/save-then-publish)
             (define-key org-mode-map [(control x) (control e)] 'org-babel-execute-src-block)
             (define-key org-mode-map [(hyper i)] 'pcomplete)
             ))

(add-hook 'orgtbl-mode-hook
          '(lambda ()
             (define-key orgtbl-mode-map [(control c) (return)] 'org-ctrl-c-ret)
             (define-key orgtbl-mode-map [(control c) (?\r)] 'org-ctrl-c-ret)
             (define-key orgtbl-mode-map [(control j)] 'org-return-indent)
             ))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (define-key org-agenda-mode-map [(meta ?\;)] 'org-agenda-set-tags)
             ;; (define-key org-agenda-mode-map [(control c) (control c)] 'org-ctrl-c-ctrl-c)
             ;; (define-key org-agenda-mode-map [(u)] 'org-agenda-backward-bulk-unmark)
             ))

(add-hook 'org-capture-mode-hook
          '(lambda ()
             (define-key org-capture-mode-map [(control c) (control c)] 'org-capture-finalize)
             (define-key org-capture-mode-map [(control c) (control K)] 'org-capture-kill)
             ))

(add-hook 'org-src-mode-hook
          '(lambda ()
             ;; (define-key org-src-mode-map [(meta \8)] 'org-edit-src-exit)
             ))

;;------------------------------refile设置------------------------------
(setq org-archive-mark-done nil)

;;  Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets
      `(
        (,(concat GTD-directory "organized.org") :maxlevel . 2)
        (nil :maxlevel . 2)
        ))
;;  Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;;  Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;;------------------------------TODO状态设置------------------------------
;; (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))
(setq org-todo-keywords (quote
                         ((sequence "TODO(t)" "WAITING(w@/!)" "SOMEDAY(s!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces            ;设置TODO状态的颜色
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("NEXT"      :foreground "blue"         :weight bold)
              ("STARTED"   :foreground "blue"         :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "orange"       :weight bold)
              ("SOMEDAY"   :foreground "magenta"      :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED"
               ("CANCELLED" . t))
              ("WAITING"
               ("WAITING" . t))
              ("SOMEDAY"
               ("WAITING" . t))
              (done
               ("WAITING"))
              ("TODO"
               ("WAITING")
               ("CANCELLED"))
              ("NEXT"
               ("WAITING"))
              ("STARTED"
               ("WAITING"))
              ("DONE"
               ("WAITING")
               ("CANCELLED")))))
;;------------------------------自定义agenda视图------------------------------
(setq org-agenda-custom-commands
      (quote (
                                        ;             ("c" .  "Custom agenda views")
              ("t" "TODO LIST!" tags "LEVEL=1+@TASKS|@HABIT/-DONE"
               ((orgo-agenda-overriding-header "TODO TASKS")))
              ("p" "Problem -> solve now!" tags-todo "-@TASKS-@HABIT/-CANCELLED-WAITING-SOMEDAY"
               ((org-agenda-overriding-header "Waiting to be solved problem")
                (org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-timestamp nil)
                (org-agenda-todo-list-sublevels t)
                (org-tags-match-list-sublevels 'indented)))
              ("S" "Problem -> Postponed" todo "SOMEDAY|WAITING"
               ((org-agenda-overriding-header "Postponed problem")))
              ("n" "Note" tags "LEVEL>1+@NOTE/-TODO-SOMEDAY"
               ((orgo-agenda-overriding-header "Organized NOTE")))
              ("r" "Refile files" tags "LEVEL=2+@REFILE"
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-timestamp nil)
                (org-agenda-overriding-header "Refile files")))
              ("l" "Live_log" tags "LEVEL=4+@LIVE_LOG"
               ((orgo-agenda-overriding-header "live_log")))
              ("d" "Diary" tags "LEVEL=4+@DIARY"
               ((orgo-agenda-overriding-header "Diary")))
              ("P" "Projects" tags-todo "LEVEL=1/!-DONE-CANCELLED-WAITING-SOMEDAY"
               ((org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-overriding-header "Projects")))
              ("A" "Tasks to be Archived" tags "LEVEL=2-@REFILE/DONE|CANCELLED"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks)))
              )))
;;------------------------------capture自定义模板------------------------------
(setq org-capture-templates
      `(
        ("t" "Task->to do" entry (file
                                  ,(concat GTD-directory "task.org"))
         "* TODO %?
")

        ("p" "Problem->to solve" entry (file+headline
                                        ,(concat GTD-directory "capture.org") "Problem")
         "* TODO %?
")

        ("n" "Notes->to record" entry (file+headline
                                       ,(concat GTD-directory "capture.org") "Notes")
         "* %?
")

        ("c" "Clipboard->save" entry (file+headline
                                      ,(concat GTD-directory "capture.org") "Clipboard")
         "* %^{Desription}
%c" :immediate-finish t)

        ("l" "live log" entry (file+datetree
                               ,(concat GTD-directory "live-log.org"))
         "** %U - %^{What are you doing?}
")

        ("d" "Diary" entry (file+datetree
                            ,(concat GTD-directory "diary.org"))
         "* %? %U
")

        ("h" "habit" entry (file
                            ,(concat GTD-directory  "habit.org"))
         "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:STYLE: habit
:END:
")
        ))
;;;------------------------------orgmode ido有关设置------------------------------
                                        ; Use IDO for target completion
(setq org-completion-use-ido t)

                                        ; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
;;------------------------------ditaa及plantuml设置------------------------------

(setq org-ditaa-jar-path (concat plugins "java/ditaa0_9.jar"))
(setq org-plantuml-jar-path (concat plugins "java/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(org-babel-do-load-languages
 'org-babel-load-languages (quote ((emacs-lisp . t)
                                   (dot . t)
                                   (ditaa . t)
                                   (R . t)
                                   (python . t)
                                   (ruby . t)
                                   (gnuplot . t)
                                   (clojure . t)
                                   (sh . t)
                                   (ledger . t)
                                   (org . t)
                                   (plantuml . t)
                                   (latex . t))))

(setq org-confirm-babel-evaluate nil)  ;不提示确认，直接运行babel代码
;;------------------------------html发布相关设置------------------------------
;; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
                                        ;
                                        ; Inline images in HTML instead of producting links to the image
(setq org-export-html-inline-images t)
                                        ; Do not use sub or superscripts - I currently don't need this functionality in my documents
(setq org-export-with-sub-superscripts nil)
                                        ; Use org.css from the norang website for export document stylesheets
(setq org-export-html-style-extra "<link rel=\"stylesheet\" href=\"./org.css\" type=\"text/css\" />")
(setq org-export-html-style-include-default nil)
                                        ; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
                                        ; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)

                                        ; List of projects
                                        ; norang       - http://www.norang.ca/
                                        ; doc          - http://doc.norang.ca/
                                        ; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
                                        ; org          - miscellaneous todo lists for publishing
(setq org-publish-project-alist
      (quote (
              ("org-mode-doc-org"
               :base-directory "~/.emacs.d/org/publish"
               :publishing-directory "~/.emacs.d/org/publish"
               ;; :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :exclude "Org-Study-Note.org"
               :exclude "Linux-Study-Note.org"
               :exclude "Emacs-Study-Note.org"
               :exclude "Ruby-Study-Note.org"
               :exclude "C-Study-Note.org"
               :publishing-function (org-publish-org-to-html)
               ;; :plain-source t
               ;; :htmlized-source t
               :auto-index t
               :index-filename "index.org"
               :index-title "index"
               :link-home "index.html"
               :style-include-default nil
               :style "<link rel=\"stylesheet\" href=\"./org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("org-mode-doc-extra"
               :base-directory "~/.emacs.d/org/"
               :publishing-directory "~/.emacs.d/org/web"
               :base-extension "css//|js//|png//|jpg//|gif//|pdf//|mp3//|swf//|zip//|gz//|txt//|el"
               :publishing-function org-publish-attachment
               ;; :recursive t
               :author t)
              ("org-mode-doc"
               :components ("org-mode-doc-org" "org-mode-doc-extra"))
              )))
;;------------------------------APPT设置------------------------------
;; (setq org-agenda-include-diary nil)
;; (setq org-agenda-diary-file "~/org/gtd/diary.org")
;; (setq org-agenda-insert-diary-extract-time t)
;; ; Erase all reminders and rebuilt reminders for today from the agenda
;; (defun bh/org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))
;; ; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt)
;; ; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (bh/org-agenda-to-appt)
;; ; Activate appointments so we get notifications
;; (appt-activate t)
;; ; If we leave Emacs running overnight - reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'bh/org-agenda-to-appt)
;; ;;------------------------------习惯habit跟踪------------------------------
;; ; global STYLE property values for completion
;; (setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
;; ; position the habit graph on the agenda to the right of the default
;; (setq org-habit-graph-column 50)
;; (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(provide 'my-customize-org-setting)

;;; my-customize-org-setting.el ends here
