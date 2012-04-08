;; This makes ido-find-file-other-window,
;; ido-switch-buffer-other-window, et. al obsolete. It’s a much better
;; abstraction, and I believe it should become apart of ido mode,
;; because any command that uses ido-completing-read can benefit from
;; it without any additional effort, including textmate.el’s
;; textmate-goto-symbol.


(require 'ido)

(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  )

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  )

(defun ido-invoke-in-vertically-buffer ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontally-buffer ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))

     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))

    ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    (switch-to-buffer this-buffer)
    result))

(defadvice ido-init-completion-maps (after ido-init-completion-maps-with-other-window-keys activate)
  (mapcar (lambda (map)
            (define-key map (kbd "C-o") 'ido-invoke-in-vertically-buffer)
            (define-key map (kbd "S-o") 'ido-invoke-in-horizontally-buffer))

          (list ido-buffer-completion-map
                ;; ido-common-completion-map
                ido-file-completion-map
                ido-file-dir-completion-map)))

(provide 'ido-other-window)
;;; ido-other-window.el ends here
