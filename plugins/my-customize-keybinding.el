;; -*-Emacs-Lisp-*-

(define-prefix-command 'meta-c-map)
(global-set-key [(meta c)] 'meta-c-map)
(define-prefix-command 'ctrl-f-map)
(global-set-key [(control f)] 'ctrl-f-map)

;;;==============================快捷键相关==============================
;; 换绑一些必要的按键.
(defun new-frame-do-some-initially-setting (&optional frame)
  "When open new frame,do some initially setting."
  (define-key key-translation-map [(control j)] [(control J)])
  (define-key key-translation-map [(control l)] [(control L)])

  (define-key key-translation-map [(meta j)] [(meta J)])
  (define-key key-translation-map [(meta l)] [(meta L)])
  (define-key key-translation-map [(meta k)] [(meta K)])
  (define-key key-translation-map [(meta f)] [(meta F)])
  (define-key key-translation-map [(meta o)] [(super o)])
  (define-key key-translation-map [(control \\)] [(hyper \\)])
  (define-key key-translation-map [(meta s)] [(control l)])
  (define-key key-translation-map [(meta ?\t)] [(hyper i)])

  (define-key key-translation-map [(control meta j)] [(control meta b)])
  (define-key key-translation-map [(control meta l)] [(control meta f)])
  (define-key key-translation-map [(control c) (control j)] [(control c) (control b)])
  (define-key key-translation-map [(control c) (control l)] [(control c) (control f)])

  (define-key key-translation-map [(control meta f)] [(control meta F)])
  (define-key key-translation-map [(control c) (control o)] [(control c) (control O)])
  (define-key key-translation-map [(control c) (control f)] [(control c) (control F)])
  (define-key key-translation-map [(control c) (control k)] [(control c) (control K)])

  (define-key key-translation-map [(control x) (n)] [(control x) (b)])
  (define-key key-translation-map [(control c) (n)] [(control x) (\4) (control o)])
  (define-key key-translation-map [(control x) (control n)] [(control x) (control b)])
  (define-key key-translation-map [(control x) (\4) (n)] [(control x) (\4) (b)])
  (define-key key-translation-map [(control x) (\5) (n)] [(control x) (\5) (b)])

  (define-key key-translation-map [(meta \6)] [(meta \^)])
  ;; Alt+回车键，Window下如果采用[(meta return)]方式定义，总是这种方式优先于[(meta ?\r)]
  ;; 而在文本terminal下，使用[(meta ?\r)]定义方式总是优先
  ;; 为了确保兼容，将将M-RET变换为<meta return>
  ;; (define-key key-translation-map [(meta ?\r)] [(meta return)])
  (define-key key-translation-map [(meta ?\r)] [(control j)])
  (define-key key-translation-map [(meta return)] [(control j)])
  (define-key key-translation-map [(control \-)] [(control \_)])

  (define-key key-translation-map [(control v)] [(control x) (v)])
  (define-key key-translation-map [(control x) (control v)] [(control x) (control V)])
  (define-key key-translation-map [(control c) (control v)] [(control c) (control V)])
  (define-key key-translation-map [(control f) (control v)] [(control f) (control V)])

  ;; (define-key key-translation-map [(control f)] [(control c) (?\;) (f)])
  ;;  (define-key key-translation-map [(control f) (control c)] [(control c) (?\;) (c)])

  ;; (define-key key-translation-map [(control \`)] [(meta \@)])
  ;; (define-key key-translation-map [(control \2)] [(control \@)])
  ;; (define-key key-translation-map [(control \3)] [(?\e)])
  ;; (define-key key-translation-map [(control \4)] [(control \\)])
  ;; (define-key key-translation-map [(control \5)] [(control \])])
  ;; (define-key key-translation-map [(control \6)] [(control \^)])
  ;; (define-key key-translation-map [(control \7)] [(control \_)])
  ;; (define-key key-translation-map [(control \8)] "\d")    ;Ctrl-8 -> DEL
  ;; (define-key key-translation-map [(control \/)] [(?\d)]) ;Ctrl-/ -> DEL
  ;; (define-key key-translation-map [(control \')] [(control g)]) ;Ctrl-\ -> Ctrl->g
  (define-key key-translation-map [(f13)] [(shift f3)])
  (define-key key-translation-map [(f14)] [(shift f4)])
  (define-key key-translation-map [(f15)] [(shift f5)])
  (define-key key-translation-map [(f16)] [(shift f6)])
  (define-key key-translation-map [(f17)] [(shift f7)])
  (define-key key-translation-map [(f18)] [(shift f8)])
  (define-key key-translation-map [(f19)] [(shift f9)])
  (define-key key-translation-map [(f20)] [(shift f10)])
  (set-fontset-font "fontset-default" 'unicode'("WenQuanYi Micro Hei Light")) ;Linux下字体设置
  (when (equal window-system 'w32)
    (set-fontset-font "fontset-default" 'unicode'("文泉驿微米黑")))
  )
(new-frame-do-some-initially-setting) ;初始化设置
(add-hook 'after-make-frame-functions 'new-frame-do-some-initially-setting) ;新建frame初始化设置
;;------------------------------编辑相关------------------------------
(global-set-key [(meta p)] 'window-move-down) ;光标位置不变，窗口向下移动两行
(global-set-key [(meta n)] 'window-move-up) ;光标位置不变，窗口向上移动四行
(global-set-key [(meta P)] 'other-window-move-down) ;下一个窗口向下移动两行
(global-set-key [(meta N)] 'other-window-move-up) ;下一个窗口向上移动一行
(global-set-key [(control ?\_)] 'undo-only) ; 只undo, 不redo. 作为C-/的补充.(偶尔用用)

(global-set-key [(meta v)] 'scroll-up) ;向下翻页
(global-set-key [(meta q)] 'scroll-down) ;向上翻页.

(global-set-key [(control J)] 'backward-char) ;向左移动一个字符 Ctrl-j
(global-set-key [(control L)] 'forward-char)  ;向右移动一个字符 Ctrl-l
(global-set-key [(meta J)] 'backward-word) ;向左移动一个单词 Alt-j
(global-set-key [(meta L)] 'forward-word) ;向右移动一个单词 Alt-l

(global-set-key [(control return)] 'indent-new-comment-line) ; 注释行缩进Ctrl-回车

(global-set-key [(meta backspace)] 'backward-delete-word)
(global-set-key [(meta ?\d)] 'backward-delete-word)

(global-set-key [(control meta \;)] 'comment-box) ;注释盒子，一般用于对一段代码做概要
(global-set-key [(meta \1)] 'move-beginning-of-line) ;光标跳到行首 Alt-1
(global-set-key [(meta \2)] 'set-mark-command) ;设置选择块起始位置 Alt-2
(global-set-key [(meta \3)] 'move-end-of-line) ;光标跳到行尾 Alt-3
(global-set-key [(home)] 'beginning-of-buffer) ;文件首 Home
(global-set-key [(end)] 'end-of-buffer) ;文件尾 End
(global-set-key [(del)] 'delete-char) ;向后删除字符 Del
(global-set-key [(meta \5)] 'query-replace-regexp) ;正则查询替换Alt-5

(global-set-key [(meta u)] 'backward-upcase-word) ; 连续标记光标前面的单词大写 Alt-u
(global-set-key [(meta U)] 'backward-capitalize-word) ;连续标记光标前面单词首字母大写 Alt-U
(global-set-key [(control c) (u)] 'backward-downcase-word) ;连续标记光标前单词全部小写 Control-c u

(global-set-key [(control meta F)] 'mark-word) ;标记单词 Ctrl-Alt-f
(global-set-key [(control right)] 'transpose-chars) ;光标前所在字母右移
(global-set-key [(control left)] 'transpose-chars-backward) ;光标前所在字母左移
(global-set-key [(meta down)] 'transpose-lines) ;光标所在行的上一行向下移动
(global-set-key [(meta up)] 'transpose-lines-backward) ;光标所在行的上一行向上移动
(global-set-key [(meta right)] 'transpose-words) ;光标前所在单词向右移动
(global-set-key [(meta left)] 'transpose-words-backward) ;光标前所在单词向左移动
(global-set-key [(meta \4)] 'ispell-word) ;单词语法检查
;;(global-set-key [(control r)] 'query-replace) ;运行查询替换 Ctrl-r
;; (global-set-key [(control meta r)] 'query-replace-regexp) ;运行正则表达式查询替换 Alt+r
(global-set-key [(control s)] 'isearch-forward-regexp) ;更改Ctrl-s为正则搜索
(global-set-key [(control r)] 'isearch-backward-regexp) ;更改Ctrl-r为正则逆向搜索
(global-set-key [(control meta s)] 'isearch-forward) ;更改Ctrl-Alt-s为非正则搜索
(global-set-key [(control meta r)] 'isearch-backward) ;更改Ctrl-Alt-r为非正则逆向搜索
(global-set-key [(control x) (\\)] 'indent-region) ;Ctrl-\,选区自动缩进,等价于Ctrl-Alt-\

(global-set-key [(control w)] 'kill-whole-line-or-kill-region) ;kill整行或kill选区.Ctrl-w
(global-set-key [(meta w)] 'copy-whole-line-or-copy-region) ;复制整行或整个选区, Alt-w
(global-set-key [(control meta w)] 'apend-killed-line)
(global-set-key [(control k)] 'kill-line-or-kill-region) ;kill-line或kill选区. Ctrl-k
(require 'mark-lines)
(global-set-key [(control meta d)] 'down-list-or-delete-region)
(global-set-key [(meta K)] 'mark-lines-next-line) ;标记行 Alt-k
(global-set-key [(control c) (control K)] 'kill-current-line-and-indent) ;

;; 重新定义了upper,lower，capitalize单词或者选区的的快捷键
(global-set-key [(control a)] 'downcase-word) ; Ctrl-a
(global-set-key [(control e)] 'upcase-word)   ; Ctrl-e
;; (global-set-key [(control meta b)] 'reposition-window) ;定义/注释显示开关

;; (global-set-key [(control \9)] "(")
;; (global-set-key [(control \0)] ")")
;; (global-set-key [(control \8)] "*")
;; (global-set-key [(control \')] "\"")
;; (global-set-key [(control \-)] "_")
;; (global-set-key [(control \=)] "+")
;; (global-set-key [(control \;)] ":")
;; (global-set-key [(control \,)] "<")
;; (global-set-key [(CONTROL \.)] ">")

(global-unset-key [(control \9)])
(global-unset-key [(control \0)])
(global-unset-key [(control \8)])
(global-unset-key [(control \')])
(global-unset-key [(control \-)])
(global-unset-key [(control \=)])
(global-unset-key [(control \;)])
(global-unset-key [(control \,)])
(global-unset-key [(control \.)])

;;------------------------------缓冲区相关------------------------------
;; C-x C-f      # => ido-find-file
;; C-x f        # => recentf-ido-find-file
;; C-x n        # => ido-switch-buffer

;; C-x C-n      # => ibuffer
;; C-x 4 C-n    # => ibuffer-other-window

;; C-x 4 f      # => ido-find-file-other-window
;; C-x 4 n      # => ido-switch-buffer-other-window
;; C-x 5 f      # => ido-find-file-other-frame
;; C-x 5 n      # => ido-switch-buffer-other-window

;; C-c C-f      # => find-file-in-project

;; C-c f        # => ido-display-file
;; C-c n        # => ido-display-buffer == C-x 4 C-o

(global-set-key [(control x) (f)] 'recentf-ido-find-file)        ;
(global-set-key [(control x) (control b)] 'ibuffer)              ;在当前window, 运行ibuffer C-x C-n
(global-set-key [(control x) (\4) (control n)] 'ibuffer-other-window) ;在另一个窗口, 运行ibuffer. C-c C-n
(global-set-key [(control c) (f)] 'ido-display-file)    ;在另一个窗口, 显示.(不切换) C-c n

(global-set-key [(control c) (w)] 'write-region) ; 将选区另存为一个新的文件.
(global-set-key [(meta \8)] 'keyboard-escape-quit) ;当前窗口最大化
(global-set-key [(control \8)] 'delete-window)
(global-set-key [(super o)] 'other-window) ;切换到另一个窗口.
(global-set-key [(meta O)] '(lambda () (interactive) (other-window -1)))
(global-set-key [(meta prior)] 'previous-multiframe-window) ;前一个窗口
(global-set-key [(meta next)] 'next-multiframe-window) ;后一个窗口
(global-set-key [(f2)] 'kill-this-buffer) ;关闭当前缓冲区 F2
(global-set-key [(meta \-)] 'repeat) ;repeat Alt--
(global-set-key [(control h) (M)] 'manual-entry) ;在emacs当中查询manual手册
(global-set-key [(control h) (I)] 'info-apropos) ;在emacs当中查询info手册
(global-set-key [(control \^)] 'enlarge-window) ;增大当前窗口的范围.
;; (global-set-key [(control \=)] 'balance-windows) ;平衡两个窗口大小.
(global-set-key [(control x) (p)] 'speedbar)
(global-set-key [(control x) (control p)] 'speedbar)
(global-set-key [(control x) (control j)] 'help-go-back-other-window) ;另一个帮助窗口的前一个帮助
(global-set-key [(control x) (control l)] 'help-go-forward-other-window) ;另一个帮助窗口的后一个帮助
(global-set-key [(control h) (control h)] 'view-man-help) ;察看man帮助

;;------------------------------其他快捷键------------------------------
(global-set-key [(meta c) (meta c)] (lambda () (interactive) (dired ".")))
(global-set-key [(f1)] 'show-existing-help-buffer) ;显示帮助窗口 F1
;; (global-set-key [(control F)] '(lambda () (interactive) ))
(global-unset-key "\C-x\C-u") ;屏蔽C-x C-u快捷键
(global-unset-key "\C-x\C-l") ;屏蔽C-x C-l快捷键
(global-set-key [(shift f3)] 'kmacro-edit-macro-repeat) ;编辑前一个宏，检查错误
(global-set-key [(shift f4)] 'kmacro-name-last-macro) ;命名前一个宏
(global-set-key [(meta \/)] 'hippie-expand) ;替换默认自动补全为hippie-expand
(global-set-key [(meta \`)] 'shell-command) ;运行shell-command Alt+波浪键
(global-set-key [(control f9)] 'tool-bar-mode) ;工具条开关 Ctrl-F9
(global-set-key [(control f10)] 'menu-bar-mode) ;菜单条开关 Ctrl-F10
(global-set-key [(control f11)] 'speedbar) ;Speedbar模式.Ctrl-F11
(global-set-key [(control x) (e)] 'eshell) ;运行eshell Ctrl-x e
(global-set-key (kbd "C-x E") (lambda () (interactive) (eshell t))) ;打开一个新的eshell

(global-set-key [(control h) (control x) (v)] 'apropos-variable) ;查看可用的Emacs变量 Ctrl-h Ctrl-v
(global-set-key [(control h) (control c)] 'apropos-command) ;c查看可用的Emacs命令 Ctrl-h Ctrl-v
(global-set-key [(control h) (a)] 'apropos) ;运行apropos Ctrl-h a
;;------------------------------程序相关------------------------------
(require 'iy-go-to-char)
(global-set-key [(hyper i)] 'iy-go-to-char)
;; (global-set-key [(control c) (control c)] 'fill-paragraph)
;; (global-set-key [(control f) (control f)] (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key [(control f) (control f)] 'helm-mini)
(global-set-key [(meta r)] 'move-to-window-line-top-bottom) ; 前一个错误 或 Alt-r
(global-set-key [(meta F)] 'zap-to-char) ; 下一个错误 或 Alt-f.

(global-set-key [(control f12)] 'ediff-auto-save) ;
(global-set-key [(control c) (h)] 'highlight-phrase)
(global-set-key [(control c) (H)] 'unhighlight-regexp)
(global-set-key [(control c) (r)] 'revert-buffer) ;放弃保存前的所有更改 alt-c r
(global-set-key [(control c) (b)] 'bury-buffer) ;将当前buffer放到最后 alt-c b
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "M-.") 'find-function-at-point) ;;打开光标所在的函数定义
(global-set-key [(meta c) (R)] 'revert-buffer-with-coding-system) ;使用指定的编码重新加载buffer.
(global-set-key [(meta c) (S)] 'set-buffer-file-coding-system) ;另存当前buffer为其他编码
(global-set-key [(meta c) (u)] 'view-url) ;查看html源码
(global-set-key [(meta c) (a)] 'add-global-abbrev) ;添加光标前字符全局缩写,通过C-x '可以扩展该缩写
(global-set-key [(meta \')] 'expand-abbrev) ;扩展缩写.
(global-set-key [(meta c) (h)] 'hide-lines) ;隐藏指定的行
(global-set-key [(meta c) (H)] 'show-all-invisible) ;显示隐藏的行

(global-set-key [(shift f9)] 'point-to-register) ;设定位置寄存器
(global-set-key [(f9)] 'jump-to-register) ;跳转到目标位置寄存器
(global-set-key [(meta c) (?\s)] 'set-this-point-as-register-1) ;同上

(global-set-key [(meta c) (l)] "lambda ") ;快速输入 lambda 字符 Alt-c l
(global-set-key [(meta c) (\.)] "# => ")
(global-set-key [(control ?\.)] "=> ")
(global-set-key [(\,)] (lambda () (interactive) (insert ", ")))
(global-set-key [(\;)] (lambda () (interactive) (insert "; ")))
(global-set-key [(control c) (control O)] 'org-open-at-point) ;C-c C-o 自动打开链接.
;; (global-set-key [(control ?\.)] (lambda () (interactive) (insert ":")
;;                          (yas/expand)))

(global-set-key [(f10)] 'bookmark-bmenu-list) ; 显示书签列表
(global-set-key [(shift f10)] 'bookmark-set) ;将当前位置加入书签.
(global-set-key [(meta \=)] 'ediff-windows-linewise) ;使用ediff逐行对比buffer内容
;; VC有关的快捷键定义.通过M-u按键序列来执行.
(global-set-key [(control x) (v) (control x) (v)] 'vc-next-action) ;C-v C-v
(global-set-key [(control x) (v) (r)] 'vc-revert) ;C-v r
(global-set-key [(control x) (v) (C)] 'vc-create-tag) ;C-v C
(global-set-key [(control x) (v) (R)] 'vc-retrieve-tag) ;C-v R
(global-set-key [(control x) (v) (n)] 'vc-revision-other-window) ;C-v n
;;;==============================模式Hook定义==============================
;; 查看模式Hook定义的技巧：进入需要更改的模式，查看变量major-mode的值。
;; (add-hook 'after-init-hook 'session-initialize)

(add-hook 'text-mode-hook
          '(lambda ()
             ))

(add-hook 'message-mode-hook
          '(lambda ()
             (define-key message-mode-map [(control x) (control c)] 'message-send-and-exit)
             ))

;; (require 'tidy-xhtml)
;; (defun my-html-mode-hook () "Customize my html-mode."
;;   (tidy-build-menu html-mode-map)
;;   (local-set-key [(control c) (return)] 'sgml-validate)
;;   (local-set-key [(control c) (?\r)] 'sgml-validate))
;; (add-hook 'html-mode-hook 'my-html-mode-hook)
(require 'rainbow-mode)
(add-hook 'html-mode-hook
          '(lambda ()
             (define-key html-mode-map [(control meta f)] 'sgml-skip-tag-forward)
             (define-key html-mode-map [(control meta b)] 'sgml-skip-tag-backward)
             (define-key html-mode-map [(control c) (q)] 'sgml-name-char)
             (define-key html-mode-map [(control c) (a)] 'sgml-attributes)
             (define-key html-mode-map [(control c) (t)] 'sgml-tag)
             (define-key html-mode-map [(control meta ?\s)] 'html-mark-sexp)
             (define-key html-mode-map [(control meta k)] 'html-kill-sexp)
             (setq sgml-xml-mode t)
             (setq sgml-validate-command "tidy")
             ;; Yasnippet相关设置.
             (set-default 'yas/buffer-local-condition
                          '(cons 'require-snippet-condition 'always))
             (rainbow-mode 1)
             ))

(add-hook 'rhtml-mode-hook
          '(lambda ()
             (modify-syntax-entry ?' "\"" rhtml-mode-syntax-table)
             (set-syntax-table rhtml-mode-syntax-table)
             ))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map [(meta n)] 'window-move-up)
             (define-key markdown-mode-map [(meta p)] 'window-move-down)
             (define-key markdown-mode-map [(meta i)] 'markdown-insert-pre)
             (define-key markdown-mode-map [(tab)] 'yas/expand)
             ))

;; (add-hook 'nxhtml-mode-hook
;;           '(lambda ()
;;              (define-key nxhtml-map [(control meta f)] 'sgml-skip-tag-forward)
;;              (define-key nxhtml-map [(control meta b)] 'sgml-skip-tag-backward)
;;              (define-key nxhtml-map [(control c) (q)] 'sgml-name-char)
;;              (define-key nxhtml-map [(control c) (return)] 'html-line)
;;              (define-key nxhtml-map [(control c) (?\r)] 'html-line)
;;              (define-key nxhtml-map [(control c) (p)] 'html-paragraph)
;;              (define-key nxhtml-map [(control c) (control p)] 'html-paragraph)
;;              (define-key nxhtml-map [(control c) (a)] 'sgml-attributes)
;;              (define-key nxhtml-map [(control c) (t)] 'sgml-tag)
;;              (define-key nxhtml-map [(control c) (i)] 'sgml-tag)
;;              ))

(add-hook 'Info-mode-hook
          '(lambda ()
             (define-key Info-mode-map [(meta n)] 'window-move-up)
             (define-key Info-mode-map [(meta c)] 'clone-buffer)
             ))

(add-hook 'help-mode-hook
          '(lambda ()
             (define-key help-mode-map [(meta \8)] 'delete-window)
             (define-key help-mode-map [(?\d)] 'help-go-back)
             (define-key help-mode-map [(backspace)] 'help-go-back)
             ))

(add-hook 'w3m-mode-hook
          '(lambda ()
             (define-key w3m-mode-map [(meta n)] 'window-move-up)
             (define-key w3m-mode-map [(meta L)] 'forward-word)
             (define-key w3m-mode-map [(meta h)] 'w3m-horizontal-recenter)
             (define-key w3m-mode-map [(control meta f)] 'w3m-next-buffer)
             (define-key w3m-mode-map [(control meta b)] 'w3m-previous-buffer)
             (define-key w3m-mode-map [(control k)] 'kill-this-buffer)
             (define-key w3m-mode-map [(control c) (control F)] 'w3m-switch-buffer)
             (define-key w3m-mode-map [(control x) (p)] 'w3m-select-buffer)
             (define-key w3m-mode-map [(f5)] 'w3m-reload-this-page)
             (define-key w3m-mode-map [(shift f5)] 'w3m-reload-all-page)
             (define-key w3m-mode-map [(meta t)] 'w3m-toggle-inline-image)
             (define-key w3m-mode-map [(T)] 'w3m-copy-buffer)
             (define-key w3m-mode-map [(N)] 'w3m-goto-url-new-session)
             (define-key w3m-mode-map [(G)] 'w3m-goto-url-new-session)
             (define-key w3m-mode-map [(?\d)] 'w3m-view-previous-page)
             (define-key w3m-mode-map [(backspace)] 'w3m-view-previous-page)
             (define-key w3m-mode-map [(f2)] 'w3m-delete-buffer)
             (define-key w3m-mode-map [(control o)] 'browse-url-at-point)
             ))

(add-hook 'w3m-select-buffer-mode-hook
          '(lambda ()
             (define-key w3m-select-buffer-mode-map [(meta \8)] 'kill-buffer-and-window)
             ))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map [(meta n)] 'window-move-up)
             (define-key eshell-mode-map [(meta p)] 'window-move-down)
             (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
             (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
             (defalias 'f 'find-file)
             ))

(add-hook 'c-mode-hook
          '(lambda ()
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (turn-on-eldoc-mode)
             (define-key lisp-mode-shared-map [(meta c) (e)] 'eval-buffer)
             (define-key read-expression-map [(tab)] 'lisp-complete-symbol)
             (define-key read-expression-map [(?\t)] 'lisp-complete-symbol)
             (setq fill-column 150)
             ))

(add-hook 'auto-complete-mode-hook
          '(lambda ()
             ;; (setq ac-use-menu-map t)
             ;; (define-key ac-menu-map [(tab)] 'ac-complete)
             (define-key ac-completing-map [(tab)] 'ac-complete)
             (local-unset-key [(control n)])
             (local-unset-key [(control p)])))

(add-hook 'isearch-mode-hook ;可以通过apropos命令,搜索`isearch-.*'来查找所有跟isearch有关的命令.
          '(lambda ()
             (define-key isearch-mode-map [(control meta s)] 'isearch-forward)
             (define-key isearch-mode-map [(control meta r)] 'isearch-backward)
             ;; (define-key isearch-mode-map [(control s)] 'isearch-forward-regexp)
             ;; (define-key isearch-mode-map [(control r)] 'isearch-backward-regexp)
             (define-key isearch-mode-map [(meta n)] 'isearch-repeat-forward)
             (define-key isearch-mode-map [(meta N)] 'isearch-repeat-forward)
             (define-key isearch-mode-map [(meta p)] 'isearch-repeat-backward)
             (define-key isearch-mode-map [(meta P)] 'isearch-repeat-backward)
             (define-key isearch-mode-map [(meta u)] 'backward-upcase-word)

             ;; (define-key isearch-mode-map [(meta r)] 'isearch-query-replace-regexp)
             (define-key isearch-mode-map [(meta \5)] 'isearch-query-replace-regexp)

             (define-key isearch-mode-map [(control L)] 'isearch-yank-char)
             (define-key isearch-mode-map [(control J)] 'isearch-delete-char)
             (define-key isearch-mode-map [(meta L)] 'isearch-yank-word)
             (define-key isearch-mode-map [(meta J)] 'isearch-delete-char)

             (define-key isearch-mode-map [(control k)] 'isearch-yank-line)
             (define-key isearch-mode-map [(control y)] 'isearch-yank-kill)

             (define-key isearch-mode-map [(control o)] 'isearch-occur) ;isearch-mode竟然不支持C-c前缀.晕
             (define-key isearch-mode-map [(meta w)] 'isearch-forward-word)

             (define-key isearch-mode-map [(meta ?\d)] 'isearch-edit-string)
             (define-key isearch-mode-map [(meta backspace)] 'isearch-edit-string)

             (define-key isearch-mode-map [(meta \')] 'expand-abbrev)
             ))

;; 注意:只能使用Ctrl前缀的快捷键,狂晕....
(define-key query-replace-map [(e)] 'edit)
(define-key query-replace-map [(E)] 'edit-replacement)
(define-key query-replace-map [(?\d)] 'backup)
(define-key query-replace-map [(backspace)] 'backup)
(define-key query-replace-map [(p)] 'backup)
;;   "Keymap that defines the responses to questions in `query-replace'.
;; The \"bindings\" in this map are not commands; they are answers.
;; The valid answers include `act', `skip', `act-and-show',
;; `exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
;; `automatic', `backup', `exit-prefix', and `help'.")

(add-hook 'bookmark-bmenu-mode-hook
          '(lambda ()
             (define-key bookmark-bmenu-mode-map [(f10)] 'bookmark-bmenu-search)
             ))


(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (define-key minibuffer-local-map [(meta \')] 'expand-abbrev)
             ))


(add-hook 'diff-mode-hook
          '(lambda ()
             (define-key diff-mode-map [(meta N)] 'other-window-move-up)
             (define-key diff-mode-map [(meta P)] 'other-window-move-down)
             ))

(add-hook 'ediff-keymap-setup-hook
          '(lambda ()
             (define-key ediff-mode-map [(control L)] 'ediff-recenter)
             ))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             ;; (define-key ibuffer-mode-map [(u)] 'ibuffer-unmark-backward)
             (define-key ibuffer-mode-map [(S)] 'ibuffer-do-isearch-regexp)
             (define-key ibuffer-mode-map [(meta ?\t)] 'ibuffer-do-isearch)
             (define-key ibuffer-mode-map [(Q)] 'ibuffer-do-query-replace-regexp)
             ;; (define-key ibuffer-mode-map [(O)] 'ibuffer-do-occur) ;;在选择的文件内occur关键字.
             (define-key ibuffer-mode-map [(U)] 'ibuffer-unmark-all)
             (define-key ibuffer-mode-map [(z)] 'ibuffer-filter-by-name)
             (define-key ibuffer-mode-map "\C-x\C-f" 'ibuffer-ido-find-file)
             (define-key ibuffer-mode-map "\C-x\C-s" 'ibuffer-do-save)
             (define-key ibuffer-mode-map "\M-n" 'window-move-up)
             (define-key ibuffer-mode-map "\M-p" 'window-move-down)
             ))

(add-hook 'message-mode-hook
          '(lambda ()
             (define-key message-mode-map [(meta n)] 'window-move-up)
             ))

(add-hook 'artist-mode-init-hook
          '(lambda ()
             ;; (define-key artist-mode-map [(kp-up)] 'picture-movement-up)
             ;; (define-key artist-mode-map [(kp-down)] 'picture-movement-down)
             ;; (define-key artist-mode-map [(kp-left)] 'picture-movement-left)
             ;; (define-key artist-mode-map [(kp-right)] 'picture-movement-right)
             ;; (define-key artist-mode-map [(kp-home)] 'picture-movement-nw)
             ;; (define-key artist-mode-map [(kp-end)] 'picture-movement-sw)
             ;; (define-key artist-mode-map [(kp-prior)] 'picture-movement-ne)
             ;; (define-key artist-mode-map [(kp-next)] 'picture-movement-se)
             (define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)
             (define-key artist-mode-map [(control L)] 'artist-forward-char)
             (define-key artist-mode-map [(control J)] 'artist-backward-char)
             (define-key artist-mode-map [(control e)] 'artist-select-op-straight-line)
             (define-key artist-mode-map [(control r)] 'artist-select-op-rectangle)
             (define-key artist-mode-map [(control t)] 'artist-select-op-text-see-thru)
             (define-key artist-mode-map [(meta T)] 'artist-select-op-text-overwrite)
             (define-key artist-mode-map [(control d)] 'artist-select-op-erase-rectangle)
             ;; (define-key artist-mode-map [(meta D)] 'artist-select-op-erase-rectangle)
             (define-key artist-mode-map [(control v)] 'artist-select-op-vaporize-line)
             (define-key artist-mode-map [(meta V)] 'artist-select-op-vaporize-lines)
             (define-key artist-mode-map [(control w)] 'artist-select-op-cut-rectangle)
             (define-key artist-mode-map [(meta w)] 'artist-select-op-copy-rectangle)
             (define-key artist-mode-map [(control y)] 'artist-select-op-paste)
             (define-key artist-mode-map [(control f)] 'artist-select-op-flood-fill)
             ;; (define-key artist-mode-map [(meta J)] '(lambda () (interactive)
             ;;                                           (artist-key-set-point)
             ;;                                           (artist-key-set-point)))
             (orgtbl-mode -1)
             (flyspell-mode-off)
             (yas/minor-mode -1)
             (turn-off-auto-fill)
             ))

;;dired使用isearch模式搜索文件名.(类似于Thunar)
(require 'dired-isearch)
;;dired开启omit模式.
(require 'dired-x)
(set-default 'dired-omit-mode 1)
;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; dired tar压缩解压缩支持
(require 'dired-tar)
;; (require 'dired-lis)
;; (require 'dired+)

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map [(S)] 'dired-do-isearch-regexp)
             (define-key dired-mode-map "\e\t" 'dired-do-isearch) ; 也可以使用[(meta ?\t)]来设定.
             (define-key dired-mode-map [(Q)] 'dired-do-query-replace-regexp)
             (define-key dired-mode-map [(control a)] 'dired-mark-subdir-files)
             (define-key dired-mode-map [(meta backspace)] 'dired-up-directory)
             (define-key dired-mode-map [(meta ?\d)] 'dired-up-directory)
             (define-key dired-mode-map [(control j)] 'dired-find-file)
             ;; (define-key dired-mode-map [(u)] 'dired-unmark-backward)
             (define-key dired-mode-map [(meta p)] 'window-move-down)
             (define-key dired-mode-map  "/" 'dired-filter-by-name)
             (define-key dired-mode-map [(f)] 'dired-isearch-forward)
             (define-key dired-mode-map [(r)] 'dired-isearch-backward)
             (define-key dired-mode-map [(\~)] (lambda () (interactive)
                                                 (dired "~/")))
             (define-key dired-mode-map [(h)] (lambda () (interactive)
                                                (dired (concat ffip-project-root "/"))))
             ;; (define-key dired-mode-map [(p)] (lambda () (interactive)
             ;;                                    (dired (concat ffip-project-root "/public"))))
             (setq ido-enable-replace-completing-read nil)
             ))

(add-hook 'speedbar-mode-hook
          '(lambda ()
             (define-key speedbar-key-map [(meta \8)] 'dframe-close-frame)
             (define-key speedbar-key-map [(s)] 'isearch-forward-regexp)
             (define-key speedbar-key-map [(r)] 'isearch-backward-regexp)
             (setq case-fold-search t)
             ))

(add-hook 'calendar-mode-hook
          '(lambda ()
             (define-key calendar-mode-map [(control J)] 'calendar-backward-day)
             (define-key calendar-mode-map [(control L)] 'calendar-forward-day)
             (define-key calendar-mode-map [(meta v)] 'calendar-scroll-left-three-months)
             (define-key calendar-mode-map [(meta q)] 'calendar-scroll-right-three-months)
             (define-key calendar-mode-map [(meta \2)] 'calendar-set-mark)
             (define-key calendar-mode-map [(control c) (control L)] 'calendar-redraw)
             ))

(add-hook 'diary-mode-hook
          '(lambda ()
             (define-key diary-mode-map [(control x) (control c)] 'quit-window)
             ))

;; (add-hook 'kill-emacs-hook
;; '(lambda ()
;; ()
;; ))

(add-hook 'log-edit-mode-hook
          '(lambda ()
             (define-key log-edit-mode-map [(control x) (v) (control x) (v)] 'log-edit-done)
             ))

(add-hook 'debugger-mode-hook
          '(lambda ()
             (define-key debugger-mode-map [(meta \8)] 'top-level)
             ))

(add-hook 'Man-mode-hook
          '(lambda ()
             (define-key Man-mode-map [(meta n)] 'window-move-up)
             (define-key Man-mode-map [(meta p)] 'window-move-down)
             (define-key Man-mode-map [(meta \8)] 'kill-buffer-and-window)
             ))

(add-hook 'yari-mode-hook
          '(lambda ()
             (define-key yari-mode-map [(meta \8)] 'delete-window)
             ))

(add-hook 'text-translator-window-mode-hook
          '(lambda ()
             (define-key text-translator-window-mode-map [(meta \8)] 'kill-this-buffer)
             ))

(add-hook 'browse-kill-ring-mode-hook
          '(lambda ()
             (define-key browse-kill-ring-mode-map [(meta \8)] 'browse-kill-ring-quit)
             ))

(add-hook 'vc-git-log-view-mode-hook
          '(lambda ()
             (define-key vc-git-log-view-mode-map [(meta \8)] 'kill-buffer-and-window)
             ))

(add-hook 'comint-mode-hook
          '(lambda ()
             (define-key comint-mode-map [(meta \8)] 'delete-window)
             (define-key comint-mode-map [(meta n)] 'window-move-up)
             (define-key comint-mode-map [(meta p)] 'window-move-down)
             (define-key comint-mode-map [(control n)] 'comint-next-input)
             (define-key comint-mode-map [(control p)] 'comint-previous-input)
             (define-key comint-mode-map [(control c) (control d)] 'dbgr-rdebug)
             (define-key comint-mode-map [(meta F)] 'compilation-next-error)
             (define-key comint-mode-map [(meta r)] 'compilation-previous-error)
             (define-key comint-mode-map [(p)] 'compilation-previous-error)
             (define-key comint-mode-map [(n)] 'compilation-next-error)
             ))

(add-hook 'compilation-mode-hook
          '(lambda ()
             (define-key compilation-mode-map [(meta \8)] 'delete-window)
             (define-key compilation-mode-map [(meta n)] 'window-move-up)
             (define-key compilation-mode-map [(meta p)] 'window-move-down)
             (define-key compilation-mode-map [(meta F)] 'compilation-next-error)
             (define-key compilation-mode-map [(meta r)] 'compilation-previous-error)
             (define-key compilation-mode-map [(p)] 'compilation-previous-error)
             (define-key compilation-mode-map [(n)] 'compilation-next-error)
             (define-key compilation-mode-map [(control c) (control c)] 'mode-compile-kill)
             ))

(add-hook 'dictionary-mode-hook
          '(lambda ()
             (define-key dictionary-mode-map [(meta \8)] 'dictionary-close)
             ))

(provide 'my-customize-keybinding)

;;; my-customize-keybinding.el ends here
