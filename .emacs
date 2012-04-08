;; -*-Emacs-Lisp-*-

;; 路径设置
(setq plugins (expand-file-name "~/.emacs.d/plugins/"))
(add-to-list 'load-path plugins) ;插件默认路径，本行必须是配置文件首行

(add-to-list 'exec-path (concat plugins "/bin"))
(setq default-directory "~/")
(setq bookmark-default-file "~/.emacs.d/.bookmark")
(setq desktop-base-file-name "~/.emacs.d/.desktop")
(setq desktop-base-lock-name "~/.emacs.d/.desktop.lock")
(setq save-place-file "~/.emacs.d/.emacs-places")
(setq smex-save-file "~/.emacs.d/.smex-items")
(setq mail-personal-alias-file "~/.emacs.d/.mailrc")
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq diary-file "~/.emacs.d/diary")

(if (file-exists-p (concat plugins "my-customize-keybinding.el")) (require 'my-customize-keybinding))
(if (file-exists-p (concat plugins "my-customize-function.el")) (require 'my-customize-function))
(if (file-exists-p (concat plugins "my-customize-ido-setting.el")) (require 'my-customize-ido-setting))
(if (file-exists-p (concat plugins "my-customize-org-setting.el")) (require 'my-customize-org-setting))
(if (file-exists-p (concat plugins "my-customize-ruby-setting.el")) (require 'my-customize-ruby-setting))
(if (file-exists-p (concat plugins "my-customize-gnus-setting.el"))
    (eval-after-load 'gnus
      '(load "~/.emacs.d/plugins/my-customize-gnus-setting.el")))
;; ------------------------------备份选项------------------------------
;; (setq make-backup-files nil) ;不创建备份文件.
(setq version-control t) ;开启版本控制
(setq kept-old-versions 2) ;备份最原始版本两次。
(setq kept-new-versions 200) ;备份最近修改的版本200次
(setq delete-old-versions t) ;删除不满足以上条件的版本
(setq backup-directory-alist '(("." . "~/.backup"))) ;设置备份文件保存的目录

(setq vc-suppress-confirm t)
(setq vc-command-messages t)

;; (setq auto-save-default nil)

(setq-default frame-title-format '(""buffer-file-name"  -  "invocation-name"@"system-name"")) ;在标题栏显示buffer名称
;; ------------------------------设置字体，UNICODE字符集------------------------------
(add-to-list 'default-frame-alist '(font . "Inconsolata-15")) ;设置新frame的字体大小
(set-frame-font "Inconsolata-15") ;设置英文字体

(set-language-environment "UTF-8") ;使用UTF-8字符集
(prefer-coding-system 'utf-8) ;设置新建buffer,子进程IO以及文件名,终端,键盘编码类型

;; ==============================杂项==============================
;; ------------------------------显示相关------------------------------
(setq read-quoted-char-radix 10) ;字符代码以十进制显示
(column-number-mode t) ;显示列号码
(setq echo-keystrokes 0.1) ;快速在minibuffer显示按键提示，间隔0.1秒
(tooltip-mode -1) ;在minibuffer显示菜单帮助提示
(setq inhibit-startup-message t) ;关闭启动帮助画面
(setq display-time-24hr-format t) ;使用24小时形式显示时间.
(setq display-time-day-and-date t) ;显示时间包含当前日期
(display-time-mode t) ;显示时间
(blink-cursor-mode -1) ;光标不闪
(show-paren-mode t) ;一直显示括号
(setq blink-matching-paren nil) ;关闭括号匹配自动跳转
(setq blink-matching-delay 1) ;default
(setq blink-matching-paren-distance 102400) ;default
(setq tramp-mode -1) ;关闭tramp-mode，不会用，汗～
(recentf-mode t) ;显示最近打开的文件列表
(scroll-bar-mode t) ;显示滚动条
(customize-set-variable 'scroll-bar-mode 'right);将滚动条放在右侧
(tool-bar-mode -1) ;去掉工具栏
(menu-bar-mode -1) ;去掉菜单栏
(setq whitespace-style '(face tabs trailing lines space-before-tab newline
                              indentation empty space-after-tab newline-mark))
;;------------------------------其他设置------------------------------
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                        64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq indent-tabs-mode nil)

(delete-selection-mode t);如果选区高亮, 输入则直接替换内容.
;; (require 'dabbrev)
;; (setq dabbrev-case-fold-search nil) ; 自动完成大小写敏感.
(setq-default case-fold-search nil) ; 搜索和匹配大小写敏感.
(setq mark-ring-max 20) ;mark记录设的长一点.
(setq set-mark-command-repeat-pop t) ;开启C-u .弹出pop功能.
(setq enable-recursive-minibuffers t) ;开启minibuffer递归调用.
(setq mode-require-final-newline t); 任意主模式, 文件尾部添加一个新行.(fundamental除外)
(set-default 'indent-tabs-mode nil) ;
(set-default 'indicate-empty-lines t) ;在左侧边缘显示空行标志
(set-default 'imenu-auto-rescan t) ;
;; (set-default 'truncate-lines t); 关闭自动折行.有时候真的很麻烦.
(setq server-name "zw963") ;设定server名称.默认名称是server.
;; (setq server-host "192.168.1.88") ;指定本emacs server是否允许远程连接.
;; (setq server-use-tcp t) ;使用tcp-ip方式连接.
(setq user-full-name "Billy.Zheng") ;发件人姓名
(setq user-mail-address "zw963@163.com") ;发件人邮件地址
;; (setq mail-archive-file-name "~/.mailbak") ;指定发送邮件是，密抄一份到固定文件，作为备份。
                                        ;(setq tramp-default-method "ftp")
(setq dired-recursive-copies 'always) ;dired递归拷贝
(setq dired-recursive-deletes 'always) ;dired递归删除
;; (setq-default major-mode 'text-mode) ;设置默认主模式为text-mode
(setq kill-ring-max 300) ;设置kill-ring可以保存的最大记录
;; (setq-default fill-column 100) ;设置默认100个字符,自动换行.
;; (setq tab-width 2) ;设置默认TAB间隔为2个空格.
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil) ;指定中文标点作为句子的结尾。取消双空格作为句子结尾的约定。
(setq auto-image-file-mode t) ;对于图片扩展名的文件显示图片
(fset 'yes-or-no-p 'y-or-n-p) ;所有的问题用y/n方式
(setq shift-select-mode nil) ;关闭shift选择选区
(windmove-default-keybindings) ;; Shift+方向键选择窗口
(setq visible-bell t) ;屏幕闪屏提示
(desktop-save-mode t) ;保存buffer桌面，函数赋值，t或者-1
(setq desktop-load-locked-desktop t)
(require 'saveplace)
(set-default 'save-place t)
;; (setq mouse-yank-at-point t) ;不在鼠标左键点击的地方插入剪贴板内容,而是使用鼠标中键
;; (global-unset-key [(mouse-2)]) ;屏蔽C-x C-u快捷键
(setq colon-double-space t) ;重排文本时,在冒号后面新加入两个空格.
(setq initial-scratch-message nil) ;不显示启动buffer提示
;; (setq initial-buffer-choice user-init-file)

;;开启几个默认关闭的操作
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
;;设置日历表的中文天干地支，在日期上按 `p C' 就可以显示农历和干支。
(setq chinese-calendar-celestial-stemal-stem
      ["甲" "乙" "丙" "丁" "戊" "已" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
(setq inhibit-default-init t) ;不进行全局初始化
;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$\\|\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("abbrev_defs" . emacs-lisp-mode))

(defun save-buffers-kill-current-frame ()
  (interactive)
  (server-edit)
  (save-buffers-kill-terminal))

(when (not(equal window-system 'w32))
  ;; ;; (server-start) ;不在windows下时，自动启动emacs server
  ;; (global-set-key [(control x) (control c)] 'save-buffers-kill-current-frame)
  (setq x-select-enable-clipboard t) ;允许emacs内的内容复制到X系统剪贴板
  ;; (desktop-save-mode -1)
  )

(when (equal window-system 'w32)
  (require 'w32-fullscreen)
  (global-set-key [(meta f10)] 'w32-fullscreen)
  )

;; ==============================启动模块设置==============================
;; ------------------------------编程相关------------------------------
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(setq mode-compile-reading-time 0)
(setq mode-compile-expert-p t)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(setq mode-compile-save-all-p nil)
(setq mode-compile-always-save-buffer-p t)
;; (global-set-key [(meta c) (k)] 'mode-compile-kill)
(add-hook 'mode-compile-before-compile-hook 'save-buffer)
(require 'pastie)
(delete-selection-mode 1)
;; ------------------------------ git相关 ------------------------------
;; (add-to-list 'load-path (concat plugins "git-emacs"))
(add-to-list 'load-path (concat plugins "magit"))
;; (require 'git-emacs)
;; (require 'git-modeline)
(require 'magit)
;; ------------------------------项目管理------------------------------
(require 'find-file-in-project)
;; (require 'find-things-fast)
(setq ffip-patterns '("*.rb"))
(setq ffip-find-options "-not -regex \".*test.*\"")
(setq ffip-project-root "~/work/depot")
;; (global-set-key [(control c) (control F)] 'find-file-in-project) ; 从当前Project列表中打开文件.
(global-set-key [(control c) (control F)] 'find-file-in-project) ; 从当前Project列表中打开文件.
;; ------------------------------nxhtml相关------------------------------
;; (load (concat plugins "nxhtml/autostart.el"))
;; ------------------------------color-theme相关设置------------------------------
;; (add-to-list 'load-path (concat plugins "color-theme"))
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      ;; (color-theme-hober) ;;字符界面方案
;;      ;; (color-theme-deep-blue) ;;图形界面方案
;;      ))
;; ;; (require 'color-theme-leuven)
;; ;; (color-theme-leuven)
;; ;; (load-file "~/.emacs.d/plugins/color-theme-railscasts.el")
;; ;; (color-theme-railscasts)
;; ------------------------------multi-web-mode------------------------------
(add-to-list 'load-path (concat plugins "multi-web-mode"))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'rhtml-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
                  (ruby-mode "<%" "%>")))

(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
;; ------------------------------color-theme-zenburn------------------------------
(require 'color-theme-zenburn)
(color-theme-zenburn)
;; ------------------------------ELPA相关设置------------------------------
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(setq package-user-dir "~/.emacs.d/emacs-starter-kit/elpa")

;; ------------------------------diff相关设置------------------------------
;; Default to unified diffs
(setq diff-switches "-Naur")
(setq vc-rcs-diff-switches "-u")
;; Cosmetics

(set-face-background 'vertical-border "white")
(set-face-foreground 'vertical-border "white")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(setq ediff-coding-system-for-read 'utf-8-auto-unix)
(setq ediff-coding-system-for-write 'utf-8-auto-unix)

(setq ediff-auto-refine-limit 30000)
;;;------------------------------auto-complete相关设置------------------------------
(add-to-list 'load-path (concat plugins "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'rhtml-mode)
(add-to-list 'ac-modes 'inf-ruby-mode)
;; (defun ac-ruby-mode-setup ()
;;   (setq ac-sources (append '(ac-source-yasnippet ac-source-gtags) ac-sources)))
;; (defun ac-common-setup ()
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))
(ac-config-default)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-yasnippet
                           ac-source-words-in-same-mode-buffers))
(require 'auto-complete-ruby)
(ac-ruby-init)
(setq ac-auto-start 4) ; less then 4, use yasnippet.
(setq ac-use-quick-help nil)
(setq ac-auto-show-menu nil)
(setq ac-ignore-case nil)               ;忽略大小写, 靠. 这个找的好辛苦.
(ac-set-trigger-key "TAB")
(global-set-key [(control meta g)] 'ac-clear-dictionary-cache)
;;;------------------------------yasnippet相关设置------------------------------
(add-to-list 'load-path (concat plugins "yasnippet"))
(require 'yasnippet)
(setq yas/snippet-dirs "~/.emacs.d/snippets")
(setq yas/triggers-in-field nil)
;; (yas/initialize)
;; (yas/load-directory yas/snippet-dirs)
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
(yas/global-mode 1)
(setq yas/x-pretty-prompt-templates t)
(define-key yas/keymap [(return)] 'yas/next-field)
(define-key yas/keymap [(?\r)] 'yas/next-field)
;;;------------------------------helm设置------------------------------
(add-to-list 'load-path (concat plugins "helm"))
(require 'helm-config)
;;;------------------------------w3m相关设置------------------------------
(add-to-list 'exec-path (concat plugins "emacs-w3m/bin"))
(add-to-list 'load-path (concat plugins "emacs-w3m"))
(setq w3m-icon-directory (concat plugins  "emacs-w3m/icons"))
(require 'w3m-load)
(autoload 'w3m-view-url-with-external-browser "w3m" "w3m" t)
;; (require 'mime-w3m)
(setq w3m-tab-width 8)
(setq w3m-default-display-inline-images t)
(setq w3m-home-page "http://www.google.com")
(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; ------------------------------启动外置chrome浏览器设置------------------------------
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "Chrome")
;; (setq browse-url-firefox-program "Chrome")
;; (setq browse-url-firefox-arguments "")
;;;------------------------------翻译相关设置------------------------------
;; google-translate设置
(add-to-list 'load-path (concat plugins "translate"))
(require 'text-translator)
(setq text-translator-auto-selection-func "google.com")
(setq text-translator-default-engine "google.com_enja")
(global-set-key [(meta ?\s)] 'google-translate-this-word)
;; dict设置
(add-to-list 'load-path (concat plugins "dictionary-1.8.7"))
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(setq dictionary-server "localhost")
(global-set-key [(meta t)] ' dictionary-lookup-definition)
;;;------------------------------其他模块设置------------------------------
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;; 使用键盘移动光标到错误上,迷你缓冲显示错误提示.
(require 'flymake-cursor)
;; kill-ring增强
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;; session模块
;; (require 'session)
;; (session-initialize)
;; 矩形块高亮显示/鼠标选择矩形块
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "exchange point and mark for rectangle." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
;; 高亮显示矩形选区快捷键
(global-set-key [(meta \7)] 'rm-exchange-point-and-mark) ;高亮显示矩形选区
(define-key global-map [(S-down-mouse-1)] 'rm-mouse-drag-region) ;鼠标选择矩形选区 Shift+鼠标左键
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
;; 注意,剪贴命令为C-x r C-w,复制为C-x r M-w,粘帖为C-x r y,删除为C-x r d.

;; 在Chrome浏览器中使用Emacs发帖
(require 'edit-server)
(setq edit-server-default-major-mode 'text-mode)
(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start))

(add-hook 'edit-server-start-hook
          (lambda ()
            (when (string-match "ruby-china.org" (buffer-name))
              (markdown-mode))
            ))

;;隐藏行模式
(require 'hide-lines)
;; 自动识别unicode格式
(require 'unicad)
;; (require 'anything-config)
;; 编辑apache脚本
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig" . conf-unix-mode))

;;isearch搜索kill ring.
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)
;;在左侧显示当前buffer的一个带预览的minibar.
(require 'minimap)
;; 使用org直接发blog
(add-to-list 'load-path (concat plugins "blog"))
(require 'org2blog)
;;tree-imenu模式.
(add-to-list 'load-path (concat plugins "tree-mode"))
(add-to-list 'load-path (concat plugins "tree-mode/imenu"))
(require 'glade-mode)
(require 'imenu-tree)
;; ------------------------------打印相关------------------------------
(require 'printing)
(setq pr-ps-name 'default)  ; 设定默认的打印机.
(setq pr-ps-printer-alist)  ; 指定一个(PostScript)打印机列表.

(setq pr-path-alist
      '((unix      "." "~/bin" ghostview mpage PATH)
        (ghostview "$HOME/bin/gsview-dir")
        (mpage     "$HOME/bin/mpage-dir")
        ))
(setq pr-txt-name      'prt_06a)
(setq pr-txt-printer-alist
      '((prt_06a "lpr" nil "prt_06a")
        (prt_07c nil   nil "prt_07c")
        ))
(setq pr-ps-name       'lps_06b)
(setq pr-ps-printer-alist
      '((lps_06b "lpr" nil "-P" "lps_06b")
        (lps_07c "lpr" nil nil  "lps_07c")
        (lps_08c nil   nil nil  "lps_08c")
        ))
(pr-update-menus t)             ; update now printer and utility menus
;; ==============================关闭的模块==============================
;; (require 'smooth-scrolling)
;; (require 'pager)
;; (require 'ibuf-ext) ;隐藏系统buffer
;; (setq undo-no-redo t) ;undo但不redo
;; (setq coding-system-for-write 'utf-8-unix) ;强制保存为utf8编码系统.
                                        ;(如果不指定这个,可以通过set-buffer-file-coding-system
                                        ; 指定使用哪种编码系统保存当前文件)
;; (setq locale-coding-system 'utf-8) ;system message使用的编码系统,这个应该自动指定,否则模式条乱码
;; (set-selection-coding-system 'utf-8) ;设置同其他软件交换信息的编码系统,这个不应该手动设置.否则
                                        ;复制粘帖数据会引起乱码.

;;    (add-to-list 'ibuffer-never-show-predicates "^\\*")
;; (global-linum-mode t); 显示行号
;; (setq show-trailing-whitespace t) ;显示行尾多余空格
;; (keyboard-translate ?\C-i ?\H-i) ;取消Tab与Ctrl-i绑定，重定义Ctrl-i为Hyper-i
;; (global-set-key [?\H-i] 'keyboard-escape-quit) ;运行ESC ESC ESC  Ctrl-i
;;(setq explicit-shell-file-name 'bin/sh') ;修改默认的shell为其他shell
;; (setq-default kill-whole-line t) ; 在行首 C-k 时，同时删除该行。
;;;==============================帮助==============================

;;快捷键绑定有几种输入方式：

;; 使用键盘宏(kbd)
;; (global-set-key (kbd "C-z") 'shell)
;; (global-set-key (kbd "<f5>") 'flyspell-mode
;; "-"用来表示键盘序列，C-z表示按下Ctrl的同时再按下z
;; <>表示函数键。在Emacs帮助中，按键函数也是会用<>括起来以示区别。
;; “'”表示后面紧跟一个函数(shell)，而不是变量。

;; 使用Lisp字符串
;; (global-set-key "\C-x\M-x" 'shell)
;; 控制字符不分大小写，\C-x 等价于 \C-X
;; 这种方式有个限制就是无法表示非标准的ASCII字符。
;; 常见的几个特殊字符TAB,RET,ESC,DEL，SPC,LFD表示为
;; \t   \C-i    TAB
;; \r   \C-m    RET
;; \n   \C-j    LFD
;; \e   \C-[    ESC
;; \d   \C-/    DEL
;; \s   \C-     SPC
;; \f   \C-l    FFD
;; \a   \C-g    BEL
;; \b   \C-h    BS
;; (global-set-key "\C-x\t" 'indent-rigidly)

;; 使用函数键及控制键
;; 如果只有一个函数键，则在方括号内直接输入，例如：
;; (global-set-key [home] 'shell)
;; (global-set-key [C-mouse-1] 'shell)
;; 两个以上函数键或者组合键需要用括号括起来
;; (global-set-key [(control z)] 'shell)
;; (global-set-key [(control x) (control b)] 'shell)

;; 常用函数键有f1-f12,left,up,right,down,prior(Pgup),next(Pgdown),home,end,del,
;; begin,insert,undo,redo,kp0-...kp9(数字键盘)
;; 常用控制键有tab(TAB),return(RET),backspace(BS),linefeed(LFD)
;; escape(ESC),delete(DEL)，control(CTRL)他们同样也是函数键。

;; 使用鼠标键
;; 鼠标键有mouse-1，mouse-2，mouse-3，和mouse在一起的组合有：
;; drag- down- click- double- triple- C- M- S-。
;; 例如：(global-set-key [(S-down-mouse-1)] 'shell)

;; 如需使用非ASCII字符，使用以下方式
;; (global-set-key [?\C-=] 'shell)-> Ctrl =
;; (global-set-key [?r ?\C- ] 'shell)-> r Ctrl+空格
;; (define-key map [(control ?\s)] 'ido-restrict-to-matches) ->Ctrl-空格
;; (global-set-key [?\M-\C-=] 'shell)-> C-M-=
;; (global-set-key [?\C-z ?\H-l] 'shell)-> C-z H-l
;; (define-key input-decode-map [(control \8)] "\d") ;Ctrl-8 -> DEL

;; 万能输入法,可以使用八进制的ASCII直接添加快捷键(C-x =查询快捷键)
;; (global-set-key [(control x) (?\164)] 'shell) -> Ctrl-x t
;; 如需定义新的前缀，参考以下代码
;; (define-prefix-command 'ctl-z-map)
;; (global-set-key (kbd "C-z") 'ctl-z-map)
;; 如果不知道某个按键的代码，可以在minibuffer，运行
;; global-set-key RET 输入要定义的按键，随便输入个命令，
;; 然后，C-x ESC ESC获得代码信息

;;;------------------------------自动生成脚本------------------------------
