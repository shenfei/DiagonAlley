(require 'package)
(setq package-archives '())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(eval-when-compile (require 'use-package))


;;; Basic Settings

(setq scroll-conservatively 101)        ;光标移出时平滑滚动而不是重定位到中央
(setq mouse-wheel-scroll-amount '(1))   ;用鼠标滚动时一次只滚动一行

;;用 y/n 替代 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;在标题栏显示 buffer 名字
(setq frame-title-format "[%b]")

;;忽略报错声
(setq ring-bell-function 'ignore)

;;关闭 UI 上不需要的元素：工具栏、菜单栏、滚动条
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq make-backup-files nil)

;;最大化 Emacs 窗口
(toggle-frame-maximized)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;自动折行
(setq truncate-lines nil)

;;让光标不闪烁，同时使用细长的光标而不是一个大方块
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;;开启当前行高亮，设置高亮行底色为灰色
(global-hl-line-mode 1)

(setq-default show-trailing-whitespace t)

;;打开版本控制检测
(setq vc-handled-backends '(Git SVN))

(setq make-backup-files nil)

;; Disable line number to avoid slowing down emacs
(when (version<= "29.0.0" emacs-version)
  (global-display-line-numbers-mode 0))

;;在 mode-line 中显示行号和列号
(line-number-mode 1)
(column-number-mode 1)

;; 设置字体
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;;(set-face-attribute 'default nil :height 160)
;;https://manateelazycat.github.io/emacs/2020/04/02/org-font.html
(let ((emacs-font-size 16)
      (emacs-font-name "Fira Code"))
  (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
  (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name)))

(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "Sarasa Term SC Nerd 16")
    (advice-add #'org-string-width :override #'org--string-width-1)
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))


;;; Packages configs

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))
;(use-package gruvbox-theme
;  :ensure t
;  :config
;  (load-theme 'gruvbox-light-hard))

;; yasnippet
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(use-package restclient)

;; all-the-icons
(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[Rr]md$" all-the-icons-fileicon "R" :face all-the-icons-lblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[Rr]proj$" all-the-icons-fileicon "R" :face all-the-icons-lblue)))

;; avy
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-0))

;;exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; moody
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 24)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Enable poly-R for Rmd
(use-package ess :ensure t)
(use-package poly-markdown :ensure t)
(use-package poly-R
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

;;projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/blogs" "~/work" "~/pensieve" "~/projects"))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;neotree
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package neotree
  :ensure t
  :after projectile
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind ("<f8>" . neotree-project-dir))


;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  ;; Custom evil keymaps
  (evil-define-key '(normal insert) 'global (kbd "C-a") 'beginning-of-line)
  (evil-define-key '(normal insert) 'global (kbd "C-e") 'end-of-line)
  (evil-define-key 'normal 'global (kbd "C-u C-u") 'evil-scroll-up)
  (evil-define-key 'normal 'global (kbd ", s") 'save-buffer)

  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-enter-vertical-split)
  (evil-define-key 'normal neotree-mode-map (kbd "i") 'neotree-enter-horizontal-split)
  (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )

;;Org Mode 设置

;;让代码块中的内容不会有额外的缩进
(setq org-edit-src-content-indentation 0)

;;当光标位于代码块区域时，让 tab 键的行为和写对应语言时的行为一样
(setq org-src-tab-acts-natively t)

;;开启代码块中代码的高亮
(setq org-src-fontify-natively t)

;;对代码块求值时，不经过再次确认，很烦的
(setq org-confirm-babel-evaluate nil)

;; Trun on org-indent-mode globally
(setq org-startup-indented t)

;; Override default value introduced in v9.4 (Emacs 27.2)
(setq org-startup-folded t)

;; M-RET will go to the end of the line before making a new line
(setq org-M-RET-may-split-line nil)

;; Org mode settings for work log
(setq org-agenda-files (list (format-time-string "~/pensieve/work_log/%Y/%Y-%m.org"))) ;; 只将当月 work log 纳入 agenda
(setq org-clock-into-drawer 2)
(setq org-clock-clocktable-default-properties '(:maxlevel 4 :scope tree))
(global-set-key (kbd "C-c r") 'org-clock-report)

(setq org-tag-alist '(("job") ("hack") ("study")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCK(b)" "|" "STAGE(s)" "DONE(d)" "POSTPONE(p)" "CANCEL(c)")))

(setq org-image-actual-width nil)

(global-set-key (kbd "C-c a") 'org-agenda)
(evil-define-key '(normal insert) 'global (kbd "C-c i") 'org-clock-in)
(evil-define-key '(normal insert) 'global (kbd "C-c o") 'org-clock-out)
(setq org-agenda-skip-scheduled-if-done t)

;;checkbox face
;;https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

(use-package org-tempo)
(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; evil org
(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

(use-package evil-org-agenda
  :after (evil org evil-org)
  :config
  (evil-org-agenda-set-keys))


;;; Custom Keybindings and Functions

;; 快速打开配置文件
(defun init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'init-file)

;; Unicode
;; 设置弯引号与 macOS 输入法一致
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
(define-key key-translation-map (kbd "M-[") (kbd "“"))
(define-key key-translation-map (kbd "M-{") (kbd "”"))
(define-key key-translation-map (kbd "M-]") (kbd "‘"))
(define-key key-translation-map (kbd "M-}") (kbd "’"))

;; meta/super mapping
;; default to exteranl keyboard use assume the alt/win keys have been swapped in system keyboard setting
(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-right-command-modifier 'super
      mac-right-option-modifier 'meta)

(defun shen/swap-meta-and-super ()
  "Set left and right cmd/opt keys to different mappings.
Swap the binding when you change between mac internal keyboard to external keyboard."
  (interactive)
  (if (eq mac-command-modifier 'meta)
      (progn
        (setq mac-command-modifier 'super)
        (setq mac-option-modifier 'meta)
        (setq mac-right-command-modifier 'meta)
        (setq mac-right-option-modifier 'super)
        (message "Switch to internal keyboard. Left: Opt->M Cmd->s Right: Cmd->M Opt->s"))
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setq mac-right-command-modifier 'super)
      (setq mac-right-option-modifier 'meta)
      (message "Switch to external keyboard. Left: Win/Opt->M Alt/Cmd->s Right: Alt/Cmd->M Win/Opt->s"))))

(global-set-key (kbd "C-c w") 'shen/swap-meta-and-super)
