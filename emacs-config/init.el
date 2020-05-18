(require 'package)
(setq package-archives '())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
  evil
  evil-org
  solarized-theme
  ;;spacemacs-theme
  ;;gruvbox-theme
  avy
  moody
  neotree
  projectile
  ess
  poly-R
  yasnippet
  org-bullets
  exec-path-from-shell
  restclient
  ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Enable poly-R for Rmd
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+R-mode))

;;加载主题
(load-theme 'solarized-light t)

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

;;neotree
(global-set-key (kbd "<f8>") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; 快速打开配置文件
(defun init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'init-file)

(setq make-backup-files nil)

;;关闭文件左侧的行号显示，避免大文件的时候卡顿
(global-linum-mode 0)

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
    (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 16")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))


;;让光标不闪烁，同时使用细长的光标而不是一个大方块
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;;开启当前行高亮，设置高亮行底色为灰色
(global-hl-line-mode 1)

;;打开版本控制检测
(setq vc-handled-backends '(Git SVN))

;;启用 moody
(setq x-underline-at-descent-line t)
(setq moody-mode-line-height 24)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)

;; avy
(global-set-key (kbd "M-s") 'avy-goto-word-0)

;;最大化 Emacs 窗口
(toggle-frame-maximized)

;;覆盖 C-x k 这个快捷键，这个快捷键原来绑定到了 kill-buffer 这个命令上，会要求用户输入 buffer 名字选择该 kill 哪个 buffer，但其实大部分时候需要的就是 kill 当前的 buffer 而已。
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;自动折行
(setq truncate-lines nil)

;;projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-project-search-path '("~/projects/blogs" "~/work" "~/pensieve" "~/projects"))

;; yasnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)

;;exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'restclient)

;;Org Mode 设置

;;让代码块中的内容不会有额外的缩进
(setq org-edit-src-content-indentation 0)

;;当光标位于代码块区域时，让 tab 键的行为和写对应语言时的行为一样
(setq org-src-tab-acts-natively t)

;;开启代码块中代码的高亮
(setq org-src-fontify-natively t)

;;对代码块求值时，不经过再次确认，很烦的
(setq org-confirm-babel-evaluate nil)

;;不要在 header 处 o/O 时引入 indent
(setq org-adapt-indentation nil)

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

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; evil org
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)