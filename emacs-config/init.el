(require 'package)
(setq package-archives '())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
  solarized-theme
  gruvbox-theme
  neotree
  projectile
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

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;;加载主题
(load-theme 'gruvbox-light-medium t)

(setq scroll-conservatively 101)        ;光标移出时平滑滚动而不是重定位到中央
(setq mouse-wheel-scroll-amount '(1))   ;用鼠标滚动时一次只滚动一行

;;用 y/n 替代 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;在标题栏显示 buffer 名字
(setq frame-title-format "[%b]")

;;关闭 UI 上不需要的元素：工具栏、菜单栏、滚动条
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;neotree
(global-set-key (kbd "<f8>") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)

;;让光标不闪烁，同时使用细长的光标而不是一个大方块
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;;开启当前行高亮，设置高亮行底色为灰色
(global-hl-line-mode 1)

;;安装 moody 这个 modline 工具（如果没安装的话）
(when (not (require 'moody nil :noerror))
  (progn
    (message "install moody now...")
    (setq url-http-attempt-keepalives nil)
    (package-refresh-contents)
    (package-install 'moody)))

;;打开版本控制检测
(setq vc-handled-backends '(Git SVN))

;;启用 moody
(setq x-underline-at-descent-line t)
(setq moody-mode-line-height 24)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)

;;最大化 Emacs 窗口
(toggle-frame-maximized)

;;覆盖 C-x k 这个快捷键，这个快捷键原来绑定到了 kill-buffer 这个命令上，会要求用户输入 buffer 名字选择该 kill 哪个 buffer，但其实大部分时候需要的就是 kill 当前的 buffer 而已。
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;自动折行
(setq truncate-lines nil)


;;Org Mode 设置

;;让代码块中的内容不会有额外的缩进
(setq org-edit-src-content-indentation 0)

;;当光标位于代码块区域时，让 tab 键的行为和写对应语言时的行为一样
(setq org-src-tab-acts-natively t)

;;开启代码块中代码的高亮
(setq org-src-fontify-natively t)

;;对代码块求值时，不经过再次确认，很烦的
(setq org-confirm-babel-evaluate nil)
