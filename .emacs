;;
;; For GUI Emacs 27
;;
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; transparent
(setq default-frame-alist
      (append (list
               '(alpha . (90 85))
               ) default-frame-alist))

;;ツールバー不要
(tool-bar-mode -1)

;; 対応するカッコを強調表示
(show-paren-mode t)

;; 矩形範囲選択
(cua-selection-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化

;; trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; タブの無効化
(setq-default indent-tabs-mode nil)

(progn
  (require 'whitespace)
  (setq whitespace-style
        '(
          face ; faceで可視化
          trailing ; 行末
          tabs ; タブ
          spaces ; スペース
          space-mark ; 表示のマッピング
          tab-mark
          ))
  (setq whitespace-display-mappings
        '(
          (space-mark ?\u3000 [?\u2423])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
          ))
  (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "RoyalBlue4"
                      :background "RoyalBlue4"
                      :underline nil)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "yellow4"
                      :background "yellow4"
                      :underline nil)
  (set-face-attribute 'whitespace-space nil
                      :foreground "gray40"
                      :background "gray20"
                      :underline nil)
  (global-whitespace-mode t))

;; Add package-archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; auto-install
(add-to-list 'load-path "~/.emacs.d/auto-install")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; exec-path-from-shell
(unless (package-installed-p 'exec-path-from-shell)
  (package-refresh-contents) (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; anzu
(unless (package-installed-p 'anzu)
  (package-refresh-contents) (package-install 'anzu))
(anzu-mode 1)

;; editorconfig
(unless (package-installed-p 'editorconfig)
  (package-refresh-contents) (package-install 'editorconfig))
(editorconfig-mode 1)
;(setq edconf-exec-path "/usr/bin/editorconfig")

;; ansi-color
(unless (package-installed-p 'xterm-color)
  (package-refresh-contents) (package-install 'xterm-color))

;; during compilation, emacs interpret ansi-colors
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Emacsのカラーテーマ
;; color
(load-theme 'deeper-blue t)

;; リドゥー設定
;; redoできるようにする
;; http://www.emacswiki.org/emacs/redo+.el
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-z") 'redo))

;; 行数を表示させる
(require 'linum)
(global-linum-mode)

;; s.el
(unless (package-installed-p 's)
  (package-refresh-contents) (package-install 's))
(require 's)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "Ricty Diminished-26" ))

;; UTF-8でソースを書くための設定
(setenv "LANG" "ja_JP.UTF-8")

;; 文字コード
(prefer-coding-system 'utf-8-unix)

;; バックアップを残さない
(setq make-backup-files nil)

;; auto-complete
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents) (package-install 'auto-complete))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; comment-tags -- https://github.com/vincekd/comment-tags
(unless (package-installed-p 'comment-tags)
  (package-refresh-contents) (package-install 'comment-tags))
(require 'comment-tags)
(add-hook 'prog-mode-hook 'comment-tags-mode)

;; multiple-cursors
(unless (package-installed-p 'multiple-cursors)
  (package-refresh-contents) (package-install 'multiple-cursors))
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; migemo
;; http://emacs.rubikitch.com/migemo/
(unless (package-installed-p 'migemo)
  (package-refresh-contents) (package-install 'migemo))
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; 辞書ファイルを環境に合わせて設定してください！
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; helm-swoop
;; http://emacs.rubikitch.com/helm-swoop/
;; https://github.com/emacsorphanage/helm-swoop
(unless (package-installed-p 'helm-swoop)
  (package-refresh-contents) (package-install 'helm-swoop))
(require 'helm)
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)

;;;
;;; Perl編集
;;;
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
;; Perl デバッガの設定
(autoload 'perl-debug "perl-debug" nil t)
(autoload 'perl-debug-lint "perl-debug" nil t)

;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)
;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
;;; I/O バッファを表示
(setq gdb-use-separate-io-buffer t)
;;; t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)

;;; magit
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))
(require 'magit)

;; bashdb
(autoload 'bashdb "bashdb" "Run bashdb" t nil)

;; my-kill-buffer
(defun my-kill-buffer (all)
  (interactive "P") ;;”P” はprefix argumentを受け取る宣言のひとつ
  (dolist (buf (buffer-list))
    (if (or all ;; prefix argumentがあれば全バッファを削除
	    (buffer-file-name buf)) ;通常はvisitしているfileを削除
	(kill-buffer buf))))

;; mark-down-mode
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; yaml-mode
(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents) (package-install 'yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; tramp
(setq tramp-default-method "ssh")

;; flycheck
(unless (package-installed-p 'flycheck)
  (package-refresh-contents) (package-install 'flycheck))
(global-flycheck-mode)

;; ruby-mode
;(load "~/.emacs.d/ruby")
;; crystal-mode
(load "~/.emacs.d/crystal")
;; go-mode
;;(load "~/.emacs.d/go")
;; c++-mode
;;(load "~/.emacs.d/cpp")
;; d-mode
;;(load "~/.emacs.d/d")

;; csharp-mode
(load "~/.emacs.d/csharp")
;; visual-basic-mode
(load "~/.emacs.d/vb.el")

;; vue-mode
;(load "~/.emacs.d/vue")
;; prolog
;(load "~/.emacs.d/prolog")
;; bison
;;(load "~/.emacs.d/bison")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(package-selected-packages
   '(visual-basic-mode csharp-mode lsp-ui-mode helm-swoop migemo multiple-cursors eglot comment-tags package-utils yafolding haskell-mode dockerfile-mode docker global-tags bison-mode ediprolog mmm-mode inf-ruby ruby-block rbenv ac-dcd go-autocomplete lsp-ui company-lsp lsp-mode protobuf-mode flycheck ctags-update yaml-mode markdown-mode d-mode s todotxt-mode anzu xterm-color editorconfig exec-path-from-shell slime magit dpkg-dev-el devscripts auto-complete))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(put 'magit-clean 'disabled nil)
