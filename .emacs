;; auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;ツールバー不要
(tool-bar-mode -1)

;; 対応するカッコを強調表示
(show-paren-mode t)

;; 矩形範囲選択
(cua-selection-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化

;; trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add package-archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Emacsのカラーテーマ
;; color
(load-theme 'deeper-blue t)

;; 行数を表示させる
(require 'linum)
(global-linum-mode)

;; タブを使う
;; http://www.emacswiki.org/emacs/tabbar.el
(unless (package-installed-p 'tabbar)
  (package-refresh-contents) (package-install 'tabbar))
(require 'tabbar)
;(global-set-key "\M-b" 'tabbar-backward-tab)
;(global-set-key "\M-t" 'tabbar-forward-tab)
(global-set-key [(backtab)]'tabbar-backward)
(global-set-key [(control tab)]'tabbar-forward)
(tabbar-mode)

;; UTF-8でソースを書くための設定
(setenv "LANG" "ja_JP.UTF-8")

;; 文字コード
(prefer-coding-system 'utf-8-unix)

;; バックアップを残さない
(setq make-backup-files nil)

;; exec-path-from-shell
(unless (package-installed-p 'exec-path-from-shell)
  (package-refresh-contents) (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; maxframe
(unless (package-installed-p 'maxframe)
  (package-refresh-contents) (package-install 'maxframe))
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;; magit
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))
(require 'magit)

;; mark-down-mode
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; anzu
(unless (package-installed-p 'anzu)
  (package-refresh-contents) (package-install 'anzu))
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
		    :foreground "red" :weight 'bold)
