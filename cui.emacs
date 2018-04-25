;;
;; Tested on Emacs 24.5.1 on Debian/Linux on WSL
;;

;; UTF-8でソースを書くための設定
(setenv "LANG" "ja_JP.UTF-8")

;; 文字コード
(set-language-environment "japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; バックアップを残さない
(setq make-backup-files nil)

;; 行末の空白削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; tramp as sudo
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; 色指定
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'auto-revert-tail-mode 'ansi-color-for-comint-mode-on)
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; package.el
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;; 初期化
(package-initialize)

;; タブを使う
;; http://www.emacswiki.org/emacs/tabbar.el
(unless (package-installed-p 'tabbar)
  (package-refresh-contents) (package-install 'tabbar))
(require 'tabbar)
(global-set-key (kbd "M-t") 'tabbar-forward)
(global-set-key (kbd "M-b") 'tabbar-backward)
(tabbar-mode)

;; 行数を表示させる
(unless (package-installed-p 'nlinum)
  (package-refresh-contents) (package-install 'nlinum))
(require 'nlinum)
(global-nlinum-mode)

;; 矩形範囲選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; magit
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))
(add-to-list 'load-path "~/.emacs.d/magit")
(eval-after-load 'info
  '(progn (info-initialize)
	  (add-to-list 'Info-directory-list "~/.emacs.d/magit/")))
(require 'magit)

;; color
(load-theme 'adwaita t)

;; auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)

;; anzu
(unless (package-installed-p 'anzu)
  (package-refresh-contents) (package-install 'anzu))
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
		    :foreground "red" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

;; scala
;; (require 'dash)
;; (require 'scala-mode2)
;; (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
;; (add-to-list 'load-path "~/.emacs.d/ensime-emacs/")
;; 					;(load "~/.emacs.d/ensime-emacs/ensime.el")
;; (require 'ensime)

;; markdown
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))

;; javascript
(setq auto-mode-alist (cons '("\\.ejs$" . javascript-mode) auto-mode-alist))

;; svn
;; (autoload 'svn-status "dsvn" "Run `svn status'." t)
;; (autoload 'svn-update "dsvn" "Run `svn update'." t)

;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-mode)
;; (require 'helm-descbinds)
;; (require 'helm-migemo)

;; キー設定
;; (global-set-key (kbd "C-;") 'helm-for-files)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
;; (define-key helm-map (kbd "M-j") 'helm-select-3rd-action)
;; (define-key helm-map (kbd "C-;") 'anything-keyboard-quit)

;; 既存のコマンドを Helm インターフェイスに置き換える
;; (helm-mode 1)
;; 自動補完を無効
;; (custom-set-variables '(helm-ff-auto-update-initial-value nil))
;; helm-mode で無効にしたいコマンド
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil))

;; slim
(unless (package-installed-p 'slim-mode)
  (package-refresh-contents) (package-install 'slim-mode))
(add-to-list 'auto-mode-alist '("\\.slim?\\'" . slim-mode))

;; yaml
(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents) (package-install 'yaml-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(define-key yaml-mode-map "\C-m" 'newline-and-indent)
