;; slime
(setq load-path (cons (expand-file-name "~/.emacs.d/slime") load-path))

;; Lisp用にSLIMEの設定
;; lisp-mode
(setq inferior-lisp-program "clisp")    ; clisp用

(require 'slime)
(slime-setup)
;; auto-install
(add-to-list 'load-path "~/.emacs.d/auto-install")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; elib
(add-to-list 'load-path "~/.emacs.d/elib1.0")

;; Emacsのカラーテーマ
;; http://code.google.com/p/gnuemacscolorthemetest/
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(when (and (require 'color-theme nil t) (window-system))
  (color-theme-initialize)
  (color-theme-renegade))

;; 背景を半透明にする
(setq default-frame-alist
      (append (list
               '(alpha . (90 85))
               ) default-frame-alist))

;; ウィンドウサイズの設定
;; 最大化する命令の定義
(defvar w32-window-state nil)

(defun w32-fullscreen-switch-frame ()
  (interactive)
  (setq w32-window-state (not w32-window-state))
  (if w32-window-state
      (w32-fullscreen-restore-frame)
    (w32-fullscreen-maximize-frame)
    ))
  
(defun w32-fullscreen-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-fullscreen-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))

(add-hook 'window-setup-hook
          '(lambda () (w32-fullscreen-maximize-frame))
          )

;; リドゥー設定
;; redoできるようにする
;; http://www.emacswiki.org/emacs/redo+.el
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-z") 'redo))

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; フォント設定 osakaの等倍
(add-to-list 'default-frame-alist
             '(font . "-outline-Osaka－等幅-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
;; UTF-8でソースを書くための設定
(setenv "LANG" "ja_JP.UTF-8")

;; 言語環境
(set-language-environment "Japanese")

;; 文字コード
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-16le-dos)

;; Shell Mode
;; MSYS の bash を使用します。
(setq explicit-shell-file-name "c:/MinGW/msys-1.0.16/bin/bash.exe")
(setq shell-file-name "c:/MinGW/msys-1.0.16/bin/sh.exe")
;; SHELL で ^M が付く場合は ^M を削除します。
(add-hook 'shell-mode-hook
	  (lambda ()
	    (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
;; shell-mode での保管(for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

;; Grep
(defadvice grep (around grep-coding-setup activate)
  (let ((coding-system-for-read 'utf-8))
    ad-do-it))

(setq grep-find-command "find . ! -name '*~' -type f -print0 | xargs -0 lgrep -n -Au8 -Ia ")

;; CEDET, ECB用の設定
;; ECBをロードするための設定
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(load-file "~/.emacs.d/cedet-1.0.1/common/cedet.el")
(require 'ecb)

;; C++モードでのコードスタイル設定
(add-hook 'c++-mode-hook
          '(lambda ()
	     ; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linuxなどがある
	     ; 今までeclipseのコードフォーマットでK&Rを使っていたんで
             (c-set-style "k&r")
	     ;; センテンスの終了である ';' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
	     ; ";"を打つと改行+インデント
	     (Define-key c-mode-base-map "\C-m" 'newline-and-indent)
             ))


(defun ecb-toggle ()
    (interactive)
      (if ecb-minor-mode
                (ecb-deactivate)
            (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)
(put 'upcase-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
