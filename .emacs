(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left14")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family #("Osaka－等幅" 5 8 (charset cp932-2-byte)))))))

;; 拡張して使うelispファイルをライブラリパスに追加 
(setq load-path 
      (append 
       (list 
	;; 個別で入れるelispファイル用
	(expand-file-name "~/elisp/"))
			  load-path))

;; Android-mode
(require 'android-mode)
(setq android-mode-sdk-dir "c:/android-sdk-windows")

;; elib
(add-to-list 'load-path "~/.emacs.d/lisp/elib1.0")

;;;; ウインドウサイズの設定
(setq default-frame-alist
      (append
       '(
	 (top . 45)      ; フレームの縦位置(ドット位置)
	 (left . 45)   ; フレームの横位置(ドット位置)
	 (width . 180)  ; フレーム幅(文字数)
	 (height . 40)) ; フレーム高(文字数)
       default-frame-alist))

(setq initial-frame-alist default-frame-alist)

;; フォント設定 osakaの等倍
(add-to-list 'default-frame-alist
             '(font . "-outline-Osaka－等幅-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))

;; ECBをロードするための設定
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(load-file "~/.emacs.d/cedet-1.0.1/common/cedet.el")
(require 'ecb)

(defun ecb-toggle ()
    (interactive)
      (if ecb-minor-mode
                (ecb-deactivate)
            (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C++のテンプレート作成用記述 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autoinsert)

;; テンプレートのディレクトリ
(setq auto-insert-directory "~/.emacs.d/insert/")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.h$"   . ["template.h" my-template])
	       ("Makefile$"   . ["Makefile" my-template])
               ) auto-insert-alist))
(require 'cl)
(defvar template-replacements-alists
  '(("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () 
          (setq file-without-ext (file-name-sans-extension
                                   (file-name-nondirectory (buffer-file-name))))))
    ("%namespace%" .
         (lambda () (setq namespace (read-from-minibuffer "namespace: "))))
    ;; Makefile向けのテンプレート
    ("%target%" . 
     (lambda () (setq target (read-from-minibuffer "target name: "))))

    ("%include%" .
         (lambda () 
           (cond ((string= namespace "") (concat "\"" file-without-ext ".h\""))
                 (t (concat "<" (replace-regexp-in-string "::" "/" namespace) "/"
                            file-without-ext ".h>")))))
    ("%include-guard%" . 
         (lambda ()
           (format "%s_H_"
                   (upcase (concat 
                             (replace-regexp-in-string "::" "_" namespace)
                             (unless (string= namespace "") "_")
                             file-without-ext)))))
    ("%name%" . user-full-name)
    ("%mail%" . (lambda () (identity user-mail-address)))
    ("%cyear%" . (lambda () (substring (current-time-string) -4)))
    ("%bdesc%" . (lambda () (read-from-minibuffer "Brief description: ")))
    ("%namespace-open%" .
       (lambda ()
         (cond ((string= namespace "") "")
               (t (progn 
                   (setq namespace-list (split-string namespace "::"))
                   (setq namespace-text "")
                   (while namespace-list
                     (setq namespace-text (concat namespace-text "namespace "
                                                 (car namespace-list) " {\n"))
                     (setq namespace-list (cdr namespace-list))
                   )
                   (eval namespace-text))))))
    ("%namespace-close%" .
       (lambda ()
         (cond ((string= namespace "") "")
               (t (progn
                   (setq namespace-list (reverse (split-string namespace "::")))
                   (setq namespace-text "")
                   (while namespace-list
                      (setq namespace-text (concat namespace-text "} // " (car namespace-list) "\n"))
                      (setq namespace-list (cdr namespace-list))
                   )
                   (eval namespace-text))))))
))

;; 雛形ファイル中のマクロを展開する定義
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))

(add-hook 'find-file-not-found-hooks 'auto-insert)

;; UTF-8でソースを書くための設定
;; Setenv
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

;; auto-completeを使う設定
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(load-file "~/.emacs.d/auto-complete-1.3.1/auto-complete.el")
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; yasnippetを使う設定
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(load-file "~/.emacs.d/yasnippet/yasnippet.el")
(load-file "~/.emacs.d/yasnippet/dropdown-list.el")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")