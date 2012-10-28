;; より下に記述した物が PATH の先頭に追加されます
;(dolist (dir (list
;	      "/mingw/bin"
;	      "/usr/bin"
;	      "/usr/local/bin"
;	      "/c/MinGW/msys-1.0.16/bin/"
;	      "/c/Python27"
;	      "/C/Program Files/Java/jdk1.6.0_25/bin"
;	      "/usr/local/src/clisp-2.49/src"
;             "~/.emacs.d/"
;	      "."
;              ))
;; PATH と exec-path に同じ物を追加します
;(setenv "PATH" (concat dir ":"))
;;(when (and (file-exists-p dir) (not (member dir exec-path)))
;;(setenv "PATH" (concat dir ":" (getenv "PATH")))
;;  (setenv "PATH" (concat dir ":" dir))
;;  (setq exec-path (append (list dir) exec-path))))
(setenv "PATH"
  (concat
   "C:\\Users\\learning\\AppData\\Roaming\\.emacs.d\\" ";"
   "C:\\MinGW\\bin" ";"
   "C:\\MinGW\\msys-1.0.16\\bin" ";"
   "C:\\Python27\\" ";"
   "C:\\MinGW\\msys-1.0.16\\local\\src\\clisp-2.49\\src" ";"
   (getenv "PATH")))


;; slime
(setq load-path (cons (expand-file-name "~/.emacs.d/slime") load-path))

;; Lisp用にSLIMEの設定
;; lisp-mode
(setq inferior-lisp-program "clisp")    ; clisp用

(require 'slime)
(slime-setup)

;; requre 'clはこの辺でしとくべき？
(require 'cl)

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

;; ------------------------------------------------------------------------
;; @ hideshow/fold-dwim.el

;; ブロックの折畳みと展開
;; http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
(when (require 'fold-dwim nil t)
  (require 'hideshow nil t)
  ;; 機能を利用するメジャーモード一覧
  (let ((hook))
    (dolist (hook
             '(emacs-lisp-mode-hook
               c-mode-common-hook
	       c++-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               css-mode-hook
               apples-mode-hook))
      (add-hook hook 'hs-minor-mode))))

;; フォント設定 osakaの等倍
(add-to-list 'default-frame-alist
             '(font . "-outline-Osaka－等幅-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
;; UTF-8でソースを書くための設定
(setenv "LANG" "ja_JP.UTF-8")

;; 文字コード
(set-language-environment "japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

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

;; コンパイルウィンドウを別窓で開く設定
(load-file "~/.emacs.d/compilewindow.el")

;; タブを使う
;; http://www.emacswiki.org/emacs/tabbar.el
(require 'tabbar)
(global-set-key [(control shift tab)] 'tabbar-backward)
(global-set-key [(control tab)]       'tabbar-forward)
(tabbar-mode)

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
	     ; 自前compilation関数 (windowを固定化する)
	     (defun compilation-open ()
               "*compilation*バッファを表示するwindowをオープンする"
               (interactive)
               (let ((cur-window (selected-window))
                     (com-buffer (get-buffer compilation-buffer-name)))
                 (if (null com-buffer)
                     (setq com-buffer (get-buffer-create compilation-buffer-name)))
                 (let ((com-window (get-buffer-window com-buffer)))
                   (if com-window
                       (select-window com-window)
                     (select-window
                      (split-window (selected-window) (- (window-height) 15) nil)))
                   (switch-to-buffer (get-buffer compilation-buffer-name))
                   (select-window cur-window))))
             (defun compilation-close ()
               "*compilation*バッファを表示しているwindowをクローズする"
               (interactive)
               (let ((com-buffer (get-buffer compilation-buffer-name)))
                 (if com-buffer
                     (let ((com-window (get-buffer-window com-buffer)))
                       (if com-window
                           (delete-window com-window))))))
             (defun my-compile (command &optional comint)
               "*compilation*バッファの表示位置を固定化してcompileコマンドを実行する関数。"
               (interactive
                (list
                 (let ((command (eval compile-command)))
                   (if (or compilation-read-command current-prefix-arg)
                       (read-from-minibuffer "Compile command: "
                                             command nil nil
                                             (if (equal (car compile-history) command)
                                                 '(compile-history . 1)
                                               'compile-history))
                     command))
                 (consp current-prefix-arg)))
               (unless (equal command (eval compile-command))
                 (setq compile-command command))
               (save-some-buffers (not compilation-ask-about-save) nil)
               (setq compilation-directory default-directory)
               (compilation-open)
               (compilation-start command comint))
             ;; cc-modeの自前スタイル設定
             (c++-add-style "personal" my-c-style t)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (setq completion-mode t)
             ;; compile-windowの設定
             (setq compilation-buffer-name "*compilation*")
             (setq compilation-scroll-output t)
             (setq compilation-read-command t)
             (setq compilation-ask-about-save nil)
             (setq compilation-window-height 10)
             (setq compile-command "make")
             ;; cc-mode内で定義されるキーバインド
             (define-key c++-mode-base-map "\C-c\C-c"   'comment-region)
             (define-key c++-mode-base-map "\C-c\C-M-c" 'uncomment-region)
             (define-key c++-mode-base-map "\C-ce"      'c-macro-expand)
             (define-key c++-mode-base-map "\C-cc"      'my-compile)
             (define-key c++-mode-base-map "\C-c\M-c"   'compilation-close)
             (define-key c++-mode-base-map "\C-cg"      'gdb)
             (define-key c++-mode-base-map "\C-ct"      'toggle-source)
             ;; cc-modeに入る時に自動的にgtags-modeにする
             ;(gtags-mode t)
             ;)
             ))

;; バックアップを残さない
(setq make-backup-files nil)

;; バッファをすべて閉じる関数
(defun my-revert-buffer ()
  (interactive)
  (dolist (buf (buffer-list))
    (if (not (buffer-file-name buf)) ;visitしているfileに限定
	nil
      (switch-to-buffer buf)
      (revert-buffer t t))))

(defun my-kill-buffer (all)
  (interactive "P") ;;”P” はprefix argumentを受け取る宣言のひとつ
  (dolist (buf (buffer-list))
    (if (or all ;; prefix argumentがあれば全バッファを削除
	    (buffer-file-name buf)) ;通常はvisitしているfileを削除
	(kill-buffer buf))))

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
 '(ecb-layout-window-sizes (quote (("left8" (0.12340425531914893 . 0.2692307692307692) (0.12340425531914893 . 0.21153846153846154) (0.12340425531914893 . 0.2692307692307692) (0.12340425531914893 . 0.17307692307692307)))))
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
