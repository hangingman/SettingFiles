;; Lisp用にSLIMEの設定
;; lisp-mode
(setq inferior-lisp-program "clisp"); clisp用
(require 'slime)
(slime-setup)

;; auto-install
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(add-to-list 'load-path "~/.emacs.d/auto-install")

;; Emacsのカラーテーマ
;; http://code.google.com/p/gnuemacscolorthemetest/
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(when (and (require 'color-theme nil t) (window-system))
 (color-theme-initialize)
 (color-theme-oswald))

;; リドゥー設定
;; redoできるようにする
;; http://www.emacswiki.org/emacs/redo+.el
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-z") 'redo))

;; 行数を表示させる
(require 'linum)
(global-linum-mode)

;; 矩形範囲選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; タブを使う
;; http://www.emacswiki.org/emacs/tabbar.el
(require 'tabbar)
(global-set-key [(backtab)]'tabbar-backward)
(global-set-key [(control tab)]'tabbar-forward)
(tabbar-mode)

;; フォント設定 osakaの等倍
(add-to-list 'default-frame-alist
             '(font . "-apple-Osaka－等幅-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1"))
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

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; yasnippet
(require 'yasnippet)

;;;
;;; Scala編集
;;;
;;
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
(add-hook 'scala-mode-hook
            '(lambda ()
               (yas/minor-mode-on)
	       (c-set-style "k&r")
	       ;; センテンスの終了である ';' を入力したら、自動改行+インデント
	       (c-toggle-auto-hungry-state 1)
               ))

;; Scalaはコンパイルの時に色指定が入る
(require 'ansi-color)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ensime
(add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
    (require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;
;;; Perl編集
;;;
;; 
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))

;;
;; C++編集
;;
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
             (define-key c++-mode-base-map "\C-c\C-M-c" 'uncommegnt-region)
             (define-key c++-mode-base-map "\C-ce"      'c-macro-expand)
             (define-key c++-mode-base-map "\C-cc"      'my-compile)
             (define-key c++-mode-base-map "\C-c\M-c"   'compilation-close)
             (define-key c++-mode-base-map "\C-cg"      'gdb)
             (define-key c++-mode-base-map "\C-ct"      'toggle-source)
             ;; cc-modeに入る時に自動的にgtags-modeにする
					;(gtags-mode t)
					;)
             ))

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
(require 'magit) 

;; bashdb
(autoload 'bashdb "bashdb" "Run bashdb" t nil)

(defun my-kill-buffer (all)
  (interactive "P") ;;”P” はprefix argumentを受け取る宣言のひとつ
  (dolist (buf (buffer-list))
    (if (or all ;; prefix argumentがあれば全バッファを削除
	    (buffer-file-name buf)) ;通常はvisitしているfileを削除
	(kill-buffer buf))))
;; anything
(require 'anything)
;; gtags
(setq gtags-prefix-key "\C-c")
(require 'gtags)
(require 'anything-gtags)
;; キーバインド
(setq gtags-mode-hook
      '(lambda ()
         (define-key gtags-mode-map "\C-cs" 'gtags-find-symbol)
         (define-key gtags-mode-map "\C-cr" 'gtags-find-rtag)
         (define-key gtags-mode-map "\C-ct" 'gtags-find-tag)
         (define-key gtags-mode-map "\C-cf" 'gtags-parse-file)))
;; gtags-mode を使いたい mode の hook に追加する
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)))

;; Subversion操作
(require 'psvn)
(setq process-coding-system-alist '(("svn" . utf-8)))
(setq default-file-name-coding-system 'utf-8)
(setq svn-status-svn-file-coding-system 'utf-8)

;; update GTAGS
;; (defun update-gtags (&optional prefix)
;;   (interactive "P")
;;   (let ((rootdir (gtags-get-rootpath))
;;         (args (if prefix "-v" "-iv")))
;;     (when rootdir
;;       (let* ((default-directory rootdir)
;;              (buffer (get-buffer-create "*update GTAGS*")))
;;         (save-excursion
;;           (set-buffer buffer)
;;           (erase-buffer)
;;           (let ((result (process-file "gtags" nil buffer nil args)))
;;             (if (= 0 result)
;;                 (message "GTAGS successfully updated.")
;;               (message "update GTAGS error with exit status %d" result))))))))
;; (add-hook 'after-save-hook 'update-gtags)