;;
;; config
;;

;; disable menu bar
(menu-bar-mode 0)

;; disable tool bar
(tool-bar-mode -1)

;; transparent background
(set-frame-parameter (selected-frame) 'alpha '(75 75))
(add-to-list 'default-frame-alist '(alpha 75 75))
(set-face-attribute 'default nil :background "black" :foreground "white")

;; column / line number mode
(line-number-mode 1)
(column-number-mode 1)

;; hightlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")

;; word wrapping
(setq-default word-wrap t)

;; column line where text should be wrapped
(setq-default fill-column 80)

;; store backup files in .emacs.d/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;
;; elpa
;;

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; auto installed packaged
(defvar prelude-packages
    '(ack-and-a-half clojure-mode expand-region haskell-mode markdown-mode
                     paredit python rainbow-delimiters solarized-theme
                     volatile-highlights yari zenburn-theme go-mode recentf
                     auto-complete auto-complete-clang cc-mode cider company
                     csharp-mode leerzeichen flycheck js2-mode json-mode
                     exec-path-from-shell markdown-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl)
(defun prelude-packages-installed-p ()
    (loop for p in prelude-packages
          when (not (package-installed-p p)) do (return nil)
          finally (return t)))

(unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (when (not (package-installed-p p))
        (package-install p))))

(provide 'prelude-packages)

;;
;; customizations
;;

(add-to-list 'load-path "~/.emacs.d/vendor")

;; sync $PATH
;;(defun set-exec-path-from-shell-PATH ()
;;  (let ((path-from-shell (replace-regexp-in-string
;;                          "[ \t\n]*$"
;;                          ""
;;                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;    (setenv "PATH" path-from-shell)
;;    (setq eshell-path-env path-from-shell) ; for eshell users
;;    (setq exec-path (split-string path-from-shell path-separator))))
;;
;; (when window-system (set-exec-path-from-shell-PATH))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; go
;; (add-hook 'before-save-hook #'gofmt-before-save)

;; cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-stacktrace-fill-column 80)
(setq cider-repl-display-in-current-window t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-result-prefix ";; => ")
(add-to-list 'exec-path "/usr/local/bin")
(setq cider-show-error-buffer nil)

;; paredit + cider
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; company + cider
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; icomplete
(require 'icomplete)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; move between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; google c/c++ style guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; leerzeichen
(require 'leerzeichen)
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(add-to-list 'ac-modes 'cc-mode)
(add-to-list 'ac-modes 'go-mode)
(add-to-list 'ac-modes 'clojure-mode)
(add-to-list 'ac-modes 'css-mode)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'json-mode)
(add-to-list 'ac-modes 'web-mode)

;; auto-complete-clang
(require 'auto-complete-clang)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; use c++11 when flychecking
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
