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
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; auto installed packaged
(defvar prelude-packages
    '(ack-and-a-half clojure-mode expand-region haskell-mode markdown-mode
                     paredit python rainbow-mode solarized-theme
                     volatile-highlights yari zenburn-theme go-mode recentf
                     auto-complete cc-mode cider company csharp-mode)
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
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; dirtree
(require 'dirtree)

;; auto-complete mode
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")

;; go mode
;; (add-hook 'before-save-hook #'gofmt-before-save)

;; go mode + auto-complete
(add-to-list 'ac-modes 'go-mode)

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

;; rainbow + cider
(add-hook 'cider-repl-mode-hook #'rainbow-mode)

;; company mode + cider
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

;; tab width

(setq tab-width 4) ; or any other preferred value

;; c mode

(defvaralias 'c-basic-offset 'tab-width)
(setq c-basic-indent 4)

