;; elpa
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; auto installed packaged
(defvar prelude-packages
  '(color-theme-solarized exec-path-from-shell dired-subtree helm ggtags
                          helm-gtags sr-speedbar company company-c-headers
                          go-mode better-defaults material-theme elpy
                          dockerfile-mode markdown-mode yaml-mode flycheck
                          js2-mode json-mode exec-path-from-shell sass-mode
                          toml-mode)
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

;; system paths
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; vendor path
(add-to-list 'load-path "~/.emacs.d/vendor")

;; disable menu bar
(menu-bar-mode 0)

;; disable tool bar
(tool-bar-mode -1)

;; transparent background
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
;; (set-face-attribute 'default nil :background "black" :foreground "white")

;; column / line number mode
(line-number-mode 1)
(column-number-mode 1)

;; hightlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")

;; word wrapping
(setq-default word-wrap t)
(setq-default fill-column 80)

;; word wrapping indicator
(setq-default whitespace-line-column 80 whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; store backup files in .emacs.d/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; indention
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; move between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; theme
;; (set-frame-parameter nil 'background-mode 'dark)
;; (set-terminal-parameter nil 'background-mode 'dark)
;; (load-theme 'solarized t)
(load-theme 'material t)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; helm
(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; ggtags
(require 'ggtags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(add-hook 'js2-mode-hook 'ggtags-mode)
(add-hook 'go-mode-hook 'ggtags-mode)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; ggtags + helm
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t)

(require 'helm-gtags)

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode-hook 'helm-gtags-mode)
(add-hook 'go-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "C-l") (lambda () (interactive)
                                                      (eshell/clear)))))

;; eshell + helm
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map
              (kbd "M-p")
              'helm-eshell-history)))

;; speedbar
(setq speedbar-show-unknown-files t)
(setq speedbar-update-flag nil)

(setq sr-speedbar-auto-refresh nil)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-backends (delete 'company-semantic company-backends))

;; c/c++
(require 'cc-mode)

;; c/c++ + company
(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(define-key c-mode-map  [(tab)] 'indent-or-complete)
(define-key c++-mode-map  [(tab)] 'indent-or-complete)

;; (global-set-key (kbd "C-.") 'company-complete)

(add-to-list 'company-backends 'company-c-headers)

;; google c/c++ style guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(put 'erase-buffer 'disabled nil)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; npm install -g eslint babel-eslint eslint-plugin-react
;;

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (custom-set-variables
;;  '(js2-basic-offset 2)
;;  '(js2-bounce-indent-p t))

(add-hook 'js2-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(require 'sass-mode)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))

;;
;; https://pylint.readthedocs.io/en/latest/user_guide/ide-integration.html
;; pip3 install pylint
;;

;; configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; keymaps to navigate to the errors
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; to avoid having to mouse hover for the error message, these functions make
;; flymake error messages appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (toml-mode yaml-mode web-mode sr-speedbar sass-mode recentf-ext material-theme markdown-mode json-mode js2-mode helm-gtags go-mode ggtags flycheck exec-path-from-shell elpy dockerfile-mode dired-subtree company-c-headers color-theme-solarized better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'toml-mode)
