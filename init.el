;; Make startup style more concise
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Add global consts
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst samuel-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(when *is-a-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-k") 'kill-this-buffer))

;; Change the default font for the current frame, as well as future frames
(set-face-attribute 'default nil :font "Iosevka Term SS08-16" )
(set-frame-font "Iosevka Term SS08-16" nil t)
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font t charset (font-spec :family "Sarasa Term SC")))
;; specify font for all unicode characters
;; (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji" :size 13) nil 'prepend)
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Config *scratch* buffer
(setq initial-major-mode 'org-mode)
(setq-default initial-scratch-message
              (concat "# Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; Enable blinking cursor
(blink-cursor-mode 1)

;; Keep cursor position when page up/down
(setq scroll-preserve-screen-position 'always)

;; Stop startup screen
(setq inhibit-startup-screen t)

;; Config frame title
(setq-default frame-title-format
              '(buffer-file-name "%f" "%b"))

;; Create the savefile dir if it doesn't exist
(unless (file-exists-p samuel-savefile-dir)
  (make-directory samuel-savefile-dir))

;; Config desktop-save-mode
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

;; Restore histories and registers after saving
(setq-default history-length 1000)

;; Highlight pair when cursor on
(show-paren-mode 1)

;; Insert parenthesis by pair
(electric-pair-mode 1)

;; Delete the selection with a keypress
(delete-selection-mode 1)

;; Avoid open file in new frame
(setq ns-pop-up-frames nil)

;; Stop use of gui dialog
(setq use-dialog-box nil) 

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Config back up files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Fix blink when input Chinese
;; https://emacs-china.org/t/mac-gui-emacs/186/25
(setq redisplay-dont-pause nil)

;; Config mode line
(line-number-mode t)
(column-number-mode 1)
;; (size-indication-mode t)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)

;; Movement function: move cursor to punctuation
(defvar xah-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq xah-punctuation-regex "[\\!\?\"\.'#$%&*+,/:;<=>@^`|~]+")
(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))
(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

(defun rename-current-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "M-o") 'switch-to-previous-buffer)

;; Set up package system and use-package
(require 'package)
;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))
;; Set up package-archives
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-cn" (concat proto "://elpa.emacs-china.org/melpa/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    ;; (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives '("gnu-cn" . (concat proto "://elpa.emacs-china.org/gnu/")))))
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" samuel-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" samuel-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  :config
  (add-hook 'ibuffer-hook (lambda ()
			    (ibuffer-vc-set-filter-groups-by-vc-root)
			    (unless (eq ibuffer-sorting-mode 'filename/process)
			      (ibuffer-do-sort-by-filename/process))))
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))
(use-package ibuffer-vc
  :after ibuffer)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package dired
  :init
  (let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls)
    (setq dired-listing-switches "-laGh1v --group-directories-first")))
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.2))

(use-package smex)

(use-package counsel
  :after (smex)
  :bind (("s-e" . ivy-switch-buffer)
	 ("C-x b" . ivy-switch-buffer)
	 :map ivy-minibuffer-map
	 ("RET" . ivy-alt-done)
	 ("C-RET" . ivy-immediate-done))
  :hook (after-init . counsel-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-virtual-abbreviate 'fullpath))

(use-package minions
  :hook (after-init . minions-mode)
  :init
  (setq minions-mode-line-lighter "..."
        minions-direct '(flycheck-mode)))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package color-theme-sanityinc-tomorrow
  :init
  ;; Use natural title bar in macOS
  ;; (when *is-a-mac*
  ;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (when *is-a-mac*
    (set-mouse-color "white")))

(use-package projectile
  :bind (("s-p" . projectile-find-file))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil))

(use-package crux
  :bind (("s-j" . crux-top-join-line)
         ("C-k" . crux-smart-kill-line)
         ("s-D" . crux-duplicate-current-line-or-region)
         ("s-<backspace>" . crux-kill-line-backwards)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package avy
  :bind (("C-;" . avy-goto-char-timer))
  :config
  (setq avy-background t))

(use-package beacon
  :hook (after-init . beacon-mode)
  :init
  (setq beacon-size 10
        beacon-blink-duration 0.5
        beacon-blink-delay 0.5))

(use-package elpy)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :init
  ;; (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2"))))

(use-package magit
  :bind (("C-x g" . magit)))

(use-package fullframe
  :after (magit)
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package flycheck)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode))

(use-package prettier-js)

(use-package pug-mode)

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))

;; Use emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Handle custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
