;; -*- lexical-binding: t -*-
;; Make startup style more concise
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add global consts
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst samuel-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; reduce the frequency of garbage collection by making it happen on
;; each 20MB of allocated data (the default is on every 0.76MB)
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'init-default-keybindings)

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
(setq-default indent-tabs-mode nil ;; don't use tabs to indent
              tab-width 2)         ;; but maintain correct appearance

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

;; Display empty lines
(setq-default indicate-empty-lines t)

;; Improve handling of long lines
(setq-default bidi-display-reordering nil)

(setq-default fill-column 80)

;; Stop creating #autosave# files
(setq auto-save-default nil)

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
(if (fboundp 'mode-line-other-buffer)
    (global-set-key (kbd "M-o") 'mode-line-other-buffer)
  (global-set-key (kbd "M-o") 'switch-to-previous-buffer))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

;; Change built-in major mode name in mode line
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))
(add-hook 'package-menu-mode-hook (lambda () (setq mode-name "PM")))

;; Calls (package-initialize)
(require 'init-package-system)

;; Handle custom-file
;; Note: I put load of custom-file after init-package-system to ensure that
;; package-selected-packages in custom-file will be updated when use-package
;; install a new package.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package savehist
  :ensure nil
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
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" samuel-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(require 'init-ibuffer)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package dired
  :ensure nil
  :init
  (when (executable-find "gls")
    (setq insert-directory-program "gls" dired-use-ls-dired t)
    (setq dired-listing-switches "-laGh1v --group-directories-first"))
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook
    (lambda ()
      (define-key dired-mode-map (kbd "^")
        (lambda () (interactive) (find-alternate-file ".."))))))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; don't muck with special buffers
        uniquify-ignore-buffers-re "^\\*"))

(use-package smex)

(use-package counsel
  :after (smex)
  :bind (("s-e" . ivy-switch-buffer)
	       ("C-x b" . ivy-switch-buffer)
         ("s-p" . counsel-git)
         ("s-x" . counsel-M-x)
	       :map ivy-minibuffer-map
	       ("RET" . ivy-alt-done)
	       ("<C-return>" . ivy-immediate-done))
  :hook (after-init . counsel-mode)
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format ""
        ivy-virtual-abbreviate 'fullpath
        ivy-initial-inputs-alist '((Man-completion-table . "^")
                                   (woman . "^"))
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."))

(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package projectile
  ;; :bind (("s-p" . projectile-find-file))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-keymap-prefix (kbd "C-x p")
        projectile-completion-system 'ivy
        projectile-enable-caching nil))

(use-package crux
  :bind (("s-j" . crux-top-join-line)
         ("C-k" . crux-smart-kill-line)
         ("s-D" . crux-duplicate-current-line-or-region)
         ("s-<backspace>" . crux-kill-line-backwards)))

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package avy
  :bind (("C-;" . avy-goto-char-timer))
  :config
  (setq avy-background nil))

(use-package ace-jump-buffer
  :after (avy))

(require 'init-appearance)

(use-package ace-window
  :bind("C-x o" . ace-window))

(use-package magit
  :bind (("C-x g" . magit))
  :hook (magit-process-mode . goto-address-mode)
  :init
  (setq json-reformat:indent-width 2))

(use-package fullframe
  :after (magit)
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil))

(use-package flycheck)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package rg
  :bind ("s-F" . rg-project)
  :init
  (setq rg-group-result t)
  :config
  (rg-define-toggle "--context 3" "m"))

(use-package css-mode
  :ensure nil
  :init
  (setq-default css-indent-offset 2))

(require 'init-javascript)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package pug-mode)

(use-package fish-mode)

(use-package emmet-mode
  :hook js2-mode)

(use-package web-mode)

;; objed use normal key to trigger itself map, such as C-n and
;; C-p. I think this package is based on this assumption: When you key
;; a move-related command, in most case you will key another
;; move-related command instead of insert-realted command. I want to
;; give it a try.
;;
;; Things I feel uncomfortable:
;; - C-e will change mode to objed-mode, which makes me feel unconvenient
;;   because I prefer insert chars after C-e
(use-package objed)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Typescript
(use-package tide
  :config
  (add-hook 'typescript-mode-hook (lambda ()
                                    (setq mode-name "TypeScript")
                                    (tide-setup)
                                    (tide-hl-identifier-mode +1))))

(use-package yaml-mode)

(require 'init-c-sharp)

(require 'init-code-style)

;; Use emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
