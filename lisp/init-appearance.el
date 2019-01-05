(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [8677 9] [92 9]) ; tab
          ))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-side-window-max-height 0.2))

(use-package color-theme-sanityinc-tomorrow
  :init
  ;; Use natural title bar in macOS
  (when *is-a-mac*
    ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (set-mouse-color "white")))

(use-package beacon
  :hook (after-init . beacon-mode)
  :init
  (setq beacon-size 10
        beacon-blink-duration 0.3
        beacon-blink-delay 0.3))

(use-package smart-mode-line
  :hook (after-init . sml/setup)
  :init
  (setq sml/theme nil)
  (setq sml/no-confirm-load-theme t)
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/Developer/" ":DEV:")))

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode))

(use-package minions
  :hook (after-init . minions-mode)
  :init
  (setq minions-mode-line-lighter "..."
        minions-direct '(flycheck-mode)))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

;; Line wrap will align indentation
(use-package adaptive-wrap
  :config
  (defun turn-on-adaptive-wrap-prefix-mode ()
    "Turns on adaptive-wrap-prefix-mode."
    (interactive)
    (adaptive-wrap-prefix-mode 1))
  (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
    adaptive-wrap-prefix-mode
    turn-on-adaptive-wrap-prefix-mode)
  (global-adaptive-wrap-prefix-mode 1))

(provide 'init-appearance)
