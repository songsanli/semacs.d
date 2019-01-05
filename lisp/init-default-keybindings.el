(when *is-a-mac*
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-b") 'bury-buffer)
  (global-set-key (kbd "s-r") 'query-replace-regexp)
  (global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)
  (define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill))

(provide 'init-default-keybindings)
