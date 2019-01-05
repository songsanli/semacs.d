(use-package editorconfig
  :hook (after-init . editorconfig-mode)
  :config
  (defun my-personal-code-style ()
    (interactive)
    (message "My personal code style!")
    (editorconfig-set-indentation "space" "2"))

  (defun my-setup-develop-environment ()
    (interactive)
    ;; Apply my setup only when editorconfig don't work
    (let ((file (editorconfig-core-get-nearest-editorconfig
                 default-directory)))
      (unless file
        (my-personal-code-style)))

    ;; Auto handle indentation by directory
    ;; (let ((proj-dir (file-name-directory (buffer-file-name))))
    ;;   if hobby project path contains string "hobby-proj1"
    ;;   (if (string-match-p "hobby-proj1" proj-dir)
    ;;   (my-personal-code-style))

    ;;   if commericial project path contains string "commerical-proj"
    ;;   (if (string-match-p "commerical-proj" proj-dir)
    ;;       (my-office-code-style)))
    )

  ;; prog-mode-hook requires emacs24+
  (add-hook 'prog-mode-hook 'my-setup-develop-environment)
  ;; a few major-modes does NOT inherited from prog-mode
  (add-hook 'lua-mode-hook 'my-setup-develop-environment)
  (add-hook 'web-mode-hook 'my-setup-develop-environment))

(use-package prettier-js
  :after (js2-mode)
  :hook (js2-mode . prettier-js-mode))

(provide 'init-code-style)
