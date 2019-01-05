(setq-default js-switch-indent-offset 2)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :init
  ;; Turn off js2 mode errors & warnings
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  ;; Config indenation
  (setq js-indent-align-list-continuation nil
        js2-bounce-indent-p t)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2"))))

(use-package rjsx-mode
  :after (js2-mode)
  :hook (js2-mode . rjsx-minor-mode))

(use-package js2-refactor
  :after (js2-mode)
  :hook (js2-mode . js2-refactor-mode))

(provide 'init-javascript)
