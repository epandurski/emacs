(deftheme my-light
  "Created 2023-02-07.")

(custom-theme-set-variables
 'my-light
 '(global-hl-line-mode t))

(custom-theme-set-faces
 'my-light
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 128 :width normal))))
 '(hl-line ((t (:inherit highlight :extend t :background "gray93"))))
 '(line-number-current-line ((t (:inherit line-number :background "gray84"))))
 '(region ((t (:background "LightGoldenrod2")))))

(provide-theme 'my-light)
