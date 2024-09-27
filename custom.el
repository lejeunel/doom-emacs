(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" default))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'awesomecv "lejeune-vita.tex"))
      :append :local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'awesomecv "zamboni-vita.tex"))
      :append :local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'awesomecv "src/lejeune-vita.tex"))
      :append :local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'awesomecv "src/zamboni-vita.tex"))
      :append :local)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-export-to-file 'awesomecv "src/zamboni-resume.tex"))
      :append :local))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
