(map! :leader
    :desc "Kill buffer" "x" #'kill-current-buffer
    :desc "Switch buffer (Persp.)" "." #'persp-switch-to-buffer
    :desc "Switch buffer" ">" #'switch-to-buffer
    :desc "Expand region" "e" #'er/expand-region
    :desc "Increase font size" "+" #'doom/increase-font-size
    :desc "Decrease font size" "-" #'doom/decrease-font-size
    :desc "Next buffer" "k" #'next-buffer
    :desc "Previous buffer" "j" #'previous-buffer
    :desc "Search buffer" "/" #'+default/search-buffer
    :desc "Search buffer" "ps" #'+default/search-project
    :desc "Switch window" "TAB" #'ace-window
    :desc "Open URL" "gx" #'browse-url-at-point)

(define-key evil-normal-state-map (kbd "H") 'centaur-tabs-backward)
(define-key evil-normal-state-map (kbd "L") 'centaur-tabs-forward)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

(define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)

;; `gruvbox-material' contrast and palette options
;; (setq doom-gruvbox-material-background  "medium"  ; or hard (defaults to soft)
;;      doom-gruvbox-material-palette     "original") ; or original (defaults to material)

;; set `doom-theme'
(setq doom-theme 'doom-gruvbox-material) ; dark variant

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20 :height 1.0 :weight 'normal))
(setq truncate-lines 'nil)

(setq highlight-indent-guides-auto-odd-face-perc 50)
(setq highlight-indent-guides-auto-even-face-perc 50)
(setq highlight-indent-guides-auto-character-face-perc 50)
;; (setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'top)

(setq display-line-numbers-type 'relative)

(custom-theme-set-faces!
    'doom-dracula
        '(org-level-4 :inherit outline-4 :height 1.0)
        '(org-level-3 :inherit outline-3 :height 1.1)
        '(org-level-2 :inherit outline-2 :height 1.3)
        '(org-level-1 :inherit outline-1 :height 1.5)
        '(org-document-title :height 1.5 :underline nil))

(setq org-hide-emphasis-markers t)

(use-package! org-appear
  :defer t
  :config
  (setq org-appear-autolinks t))
(add-hook! org-mode :append #'org-appear-mode)

(use-package! company
  :defer t
  :custom-face
  (company-tooltip
   ((t (:family "JetBrainsMono Nerd Font" :height 1.0))))
  :config
        (define-key evil-insert-state-map (kbd "C-l") 'company-complete)
        (setq company-idle-delay 0.1)
        (setq company-tooltip-idle-delay 0.1)
        (add-to-list '+lsp-company-backends 'company-files)
        (set-company-backend! 'org-mode 'company-files 'company-capf)
)

(map! :localleader
      :map LaTeX-mode-map
      :desc "Master"    "m" 'TeX-command-master)

(setq TeX-engine-alist '((default
                          "Tectonic"
                          "tectonic -X compile -f plain %T"
                          "tectonic -X watch"
                          nil)))

(setq LaTeX-command-style '(("" "%(latex)")))

(setq TeX-process-asynchronous t
      TeX-check-TeX nil
      TeX-engine 'default)

(add-hook! LaTeX-mode
(let ((tex-list (assoc "TeX" TeX-command-list))
      (latex-list (assoc "LaTeX" TeX-command-list)))
  (setf (cadr tex-list) "%(tex)"
        (cadr latex-list) "%l")))

(use-package! typopunct
  :defer t
  :config
  (typopunct-change-language 'french t))

(setq langtool-language-tool-jar (concat (getenv "HOME") "/bin/LanguageTool-5.2/languagetool-commandline.jar"))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

(use-package! yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "-i --simple-prompt")

(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))

(defvar python--pdb-breakpoint-string "breakpoint()")
(defun python-add-breakpoint ()
"Inserts a python breakpoint using `pdb'"
    (interactive)
    (back-to-indentation)
    ;; this preserves the correct indentation in case the line above
    ;; point is a nested block
    (split-line)
    (insert python--pdb-breakpoint-string)
    (python-set-debug-highlight))


(defun python-set-debug-highlight ()
    (interactive)
    (highlight-lines-matching-regexp "breakpoint[.]?" 'hi-red-b))

(defun python-add-debug-highlight ()
    "Adds a highlighter for use by `python--pdb-breakpoint-string'"
    (interactive)
    (python-set-debug-highlight))
    (add-hook 'python-mode-hook 'python-add-debug-highlight)

(map! :localleader
      :map python-mode-map
      :desc "Insert breakpoint"    "d" 'python-add-breakpoint)

(use-package! numpydoc
  :defer t
  :init
  (setq numpydoc-insertion-style 'yas))

(map! :localleader
      :map python-mode-map
      :desc "Auto docstring"  "s" 'numpydoc-generate)

(after! lsp-mode
  ;; Ensure python-mode uses lsp
  (add-hook 'python-mode-hook #'lsp)

  ;; Prevent doom from disabling one or the other
  (setq lsp-disabled-clients '())

  ;; Register ruff-lsp as an additional client
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff lsp"))
    :major-modes '(python-mode)
    :server-id 'ruff-lsp
    :add-on? t))) ;; <-- important! allows both Pyright + Ruff

;; (after! python
;;   (setq +format-with-lsp nil) ;; disable LSP formatting if any
;;   (add-hook 'before-save-hook
;;             (lambda ()
;;               (when (eq major-mode 'python-mode)
;;                 (call-process "ruff" nil nil nil "format" (buffer-file-name)))
;;               nil)))

(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'dockerfile-mode))

(setq lsp-go-use-gofumpt t)

(map! :leader
      (:prefix-map ("r" . "Harpoon")
       (:desc "Menu" "m" #'harpoon-quick-menu-hydra
        :desc "Add file" "a" #'harpoon-add-file
        :desc "Edit file" "r" #'harpoon-toggle-file
        :desc "Clear" "c" 'harpoon-clear)))

(map! :leader "1" 'harpoon-go-to-1)
(map! :leader "2" 'harpoon-go-to-2)
(map! :leader "3" 'harpoon-go-to-3)
(map! :leader "4" 'harpoon-go-to-4)
(map! :leader "5" 'harpoon-go-to-5)
(map! :leader "6" 'harpoon-go-to-6)
(map! :leader "7" 'harpoon-go-to-7)
(map! :leader "8" 'harpoon-go-to-8)
(map! :leader "9" 'harpoon-go-to-9)
(map! :leader "0" 'harpoon-go-to-10)

(setq org-export-use-babel t)
(setq org-directory "~/org/")
(load-library "ox-reveal")
(setq org-latex-pdf-process '("tectonic %f"))
(setq org-export-in-background t)

(add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! ox-latex
    (add-to-list 'org-latex-classes
                '("koma-article" "\\documentclass{scrartcl}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                '("koma-article-fr" "\\documentclass[french]{scrartcl}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                '("memoir-fr"
                "\\documentclass[a4paper,11pt,titlepage, twoside]{memoir}
                    \\usepackage[utf8]{inputenc}
                    \\usepackage[T1]{fontenc}
                    \\usepackage{fixltx2e}
                    \\usepackage{hyperref}
                    \\usepackage{mathpazo}
                    \\usepackage{color}
                    \\usepackage{enumerate}
                    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                    \\tolerance=1000
                    \\linespread{1.1}
                    \\hypersetup{pdfborder=0 0 0}"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                '("TMI"
                "\\documentclass[journal, web, twoside]{ieeecolor}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

)

(use-package! ox-awesomecv
  :after org)

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(use-package! org-ref
  :defer t
    :init
        (setq org-ref-bibliography-notes "~/org/paper-notes/paper-notes.org"
            org-ref-default-bibliography "~/org/refs.bib"
            bibtex-completion-bibliography org-ref-default-bibliography
            bibtex-completion-notes-path "~/org/paper-notes/paper-notes.org"
            bibtex-completion-pdf-open-function
                (lambda (fpath)
                (call-process "zathura" nil 0 nil fpath))))

(map! :localleader
      :map org-mode-map
      :desc "Insert citation" "c" 'org-cite-insert)

(setq org-capture-templates
  (quote
   (("t" "todo" entry
     (file+headline "~/org/todo.org" "Tasks")
     "* TODO %U %?\n\n"
     :empty-lines-after 1)
    ("n" "note" entry
     (file+headline "~/org/notes.org" "Inbox")
     "* %U %? \n\n"
     :empty-lines-after 1))))

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

(after! iflipb
  (setq iflipb-ignore-buffers "$^")
  )

(setq +tree-sitter-hl-enabled-modes t)

(setq confirm-kill-emacs nil)

(defun tmux-sessionizer ()
  (interactive)
  (with-current-buffer
      (shell-command "tmux neww tmux-sessionizer")))
(map! :nvi "C-f" #'tmux-sessionizer)

(setq ll/notmuch-default-query "tag:inbox AND not tag:deleted AND date:3months..")
(setq notmuch-saved-searches
    `((:name "inbox"
       :query ,ll/notmuch-default-query
       :key "i"
       :search-type: tree)
      (:name "sent"
       :query "tag:sent AND not tag:deleted")
      (:name "gmail"
       :query "tag:Gmail/Inbox AND not tag:deleted"
       :key "g"
       :search-type: tree)
      (:name "personal"
       :query "tag:Gandi/Inbox AND not tag:deleted"
       :key "p"
       :search-type tree)))
(setq +notmuch-mail-folder "~/.mail")
(setq +notmuch-sync-backend 'mbsync)
(setq +notmuch-home-function (lambda () (notmuch-search ll/notmuch-default-query)))

(setq notmuch-always-prompt-for-sender t)
(setq user-full-name "Laurent Lejeune")
(setq user-mail-address "me@lejeunel.org")
(setq mail-host-address "lejeunel.org")

(after! notmuch
  (setq notmuch-show-part-action-list
        '(("Open with xdg-open" . (lambda (part) (start-process "xdg-open" nil "xdg-open" (notmuch-show-get-filename part))))
          ("View" . notmuch-show-view-part)
          ("Save" . notmuch-show-save-part))))

(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(setq message-make-forward-subject-function 'message-forward-subject-fwd)

(defun my-notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject"
  (when (and (null (message-field-value "Subject"))
             (not (y-or-n-p "Subject is empty, send anyway? ")))
    (error "Sending message cancelled: empty subject.")))
(add-hook 'message-send-hook 'my-notmuch-mua-empty-subject-check)
