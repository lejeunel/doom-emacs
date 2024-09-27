(map! :leader
    :desc "Kill buffer" "x" #'kill-this-buffer
    :desc "Switch buffer (Persp.)" "." #'persp-switch-to-buffer
    :desc "Switch buffer" ">" #'switch-to-buffer
    :desc "Expand region" "e" #'er/expand-region
    :desc "Increase font size" "+" #'doom/increase-font-size
    :desc "Decrease font size" "-" #'doom/decrease-font-size
    :desc "Next buffer" "k" #'iflipb-next-buffer
    :desc "Previous buffer" "j" #'iflipb-previous-buffer
    :desc "Search buffer" "/" #'+default/search-buffer
    :desc "Search buffer" "ps" #'+default/search-project
    :desc "Switch window" "TAB" #'ace-window)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

(define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)

(setq doom-theme 'doom-gruvbox)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 22 :height 1.0 :weight 'normal))

(setq highlight-indent-guides-auto-odd-face-perc 50)
(setq highlight-indent-guides-auto-even-face-perc 50)
(setq highlight-indent-guides-auto-character-face-perc 50)
(setq highlight-indent-guides-method 'character)
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

(setq flycheck-python-ruff-executable "ruff")
(add-hook 'python-mode-hook
  (lambda ()
    (setq flycheck-checker 'python-ruff)))

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

(defun tmux-sessionizer ()
  (interactive)
  (with-current-buffer
      (shell-command "tmux neww tmux-sessionizer")))
(map! :nvi "C-f" #'tmux-sessionizer)
