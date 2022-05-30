;; maximize on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; disable exit confirmation
(setq confirm-kill-emacs nil)

;; set font
(setq doom-font (font-spec :family "Input Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Corbert" :size 17))

;; overall theme
(setq doom-theme 'doom-one)
;; treemacs theme
(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-treemacs-enable-variable-pitch nil)
;; log in splash image
(setq fancy-splash-image "~/.doom.d/splash/splash.png")

;; default to absolute
(setq display-line-numbers-type t)

;; set project root
(setq projectile-project-search-path
      '("~/Documents/Learning/programming-rust"))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; disable minibuffer hover doc
  (lsp-eldoc-enable-hover nil)
  ;; ignore duplicate sideline diagnostics
  (lsp-ui-sideline-ignore-duplicate t)
  ;; show hover info on sideline
  (lsp-ui-sideline-show-hover t))

(after! rustic
  (setq rustic-lsp-server 'rls))

;; set org mode root
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org"))

(defun ays/org-mode-hook-setup ()
  ;; enable variable pitch for text
  (variable-pitch-mode t)
  ;; disable line numbers
  (display-line-numbers-mode -1)
  ;; disable company autocomplete
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))

(defun ays/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Corbert" :height (cdr face) :weight 'bold)))

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(defun ays/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . ays/org-mode-visual-fill))

(use-package! org
  ;; apply hooks
  :hook (org-mode . ays/org-mode-hook-setup)
  :config
  ;; replace ... when headings are folded
  (setq org-ellipsis " ▾")

  ;; agenda logging
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)

  ;; todo
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "INPROG(i)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; tag
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)))

  ;; captures
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("tt" "Project" entry (file+olp "~/org/tasks.org" "Inbox")
           "* PLAN %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)))

  ;; fonts
  (ays/org-font-setup))

(org-babel-do-load-languages
  'org-babel-load-languages '((elisp . t)
                              (python . t)
                              (shell . t)
                              (sql . t)
                              (js . t)))
;; add conf to source langs
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))

(defun ays/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.doom.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ays/org-babel-tangle-config)))

(map! :leader
      :desc "Query replace"
      "c r" #'anzu-query-replace)

(map! :leader
      :desc "Query replace regex"
      "c R" #'anzu-query-replace-regexp)

(map! :leader
      :desc "Search project rg"
      "s p" #'counsel-projectile-rg)
