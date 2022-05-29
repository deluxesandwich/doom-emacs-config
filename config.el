;; maximize on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; disable exit confirmation
(setq confirm-kill-emacs nil)

;; set splash image
(setq fancy-splash-image "splash/I-am-doom.png")

;; set font
(setq doom-font (font-spec :family "Input Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 17))

;; set theme
(setq doom-theme 'doom-one)
(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; show line number
(setq display-line-numbers-type t)

;; set org mode root
(setq org-directory "~/org/")

;; set project root
(setq projectile-project-search-path '("~/the-rust-book", "~/.doom.d"))

;; rust
(after! rustic
  (setq rustic-lsp-server 'rls))

;; org mode
(defun ays/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (mixed-pitch-mode 1))

(defun ays/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :inherit 'variable-pitch :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil            :inherit 'fixed-pitch :foreground nil)
  (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil            :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil         :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-list-dt nil          :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-todo nil             :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-done nil             :inherit '(shadow fixed-pitch))
  ;; Enlarge title
  (set-face-attribute 'org-document-title nil   :inherit 'variable-pitch :height 2.0))

(use-package! org
  :hook (org-mode . ays/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        display-line-numbers-type nil)
  (ays/org-font-setup))

;; set bullet style for headings
(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; add padding and center for a document style editing
(defun ays/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . ays/org-mode-visual-fill))
