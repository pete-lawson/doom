(setq doom-theme 'doom-one)

(setq doom-font (font-spec :size 20)
      doom-variable-pitch-font (font-spec :family "CMU Serif"))

(use-package! mixed-pitch
  ;; Enable in all text modes
  :hook (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 250))

(setq default-frame-alist '((undecorated . t)))

(setq display-line-numbers-type nil)

(after! org
  (setq org-hide-emphasis-markers t))

;; biblio
(after! citar
  (setq citar-bibliography '("~/Dropbox/JHU/references/references.bib")))

(setq org-directory "~/Dropbox/JHU/notes")

(after! org
  (setq org-roam-directory "~/Dropbox/JHU/notes")
  (setq org-roam-db-location "~/.roam/org-roam.db")
  (setq org-startup-with-latex-preview t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

;; Bind this to C-c n I

(after! org
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates) '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
  (map! :leader
        :desc "Insert node immediate"
        "n r I" #'org-roam-node-insert-immediate)
)

(after! org-roam
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n") :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+category: ${title}\n#+filetags: :project:") :unnarrowed t)
          ("c" "consult" plain "* Description\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n*"
           :if-new (file+head "consults/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+category: ${title}\n#+filetags: :consult:") :unnarrowed t)

          )))

(after! org
  (custom-theme-set-faces
   'user
   '(org-level-1 ((t (:inherit outline-1 :family "CMU Sans Serif Demi Condensed" :height 1.2))) t)
   '(org-level-2 ((t (:inherit outline-2 :family "CMU Sans Serif Demi Condensed"))) t)
   '(org-level-3 ((t (:inherit outline-3 :family "CMU Sans Serif Demi Condensed"))) t)
   '(org-level-4 ((t (:inherit outline-4 :family "CMU Sans Serif Demi Condensed"))) t)
   '(org-level-5 ((t (:inherit outline-5 :family "CMU Sans Serif Demi Condensed"))) t)
   '(org-level-6 ((t (:inherit outline-6 :family "CMU Sans Serif Demi Condensed"))) t)
   '(org-level-7 ((t (:inherit outline-7 :family "CMU Sans Serif Demi Condensed"))) t)))

(after! org
  (setq org-agenda-files '("~/Dropbox/JHU/notes/projects"
                           "~/Dropbox/JHU/notes/consults"
                           "~/Dropbox/JHU/notes/daily")))
