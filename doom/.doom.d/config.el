;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Luciano Laratelli"
      user-mail-address "luciano@laratel.li"
      default-input-method "TeX"
      )

(setq doom-font (font-spec :family "Fira Code Retina" :size 14)
      doom-unicode-font (font-spec :family "JuliaMono Medium" :size 14))

(setq doom-theme 'doom-dracula)

(setq org-directory "~/Dropbox/org/")
(setq org-roam-directory "~/Dropbox/org/roam")
(setq org-journal-date-prefix "* ")
(setq org-journal-file-format "Journal %Y-%m.org")
(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-type 'monthly)

(setq journals (make-hash-table :test 'equal))

(setq journals '(("work" "~/Dropbox/org/work" "n j w")
                 ("personal" "~/Dropbox/org/journal" "n j j")))


(cdr (assoc "work" journals))
(cdr (assoc "personal" journals))

(add-to-list 'safe-local-variable-values
             '(org-journal-dir . "~/Dropbox/org/work")
             )

(unbind-key "C-h")

(map! :after ivy
     :map ivy-minibuffer-map
       "DEL" #'ivy-backward-delete-char)

(defun work-journal-new-entry ()
  (interactive)
  (setq org-journal-dir "~/Dropbox/org/work")
  (call-interactively #'org-journal-new-entry)
  )

(add-to-list 'safe-local-variable-values
             '(org-journal-dir . "~/Dropbox/org/journal")
             )

(defun my-journal-new-entry ()
  (interactive)
  (setq org-journal-dir "~/Dropbox/org/journal")
  (call-interactively #'org-journal-new-entry)
  )


(map! :leader
      "n j j" #'my-journal-new-entry
      "n j w" #'work-journal-new-entry
      )

(setq doom-localleader-key ",")


(map! :after org
      :map org-mode-map
      :leader
      "a c r" #'jupyter-org-clear-all-results
      :localleader
      "s p p" #'org-priority
      "s p u" #'org-priority-up
      "s p d" #'org-priority-down
      "j k"   #'org-previous-visible-heading
      "j j"   #'org-next-visible-heading
      "j i"   #'org-insert-heading
      )

(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; (display-time-mode 1)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)

(map! :leader
      "w /" #'evil-window-vsplit
      "w -" #'evil-window-split)

(require `evil-surround)

(add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode 1)))

(map! "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right
      )

;; unmap key bindings I use in i3
(map! "M-C-c" nil)
(map! "M-C-d" nil)
(map! "M-C-l" nil)
(map! "M-C-r" nil)
(map! "M-C-s" nil)
(map! "M-C-q" nil)
(map! "M-C-t" nil)

(map! "M-r" 'raise-sexp)
(map! "M-f" 'sp-splice-sexp-killing-forward)
(map! "M-b" 'sp-splice-sexp-killing-backward)

;; I could use bind-keys* here, but that overwrites the keymap *everywhere*,
;; including in e.g. an ivy completion buffer. This does the thing but not at
;; that top-most level.

(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'normal magit-mode-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'normal magit-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'normal magit-mode-map (kbd "C-l") 'evil-window-right)
  (evil-define-key 'visual magit-mode-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'visual magit-mode-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'visual magit-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'visual magit-mode-map (kbd "C-l") 'evil-window-right)
  )

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'normal org-mode-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'normal org-mode-map (kbd "C-l") 'evil-window-right)
  (evil-define-key 'visual org-mode-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'visual org-mode-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'visual org-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'visual org-mode-map (kbd "C-l") 'evil-window-right)
  )

(map! :after vterm
      :map vterm-mode-map
      "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right
      )

(setq company-idle-delay 0.01
      company-minimum-prefix-length 2)

(map! :leader
      "r" #'rtags-find-symbol-at-point
      "f j" `open-junk-file)

(defun eval-region-or-buffer ()
  (interactive)
  (let ((debug-on-error t))
    (cond
     (mark-active
      (call-interactively 'eval-region)
      (message "Region evaluated!")
      (setq deactivate-mark t))
     (t
      (eval-buffer)
      (message "Buffer evaluated!")))))

(map! :leader
      "e" #'eval-region-or-buffer)


(defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(use-package! bison-mode
  :defer t)

(use-package! impatient-mode
  :defer t)

(use-package! open-junk-file
  :custom
  (open-junk-file-format "~/Dropbox/junk/%Y/%m/%d-%H%M%S."))

(use-package! make-mode
  :defer t)

(use-package! flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

(defun my/fix-file-formatting-and-tabs (directory extension)
  ;; inspired by https://stackoverflow.com/a/55302689
  ;; and stolen from https://emacs.stackexchange.com/a/34222
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "File extension: ")))
  (dolist (file (directory-files-recursively directory (concat "^[a-z0-9A-Z]?+\\" extension "$")))
    (find-file file)
    (format-all-buffer (point-min)(point-max))
    (untabify(point-min)(point-max))
    (save-buffer)
    (kill-buffer nil)))

(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

(defun random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-string (n)
  "Generate a slug of n random alphanumeric characters.
Inefficient implementation; don't use for large n."
                                        ; thanks to https://stackoverflow.com/a/60790863/5692730
  (if (= 0 n)
      ""
    (concat (random-alnum) (random-string (1- n)))))


(defun my/org-clj-template ()
  "Make a template at point."
  (let ((section-name (random-string 5)))
    (save-excursion
      (insert "#+name: " section-name "\n")
      (insert "#+begin_src clojure :exports code\n\n")
      (insert "#+end_src\n")
      (insert "\\Rightarrow call_" section-name"[:exports results]()\n\n")
      )
    (forward-line 2)
    (evil-insert)
    )
  )


(setq backup-directory-alist `(("." . "~/.BACKUPS")))
(setq backup-by-copying t)


(setq rainbow-delimiters-max-face-count nil)

(setq org-pretty-entities t)

(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . bnf-mode))

(after! bnf-mode (add-hook 'bnf-mode-hook
          (lambda () (progn
                       (setq comment-start "(* ")
                       (setq comment-end " *)")
                       (setq comment-add 0)))))



;;Allow bold, italics, etc in middle of word
(after! org
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  )

;;https://stackoverflow.com/a/30697761/5692730
(defun my/sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))


(defun my/clear-subtree ()
  (interactive)
  (org-mark-subtree) ;; mark the current subtree
  (forward-line) ;; move point forward, so the headline isn't in the region
  (delete-region (region-beginning) (region-end)) ;; delete the rest
  )


(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-babel-clojure-backend 'cider)

(setq org-latex-caption-above nil)

(smartparens-global-strict-mode 1)

(add-hook! clojure-mode #'evil-cleverparens-mode)
