
# Table of Contents

1.  [Personal Information](#org86eae3e)
2.  [Acknowledgements](#org3ce7ba5)
3.  [Theme and Appearance](#org3138e55)
    1.  [Font](#org356ecfe)
    2.  [Theme](#org1d80713)
4.  [Org](#org88cfc8b)
    1.  [General](#org8b45662)
        1.  [Definitions](#org51a8ae8)
        2.  [Keybinds](#orgce444cc)
        3.  [Hooks](#org7924c5e)
    2.  [Org Journal](#orgad7e15b)
        1.  [Definitions](#org0f1f676)
        2.  [Keybinds](#org00c35c4)
    3.  [Org Roam](#org52755d5)
        1.  [Definitions](#org27ab86c)
5.  [Non-Org Programming Languages](#org295c8d1)
    1.  [Clojure](#org6ed4f0d)
6.  [Miscellaneous](#org6933372)
    1.  [macOS specific commands](#org75dbc6d)
    2.  [emacs internal stuff](#org3ad2e6d)
    3.  [keybinds](#orgc978c94)
    4.  [definitions](#org406c42a)
7.  [Navigation](#org9ff8c33)
8.  [Top-level keybindings](#org58b05af)
9.  [Stolen functions (the Luciano namespace)](#org7680b0e)
10. [Archive](#orgefacf90)

This is my configuration for the [DOOM emacs](https://github.com/hlissner/doom-emacs) framework, specifically for my macOS
device(s). I should note that I&rsquo;ve optimized my keybindings for my keyboard, the
[advantage2](https://kinesis-ergo.com/shop/advantage2/) from Kinesis. I rest my thumb on SPC and backSPC and have easy access
to modifier keys from that position as well. I also have Caps Lock remapped to
ESC everywhere; this is a holdover from when I used vim full-time, but it serves
me well with EVIL. Since I have CTRL so easily accessible, I&rsquo;m not at risk for
Emacs pinky!

    ;;; config.el -*- lexical-binding: t; -*-


<a id="org86eae3e"></a>

# Personal Information

    (setq user-full-name "Luciano Laratelli"
          user-mail-address "luciano@laratel.li")


<a id="org3ce7ba5"></a>

# Acknowledgements

I mention these folks throughout these folks throughout my config; they&rsquo;ve
helped me develop it either by teaching me what they knew, helping me write
elisp, or by sharing their configs for anyone to steal. Thanks guys!

-   [Elijah](https://github.com/djeis97)
-   [Kiran](https://github.com/kiranshila)
-   [Dominik](https://github.com/djshorty)


<a id="org3138e55"></a>

# Theme and Appearance


<a id="org356ecfe"></a>

## Font

Setting the size explicitly here is important, because it prevents a bug in
`doom-big-font-mode` that caused the size of text to only ever increase, instead
of the intended behavior of switching between normal and regular font size. This
combination produces nice ligatures in a great monospace font, and I have my
friends Kiran and Elijah to thank for it.

    (setq doom-font (font-spec :family "PragmataPro for Powerline" :size 14)
          doom-unicode-font (font-spec :family "JuliaMono Medium" :size 14))


<a id="org1d80713"></a>

## Theme

I have used quite a few others in the past; this is my current daily driver.

    (setq doom-theme 'doom-one)


<a id="org88cfc8b"></a>

# Org


<a id="org8b45662"></a>

## General


<a id="org51a8ae8"></a>

### Definitions

Some general org stuff. My laptop is my work computer, so I use a different org
directory on this machine.

    (setq org-directory "~/Dropbox/org/work"
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-edit-src-content-indentation 0
          org-pretty-entities t
          org-babel-clojure-backend 'cider)
    
    (setq org-duration-format 'h:mm)

This next bit handles latex exporting and bibtex stuff; parts of it have been
ripped from Kiran.

    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process (list "latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f")
          org-latex-caption-above nil)
    
    (setq-default
     reftex-default-bibliography '("~/Dropbox/bibtex/library.bib")
     ;; org-ref-default-bibliography '("~/Dropbox/bibtex/library.bib")
     bibtex-completion-bibliography "~/Dropbox/bibtex/library.bib"
     bibtex-completion-library-path "~/Dropbox/bibtex/")

    ;; (use-package! org-ref
    ;;   :after org
    ;;   :config
    ;;   (setq org-ref-completion-library 'org-ref-ivy-cite))

A handy function to

    (defun my/stamp-now ()
        (let ((current-prefix-arg '(2)))
          (call-interactively 'org-time-stamp)))


<a id="orgce444cc"></a>

### Keybinds

    (map! :after org
          :map org-mode-map
          "s-RET" #'+org/insert-item-below
          "M-RET" #'+org/insert-item-below
          :leader
          ; "a c r" #'jupyter-org-clear-all-results
          "RET" #'+org/insert-item-below
          "k" #'org-previous-visible-heading
          "j" #'org-next-visible-heading
          :localleader
          "s p p" #'org-priority
          "s p u" #'org-priority-up
          "s p d" #'org-priority-down
          "j k"   #'outline-up-heading
          "d n" #'my/stamp-now
          )


<a id="org7924c5e"></a>

### Hooks

Each of my journal files starts with an org clock report. Every time I clock
out, this updates my report.

    (add-hook 'org-clock-out-hook  (lambda ()
                                     (save-excursion
                                       (evil-goto-first-line)
                                       (evil-next-line)
                                       (org-clock-report)
                                       )))


<a id="orgad7e15b"></a>

## Org Journal


<a id="org0f1f676"></a>

### Definitions

    (setq org-journal-date-format "%A, %d %B %Y")
    (setq org-journal-date-prefix "* ")
    (setq org-journal-dir "~/Dropbox/org/work/journal")
    (setq org-journal-file-format "%Y/%U_%m-%d.org")
    (setq org-journal-file-type 'weekly)
    
    (defun org-journal-file-header-func (time)
      "Custom function to create journal header."
      (concat
       "#+BEGIN: clocktable :scope file :maxlevel 2\n#+END:\n"))
    
    (setq org-journal-file-header 'org-journal-file-header-func)


<a id="org00c35c4"></a>

### Keybinds

I clock in and out enough (specifically in my journal) that these are worth it
for me.

    (map! :after org-journal
     :map org-journal-mode-map
     :localleader
     "c" 'nil
     )
    
    (map! :after org-journal
          :map org-journal-mode-map
          :localleader
          (:prefix ("c" . "clock")
           "c" #'org-clock-cancel
           "l" #'+org/toggle-last-clock
           "i" #'org-clock-in
           "I" #'org-clock-in-last
           "o" #'org-clock-out
           "r" #'org-resolve-clocks
           "R" #'org-clock-report
           "t" #'org-evaluate-time-range
           )
    )


<a id="org52755d5"></a>

## Org Roam


<a id="org27ab86c"></a>

### Definitions

Roam stuff. I learned about capture templates from my friend Dom, and stole his
for my own use. :) I have a sketch at a directory structure here, which is
something org roam is sold as a cure for. These directories just help me keep a
*little* organized.

    (setq org-roam-directory "~/Dropbox/org/roam")
    
    (setq +org-roam-open-buffer-on-find-file nil)
    
    (after! org-roam
      (org-roam-db-build-cache ())
    
      (setq org-roam-capture-templates
            '(("d" "default" plain (function org-roam--capture-get-point)
               :file-name "general/%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n\n- tags ::  %?\n\n* "
               :unnarrowed t)
    
              ("a" "New Area" plain (function org-roam--capture-get-point)
               "%?"
               :file-name "%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n- tags :: [[file:~/Dropbox/org/roam/20210421-index.org.gpg][Indexes]]\n\n* "
               :unnarrowed t)
    
              ("e" "emacs")
              ("eo" "org mode")
              ("eoo" "general org" plain (function org-roam--capture-get-point)
               "%?"
               :file-name "emacs/org/%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n- tags :: [[file:~/Dropbox/org/roam/20210421-emacs.org.gpg][Emacs]]\n\n* "
               :unnarrowed t)
              ("eor" "org roam" plain (function org-roam--capture-get-point)
               "%?"
               :file-name "emacs/org/roam/%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n- tags :: [[file:~/Dropbox/org/roam/20210421-org_roam.org.gpg][org-roam]]\n\n* "
               :unnarrowed t)
    
              ("w" "work")
              ("wc" "cardhop" plain (function org-roam--capture-get-point)
               "%?"
               :file-name "flexibits/cardhop/%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n- tags :: [[file:~/Dropbox/org/roam/20210421-cardhop.org.gpg][cardhop]]\n\n* "
               :unnarrowed t)
              ("wf" "fantastical" plain (function org-roam--capture-get-point)
               "%?"
               :file-name "flexibits/fantastical/%<%Y%m%d>-${slug}"
               ;; added a double space at the end for the double-space insert link issue.
               :head "#+TITLE: ${title}\n#+Created: %u\n- tags :: [[file:~/Dropbox/org/roam/20210421-fantastical.org.gpg][fantastical]]\n\n* "
               :unnarrowed t)
              )
            )
    
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain #'org-roam-capture--get-point "%?"
               :file-name "website/%(url-host (url-generic-parse-url \"${ref}\"))-${slug}"
               :head "#+TITLE: ${title}\n#+Created: %u\n#+last_modified: %U\n#+roam_key: ${ref}\n- tags ::  "
               :unnarrowed t))))


<a id="org295c8d1"></a>

# Non-Org Programming Languages


<a id="org6ed4f0d"></a>

## Clojure

Need as many =\*parens-mode=s as possible

    ;; (add-hook! clojure-mode #'evil-cleverparens-mode)


<a id="org6933372"></a>

# Miscellaneous


<a id="org75dbc6d"></a>

## macOS specific commands

Unbind these two, for use in org mode:

    (map! "s-RET" nil
          "M-RET" nil)

Why use `global-unset-key` instead of `map!=ing to =nil`? Beats me!

    (global-unset-key (kbd "s-h"))
    (global-unset-key (kbd "s-j"))
    (global-unset-key (kbd "s-k"))
    (global-unset-key (kbd "s-l"))

`ls` is dumb on macOS:

    (when (string= system-type "darwin")
      (setq dired-use-ls-dired nil))


<a id="org3ad2e6d"></a>

## emacs internal stuff

    (setq backup-directory-alist `(("." . "~/.BACKUPS")))
    (setq backup-by-copying t)
    
    (setq auth-sources '("~/.authinfo.gpg"))
    (setq epa-pinentry-mode 'ask)
    (setenv "SSH_AUTH_SOCK" (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))


<a id="orgc978c94"></a>

## keybinds

This is Kiran&rsquo;s insane hack for having comma count as `SPC m` without remapping
`:localleader:` away from `SPC m`. Thanks Kiran!

    (map! :n "," (cmd! (push (cons t ?m) unread-command-events)
                       (push (cons t 32) unread-command-events)))


<a id="org406c42a"></a>

## definitions

    (require `evil-surround)
    
    (add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode 1)))
    
    (setq company-idle-delay 0.01
          company-minimum-prefix-length 2)
    
    
    (setq company-global-modes '(not org-mode))
    
    (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    
    
    (use-package! open-junk-file
      :custom
      (open-junk-file-format "~/Dropbox/junk/%Y/%m/%d-%H%M%S."))
    
    (after! tramp
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
      )

Smartparens! They&rsquo;re smart!

    (smartparens-global-strict-mode 1)
    
    (defun disable-smartparens ()
      (smartparens-mode 0))
    
    (add-hook 'org-mode-hook 'disable-smartparens)

    (setq epa-file-cache-passphrase-for-symmetric-encryption nil)


<a id="org9ff8c33"></a>

# Navigation

Easily split windows:

    (map! :leader
          "w /" #'evil-window-vsplit
          "w -" #'evil-window-split)

This next group of keybindings gives me easy, `hjkl` window navigation everywhere
that matter to me. I used to do this using `bind-keys*`, but the issue was that
that rebound `M-{hjkl}` *everywhere*, even in ivy completion buffers, etc. Doing
it this way makes it happen in the modes I care about. TODO: this needs to be
cleaned up and checked for functionality.

    (map! "s-h" #'evil-window-left
          "s-j" #'evil-window-down
          "s-k" #'evil-window-up
          "s-l" #'evil-window-right
          )
    (with-eval-after-load 'magit
      (evil-define-key 'normal magit-mode-map (kbd "M-h") 'evil-window-left)
      (evil-define-key 'normal magit-mode-map (kbd "M-j") 'evil-window-down)
      (evil-define-key 'normal magit-mode-map (kbd "M-k") 'evil-window-up)
      (evil-define-key 'normal magit-mode-map (kbd "M-l") 'evil-window-right)
      (evil-define-key 'visual magit-mode-map (kbd "M-h") 'evil-window-left)
      (evil-define-key 'visual magit-mode-map (kbd "M-j") 'evil-window-down)
      (evil-define-key 'visual magit-mode-map (kbd "M-k") 'evil-window-up)
      (evil-define-key 'visual magit-mode-map (kbd "M-l") 'evil-window-right)
      )
    
    (with-eval-after-load 'org
      (evil-define-key 'normal org-mode-map (kbd "M-h") 'evil-window-left)
      (evil-define-key 'normal org-mode-map (kbd "M-j") 'evil-window-down)
      (evil-define-key 'normal org-mode-map (kbd "M-k") 'evil-window-up)
      (evil-define-key 'normal org-mode-map (kbd "M-l") 'evil-window-right)
      (evil-define-key 'visual org-mode-map (kbd "M-h") 'evil-window-left)
      (evil-define-key 'visual org-mode-map (kbd "M-j") 'evil-window-down)
      (evil-define-key 'visual org-mode-map (kbd "M-k") 'evil-window-up)
      (evil-define-key 'visual org-mode-map (kbd "M-l") 'evil-window-right)
      )
    
    (map! :after vterm
          :map vterm-mode-map
          "s-h" #'evil-window-left
          "s-j" #'evil-window-down
          "s-k" #'evil-window-up
          "s-l" #'evil-window-right
          )

Easy workspace navigation:

    (map! :leader
          :nv "[" #'+workspace/switch-left
          :nv "]" #'+workspace/switch-right)


<a id="org58b05af"></a>

# Top-level keybindings

    (map! "M-r" 'raise-sexp
          "M-f" 'sp-splice-sexp-killing-forward
          "M-b" 'sp-splice-sexp-killing-backward)
    
    (map! :leader
          "r" #'rtags-find-symbol-at-point
          "f j" `open-junk-file)


<a id="org7680b0e"></a>

# Stolen functions (the Luciano namespace)

This function puts the full path of a file on the clipboard. I forgot where I
stole it from, but it isn&rsquo;t mine.

    (defun luciano/put-file-name-on-clipboard ()
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

This function formats and auto-tabs all of the files in a directory with a
specific extension. Stolen from [here](https://stackoverflow.com/a/55302689) and [here](https://emacs.stackexchange.com/a/34222).

    (defun luciano/fix-file-formatting-and-tabs (directory extension)
      (interactive (list (read-directory-name "Directory: ")
                         (read-string "File extension: ")))
      (dolist (file (directory-files-recursively directory (concat "^[a-z0-9A-Z]?+\\" extension "$")))
        (find-file file)
        (format-all-buffer (point-min)(point-max))
        (untabify(point-min)(point-max))
        (save-buffer)
        (kill-buffer nil)))

If you have exactly two windows open in a frame, this function will toggle them between vertical and horizontal splits. Not my own work; sadly I didn&rsquo;t keep the attribution.

    (defun luciano/toggle-window-split ()
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

`Function to wrap blocks of text in org templates.` Taken from [here](http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/        ;;).

    (defun luciano/org-begin-template ()
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

    (defun my/org-auto-capitalize-headings-and-lists ()
      "Create a buffer-local binding of sentence-end to auto-capitalize
    section headings."
      ;; courtesy of https://emacs.stackexchange.com/questions/3949/fixing-auto-capitalize-to-work-with-org-mode-headings-and-lists
      (make-local-variable 'sentence-end)
      (setq sentence-end (concat (rx (or
                                      (seq line-start (1+ "*") (1+ space))))
                                 "\\|" (sentence-end))))

This next group of functions work together to make Clojure source blocks that
get exported to PDF, with the result of evaluating the source code block right
under the block in the document, prefixed by a little =>. The arrow bit comes
from [this](https://stackoverflow.com/a/64893411/5692730) answer on
stackoverflow. `random-alnum` and `random-string` come from two separate answers
to [this](https://stackoverflow.com/q/37038441/5692730) question on stackoverflow.

    (defun random-alnum ()
      (let* ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
             (i (% (abs (random)) (length alnum))))
        (substring alnum i (1+ i))))
    
    (defun random-string (n)
      "Generate a slug of n random alphanumeric characters.
    Inefficient implementation; don't use for large n."
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
          (insert "\\Rightarrow call_" section-name"[:exports results]()\n\n"))
        (forward-line 2)
        (evil-insert)))

Sort a group of lines by length, taken from [here](https://stackoverflow.com/a/30697761/5692730).

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


<a id="orgefacf90"></a>

# Archive

I&rsquo;ve retired the code in this section from service, or just want to hold onto it
because it was difficult to find. These do not get tangled into the final
`config.el`.

This was used to fix a bug in ivy on my laptop, which I don&rsquo;t think is active anymore.

    (map! :after ivy
          :map ivy-minibuffer-map
          "DEL" #'ivy-backward-delete-char)

This function deletes an entire subtree in an org document.

    (defun my/clear-subtree ()
      (interactive)
      (org-mark-subtree) ;; mark the current subtree
      (forward-line) ;; move point forward, so the headline isn't in the region
      (delete-region (region-beginning) (region-end)) ;; delete the rest
      )

