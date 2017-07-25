;; -*- emacs-lisp -*-

(setq package-user-dir "~/docs/emacs/elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; real .emacs starts here

;; use-package for the case when init.el is byte-compiled
(use-package diminish)
(use-package bind-key)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package :commands use-package-autoload-keymap)
(bind-key "C-z" nil)

;; First: everything that is only setq
(setq custom-file "~/docs/emacs/errge-cf-own-custom.el")
(load "~/docs/emacs/errge-cf-own-custom")

;; Workaround for i3 focus issue on emacs exit if an emacs package
;; called x-focus-frame before.  The i3 guys refused fixing their shit
;; and instead decided to be ssholes and rude.
(setf (symbol-function 'x-focus-frame) #'ignore)

(set-default 'truncate-lines t)
(set-default 'show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "pink")
(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      unibyte-display-via-language-environment t
      column-number-mode t
      echo-keystrokes 0.1
      kill-whole-line t
      make-backup-files nil
      auto-save-timeout 10
      auto-save-file-name-transforms (progn
                                       (make-directory "~/.emacs.d/auto-save-files/" t)
                                       `((".*" "~/.emacs.d/auto-save-files/" t)))
      mouse-yank-at-point t
      switch-to-buffer-preserve-window-point t
      select-enable-clipboard t
      select-enable-primary t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Second: deferred packages, eval-after-loads and autoloads
;; beautiful hungarian letters in ps-print
(use-package ps-print
  :defer t
  :config
  (require 'ps-mule)
  (eval-when-compile (require 'ps-mule))
  (setq ps-paper-type 'a4
        ps-multibyte-buffer 'bdf-font ;; always use my fonts, not the printer's
        ps-font-size 10
        bdf-directory-list '("~/docs/emacs/fonts")
        ps-mule-font-info-database-bdf '((unicode-bmp
                                          (normal bdf "djvmono.bdf")
                                          (bold bdf "djvmono-b.bdf")
                                          (italic bdf "djvmono-o.bdf")
                                          (bold-italic bdf "djvmono-bo.bdf"))
                                         (iso-8859-1
                                          (normal bdf "djvmono.bdf")
                                          (bold bdf "djvmono-b.bdf")
                                          (italic bdf "djvmono-o.bdf")
                                          (bold-italic bdf "djvmono-bo.bdf")))))

;; also-dependency-for-gnus!
(use-package bbdb
  :defer t
  :init (autoload 'bbdb "bbdb-com" nil t))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :commands (gfm-mode))

(use-package expand-region
  :bind (("C-z e" . er/expand-region)
         ("C-z C-e" . er/expand-region)))

(use-package compile
  :bind (("C-z c" . compile)
         ("C-z C-c" . compile))
  :config (setq compilation-ask-about-save nil
                compilation-read-command nil
                compilation-scroll-output t
                compile-command "make"))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package lua-mode
  :mode ("\\.lua\\'" "\\.luac\\'")
  :config (setq lua-indent-level 2))

(use-package ledger-mode
  :commands (ledger-mode)
  :bind (:map ledger-mode-map
         ("C-c C-c" . errge/ledger-check-sequence)
         ("M->" . errge/ledger-goto-today))
  :functions (ledger-read-string-with-default ledger-next-amount ledger-post-align-postings ledger-mode-remove-extra-lines)
  :config
    (setq ledger-post-account-alignment-column 2
          ledger-post-amount-alignment-column 80)

    (defun errge/ledger-goto-today ()
      (interactive)
      (let ((today (format-time-string "%Y/%m/%d"))
            (cont t))
        (goto-char (point-min))
        (while (and (re-search-forward "^[[:digit:]]\\{4\\}/[[:digit:]]\\{2\\}/[[:digit:]]\\{2\\}" nil t)
                    (string< (match-string 0) today))))
      (beginning-of-line))

    (defun errge/ledger-check-sequence ()
      (interactive)
      (let* ((next (lambda ()
                     (and
                      (search-forward-regexp "^[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]" nil t)
                      (goto-char (match-end 0))
                      (match-string 0))))
             (newpos (catch 'found
                       (save-excursion
                         (goto-char (point-min))
                         (let ((curdate (funcall next)))
                           (while curdate
                             (let ((nextdate (funcall next)))
                               (and nextdate
                                    (string< nextdate curdate)
                                    (throw 'found (point)))
                               (setq curdate nextdate))))))))
        (if newpos
            (goto-char newpos)
          (message "all ok!")))))

(use-package rjsx-mode
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package ess
  :load-path "lisp"
  :defer t
  :init
  (progn
    (setq ess-ask-for-ess-directory nil
          ess-tab-complete-in-script t)
    (autoload 'R "ess-site.el" "Call 'R'." t)
    (autoload 'S "ess-site" "Call 'S'." t)
    (autoload 'R-mode "ess-site.el" "Major mode for editing R source." t)
    (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
    (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
    (add-to-list 'auto-mode-alist '("\\.r$" . R-mode))))

(use-package with-editor
  :commands (with-editor-async-shell-command with-editor-shell-command with-editor-export-editor)
  :init
  (progn
    (define-key (current-global-map) [remap async-shell-command] 'with-editor-async-shell-command)
    (define-key (current-global-map) [remap shell-command] 'with-editor-shell-command))
  (with-eval-after-load 'shell
    (add-hook 'shell-mode-hook 'with-editor-export-editor))
  (with-eval-after-load 'term
    (add-hook 'term-exec-hook 'with-editor-export-editor))
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package magit
  :bind (("C-z s" . magit-status)
         ("C-z C-s" . magit-status))
  :config (setq magit-save-repository-buffers 'dontask
                magit-completing-read-function 'ivy-completing-read))

(use-package multiple-cursors
  :bind (("C-z m" . mc/edit-lines)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-next-like-this)
         ("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init (setq mc/list-file "~/docs/emacs/errge-cf-multiple-cursors-commands.el"))

(use-package cc-mode
  :bind (:map c++-mode-map
         ("C-z f" . ff-find-other-file)
         ("C-z C-f" . ff-find-other-file)
         :map c-mode-map
         ("C-z f" . ff-find-other-file)
         ("C-z C-f" . ff-find-other-file)))

(use-package windmove
  :config (defun errge/other-window-back ()
            (interactive)
            (other-window -1))
  :bind* (("<S-left>" . windmove-left)
          ("<S-right>" . windmove-right)
          ("<S-up>" . windmove-up)
          ("<S-down>" . windmove-down)
          ("<f1>" . errge/other-window-back)
          ("<f2>" . other-window)))

(use-package avy
  :bind (("C-z j" . avy-goto-char-timer)
         ("C-z C-j" . avy-goto-char-timer)))

(use-package ace-window
  :bind (("C-z o" . ace-window))
  :config (setq aw-scope 'frame
                aw-dispatch-always t))

;; these work because of (describe-variable 'package--builtins)
(use-package calendar
  :defer t
  :config (setq calendar-week-start-day 1))

(use-package vc
  :defer t
  :config (setq vc-follow-symlinks t))

;; no multiframe ediff please
(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; comint not in package--builtins :(
(with-eval-after-load 'comint
  (bind-key "<up>" 'comint-previous-matching-input-from-input comint-mode-map)
  (bind-key "<down>" 'comint-next-matching-input-from-input comint-mode-map)
  (setq comint-scroll-to-bottom-on-output 'others
        comint-scroll-to-bottom-on-input 'this))

(use-package epa
  :defer t
  :config
  (progn
    (setq errge/my-gpg-key-id "2930100100003344")
    (setq epa-file-encrypt-to errge/my-gpg-key-id)
    ;; This is actually an Emacs Bug, we should fix it in GNU Emacs.
    ;; The problem is that they assume that they can use the
    ;; gpg list-pac to get out the epa-file-encrypt-to, but that
    ;; can contain a hidden recipient (all 0 keyid).  If we manage
    ;; to decrypt the session key for that key id, then we have to
    ;; replace 000000... with the keyid we managed to decrypt with,
    ;; if there are other 0000... keyids, we should just ignore those,
    ;; as we have no possible way to know whose they are.
    ;; This is just a workaround for myself.
    (defadvice epa-file-insert-file-contents (after errge/replace-anonymous-with-me activate compile)
      (let ((rest (member "0000000000000000" epa-file-encrypt-to)))
        (if rest
            (setf (car rest) errge/my-gpg-key-id))))))

(use-package recentf
  :commands (recentf-mode) ; ivy calls this for C-x b
  :functions (recentf-load-list recentf-cleanup recentf-save-list)
  :config
  (progn
    ;; Recentf merge file and history on save, so multiple parallel
    ;; emacsen don't overwrite each other recent files.

    ;; these hacks are not compatible for some reason with auto-cleanup...
    (setq recentf-auto-cleanup 'never
          recentf-max-saved-items 200000)

    (defun errge/recentf-return-current-list-from-filesystem ()
      (let (recentf-list
            (recentf-initialize-file-name-history nil))
        (recentf-load-list)
        recentf-list))

    (defun errge/recentf-merge (from-filesystem from-memory)
      (require 'cl-lib)
      (let* ((new-on-fs (cl-set-difference from-filesystem from-memory :test 'equal)))
        (delete-dups (append new-on-fs from-memory))))

    (defun errge/recentf-refresh-from-filesystem ()
      (setq recentf-list
            (errge/recentf-merge
             (errge/recentf-return-current-list-from-filesystem)
             recentf-list)))

    (defun errge/recentf-refresh-and-save ()
      (errge/recentf-refresh-from-filesystem)
      (recentf-cleanup)
      (recentf-save-list))

    (defadvice recentf-save-list (before errge/recentf-save-file-advice () activate compile)
      (errge/recentf-refresh-from-filesystem))))

(use-package ivy
  :init (setq completing-read-function 'ivy-completing-read
              completion-in-region-function 'ivy-completion-in-region)
  :commands (ivy-mode ivy-completing-read ivy-completion-in-region ivy-read)
  :bind (("<f6>" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :config (setq ivy-use-virtual-buffers t
                ivy-use-selectable-prompt t))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package smex :defer t)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config
  (progn
    ;; history first for M-x
    (require 'smex)
    ;; https://github.com/abo-abo/swiper/issues/627
    (setq counsel-find-file-speedup-remote nil)))

(use-package projectile
  :commands (projectile-project-root projectile-project-p) ; for neotree below
  :bind-keymap (("C-c p" . projectile-command-map))
  :defines (projectile-use-git-grep projectile-completion-system projectile-verbose)
  :config
  (setq projectile-use-git-grep t
        projectile-completion-system 'ivy
        projectile-verbose nil))

(use-package neotree
  :commands (neotree-toggle errge/neotree-project-dir)
  :bind (("<f8>" . errge/neotree-project-dir))
  :functions (neo-global--window-exists-p neotree-dir neotree-find)
  :config
  ;; from https://www.emacswiki.org/emacs/NeoTree
  (defun errge/neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))))


(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package ag
  :commands (ag))

(use-package browse-kill-ring
  :commands (browse-kill-ring))

(use-package org-mime
  :commands (org-mime-htmlize))

(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.hsc\\'"))
(use-package ghc
  :commands (ghc-init)
  :config
  (setq ghc-indent-offset 2))

(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-methods (cl-remove "docker" tramp-methods :key #'car :test #'equal))
    (push '("docker"
            (tramp-login-program "docker")
            (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
            (tramp-remote-shell "/bin/sh")
            (tramp-remote-shell-args ("-i") ("-c")))
          tramp-methods)))

(use-package undo-tree
  :bind (("C-_" . undo-tree-undo)
         ("C-S-/" . undo-tree-redo)
         ("M-_" . undo-tree-redo)
         ("C-x u" . undo-tree-undo)
         ("C-/" . undo-tree-undo)
         ("<undo>" . undo-tree-undo))
  :commands (global-undo-tree-mode)
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package company
  :diminish company-mode
  :functions (global-company-mode)
  :bind (("M-/" . company-complete)
         ("C-M-/" . company-dabbrev))
  :config
  (global-company-mode))

;; A default emacs firefox megprobal -remote parametert hasznalni csak
;; azert, hogy allitgassa a new-tab vs new-window settinget.  Mi a
;; sajat firefox wrapperunket akarjuk meghivni a leheto legegyszerubben.
(autoload 'browse-url-interactive-arg "browse-url")
(autoload 'browse-url-encode-url "browse-url")
(autoload 'browse-url-process-environment "browse-url")
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function
        #'(lambda (url &optional new-window)
            (interactive (browse-url-interactive-arg "URL: "))
            (setq url (browse-url-encode-url url))
            (let* ((process-environment (browse-url-process-environment)))
              (apply 'start-process
                     (concat "firefox " url) nil
                     "/home/errge/docs/bin/common/firefox"
                     (list url))))))

(use-package diff-hl
  :commands (global-diff-hl-mode)
  :functions (diff-hl-margin-mode)
  :config
  (global-diff-hl-mode 1)
  (require 'diff-hl-margin)
  (diff-hl-margin-mode))
(with-eval-after-load 'vc-git
  (require 'diff-hl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third: the sad ones, nothing to defer on and they are slow, so we defer on time...

(use-package smart-mode-line
  :defer 1
  :functions (sml/setup)
  :config (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fourth: require, defuns and bind-keys that are evaluated right
;; here, right now, might be slow, test these first for slowness!

;; These are not use-packagefied, because they are not existing on
;; melpa outside of Debian... :( And on debian, we use nix/ceh instead
;; of the shipped emacs package.
(add-to-list 'load-path "~/docs/emacs/dpkg-dev-el")
(eval-when-compile
  (byte-recompile-directory "~/docs/emacs/dpkg-dev-el" 0))
(require 'dpkg-dev-el-loaddefs)
(require 'debian-el-loaddefs)
(setq debian-changelog-mailing-address "risko@debian.org")

(global-hl-line-mode)

(use-package which-key
  :diminish which-key-mode
  :config (progn
            (setq which-key-idle-secondary-delay 0.1
                  which-key-idle-delay 0.3)
            (which-key-mode 1)))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (progn
    (setq ws-butler-keep-whitespace-before-point nil)
    (ws-butler-global-mode 1)))

(defun errge/mini-calc (expr)
  "Calculate expression and insert into buffer"
  (interactive (list (read-from-minibuffer "Enter expression: " (thing-at-point 'sexp t))))
  (let ((result (calc-eval expr)))
    (unless buffer-read-only
      (when (equal (thing-at-point 'sexp t) expr)
        (backward-delete-char (length expr)))
      (insert result))
    (message "%s" result)))
(bind-key "C-z a" 'errge/mini-calc)
(bind-key "C-z C-a" 'errge/mini-calc)

(defvar errge/comment-dwim-content)
(defun errge/comment-dwim (arg)
  "If region is active, remember it in errge/comment-dwim-content.
Then when you press M-; immediately after the first run, the
second run will insert the commented out stuff in verbatim."
  (interactive "*P")
  (if (not (use-region-p))
      (comment-dwim arg)
    (set 'errge/comment-dwim-content
         (buffer-substring (region-beginning) (region-end)))
    (comment-dwim arg)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [(meta ?\;)]
         (lambda ()
           (interactive)
           (insert errge/comment-dwim-content)))
       map))))
(bind-key "M-;" 'errge/comment-dwim)

(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))

;; start server, so we can connect anytime with emacsclient
(unless noninteractive
  (setq server-socket-dir (format "/tmp/emacs-%d-%s-%d"
                                  (user-uid)
                                  (format-time-string "%Y%m%d-%H%M%S")
                                  (emacs-pid)))
  (server-start)
  (add-hook 'kill-emacs-hook #'(lambda () (delete-directory server-socket-dir t))))

;;; Local Variables: ***
;;; indent-tabs-mode:nil ***
;;; End: ***
