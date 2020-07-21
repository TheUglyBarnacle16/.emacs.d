(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char)
  ("M-p" . avy-copy-line))

(global-set-key (kbd "C-x b") 'ibuffer)

(setq ibuffer-expert t)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'kill-all-buffers)

(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

(defun config-visit()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-vertically)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-horizontally)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/logo/PinkEmacsIcon.png")
  (setq dashboard-items '((recents . 10)
                          (agenda  . 10))))

(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode "cc-mode" "C Editing Mode" t)
(autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
(autoload 'c-add-style "cc-mode" "Add coding style" t)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(use-package emms
  :ensure t
  :config
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all)
    (setq emms-seek-seconds 5)
    (setq emms-player-list '(emms-player-mpd))
    (setq emms-info-functions '(emms-info-mpd))
    (setq emms-player-mpd-server-name "localhost")
    (setq emms-player-mpd-server-port "6601")
  :bind
    ("M-s-p" . emms)
    ("M-s-b" . emms-smart-browse)
    ("M-s-r" . emms-player-mpd-update-all-reset-cache))

(setq mpc-host "localhost:6601")

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
(global-set-key (kbd "M-s-c") 'mpd/start-music-daemon)

(defun mpd/kill-music-daemon ()
  "Stops playback and kills the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))
(global-set-key (kbd "M-s-k") 'mpd/kill-music-daemon)

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
(global-set-key (kbd "M-s-u") 'mpd/update-database)

(setq make-backup-file nil)
(setq auto-save-default nil)

(setq ring-bell-function 'ignore)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(setq display-time-12hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode 1)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%B\ %d,\ %Y)")))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            ))

(electric-pair-mode t)

(global-linum-mode t)
(setq linum-format "%d ")

(setq-default mouse-yank-at-pint t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(global-subword-mode 1)

(global-set-key (kbd "C-c w") 'whitespace-mode)

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(setq-default tab-width 4)

(setq-default show-trailing-whitespace t)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-seperator (quote arrow))
  (spaceline-emacs-theme))

(use-package multiple-cursors
  :ensure t)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-agenda-files (quote ("~/agenda.org")))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook '(lambda() (visual-line-mode 1)))

(setq org-src-window-setup 'current-window)

(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "PROGRESS(p)" "DONE(d)")))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :init
  (setq elfeed-feeds
    '(("https://jacobmccormick.xyz/rss.xml" blog mine)
      ("https://lukesmith.xyz/rss.xml" blog linux)
      ("https://videos.lukesmith.xyz/feeds/videos.xml?videoChannelId=2" video linux)
      ("https://www.distrotube.com/videos/index.xml" video linux)
      ("https://christitus.com/index.xml" blog linux)
      ("http://vault.lunduke.com/LundukeShowMP3.xml" blog linux))))

(use-package free-keys
  :ensure t)

;; enale use of gnuplot mode
(autoload 'gnuplot-mode "gnupot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
;; auto enable gnuplot mode for .gp files
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
;; keyboard shortcut
(global-set-key (kbd "C-M-g") 'org-plot/gnuplot)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package swiper
  :ensure t
:bind ("C-s" . swiper))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))
(add-hook 'text-mode-hook 'yas-minor-mode)

(use-package sudo-edit
  :ensure t
  :bind ("M-E" . sudo-edit))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 3)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "w" "e" "r"))
  :bind
  ([remap other-window] . switch-window))

(use-package symon
  :ensure t)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(when window-system (global-hl-line-mode t))

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'beacon-mode)
  (diminish 'subword-mode)
  (diminish 'yas-minor-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'org-indent-mode)
  (diminish 'visual-line-mode))
