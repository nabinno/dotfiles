;;; init.el --- This file bootstraps the configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; This file bootstraps the configuration, which is divided into a
;;; number of other files.
;;;
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (concat (getenv "DOTFILES_PATH") "/.emacs.d/lisp"))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; (require 'init-elpa)      ;; Machinery for installing required packages (DEPRECATED)
;; (require 'init-el-get)    ;; Machinery for installing required packages (DEPRECATED)
;; (require 'init-straight)  ;; Machinery for installing required packages (DEPRECATED)
(require 'init-leaf)  ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-dired)

(leaf wgrep :ensure t)
(leaf project-local-variables :el-get emacsmirror/project-local-variables)
(leaf diminish :ensure t)
(leaf scratch :ensure t)
(leaf mwe-log-commands :el-get tshemeng/mwe-log-commands)

;; (require 'init-frame-hooks)
;; (require 'init-xterm)
;; (require 'init-themes)
;; (require 'init-osx-keys)
;; (require 'init-gui-frames)
;; (require 'init-proxies)
;; (require 'init-sekka)
(require 'init-w3m)
;; (require 'init-mu4e)
(require 'init-view)
(require 'init-grep)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-yasnippet)

;; (require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-company)
;; (require 'init-auto-complete)
(require 'init-vertico)
(require 'init-ivy)
(require 'init-helm)
(require 'init-windows)
(require 'init-other-window)
;; (require 'init-e2wm)
;; (require 'init-sessions)
;; (require 'init-fonts)
(require 'init-elscreen)

;; (require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-lsp)
;; (require 'init-mmm)
(require 'init-web)
(require 'init-editing-utils)
;; (require 'init-crontab)
;; (require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-golang)
(require 'init-erlang)
(require 'init-ocaml)
(require 'init-javascript)
(require 'init-express)
(require 'init-php)
(require 'init-org)
;; (require 'init-plantuml)
;; (require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
;; (require 'init-haskell)
(require 'init-ruby-mode)
(require 'init-rails)
(require 'init-sql)
;; (require 'init-edbi)
(require 'init-restclient)
;; (require 'init-perl)
(require 'init-csharp)
(require 'init-java)
;; (require 'init-scala)
(cond ((string-equal system-type "cygwin") (require 'init-android)))
(require 'init-digdag)
(require 'init-analytics)
(require 'init-orchestration)

(require 'init-origami)
;; (require 'init-paredit)
(require 'init-project)
(require 'init-projectile)
(require 'init-llm)
;; (require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (when (>= emacs-major-version 24)
;;   (require 'init-clojure-cider))
;; (require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-shell)
(require 'init-eshell)
(require 'init-powershell)
(require 'init-marmalade)
(require 'init-translation)
(require 'init-misc)

;; (require 'init-dash)
;; (require 'init-ledger)
;; Extra packages which don't require any configuration

;; (require 'init-diagram)
(leaf gnuplot :ensure t)
(leaf lua-mode :ensure t)
(leaf htmlize :ensure t)
(leaf dsvn :ensure t)
(leaf osx-location :ensure t :if *is-a-mac*)
(leaf regex-tool :ensure t)

(require 'init-irc)
(require 'init-esa)
(require 'init-proced)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

;;----------------------------------------------------------------------------
;; Workaround for Windows Terminal
;;----------------------------------------------------------------------------
(when (require 'elscreen nil 'noerror)
  (global-set-key (kbd "<f2>") 'elscreen-create))


(provide 'init)
;;; Local Variables:
;;; coding: utf-8
;;; no-byte-compile: t
;;;
;;; init.el ends here
