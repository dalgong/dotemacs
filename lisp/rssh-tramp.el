;;; rssh-tramp.el --- TRAMP method for rssh -*- lexical-binding: t; -*-

;; Author: rssh
;; Keywords: comm, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Provides a TRAMP method "rssh" that uses the rssh SSH multiplexer for
;; remote file access and shell commands.  All TRAMP operations for a given
;; host are multiplexed over a single SSH connection managed by the rssh
;; daemon.
;;
;; Usage:
;;   C-x C-f /rssh:hostname:/path/to/file
;;   C-x C-f /rssh:user@hostname:/path/to/file
;;
;; Setup:
;;   1. Ensure `rssh` (the wrapper script) is on your PATH.
;;   2. Add to your init file:
;;
;;        (require 'rssh-tramp)
;;
;; How it works:
;;   TRAMP starts `rssh --pipe host` which opens a remote shell.
;;   --pipe mode disables raw terminal manipulation and status messages
;;   so TRAMP can control the connection itself.
;;
;;   If a daemon is running (from a prior `rssh connect host`), the
;;   session is multiplexed over the existing SSH connection.  Otherwise,
;;   rssh runs SSH inline.
;;
;;   For best results, pre-establish the connection from a terminal:
;;     $ rssh connect myhost
;;   Then all TRAMP operations reuse that single SSH connection.

;;; Code:

(require 'tramp)

(defgroup rssh-tramp nil
  "TRAMP method using rssh multiplexer."
  :group 'tramp
  :prefix "rssh-tramp-")

(defcustom rssh-tramp-rssh-program "rssh"
  "Path to the rssh program."
  :type 'string
  :group 'rssh-tramp)

(defconst rssh-tramp-method "rssh"
  "TRAMP method for rssh connections.")

;; Remove any stale entry before adding — add-to-list won't replace.
(setq tramp-methods
      (assoc-delete-all rssh-tramp-method tramp-methods))

(add-to-list 'tramp-methods
             `(,rssh-tramp-method
               (tramp-login-program        ,rssh-tramp-rssh-program)
               (tramp-login-args            (("-l" "%u") ("--pipe") ("%h")))
               (tramp-remote-shell          "/bin/bash")
               (tramp-remote-shell-login    ("-l"))
               (tramp-remote-shell-args     ("-c"))
               (tramp-connection-timeout    60)))

;; Register completion function so C-x C-f /rssh: TAB works.
(tramp-set-completion-function
 rssh-tramp-method tramp-completion-function-alist-ssh)

(provide 'rssh-tramp)
;;; rssh-tramp.el ends here
