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
               (tramp-direct-async          ("-t"))
               (tramp-remote-shell          "/bin/bash")
               (tramp-remote-shell-login    ("-l"))
               (tramp-remote-shell-args     ("-c"))
               (tramp-connection-timeout    60)))

;; Enable direct async processes: each `start-file-process` gets its
;; own rssh connection rather than sharing the main TRAMP shell.  This
;; prevents concurrent marker corruption (args-out-of-range) and gives
;; each process a fresh PTY with echo enabled (fixing eat).
;;
;; We use both the modern connection-local API and the deprecated
;; tramp-connection-properties as a belt-and-suspenders approach.
;; TRAMP checks both with OR in tramp-direct-async-process-p.

;; Modern API (Emacs 29+):
(connection-local-set-profile-variables
 'rssh-tramp-direct-async-profile
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 `(:application tramp :protocol ,rssh-tramp-method)
 'rssh-tramp-direct-async-profile)

;; Deprecated fallback — tramp-connection-properties is checked when
;; the connection hash table is first created for a given host.
(add-to-list 'tramp-connection-properties
             (list (concat "\\`/" rssh-tramp-method ":")
                   "direct-async-process" t))

;; Safety net: if args-out-of-range still occurs on the main connection
;; (e.g., during synchronous TRAMP operations), clean up and retry.
(defun rssh-tramp--recover-connection (orig-fun vec)
  "Catch args-out-of-range in rssh connections and retry after cleanup."
  (condition-case err
      (funcall orig-fun vec)
    (args-out-of-range
     (if (and (tramp-file-name-p vec)
              (string= (tramp-file-name-method vec) rssh-tramp-method))
         (progn
           (tramp-cleanup-connection vec 'keep-debug 'keep-password)
           (funcall orig-fun vec))
       (signal (car err) (cdr err))))))

(advice-add 'tramp-maybe-open-connection :around
            #'rssh-tramp--recover-connection)

;; Register completion function so C-x C-f /rssh: TAB works.
(tramp-set-completion-function
 rssh-tramp-method tramp-completion-function-alist-ssh)

;; Fix eat terminal on rssh remote connections.  With direct-async,
;; eat's bash gets a real PTY and reads ~/.bashrc, which on Meta
;; devservers sources shell integration that emits OSC escape sequences
;; eat's terminal emulator can't parse, corrupting the display.
;; Start bash with --norc --noprofile to prevent this.
(with-eval-after-load 'eat
  (advice-add 'eat-exec :around #'rssh-tramp--eat-exec))

(defun rssh-tramp--eat-exec (orig-fun buffer name command startfile switches)
  "Use --norc --noprofile for eat shells on rssh remote connections."
  (if (and (file-remote-p default-directory)
           (string= (file-remote-p default-directory 'method)
                    rssh-tramp-method))
      (funcall orig-fun buffer name command startfile
               (append switches '("--norc" "--noprofile")))
    (funcall orig-fun buffer name command startfile switches)))

(provide 'rssh-tramp)
;;; rssh-tramp.el ends here
