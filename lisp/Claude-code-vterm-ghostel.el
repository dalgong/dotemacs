;;; claude-code-vterm-ghostel.el --- Ghostel shim for Claude Code vterm backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file from `~/.emacs.d/lisp`, then enable
;; `claude-code-vterm-ghostel-mode' to keep
;; `claude-code-terminal-backend' set to `vterm' while routing Claude
;; buffers through Ghostel instead.
;;
;; Usage:
;;   (when (require 'claude-code-vterm-ghostel nil t)
;;       (claude-code-vterm-ghostel-mode t))
;;
;;; Code:

(require 'cl-lib)
(require 'claude-code)
(require 'ghostel)
(require 'vterm)

(defgroup claude-code-vterm-ghostel nil
  "Ghostel compatibility for Claude Code's vterm backend."
  :group 'claude-code)

(defvar claude-code-vterm-ghostel-mode nil
  "Non-nil when `claude-code-vterm-ghostel-mode' is enabled.")

(defvar-local claude-code-vterm-ghostel--buffer nil
  "Non-nil when the current buffer uses the Ghostel vterm shim.")
(put 'claude-code-vterm-ghostel--buffer 'permanent-local t)

(defconst claude-code-vterm-ghostel--locked-title :claude-code-vterm-ghostel
  "Sentinel value used to stop Ghostel from renaming Claude buffers.")

(defun claude-code-vterm-ghostel--target-buffer-p (&optional buffer)
  "Return non-nil when BUFFER should use the Ghostel vterm shim."
  (let ((buffer (or buffer (current-buffer))))
    (and claude-code-vterm-ghostel-mode
         (buffer-live-p buffer)
         (with-current-buffer buffer
           (claude-code--buffer-p buffer)))))

(defun claude-code-vterm-ghostel--buffer-p (&optional buffer)
  "Return non-nil when BUFFER already uses the Ghostel vterm shim."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (bound-and-true-p claude-code-vterm-ghostel--buffer)))))

(defun claude-code-vterm-ghostel--ensure-module ()
  "Ensure Ghostel's native module is available."
  (unless (fboundp 'ghostel--new)
    (let* ((dir (file-name-directory (locate-library "ghostel")))
           (module (expand-file-name
                    (concat "ghostel-module" module-file-suffix)
                    dir)))
      (ghostel--ensure-module dir)
      (unless (file-exists-p module)
        (error "Ghostel native module not available at %s" module))
      (module-load module))))

(defun claude-code-vterm-ghostel--command ()
  "Return the command string Claude Code asked vterm to start."
  (or vterm-shell shell-file-name))

(defun claude-code-vterm-ghostel--start-process (command)
  "Start Ghostel with COMMAND in the current buffer."
  (let* ((height (max 1 (window-body-height)))
         (width (max 1 (window-max-chars-per-line)))
         (remote-p (file-remote-p default-directory))
         (process-environment
          (append
           (list
            "INSIDE_EMACS=ghostel"
            (format "TERM=%s" claude-code-term-name)
            "COLORTERM=truecolor")
           process-environment))
         (proc
          (make-process
           :name "ghostel"
           :buffer (current-buffer)
           :command
           `("/bin/sh" "-c"
             ,(format
               "stty -nl sane %s erase ^? rows %d columns %d >/dev/null && exec %s"
               (if (eq system-type 'berkeley-unix) "" "iutf8")
               height width command))
           :connection-type 'pty
           :file-handler remote-p
           :filter #'claude-code-vterm-ghostel--filter
           :sentinel #'ghostel--sentinel)))
    (setq ghostel--process proc)
    (set-process-coding-system proc 'binary 'binary)
    (set-process-window-size proc height width)
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'adjust-window-size-function
                 #'vterm--window-adjust-process-window-size)
    proc))

(defun claude-code-vterm-ghostel--initialize-buffer ()
  "Initialize the current Claude buffer with Ghostel."
  (claude-code-vterm-ghostel--ensure-module)
  (unless (and (derived-mode-p 'ghostel-mode)
               ghostel--term
               (process-live-p (get-buffer-process (current-buffer))))
    (ghostel-mode)
    (setq-local claude-code-vterm-ghostel--buffer t)
    (setq-local vterm-copy-mode nil)
    ;; Any non-nil value different from the live buffer name disables
    ;; Ghostel's automatic title-based renaming.
    (setq-local ghostel--managed-buffer-name
                claude-code-vterm-ghostel--locked-title)
    (let ((height (window-body-height))
          (width (window-max-chars-per-line)))
      (setq ghostel--term
            (ghostel--new height width ghostel-max-scrollback))
      (ghostel--apply-palette ghostel--term))
    (claude-code-vterm-ghostel--start-process
     (claude-code-vterm-ghostel--command)))
  (current-buffer))

(defun claude-code-vterm-ghostel--filter (process input)
  "Feed PROCESS INPUT to Ghostel and preserve Claude bell notifications."
  (when (and (string-match-p "\007" input)
             (claude-code--buffer-p (process-buffer process))
             (not (string-match-p "]0;.*\007" input)))
    (claude-code--notify nil))
  (ghostel--filter process input))

(defun claude-code-vterm-ghostel--key-name (key)
  "Translate a vterm KEY value to a Ghostel key name."
  (cond
   ((and (stringp key) (= (length key) 1))
    (pcase (aref key 0)
      (?\e "escape")
      (?\r "return")
      (?\n "return")
      (?\t "tab")
      (?\177 "backspace")
      (_ key)))
   ((and (stringp key)
         (> (length key) 2)
         (string-prefix-p "<" key)
         (string-suffix-p ">" key))
    (downcase (substring key 1 -1)))
   ((and (vectorp key) (> (length key) 0))
    (let ((base (event-basic-type (aref key 0))))
      (cond
       ((characterp base) (char-to-string base))
       ((symbolp base) (symbol-name base))
       (t nil))))
   ((symbolp key)
    (symbol-name key))
   (t nil)))

(defun claude-code-vterm-ghostel--modifiers (key shift meta ctrl)
  "Translate vterm KEY modifier args to a Ghostel modifier string.

SHIFT, META, and CTRL are the explicit modifier flags from
`vterm-send-key'."
  (let* ((event (and (vectorp key) (> (length key) 0) (aref key 0)))
         (event-modifiers (and event (event-modifiers event)))
         (use-explicit-modifiers (null event-modifiers))
         (mods
          (delq nil
                (list
                 (when (or (and use-explicit-modifiers shift)
                           (memq 'shift event-modifiers))
                   "shift")
                 (when (or (and use-explicit-modifiers meta)
                           (memq 'meta event-modifiers)
                           (memq 'alt event-modifiers))
                   "meta")
                 (when (or (and use-explicit-modifiers ctrl)
                           (memq 'control event-modifiers))
                   "ctrl")
                 (when (memq 'super event-modifiers)
                   "super")
                 (when (memq 'hyper event-modifiers)
                   "hyper")))))
    (mapconcat #'identity (delete-dups mods) ",")))

(defun claude-code-vterm-ghostel--mode-advice (orig-fun &rest args)
  "Run Ghostel instead of `vterm-mode' for Claude buffers.

ORIG-FUN is the original `vterm-mode' function.  ARGS are its
arguments."
  (if (claude-code-vterm-ghostel--target-buffer-p)
      (claude-code-vterm-ghostel--initialize-buffer)
    (apply orig-fun args)))

(defun claude-code-vterm-ghostel--send-string-advice
    (orig-fun string &optional paste-p)
  "Route `vterm-send-string' through Ghostel for shimmed Claude buffers.

ORIG-FUN is the original `vterm-send-string' function.  STRING and
PASTE-P are its arguments."
  (if (claude-code-vterm-ghostel--buffer-p)
      (if paste-p
          (ghostel--paste-text string)
        (ghostel--send-key string))
    (funcall orig-fun string paste-p)))

(defun claude-code-vterm-ghostel--send-key-advice
    (orig-fun key &optional shift meta ctrl accept-proc-output)
  "Route `vterm-send-key' through Ghostel for shimmed Claude buffers.

ORIG-FUN is the original `vterm-send-key' function.  KEY, SHIFT,
META, CTRL, and ACCEPT-PROC-OUTPUT are its arguments."
  (if (claude-code-vterm-ghostel--buffer-p)
      (let* ((modifiers
              (claude-code-vterm-ghostel--modifiers key shift meta ctrl))
             (key-name
              (claude-code-vterm-ghostel--key-name key)))
        (cond
         ((and (stringp key)
               (= (length key) 1)
               (equal modifiers ""))
          (ghostel--send-key key))
         (key-name
          (ghostel--send-encoded key-name modifiers))
         ((stringp key)
          (ghostel--send-key key))
         (t
          (user-error "Ghostel shim cannot encode vterm key %S" key)))
        (when accept-proc-output
          (when-let* ((proc (get-buffer-process (current-buffer))))
            (accept-process-output proc vterm-timer-delay nil t))))
    (funcall orig-fun key shift meta ctrl accept-proc-output)))

(defun claude-code-vterm-ghostel--copy-mode-advice (orig-fun &optional arg)
  "Route `vterm-copy-mode' through Ghostel for shimmed Claude buffers.

ORIG-FUN is the original `vterm-copy-mode' function.  ARG is its
argument."
  (if (claude-code-vterm-ghostel--buffer-p)
      (let* ((was-active (bound-and-true-p vterm-copy-mode))
             (enable (if (null arg)
                         (not was-active)
                       (> (prefix-numeric-value arg) 0))))
        (if enable
            (unless (bound-and-true-p ghostel--copy-mode-active)
              (ghostel-copy-mode))
          (when (bound-and-true-p ghostel--copy-mode-active)
            (ghostel-copy-mode-exit)))
        (setq-local vterm-copy-mode
                    (bound-and-true-p ghostel--copy-mode-active))
        (unless (eq was-active vterm-copy-mode)
          (run-hooks 'vterm-copy-mode-hook))
        vterm-copy-mode)
    (funcall orig-fun arg)))

(defun claude-code-vterm-ghostel--window-adjust-advice (orig-fun process windows)
  "Route `vterm--window-adjust-process-window-size' through Ghostel.

ORIG-FUN is the original window resize handler.  PROCESS and WINDOWS
are its arguments."
  (if (claude-code-vterm-ghostel--buffer-p (process-buffer process))
      (ghostel--window-adjust-process-window-size process windows)
    (funcall orig-fun process windows)))

(defconst claude-code-vterm-ghostel--advice-specs
  '((vterm-mode claude-code-vterm-ghostel--mode-advice)
    (vterm-send-string claude-code-vterm-ghostel--send-string-advice)
    (vterm-send-key claude-code-vterm-ghostel--send-key-advice)
    (vterm-copy-mode claude-code-vterm-ghostel--copy-mode-advice)
    (vterm--window-adjust-process-window-size
     claude-code-vterm-ghostel--window-adjust-advice))
  "Advice installed by `claude-code-vterm-ghostel-mode'.")

(defun claude-code-vterm-ghostel--set-advices (enable)
  "Enable or disable Ghostel shim advices according to ENABLE."
  (dolist (spec claude-code-vterm-ghostel--advice-specs)
    (pcase-let ((`(,function ,advice) spec))
      (if enable
          (advice-add function :around advice)
        (advice-remove function advice)))))

;;;###autoload
(define-minor-mode claude-code-vterm-ghostel-mode
  "Use Ghostel behind Claude Code's vterm backend.

Enable this before starting Claude buffers with
`claude-code-terminal-backend' set to `vterm'."
  :global t
  :group 'claude-code-vterm-ghostel
  (claude-code-vterm-ghostel--set-advices
   claude-code-vterm-ghostel-mode))

(provide 'claude-code-vterm-ghostel)
;;; claude-code-vterm-ghostel.el ends here
