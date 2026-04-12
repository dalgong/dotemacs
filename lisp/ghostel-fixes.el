;;; ghostel-fixes.el --- Advice-based fixes for ghostel -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Two fixes applied as advice / keymap additions so they survive
;; ghostel updates without redefining entire functions.
;;
;; Fix 1 — DEL key silently dropped
;;
;;   `ghostel--send-event' calls `event-basic-type' on the key event.
;;   For DEL (character 127) this returns the integer 127, which is
;;   turned into `(string 127)' — a one-char string containing the
;;   raw DEL control character.  The ghostty key encoder expects
;;   semantic names like "backspace", so it doesn't recognize this,
;;   and the raw-sequence fallback doesn't match either.  Result:
;;   the DEL key is silently dropped.
;;
;;   The around-advice intercepts events whose base is char 127 and
;;   rebinds `last-command-event' to the equivalent `backspace' event
;;   (preserving any modifiers) before the original function runs.
;;
;; Fix 2 — xterm-paste event not handled
;;
;;   When Emacs runs in a terminal with bracketed paste, pasting
;;   text fires an [xterm-paste] event.  The default `xterm-paste'
;;   handler calls `insert-for-yank', which fails in ghostel's
;;   read-only buffer.  We bind [xterm-paste] in `ghostel-mode-map'
;;   to a command that extracts the pasted text from the event and
;;   routes it through `ghostel--paste-text'.
;;
;;; Code:

(require 'ghostel)

;; ---------------------------------------------------------------------------
;; Fix 1: DEL (char 127) → backspace translation in ghostel--send-event
;; ---------------------------------------------------------------------------

(defun ghostel-fixes--translate-del (orig-fn &rest args)
  "Around advice for `ghostel--send-event'.
When `last-command-event' has base character 127 (DEL), rebind it
to the equivalent backspace event so the ghostty key encoder
receives the name \"backspace\" instead of a raw control character.

For unmodified DEL, the translated event is the symbol `backspace',
whose `event-basic-type' is `backspace' — matched by the symbolp
branch in `ghostel--send-event'.

For modified DEL (M-DEL, C-DEL, etc.), the translated event is
e.g. `M-backspace', whose `event-basic-type' is nil — matched by
the null-base branch, which strips modifier prefixes from the
symbol name to recover \"backspace\"."
  (let* ((event last-command-event)
         (base (event-basic-type event)))
    (if (and (integerp base) (= base 127))
        (let ((last-command-event
               (event-convert-list
                (append (event-modifiers event) '(backspace)))))
          (apply orig-fn args))
      (apply orig-fn args))))

(advice-add 'ghostel--send-event :around #'ghostel-fixes--translate-del)
(define-key ghostel-mode-map (kbd "C-]")   (lambda () (interactive) (ghostel--send-encoded "escape" "")))
(define-key ghostel-mode-map (kbd "M-d")   (lambda () (interactive) (ghostel--send-encoded "d" "meta")))
(define-key ghostel-mode-map (kbd "M-f")   (lambda () (interactive) (ghostel--send-encoded "f" "meta")))
(define-key ghostel-mode-map (kbd "M-b")   (lambda () (interactive) (ghostel--send-encoded "b" "meta")))
(define-key ghostel-mode-map (kbd "M-DEL") (lambda () (interactive) (ghostel--send-encoded "backspace" "meta")))

;; ---------------------------------------------------------------------------
;; Fix 2: xterm-paste → ghostel--paste-text
;; ---------------------------------------------------------------------------

(defun ghostel--xterm-paste (event)
  "Handle an xterm bracketed paste EVENT in the ghostel terminal.
Extract the pasted text from EVENT and send it to the shell process,
wrapping in bracketed-paste escapes when the child terminal has
mode 2004 enabled."
  (interactive "e")
  (let ((text (nth 1 event)))
    (when (and text (stringp text) (not (string-empty-p text)))
      (ghostel--paste-text text))))

(define-key ghostel-mode-map [xterm-paste] #'ghostel--xterm-paste)

;; Fix 3: fix for insert-for-yank
(advice-add 'insert-for-yank :around 'ghostel-insert-for-yank)
(defun ghostel-insert-for-yank (o &rest args)
  (if (and (boundp 'ghostel--term) ghostel--term)
      (ghostel--paste-text (car args))
    (apply o args)))

(provide 'ghostel-fixes)
;;; ghostel-fixes.el ends here
