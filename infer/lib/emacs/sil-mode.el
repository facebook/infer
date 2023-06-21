;;; sil-mode.el --- Major mode for a textual representation of SIL. -*- lexical-binding: t -*-

;; Copyright (c) Facebook, Inc. and its affiliates.
;;
;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Commentary:
;;
;; This package provides a major mode for files containing a textual
;; representation of Infer's intermediate language called SIL (Smallfoot
;; Intermediate Language).
;;
;; There are many ways to setup `sil-mode.el`.  You could either symlink it into
;; your `site-lisp` directory or add the current directory to Emacs's
;; `load-path`.
;;
;; Example configuration:
;;
;;   ;; Load Infer's Emacs extensions
;;   (let* ((infer-dir (file-name-as-directory (expand-file-name "~/infer")))
;;          (infer-emacs (expand-file-name "infer/lib/emacs" infer-dir)))
;;     (when (file-exists-p infer-emacs)
;;       (add-to-list 'load-path infer-emacs)))
;;
;;   ;; Load sil-mode
;;   (require 'sil-mode nil 'noerror)

;;; Code:

(eval-when-compile
  (require 'rx))

(defcustom sil-indent-offset 2
  "Indent SIL by this number of spaces."
  :type 'integer
  :group 'sil-mode
  :safe #'integerp)

(defvar sil--keywords
  '(
    "attribute"
    "declare"
    "define"
    "extends"
    "global"
    "jmp"
    "load"
    "local"
    "prune"
    "ret"
    "store"
    "throw"
    "type"
    ".handlers"
    ))

(defvar sil--function-name-regex '(+ (| alnum "_" "$" "<" ">")))
(defvar sil--type-name-regex '(+ (| alnum "$" "::" "_")))
(defvar sil--var-name-regex '(+ (| alnum "_" "$")))

(defvar sil--font-lock-default
  (let (
	(keywords-regex (rx-to-string `(: bow (or ,@sil--keywords) eow)))
	(block-label-regex (rx-to-string `(: "#" ,sil--var-name-regex)))
	(type-use-regex (rx-to-string `(: (not ":") ":" (* blank) (: (* "*") (group ,sil--type-name-regex)))))
	(type-def-regex (rx-to-string `(: "type" (+ blank) (group ,sil--type-name-regex))))
	(type-in-fn-regex (rx-to-string `(: (group ,sil--type-name-regex) "." ,sil--function-name-regex "(")))
	(variable-def-regex (rx-to-string `(: (group ,sil--var-name-regex) (* blank) (| ":" "=") (not ":"))))
	(variable-store-regex (rx-to-string `(: "store" (* (| blank "&")) (group ,sil--var-name-regex))))
	(function-regex (rx-to-string `(: (group ,sil--function-name-regex) "(")))
	(number-regex (rx-to-string '(: symbol-start (+ num) symbol-end)))
	(comment-regex "//.*$")
	(important-punct-regex (rx-to-string '(| "*" "&" "...")))
	)
    `((
       (,comment-regex 0 font-lock-comment-face)
       (,keywords-regex 0 font-lock-keyword-face)
       (,number-regex 0 font-lock-constant-face)
       (,type-in-fn-regex 1 font-lock-type-face)
       (,type-use-regex 1 font-lock-type-face)
       (,type-def-regex 1 font-lock-type-face)
       (,function-regex 1 font-lock-function-name-face)
       (,block-label-regex 0 font-lock-reference-face)
       (,variable-def-regex 1 font-lock-variable-name-face)
       (,variable-store-regex 1 font-lock-variable-name-face)
       (,important-punct-regex 0 font-lock-warning-face)
       ))
    ))

;;;###autoload
(define-derived-mode sil-mode prog-mode "SIL"
  "Major mode for textual SIL files."
  :group 'sil-mode
  (setq font-lock-defaults sil--font-lock-default)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "//+ *")
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sil" . sil-mode))

(provide 'sil-mode)

;;; sil-mode.el ends here
