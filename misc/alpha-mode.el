(require 'utils)
(require 'Lisp)

(defvar alpha-mode-hook nil "Contains the hooks called at Alpha Mode startup")

(defconst alpha-special-forms (list "alter" "bind" "choose" "do"
                                    "<-" "->"
                                    "id" "@" "#"))
(defconst alpha-decls (list "verb" "noun" "lang"))
(defconst alpha-builtins (list ">" "<" ">=" "<=" "=" "<>"
                               "+" "-" "*" "/" "%"
                               "&" "|" "x|" "not"
                               "?"))
(defconst alpha-default-offset 2)

(defconst alpha-mode-keywords
  (list
   (cons 'alpha-search-comment font-lock-comment-face)
   (cons 'alpha-search-string font-lock-string-face)
   (cons (regexp-opt alpha-special-forms 'words) font-lock-keyword-face)
   (cons (regexp-opt alpha-decls 'words) font-lock-variable-name-face)
   (cons (regexp-opt alpha-builtins 'words) font-lock-builtin-face)
   (cons "_" font-lock-constant-face)))
(defconst alpha-mode-syntax-table
  (with-syntax-table (make-char-table 'syntax-table)
    (let ((set-syntax-list (lambda (es v) (mapc (lambda (e) (modify-syntax-entry e v)) es)) ))
      (funcall set-syntax-list '((0 . 255)) "w")
      (funcall set-syntax-list '(?\( ?\[ ?{) "(")
      (funcall set-syntax-list '(?\  ?\t ?\n) " ")
      (funcall set-syntax-list '(?\) ?\] ?}) ")")
      (funcall set-syntax-list '(?. ?: ?\, ?\; ?_) ".")
      (funcall set-syntax-list '(?\\) "\\")
      (funcall set-syntax-list '(?\") "\"")
      (syntax-table))))
(defconst alpha-mode-map
  (with-local-map (copy-keymap lisp-mode-map)
    (local-set-key (kbd "C-M-q") 'alpha-indent-sexp)
    (local-set-key (kbd "M-<up>") 'alpha-rotate-parens)
    (local-set-key (kbd "M-<down>") 'alpha-rotate-parens-inv)
    (current-local-map)))

(defvar alpha-basic-offset nil)

(defun alpha-backward-up-tightexpr ()
  (skip-syntax-forward " .")
  (while (looking-back "\\S-\\(\\s-*\\s.\\s-*\\)?") (backward-sexp)))

(defun alpha-basic-offset ()
  (if (integerp alpha-basic-offset) alpha-basic-offset alpha-default-offset))
(defun alpha-indent-line ()
  (let ((inc (alpha-basic-offset))
        (inhibit-point-motion-hooks t))
    (save-excursion
      (indent-line-to
       (condition-case nil
           (save-excursion
             (beginning-of-line)
             (if (looking-at "\\s-*\\s.")
                 (progn
                   (let ((here (point))
                         start)
                     (alpha-backward-up-tightexpr)
                     (setq start (point))
                     (search-forward-regexp "\\s." here t)
                     (backward-char)
                     (if (looking-at "\\s.")
                         (current-column)
                       (goto-char start)
                       (+ inc (current-column)))))
               (if (looking-at "[ \t]*\\s)") (setq inc 0))
               (backward-up-list)
               (forward-char)
               (skip-chars-forward " \t")
               (if (not (looking-at "\n")) (current-column)
                 (+ (current-indentation) inc))))
         (scan-error 0))))
    (when (looking-back "^[ \t]*") (skip-chars-forward " \t"))))
(defun alpha-indent-sexp ()
  (interactive)
  (let ((end (save-excursion (search-forward "\n") (backward-up-list) (forward-sexp) (point))))
    (indent-region (point) end)))

(defun alpha-rotate-parens-base (alist)
  (save-excursion
    (skip-syntax-forward "-")
    (awhen (assq (following-char) alist)
      (let ((beg (point))) 
        (forward-sexp)
        (delete-char -1)
        (insert-char (cddr it) 1)
        (goto-char beg)
        (delete-char 1)
        (insert-char (cadr it) 1)))))
(fset 'alpha-rotate-parens (command (curry alpha-rotate-parens-base '((?\( ?\[ . ?\]) (?\[ ?{ . ?}) (?{ ?\( . ?\))))))
(fset 'alpha-rotate-parens-inv (command (curry alpha-rotate-parens-base '((?\( ?{ . ?}) (?\[ ?\( . ?\)) (?{ ?\[ . ?\])))))

(defun alpha-search-string (&optional lim)
  (interactive)
  (and (search-forward-regexp "\\_<'\\|\"" lim t)
       (prog1 t (when (looking-back "\"") (backward-char)))
       (let* ((start (point)) (sep (following-char)) (prev (1- start)))
         (unless (looking-back "'") (setq prev start))
         (forward-char)
         (and (search-forward (string sep) nil t)
              (prog1 t
                (put-text-property start (point) 'font-lock-multiline t) 
                (set-match-data (list prev (point) start (point))))))))
(defun alpha-search-comment (&optional lim)
  (interactive)
  (and (search-forward-regexp "\\_<~" lim t)
       (let ((start (1- (point))))
         (while (not (looking-at "\\s-\\|\\s.\\|\\'")) (forward-sexp))
         (prog1 t 
           (put-text-property start (point) 'font-lock-multiline t)
           (set-match-data (list start (point)))))))

(defadvice forward-sexp (around alpha-forward-string activate compile)
  (if (not (eq major-mode 'alpha-mode)) ad-do-it
    (skip-syntax-forward " .")
    (if (looking-at "'") (alpha-search-string)
      ad-do-it)))
  
(defun alpha-font-lock-extend-string-region ()
  (save-excursion
    (goto-char font-lock-beg)
    (when (alpha-search-string font-lock-end)
      (when (< font-lock-end (match-end 0))
        (setq font-lock-end (match-end 0))))))

;;;###autoload (autoload 'alpha-mode "alpha-mode")
(define-derived-mode alpha-mode lisp-mode "Alpha"
  "Major mode for editing Alpha source files.\n\n\\{alpha-mode-map}"
  :syntax-table alpha-mode-syntax-table
  (run-mode-hooks 'alpha-mode-hook)
  (use-local-map alpha-mode-map)
  (add-to-list 'font-lock-extend-region-functions 'alpha-font-lock-extend-string-region)
  (set (make-local-variable 'font-lock-defaults) '(alpha-mode-keywords t))
  (set (make-local-variable 'indent-line-function) 'alpha-indent-line))
