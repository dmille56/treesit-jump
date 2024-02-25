;;; treesit-jump --- Summary

;; Jump around using treesitter

;; requires Emacs 29+ for treesitter support
;; requires avy

;;; Commentary:

;; Notes:
;; :TODO: add jumping between parents of the node under your cursor
;; :TODO: test different queries per language and make sure that they can compile
;; :TODO: add documentation for each function
;; :TODO: add override queries for each language
;; :TODO: add compiled queries using treesit-query-compile for faster searching

;; Useful links:
;; https://github.com/emacs-mirror/emacs/blob/master/admin/notes/tree-sitter/starter-guide
;; https://git.sr.ht/~meow_king/ts-query-highlight

;;; Code:

(require 'treesit)
(require 'avy)
(require 'cl-lib)

(defgroup treesit-jump nil
  "Customize group for treesit-jump.el."
  :group 'emacs)

(defcustom treesit-jump-queries-filter-list '("inner" "test" "param")
  "Query captures to filter out of results uses regex."
  :type '(repeat string)
  :group 'treesit-jump)

(defcustom treesit-jump-queries-filter-func #'treesit-jump-queries-filter-default-func
  "Function used to filter matched treesit queries."
  :type 'function
  :group 'treesit-jump)

(defcustom treesit-jump-positions-filter-func #'avy-process
  "Function used to select matched treesit queries on screen."
  :type 'function
  :group 'treesit-jump)

(defcustom treesit-jump-major-mode-language-alist nil
  "Alist that maps major modes to tree-sitter language names."
  :group 'treesit-jump
  :type '(alist :key-type symbol
                :value-type string))
(pcase-dolist (`(,major-mode . ,lang-symbol)
               (reverse '((c++-mode . "cpp")
                          (c++-ts-mode . "cpp")
                          (c-mode . "c")
                          (c-ts-mode . "c")
                          (csharp-mode . "csharp")
                          (csharp-ts-mode . "csharp")
                          (elixir-mode . "elixir")
                          (elixir-ts-mode . "elixir")
                          (elm-mode . "elm")
                          (elm-ts-mode . "elm")
                          (ess-r-mode . "r")
                          (go-mode . "go")
                          (go-ts-mode . "go")
                          (haskell-mode . "haskell")
                          (haskell-ts-mode . "haskell")
                          (html-mode . "html")
                          (html-ts-mode . "html")
                          (java-mode . "java")
                          (java-ts-mode . "java")
                          (javascript-mode . "javascript")
                          (javascript-ts-mode . "javascript")
                          (js-mode . "javascript")
                          (js-ts-mode . "js")
                          (js2-mode . "javascript")
                          (js3-mode . "javascript")
                          (julia-mode . "julia")
                          (julia-ts-mode . "julia")
                          (matlab-mode . "matlab")
                          (php-mode . "php")
                          (php-ts-mode . "php")
                          (powershell-mode . "powershell")
                          (powershell-ts-mode . "powershell")
                          (prisma-mode . "prisma")
                          (prisma-ts-mode . "prisma")
                          (python-mode . "python")
                          (python-ts-mode . "python")
                          (rjsx-mode . "javascript")
                          (ruby-mode . "ruby")
                          (ruby-ts-mode . "ruby")
                          (rust-mode . "rust")
                          (rust-ts-mode . "rust")
                          (rustic-mode . "rust")
                          (sh-mode . "bash")
                          (bash-ts-mode . "sh")
                          (shell-script-mode . "bash")
                          (typescript-mode . "typescript")
                          (typescript-ts-mode . "typescript")
                          (verilog-mode . "verilog")
                          (zig-mode . "zig"))))
  (setf (map-elt treesit-jump-major-mode-language-alist
                 major-mode) lang-symbol))

(setq treesit-jump-queries-dir (funcall (lambda ()
                                          (file-name-as-directory
                                           (concat (file-name-directory
                                                    (or load-file-name buffer-file-name (symbol-file 'treesit-jump-queries-dir)))
                                                   "treesit-queries")))))

(defun treesit-jump-queries-filter-default-func (query)
  (let* (
        (capture-name (symbol-name (car query)))
        (matches (seq-filter (lambda (s) (string-match s capture-name)) treesit-jump-queries-filter-list))
        )
    (if matches nil t)
    ))

(defun treesit-jump-query-get-captures (query-list)
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (raw-captures (apply #'append (mapcar (lambda (query) (treesit-query-capture root-node query start-window end-window)) query-list)))
         (captures (seq-filter (lambda (x) (funcall treesit-jump-queries-filter-func x)) raw-captures))
         )
    captures
    ))

(defun treesit-jump-query-select (query-list)
  (let* (
         (captures (treesit-jump-query-get-captures query-list))
         (positions (sort (mapcar #'treesit-node-start (mapcar #'cdr captures)) #'<))
         (selected-pos (funcall treesit-jump-positions-filter-func positions))
         )
    (if selected-pos (cl-find-if (lambda (x) (= (treesit-node-start (cdr x)) selected-pos)) captures) nil)
    ))

(defun treesit-jump-query-select-go-to (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         )
    (when start
      (goto-char start)
      )))

(defun treesit-jump-query-select-visual (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected)))
         )
    (when (and start end)
          (goto-char start)
          (set-mark end)
         )))

(defun treesit-jump-query-select-delete (query-list)
  (interactive)
  (let* (
         (selected (treesit-jump-query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected)))
         )
    (when (and start end)
      (delete-region start end))))

(defun treesit-jump--get-inherits-line (filename)
  "Get the inherits line from `FILENAME'.
It might not be on the fist line and so we cannot just get the first line."
  (with-temp-buffer
    (if (file-exists-p filename)
        (progn
          (insert-file-contents filename)
          (goto-char (point-min))
          (search-forward "; inherits: " nil t)
          (let ((line (thing-at-point 'line t)))
            (if (string-match "^; inherits: \\([a-z_,()]+\\)$" line)
                (match-string 1 line)))))))

(defun treesit-jump--get-query-from-dir (language queries-dir top-level)
  "Get tree sitter query for `LANGUAGE' from `QUERIES-DIR'.
`TOP-LEVEL' is used to mention if we should load optional inherits."
  (let (
        (filename (concat queries-dir language "/textobjects.scm"))
        )
    (with-temp-buffer
      (if (file-exists-p filename)
          (progn
            (insert-file-contents filename)
            (goto-char (point-min))
            (let ((inherits-line (treesit-jump--get-inherits-line filename)))
              (if inherits-line
                  (insert (string-join (mapcar (lambda (x)
                                                 (if (string-prefix-p "(" x)
                                                     (if top-level
                                                         (treesit-jump--get-queries-from-dir (substring x 1 -1)
                                                                                                       queries-dir nil))
                                                   (treesit-jump--get-queries-from-dir x queries-dir nil)))
                                               (split-string inherits-line ","))
                                       "\n"))))
            (buffer-string))))))

(defun treesit-jump-get-and-process-captures (query-process-func)
  (interactive)
  (let* (
        (lang-name (alist-get major-mode treesit-jump-major-mode-language-alist))
        (queries-dir treesit-jump-queries-dir)
        (query (treesit-jump--get-query-from-dir lang-name queries-dir t))
        (queries-list (list query))
        )
    (funcall query-process-func queries-list)
    ))

(defun treesit-jump-jump ()
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump-query-select-go-to))

(defun treesit-jump-select ()
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump-query-select-visual))

(defun treesit-jump-delete ()
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump-query-select-delete))

;; :TODO: remove this global set-key
(global-set-key (kbd "<f9>") 'treesit-jump-jump)

(provide 'treesit-jump)
;;; treesit-jump.el ends here
