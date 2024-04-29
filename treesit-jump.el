;;; treesit-jump.el --- Jump around code using treesit -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Donovan Miller
;; URL: https://github.com/dmille56/treesit-jump
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (avy "0.4") (transient "0.5.3") (gptel "0.8.5"))
;; Keywords: treesit, treesitter, avy, jump, matching, gptel

;;; Commentary:

;; Requires Emacs 29+ for treesitter support
;; Requires avy and transient

;;; Code:

;; Notes:
;; :TODO: test different queries per language and make sure that they can compile and work... Tested: Python, C, C++, Java, C#, JavaScript, TypeScript, Go, Rust
;; :TODO: refactor: jumping between parents of the node under your cursor
;; :TODO: add better visual selection between regions

;; Useful links:
;; https://github.com/emacs-mirror/emacs/blob/master/admin/notes/tree-sitter/starter-guide
;; https://git.sr.ht/~meow_king/ts-query-highlight

(require 'treesit)
(require 'cl-lib)
(require 'avy nil 'noerror)
(require 'gptel)
(require 'map)

;;;###autoload
(eval-and-compile (require 'transient)) ;; use eval-and-compile to fix byte-compile issues with transient macro

;;;###autoload
(transient-define-prefix treesit-jump-transient ()
  "Transient for using treesit-jump."
  ["Treesit-Jump"
   ("j" "jump" treesit-jump-jump)
   ("s" "select" treesit-jump-select)
   ("d" "delete" treesit-jump-delete)
   ("p" "parent jump" treesit-jump-parent-jump)
   ("g" "gptel describe" treesit-jump-gptel-describe)
   ("c" "clear cache" treesit-jump-queries-clear-cache)])

(defgroup treesit-jump nil
  "Customize group for treesit-jump.el."
  :group 'emacs)

(defcustom treesit-jump-queries-filter-list nil
  "Query captures to filter out of results uses regex for all modes."
  :type '(repeat string)
  :group 'treesit-jump)

(defcustom treesit-jump-queries-filter-mode-alist nil
  "Query captures to filter out of results using regex for each mode."
  :group 'treesit-jump
  :type '(alist :key-type (symbol) :value-type '(repeat string)))

(defcustom treesit-jump-queries-filter-func #'treesit-jump--queries-filter-default-func
  "Function used to filter matched treesit queries."
  :type 'function
  :group 'treesit-jump)

(defcustom treesit-jump-positions-select-fun #'avy-process
  "Function used to select matched treesit queries on screen."
  :type 'function
  :group 'treesit-jump)

(defcustom treesit-jump-queries-extra-alist nil
  "Alist that maps major modes to extra queries to search for."
  :group 'treesit-jump
  :type '(alist :key-type (symbol) :value-type '(repeat string)))

(defcustom treesit-jump-code-describe-prompt "You are an expert programmer.  Describe this code."
  "Prompt string to use when describing code using treesit-jump."
  :group 'treesit-jump
  :type 'string)

(defcustom treesit-jump-code-gpt-seperator-face font-lock-keyword-face
  "Face to use to seperate gpt reponses."
  :group 'treesit-jump
  :type 'face)

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
                          (csharp-mode . "c-sharp")
                          (csharp-ts-mode . "c-sharp")
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
                          (js-ts-mode . "javascript")
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
                          (tsx-mode . "tsx")
                          (tsx-ts-mode . "tsx")
                          (typescript-mode . "typescript")
                          (typescript-ts-mode . "typescript")
                          (verilog-mode . "verilog")
                          (zig-mode . "zig"))))
  (setf (map-elt treesit-jump-major-mode-language-alist
                 major-mode) lang-symbol))

(defvar treesit-jump-queries-dir (funcall (lambda ()
                                          (file-name-as-directory
                                           (concat (file-name-directory
                                                    (or load-file-name buffer-file-name (symbol-file 'treesit-jump-queries-dir)))
                                                   "treesit-queries")))))

(defvar treesit-jump-queries-cache (make-hash-table :test 'equal))
(defvar treesit-jump-queries-extra-cache (make-hash-table :test 'equal))

(defvar treesit-jump--gpt-buffer-name "treesit-jump-gpt-buffer")

(defun treesit-jump--get-or-create-buffer (buffer-name)
  "Get BUFFER-NAME if it exists, otherwise create it."
  (or (get-buffer buffer-name)
      (generate-new-buffer buffer-name)))

;;;###autoload
(defun treesit-jump-queries-clear-cache ()
  "Clear the queries cache."
  (interactive)
  (setq treesit-jump-queries-cache (make-hash-table :test 'equal))
  (setq treesit-jump-queries-extra-cache (make-hash-table :test 'equal))
  (message "Treesit-jump cache cleared."))

(defun treesit-jump--queries-filter-default-func (query)
  "Filter out results from the `QUERY' that are in the query filter list."
  (let* (
        (capture-name (symbol-name (car query)))
        (major-mode-filter-list (alist-get major-mode treesit-jump-queries-filter-mode-alist))
        (filter-list (append treesit-jump-queries-filter-list major-mode-filter-list))
        (matches (seq-filter (lambda (s) (string-match s capture-name)) filter-list)))
    (if matches nil t)))

(defun treesit-jump--query-get-captures (query-list)
  "Get visible treesit captures from a `QUERY-LIST'."
  (let* (
         (start-window (window-start))
         (end-window (window-end (selected-window) t))
         (root-node (treesit-buffer-root-node))
         (raw-captures (apply #'append (mapcar (lambda (query) (treesit-query-capture root-node query start-window end-window)) query-list)))
         (captures (seq-filter (lambda (x) (funcall treesit-jump-queries-filter-func x)) raw-captures)))
    captures))

(defun treesit-jump--query-select (query-list)
  "Get captures based upon the `QUERY-LIST' and then return the user selected one."
  (let* (
         (captures (treesit-jump--query-get-captures query-list))
         (positions (sort (mapcar #'treesit-node-start (mapcar #'cdr captures)) #'<))
         (selected-pos (funcall treesit-jump-positions-select-fun positions)))
    (if selected-pos (cl-find-if (lambda (x) (= (treesit-node-start (cdr x)) selected-pos)) captures) nil)))

(defun treesit-jump--query-select-go-to (query-list)
  "Input a `QUERY-LIST' select a capture from it and go to it."
  (interactive)
  (let* (
         (selected (treesit-jump--query-select query-list))
         (start (treesit-node-start (cdr selected))))
    (when start
      (goto-char start))))

(defun treesit-jump--query-select-visual (query-list)
  "Input a `QUERY-LIST' select a capture from it and select it's region."
  (interactive)
  (let* (
         (selected (treesit-jump--query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected))))
    (when (and start end)
          (goto-char start)
          (set-mark end))))

(defun treesit-jump--query-select-delete (query-list)
  "Input a `QUERY-LIST' select a capture from it and delete it."
  (interactive)
  (let* (
         (selected (treesit-jump--query-select query-list))
         (start (treesit-node-start (cdr selected)))
         (end (treesit-node-end (cdr selected))))
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
  "Get treesit query for `LANGUAGE' from `QUERIES-DIR'.
`TOP-LEVEL': is used to mention if we should load optional inherits."
  (let (
        (filename (concat queries-dir language "/textobjects.scm")))
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
                                                         (treesit-jump--get-query-from-dir (substring x 1 -1)
                                                                                                       queries-dir nil))
                                                   (treesit-jump--get-query-from-dir x queries-dir nil)))
                                               (split-string inherits-line ","))
                                       "\n"))))
            (buffer-string))))))

(defun treesit-jump--get-query-from-cache-or-dir (language queries-dir top-level)
  "Get treesit query for the `LANGUAGE' from the `QUERIES-DIR'.
`TOP-LEVEL': should we load optional inherits.  Using caching."
  (let (
         (cache-res (gethash language treesit-jump-queries-cache nil))
         (query-res nil))
    (if (not cache-res)
        (progn
          (setq query-res (treesit-jump--get-query-from-dir language queries-dir top-level))
          (puthash language (treesit-query-compile (intern language) query-res) treesit-jump-queries-cache)
          (gethash language treesit-jump-queries-cache nil))
      cache-res)))

(defun treesit-jump--get-extra-queries (language)
  "Get extra queries from the `LANGUAGE' and current major-mode."
  (let (
         (lang-symbol (intern language))
         (cache-res (gethash major-mode treesit-jump-queries-extra-cache nil))
         (query-res nil))
    (if (not cache-res)
        (progn
          (setq query-res (alist-get major-mode treesit-jump-queries-extra-alist))
          (puthash major-mode (mapcar (lambda (x) (treesit-query-compile lang-symbol x)) query-res) treesit-jump-queries-extra-cache)
          (gethash major-mode treesit-jump-queries-extra-cache nil))
      cache-res)))

(defun treesit-jump-get-and-process-captures (query-process-func)
  "Get captures and process them with the `QUERY-PROCESS-FUNC'."
  (interactive)
  (let (
        (lang-name (alist-get major-mode treesit-jump-major-mode-language-alist)))

    (unless (treesit-available-p)
      (error "Treesit is not available... Check and make sure you're using Emacs 29+"))

    (unless lang-name
      (error (format "Treesit-jump does not support mode: %s" major-mode)))

    (unless (treesit-language-available-p (intern lang-name))
      (error (format "Treesit is not available for language %s" lang-name)))
    
    (unless (file-directory-p treesit-jump-queries-dir)
      (error (format "Treesit-queries directory not found at: %s. Make sure your config include the treesit-queries directory if using straight." treesit-jump-queries-dir)))
    
    (let* (
           (queries-dir treesit-jump-queries-dir)
           (query (treesit-jump--get-query-from-cache-or-dir lang-name queries-dir t))
           (extra-queries (treesit-jump--get-extra-queries lang-name))
           (queries-list (append (list query) extra-queries)))
      (funcall query-process-func queries-list))))

;;;###autoload
(defun treesit-jump-jump ()
  "Select and jump to a treesit query for the current major-mode."
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump--query-select-go-to))

;;;###autoload
(defun treesit-jump-select ()
  "Select and select the region of a treesit query for the current major-mode."
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump--query-select-visual))

;;;###autoload
(defun treesit-jump-delete ()
  "Select and delete the region of a treesit query for the current major-mode."
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump--query-select-delete))

(defun treesit-jump--gptel-callback (response info)
  "Callback function from calling gptel-request.
Outputs the RESPONSE to a new buffer.  INFO unused for now."
  (if response
      (let ((buffer (treesit-jump--get-or-create-buffer treesit-jump--gpt-buffer-name)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n")
          (let ((start (point)))
            (insert "----------------------------------------------------------------------------\n")
            (put-text-property start (point) 'face treesit-jump-code-gpt-seperator-face))
          (insert "\n")
          (insert response)
          (insert "\n")
          (if (> (length (window-list)) 1)
              (other-window 1) (split-window))
          (switch-to-buffer buffer)
          (goto-char (point-max))
          (recenter -1)))))

;;;###autoload
(defun treesit-jump-gptel-describe ()
  "Select and select the region of a treesit query for the current major-mode."
  (interactive)
  (treesit-jump-get-and-process-captures #'treesit-jump--query-select-visual)
  (if (use-region-p)
      (progn
        (gptel-request nil :system treesit-jump-code-describe-prompt :callback #'treesit-jump--gptel-callback)
        (deactivate-mark))))

(defun treesit-jump-get-parent-nodes-from-point ()
  "Get a list of the parents of nodes of treesit node at the current point."
  (let* (
         (node (treesit-node-at (point)))
         (parent (treesit-node-parent node))
         (node-list (list node)))
    (while parent
      (setq node-list (append node-list (list parent)))
      (setq parent (treesit-node-parent parent)))
    node-list))

(defun treesit-jump--parent-select (node-list)
  "Select a node from the `NODE-LIST'."
  (let* (
         (positions (sort (mapcar #'treesit-node-start node-list) #'<))
         (selected-pos (funcall treesit-jump-positions-select-fun positions)))
    (if selected-pos (cl-find-if (lambda (x) (= (treesit-node-start x) selected-pos)) node-list) nil)))

;;;###autoload
(defun treesit-jump-parent-jump ()
  "Select and jump to a treesit parent of the current node."
  (interactive)
  (let* (
         (node-list (treesit-jump-get-parent-nodes-from-point))
         (selected (treesit-jump--parent-select node-list))
         (start (treesit-node-start selected)))
    (when start
      (goto-char start))))

(provide 'treesit-jump)
;;; treesit-jump.el ends here
