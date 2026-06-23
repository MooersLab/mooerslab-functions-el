;;; mooerslab.el --- A collection of utility functions to improve our workflows.

;; Copyright (C) 2025 Blaine Mooers and the University of Oklahoma Board of Regents

;; Author: blaine-mooers@ou.edu
;; Maintainer: blaine-mooers@ou.edu
;; URL: https://github.com/MooersLab/mooerslab-functions-el
;; Version: 0.7
;; Keywords: data, pdb
;; License: MIT
;; Updated 2025 August 20

;;; This package is known to work (insofar as it's tested) with Emacs 30.1.

;;; Code:
; These functions work by:
;
; Taking a selected region containing filenames (one per line)
; Sorting the filenames alphabetically
; Parsing each filename to extract author, year, and title
; Using the same character-by-character processing logic to add spaces
; Formatting each as an org-mode link with a bullet point
; Replacing the region with the formatted org-mode links
;
; To use them:
;
; Select a region containing PDF or EPUB or CHM filenames (one per line)
; Run M-x mooerslab-book-file-list-to-org-links for books
; Or run M-x mooerslab-paper-file-list-to-org-links for papers

(define-prefix-command 'mooerslab-bib-prefix)
(keymap-global-set "C-c b" 'mooerslab-bib-prefix)
(keymap-set mooerslab-bib-prefix "d" #'doi-add-bibtex-entry)
;; room to grow, all under the same "C-c b" branch:
;; (keymap-set mooerslab-bib-prefix "f" #'doi-add-bibtex-entry-from-pdf)
;; (keymap-set mooerslab-bib-prefix "k" #'my-insert-citekey)


;;; mooerslab-org-roam-retype-refile
(defun mooerslab-org-roam-retype-refile (new-type)
  "Refile current org-roam note to a different type and subfolder (e.g., main (i.e., permanent), \n (i.e., hub), structure (i.e., synthesis), keyword, project, and literature (i.e., reference)). \n Retyping chages the fitletag in the frontmatter of the note."
  (interactive
   (list
    (completing-read "New type: " '("main" "hub" "structure" "keyword" "literature" "protocols" "eab" "projects" "areas" "resources" "archives"))))
  (when (and buffer-file-name
             (string-match "/org-roam/\\([^/]+\\)/" buffer-file-name))
    (let* ((old-type (match-string 1 buffer-file-name))
           (file-name (file-name-nondirectory buffer-file-name))
           (new-path (concat (file-name-directory (directory-file-name (file-name-directory buffer-file-name)))
                            new-type
                            "/"
                            file-name)))
      ;; Update filetags
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+filetags:.*$" nil t)
          (replace-match (concat "#+filetags: :" new-type ":") nil nil)))
      ;; Save the buffer
      (save-buffer)
      ;; Move the file
      (rename-file buffer-file-name new-path t)
      ;; Visit the new file
      (find-file new-path)
      ;; Sync the database
      (org-roam-db-sync)
      (message "Note retyped and refiled to %s" new-type))))

;; Add this to your org-roam configuration
(bind-key "C-c n r" #'mooerslab-org-roam-retype-refile)





(defun mooerslab-latex-insert-screenshot (filename)
  "Capture a screenshot and insert it into a LaTeX figure environment. Wraps filepath with figure environment."
  (interactive "sEnter figure name: ")
  (let* ((dir "figures/")
         (path (concat dir filename ".png")))
    (unless (file-exists-p dir) (make-directory dir))
    (shell-command (concat "screencapture -i " path)) ; macOS command
    (insert (format "\\begin{figure}[ht]
    \\centering
    \\includegraphics[width=0.8\\textwidth]{%s}
    \\caption{Add caption here}
    \\label{fig:%s}
\\end{figure}" path filename))))


(defun mooerslab-switch-pdf-reader ()
  "Toggle between reader-mode and pdf-tools for PDF files."
  (interactive)
  (let ((filename buffer-file-name)
        (line (line-number-at-pos))
        (col (current-column)))
    (cond
     ((eq major-mode 'reader-mode)
      (kill-buffer)
      (find-file filename)
      (pdf-view-mode)
      (message "Switched to pdf-tools"))
     ((eq major-mode 'pdf-view-mode)
      (kill-buffer)
      (find-file filename)
      (reader-mode)
      (message "Switched to reader-mode"))
     (t (message "Not in a PDF reader mode")))))

;; Define the prefix keymap first
(define-prefix-command 'mooerslab-toggle-map)
(global-set-key (kbd "C-c t") 'mooerslab-toggle-map)

;; Now you can add keys to it
(define-key mooerslab-toggle-map (kbd "t") 'my-append-todo-to-heading)
(define-key mooerslab-toggle-map (kbd "p") 'mooerslab-switch-pdf-reader)


(global-set-key (kbd "C-c t p") 'mooerslab-switch-pdf-reader)


(defun mooerslab-kill-parens-and-contents ()
"Kill enclosing parentheses and their contents when point is inside."
(interactive)
(backward-up-list)
(kill-sexp))
(global-set-key (kbd "C-c (") #'mooerslab-kill-parens-and-contents)


(defun mooerslab-org-link-to-latex-href ()
  "Convert the Org-mode link at point [[url][desc]]
into a LaTeX \\href{url}{desc}.
Must be in org-mode.

If the link has no description ([[url]]), use the URL as the text."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (let* ((pt (point))
           start end)
      ;; Find the surrounding [[ ... ]]
      (unless (and (search-backward "[[" nil t)
                   (setq start (point))
                   (search-forward "]]" nil t)
                   (setq end (point))
                   (<= start pt)
                   (>= end pt))
        (user-error "Point is not inside an Org link [[...]]"))
      ;; Extract inside of [[ ... ]]
      (let* ((inner (buffer-substring-no-properties (+ start 2) (- end 2)))
             ;; Split on the literal sequence "]["
             (parts (split-string inner "]\\[")) ; regexp, so we escape the brackets
             (url   (car parts))
             (desc  (or (cadr parts) url)))
        (goto-char start)
        (delete-region start end)
        (insert (format "\\href{%s}{%s}" url desc))))))

(defun mooerslab-org-links-region-to-latex-href (beg end)
  "Convert all Org-mode links [[url][desc]] in region to LaTeX \\href{url}{desc}.
Must be in org-mode.
If a link has no description ([[url]]), use the URL as the text."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward "[[" nil t)
        (let ((start (match-beginning 0)))
          (when (search-forward "]]" nil t)
            (let* ((end   (point))
                   (inner (buffer-substring-no-properties (+ start 2) (- end 2)))
                   ;; Split on the literal sequence "]["
                   (parts (split-string inner "]\\["))
                   (url   (car parts))
                   (desc  (or (cadr parts) url)))
              (goto-char start)
              (delete-region start end)
              (insert (format "\\href{%s}{%s}" url desc)))))))))


(defun mooerslab-book-file-list-to-org-links ()
  "Convert a list of book filenames in region to org-mode links.
Each line should contain a filename in the format:
AuthorYEARTitle.pdf or AuthorYEARTitle.epub
The function will:
1. Sort lines alphabetically
2. Parse each filename to extract author, year, and title
3. Format as org-mode links to /Users/blaine/0booksLabeled/

Example input:
Alpaydin2016MachineLearningTheNewAI.pdf
Johnson2006RSSandAtominActionWeb2BuildingBlocks.epub

Example output:
- [[file:/Users/blaine/0booksLabeled/Alpaydin2016MachineLearningTheNewAI.pdf][Alpaydin (2016) Machine Learning The New AI]]
- [[file:/Users/blaine/0booksLabeled/Johnson2006RSSandAtominActionWeb2BuildingBlocks.epub][Johnson (2006) RSSand Atomin Action Web2Building Blocks]]"
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (split-string (buffer-substring-no-properties start end) "\n" t))
         (sorted-lines (sort lines #'string<))
         (org-links '()))
    ;; Process each line
    (dolist (line sorted-lines)
      (when (string-match "\\`\\([A-Za-z]+\\)\\([0-9X]+\\)\\(.*\\)\\.\\(pdf\\|epub\\|chm\\)\\'" line)
        (let* ((author (match-string 1 line))
               (year (match-string 2 line))
               (title-raw (match-string 3 line))
               (extension (match-string 4 line))
               ;; Process title character by character with lookahead
               (title-spaced (let ((result "")
                                   (len (length title-raw)))
                               (dotimes (i len)
                                 (let ((char (aref title-raw i))
                                       (prev-char (if (> i 0) (aref title-raw (1- i)) nil))
                                       (next-char (if (< i (1- len)) (aref title-raw (1+ i)) nil)))
                                   ;; Add space before uppercase if:
                                   ;; - Previous char is lowercase OR digit
                                   ;; - AND current char is uppercase
                                   ;; - AND next char is lowercase (start of new word)
                                   (when (and prev-char
                                              next-char
                                              (or (and (>= prev-char ?a) (<= prev-char ?z))
                                                  (and (>= prev-char ?0) (<= prev-char ?9)))
                                              (and (>= char ?A) (<= char ?Z))
                                              (and (>= next-char ?a) (<= next-char ?z)))
                                     (setq result (concat result " ")))
                                   (setq result (concat result (char-to-string char)))))
                               result))
               ;; Clean up multiple spaces
               (title-clean (replace-regexp-in-string "  +" " " title-spaced))
               (org-link (format "- [[file:/Users/blaine/0booksLabeled/%s][%s (%s) %s]]"
                                 line
                                 author
                                 year
                                 title-clean)))
          (push org-link org-links))))
    ;; Replace region with org-mode links output
    (delete-region start end)
    (insert (mapconcat #'identity (nreverse org-links) "\n"))))

(defun mooerslab-paper-file-list-to-org-links ()
  "Convert a list of paper filenames in region to org-mode links.
Each line should contain a filename in the format:
AuthorYEARTitle.pdf or AuthorYEARTitle.epub
The function will:
1. Sort lines alphabetically
2. Parse each filename to extract author, year, and title
3. Format as org-mode links to /Users/blaine/0papersLabeled/

Example input:
Smith2020ProteinStructureDetermination.pdf
Jones2019CrystallographyMethods.epub

Example output:
- [[file:/Users/blaine/0papersLabeled/Jones2019CrystallographyMethods.epub][Jones (2019) Crystallography Methods]]
- [[file:/Users/blaine/0papersLabeled/Smith2020ProteinStructureDetermination.pdf][Smith (2020) Protein Structure Determination]]"
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (split-string (buffer-substring-no-properties start end) "\n" t))
         (sorted-lines (sort lines #'string<))
         (org-links '()))
    ;; Process each line
    (dolist (line sorted-lines)
      (when (string-match "\\`\\([A-Za-z]+\\)\\([0-9X]+\\)\\(.*\\)\\.\\(pdf\\|epub\\|chm\\)\\'" line)
        (let* ((author (match-string 1 line))
               (year (match-string 2 line))
               (title-raw (match-string 3 line))
               (extension (match-string 4 line))
               ;; Process title character by character with lookahead
               (title-spaced (let ((result "")
                                   (len (length title-raw)))
                               (dotimes (i len)
                                 (let ((char (aref title-raw i))
                                       (prev-char (if (> i 0) (aref title-raw (1- i)) nil))
                                       (next-char (if (< i (1- len)) (aref title-raw (1+ i)) nil)))
                                   ;; Add space before uppercase if:
                                   ;; - Previous char is lowercase OR digit
                                   ;; - AND current char is uppercase
                                   ;; - AND next char is lowercase (start of new word)
                                   (when (and prev-char
                                              next-char
                                              (or (and (>= prev-char ?a) (<= prev-char ?z))
                                                  (and (>= prev-char ?0) (<= prev-char ?9)))
                                              (and (>= char ?A) (<= char ?Z))
                                              (and (>= next-char ?a) (<= next-char ?z)))
                                     (setq result (concat result " ")))
                                   (setq result (concat result (char-to-string char)))))
                               result))
               ;; Clean up multiple spaces
               (title-clean (replace-regexp-in-string "  +" " " title-spaced))
               (org-link (format "- [[file:/Users/blaine/0papersLabeled/%s][%s (%s) %s]]"
                                 line
                                 author
                                 year
                                 title-clean)))
          (push org-link org-links))))
    ;; Replace region with org-mode links output
    (delete-region start end)
    (insert (mapconcat #'identity (nreverse org-links) "\n"))))



(defun mooerslab-book-pdf-to-org-link (pdf-path)
  "Convert a PDF file path to an org-mode link.
The link description is derived from the filename by:
1. Removing the .pdf extension
2. Parsing author, year, and title
3. Adding spaces to separate CamelCase appropriately."
  (interactive "fSelect PDF file: ")
  (let* ((filename (file-name-nondirectory pdf-path))
         (name-sans-ext (file-name-sans-extension filename))
         org-link)
    (if (string-match "\\`\\([A-Za-z]+\\)\\([0-9X]+\\)\\(.*\\)\\'" name-sans-ext)
        (let* ((author (match-string 1 name-sans-ext))
               (year (match-string 2 name-sans-ext))
               (title-raw (match-string 3 name-sans-ext))
               ;; Process title character by character with lookahead

               (title-spaced (let ((result "")
                                   (len (length title-raw)))
                               (dotimes (i len)
                                 (let ((char (aref title-raw i))
                                       (prev-char (if (> i 0) (aref title-raw (1- i)) nil))
                                       (next-char (if (< i (1- len)) (aref title-raw (1+ i)) nil)))
                                   ;; Add space before uppercase if:
                                   ;; - Previous char is lowercase OR digit
                                   ;; - AND current char is uppercase
                                   ;; - AND next char is lowercase (start of new word)
                                   (when (and prev-char
                                              next-char
                                              (or (and (>= prev-char ?a) (<= prev-char ?z))
                                                  (and (>= prev-char ?0) (<= prev-char ?9)))
                                              (and (>= char ?A) (<= char ?Z))
                                              (and (>= next-char ?a) (<= next-char ?z)))
                                     (setq result (concat result " ")))
                                   (setq result (concat result (char-to-string char)))))
                               result))
               ;; Clean up multiple spaces
               (title-clean (replace-regexp-in-string "  +" " " title-spaced))
               (description (format "%s (%s) %s" author year title-clean)))
          (setq org-link (format "[[file:%s][%s]]" pdf-path description)))
      ;; Fallback if filename does not match expected pattern
      (setq org-link (format "[[file:%s][%s]]" pdf-path name-sans-ext)))
    (if (called-interactively-p 'any)
        (progn
          (insert org-link)
          (message "Inserted org link for: %s" filename))
      org-link)))

(defun mooerslab-book-pdf-to-org-link-at-point ()
  "Insert an org-mode link for a PDF file selected interactively.
Defaults to /Users/blaine/0booksLabeled/ directory."
  (interactive)
  (let ((default-directory "/Users/blaine/0booksLabeled/"))
    (call-interactively #'mooerslab-book-pdf-to-org-link)))


(defun mooerslab-paper-pdf-to-org-link (pdf-path)
  "Convert a PDF file path to an org-mode link.
The link description is derived from the filename by:
1. Removing the .pdf extension
2. Parsing author, year, and title
3. Adding spaces to separate CamelCase appropriately."
  (interactive "fSelect PDF file: ")
  (let* ((filename (file-name-nondirectory pdf-path))
         (name-sans-ext (file-name-sans-extension filename))
         org-link)
    (if (string-match "\\`\\([A-Za-z]+\\)\\([0-9X]+\\)\\(.*\\)\\'" name-sans-ext)
        (let* ((author (match-string 1 name-sans-ext))
               (year (match-string 2 name-sans-ext))
               (title-raw (match-string 3 name-sans-ext))
               ;; Process title character by character with lookahead
               (title-spaced (let ((result "")
                                   (len (length title-raw)))
                               (dotimes (i len)
                                 (let ((char (aref title-raw i))
                                       (prev-char (if (> i 0) (aref title-raw (1- i)) nil))
                                       (next-char (if (< i (1- len)) (aref title-raw (1+ i)) nil)))
                                   ;; Add space before uppercase if:
                                   ;; - Previous char is lowercase OR digit
                                   ;; - AND current char is uppercase
                                   ;; - AND next char is lowercase (start of new word)
                                   (when (and prev-char
                                              next-char
                                              (or (and (>= prev-char ?a) (<= prev-char ?z))
                                                  (and (>= prev-char ?0) (<= prev-char ?9)))
                                              (and (>= char ?A) (<= char ?Z))
                                              (and (>= next-char ?a) (<= next-char ?z)))
                                     (setq result (concat result " ")))
                                   (setq result (concat result (char-to-string char)))))
                               result))
               ;; Clean up multiple spaces
               (title-clean (replace-regexp-in-string "  +" " " title-spaced))
               (description (format "%s (%s) %s" author year title-clean)))
          (setq org-link (format "[[file:%s][%s]]" pdf-path description)))
      ;; Fallback if filename does not match expected pattern
      (setq org-link (format "[[file:%s][%s]]" pdf-path name-sans-ext)))
    (if (called-interactively-p 'any)
        (progn
          (insert org-link)
          (message "Inserted org link for: %s" filename))
      org-link)))

(defun mooerslab-paper-pdf-to-org-link-at-point ()
  "Insert an org-mode link for a PDF file selected interactively.
Defaults to /Users/blaine/0papersLabeled/ directory."
  (interactive)
  (let ((default-directory "/Users/blaine/0papersLabeled/"))
    (call-interactively #'mooerslab-paper-pdf-to-org-link)))





(defun mooerslab-convert-pdf-list-to-html-list ()
  "Convert a list of PDF filenames in region to HTML list items.
Each line should contain a PDF filename in the format:
AuthorYEARTitle.pdf

The function will:
1. Sort lines alphabetically
2. Parse each filename to extract author, year, and title
3. Format as HTML list items with file:// links to /Users/blaine/0booksLabeled/

Example input:
Alpaydin2016MachineLearningTheNewAI.pdf

Example output:
<li><a href=\"file:///Users/blaine/0booksLabeled/Alpaydin2016MachineLearningTheNewAI.pdf\">Alpaydin (2016) Machine Learning The New AI</a></li>"
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((start (region-beginning))
         (end (region-end))
         (lines (split-string (buffer-substring-no-properties start end) "\n" t))
         (sorted-lines (sort lines #'string<))
         (html-lines '()))
    ;; Process each line
    (dolist (line sorted-lines)
      (when (string-match "\\`\\([A-Za-z]+\\)\\([0-9X]+\\)\\(.*\\)\\.\\(pdf\\|epub\\|chm\\)\\'" line)
        (let* ((author (match-string 1 line))
               (year (match-string 2 line))
               (title-raw (match-string 3 line))
               (extension (match-string 4 line))
               ;; Process title character by character with lookahead
               (title-spaced (let ((result "")
                                   (len (length title-raw)))
                              (dotimes (i len)
                                (let ((char (aref title-raw i))
                                      (prev-char (if (> i 0) (aref title-raw (1- i)) nil))
                                      (next-char (if (< i (1- len)) (aref title-raw (1+ i)) nil)))
                                  ;; Add space before uppercase if:
                                  ;; - Previous char is lowercase OR digit
                                  ;; - AND current char is uppercase
                                  ;; - AND next char is lowercase (start of new word)
                                  (when (and prev-char
                                            next-char
                                            (or (and (>= prev-char ?a) (<= prev-char ?z))
                                                (and (>= prev-char ?0) (<= prev-char ?9)))
                                            (and (>= char ?A) (<= char ?Z))
                                            (and (>= next-char ?a) (<= next-char ?z)))
                                    (setq result (concat result " ")))
                                  (setq result (concat result (char-to-string char)))))
                              result))
               ;; Clean up multiple spaces
               (title-clean (replace-regexp-in-string "  +" " " title-spaced))
               (html-line (format "<li><a href=\"file:///Users/blaine/0booksLabeled/%s\">%s (%s) %s</a></li>"
                                  line
                                  author
                                  year
                                  title-clean)))
          (push html-line html-lines))))
    ;; Replace region with HTML output
    (delete-region start end)
    (insert (mapconcat #'identity (nreverse html-lines) "\n"))))


; ;; Renumber starting from 3 (as in your example)
; (renumber-tips-and-rules 3)
;
; ;; Renumber starting from 1
; (renumber-tips-from-beginning)
;
; ;; Interactive prompt for starting number
; M-x renumber-tips-interactive
(defun mooerslab-latex-renumber-latex-tips-and-rules (start-number)
  "Renumber LaTeX section headlines that start with 'Tip' or 'Rule' beginning from START-NUMBER.
This function gracefully handles gaps in the numbered list by sequentially
renumbering all found headlines. Handles \\section, \\subsection, \\subsubsection, etc."
  (interactive "nStarting number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((current-number start-number)
          (case-fold-search t))
      (while (re-search-forward
              "\\\\\\(sub\\)*section{\\(Tip\\|Rule\\) \\([0-9]+\\):" nil t)
        (let ((section-type (match-string 1))
              (keyword (match-string 2))
              (old-number (match-string 3)))
          ;; Replace the number in the section headline
          (replace-match
           (format "\\%ssection{%s %d:"
                   (if section-type section-type "")
                   keyword
                   current-number)
           nil nil nil 0)

          ;; Increment counter for next headline
          (setq current-number (1+ current-number))))

      ;; Report results
      (message "Renumbered %d LaTeX section headlines starting from %d"
               (- current-number start-number)
               start-number))))


(defun mooerslab-org-enclose-region-in-quote ()
  "Enclose the selected region in a #+BEGIN_QUOTE and #+END_QUOTE block."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "\n#+END_QUOTE")
        (goto-char start)
        (insert "#+BEGIN_QUOTE\n"))
    (message "No region selected!")))


(defun mooerslab-latex-enclose-region-in-quote (begin end)
  "Wrap the selected region in a LaTeX quote environment.
BEGIN and END define the region to be wrapped."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (insert "\\begin{quote}\n"
            selected-text
            (if (string-suffix-p "\n" selected-text) "" "\n")
            "\\end{quote}")))


(defun mooerslab-latex-renumber-latex-tips-from-beginning ()
  "Convenience function to renumber LaTeX tips and rules starting from 1."
  (interactive)
  (renumber-latex-tips-and-rules 1))


(defun mooerslab-latex-renumber-latex-tips-interactive ()
  "Interactively renumber LaTeX tips and rules with user-specified starting number."
  (interactive)
  (let ((start-num (read-number "Starting number: " 1)))
    (renumber-latex-tips-and-rules start-num)))


(defun mooerslab-latex-renumber-latex-sections-general (pattern start-number)
  "General function to renumber LaTeX sections matching PATTERN starting from START-NUMBER.
PATTERN should be a regex that captures the section type and number.
Example: '\\\\\\\\\\(sub\\)*section{\\([^0-9]*\\)\\([0-9]+\\)' for any numbered section."
  (interactive "sRegex pattern: \nnStarting number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((current-number start-number)
          (count 0))
      (while (re-search-forward pattern nil t)
        (let ((full-match (match-string 0))
              (prefix (match-string 1))
              (text-part (match-string 2))
              (old-number (match-string 3)))
          ;; Replace with new number
          (replace-match
           (format "\\%ssection{%s%d"
                   (if prefix prefix "")
                   text-part
                   current-number))
          (setq current-number (1+ current-number))
          (setq count (1+ count))))

      ;; Report results
      (message "Renumbered %d LaTeX sections starting from %d" count start-number))))


(defun rmooerslab-latex-renumber-latex-any-numbered-sections (start-number)
  "Renumber any LaTeX section that contains a number, starting from START-NUMBER.
Works with \\section, \\subsection, \\subsubsection, etc."
  (interactive "nStarting number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((current-number start-number)
          (case-fold-search t))
      (while (re-search-forward
              "\\\\\\(\\(?:sub\\)*section\\){\\([^}]*?\\)\\([0-9]+\\)\\([^}]*\\)}" nil t)
        (let ((section-command (match-string 1))
              (prefix-text (match-string 2))
              (old-number (match-string 3))
              (suffix-text (match-string 4)))
          ;; Replace the number in the section headline
          (replace-match
           (format "\\%s{%s%d%s}"
                   section-command
                   prefix-text
                   current-number
                   suffix-text)
           nil nil nil 0)

          ;; Increment counter for next headline
          (setq current-number (1+ current-number))))

      ;; Report results
      (message "Renumbered %d LaTeX numbered sections starting from %d"
               (- current-number start-number)
               start-number))))



(defun mooerslab-latex-renumber-latex-specific-section-type (section-type start-number)
  "Renumber specific LaTeX section type (e.g., 'section', 'subsection') starting from START-NUMBER."
  (interactive "sSection type (section/subsection/subsubsection): \nnStarting number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((current-number start-number)
          (pattern (format "\\\\%s{\\([^}]*?\\)\\([0-9]+\\)\\([^}]*\\)}" section-type)))
      (while (re-search-forward pattern nil t)
        (let ((prefix-text (match-string 1))
              (old-number (match-string 2))
              (suffix-text (match-string 3)))
          ;; Replace the number
          (replace-match
           (format "\\%s{%s%d%s}"
                   section-type
                   prefix-text
                   current-number
                   suffix-text)
           nil nil nil 0)

          (setq current-number (1+ current-number))))

      ;; Report results
      (message "Renumbered %d \\%s headlines starting from %d"
               (- current-number start-number)
               section-type
               start-number))))


(defun mooerslab-org-renumber-tips-and-rules (start-number)
  "Renumber headlines that start with 'Tip' or 'Rule' beginning from START-NUMBER.
This function gracefully handles gaps in the numbered list by sequentially
renumbering all found headlines. Also updates corresponding CUSTOM_ID properties."
  (interactive "nStarting number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((current-number start-number)
          (case-fold-search t))
      (while (re-search-forward "^\\*+ \\(Tip\\|Rule\\) \\([0-9]+\\):" nil t)
        (let ((keyword (match-string 1))
              (old-number (match-string 2))
              (start-pos (match-beginning 0)))
          ;; Replace the number in the headline
          (replace-match (format "* %s %d:" keyword current-number) nil nil nil 0)

          ;; Look for and update CUSTOM_ID property
          (save-excursion
            (forward-line 1)
            (when (looking-at ":PROPERTIES:")
              (forward-line 1)
              (when (re-search-forward "^:CUSTOM_ID: rule-\\([0-9]+\\)-"
                                     (save-excursion
                                       (re-search-forward "^:END:" nil t)
                                       (point))
                                     t)
                (replace-match (format ":CUSTOM_ID: rule-%d-" current-number)))))

          ;; Increment counter for next headline
          (setq current-number (1+ current-number))))

      ;; Report results
      (message "Renumbered %d headlines starting from %d"
               (- current-number start-number)
               start-number))))

(defun mooerslab-org-renumber-tips-from-beginning ()
  "Convenience function to renumber tips and rules starting from 1."
  (interactive)
  (renumber-tips-and-rules 1))

(defun mooerslab-org-renumber-tips-interactive ()
  "Interactively renumber tips and rules with user-specified starting number."
  (interactive)
  (let ((start-num (read-number "Starting number: " 1)))
    (renumber-tips-and-rules start-num)))



(defun mooerslab-check-ollama-connection ()
     "Check if Ollama is running on localhost:11434."
     (interactive)
     (url-retrieve "http://localhost:11434/api/tags"
                   (lambda (status)
                     (if (plist-get status :error)
                         (message "Ollama is NOT running at localhost:11434")
                       (message "Ollama is running at localhost:11434")))))


(defun mooerslab-prose-markdown-region-to-latex (start end)
  "Convert the region from START to END from Markdown to LaTeX for prose and lists.
Converts:
- Double asterisks (**text**) to \\textit{text}
- # Section -> \\section{Section}
- ## Subsection -> \\subsection{Subsection}
- ### Subsubsection -> \\subsubsection{Subsubsection}
- Unordered lists (-, *, +) to \\itemize with \\item
- Ordered lists (1., 2., etc.) to \\enumerate with \\item
Handles nested lists by tracking indentation levels."
  (interactive "r")
  (let* ((text (buffer-substring start end))
         (lines (split-string text "\n"))
         (output-lines '())
         (stack '()) ; Stack of (list-type . indent-level)
         (new-line nil))

    ;; Process each line
    (dolist (line lines)
      ;; Step 1: Convert emphasis (**text** to \textit{text})
      (setq new-line (replace-regexp-in-string "\\*\\*\\([^*]+?\\)\\*\\*" "\\\\textit{\\1}" line))

      ;; Step 2: Check if it's a heading (up to 3 leading spaces allowed)
      (if (string-match "^\\s-\\{0,3\\}\\(#\\{1,3\\}\\)\\s-+\\(.*\\)$" new-line)
          ;; It's a heading - close any open lists first
          (let* ((hashes (match-string 1 new-line))
                 (heading-text (match-string 2 new-line))
                 (latex-heading
                  (cond
                   ((= (length hashes) 1) (concat "\\section{" heading-text "}"))
                   ((= (length hashes) 2) (concat "\\subsection{" heading-text "}"))
                   ((= (length hashes) 3) (concat "\\subsubsection{" heading-text "}")))))
            ;; Close any open lists
            (while stack
              (let ((env (pop stack)))
                (push (format "\\end{%s}" (car env)) output-lines)))
            (push latex-heading output-lines))

        ;; Step 3: Not a heading, check for list items
        (let ((indent 0)
              (type nil)
              (content nil))

          ;; Check for unordered list item
          (cond
           ((string-match "^\\(\\s-*\\)\\([-*+]\\)\\s-*\\(.*\\)$" new-line)
            (setq indent (length (match-string 1 new-line)))
            (setq type 'itemize)
            (setq content (match-string 3 new-line)))

           ;; Check for ordered list item
           ((string-match "^\\(\\s-*\\)\\([0-9]+\\.\\)\\s-*\\(.*\\)$" new-line)
            (setq indent (length (match-string 1 new-line)))
            (setq type 'enumerate)
            (setq content (match-string 3 new-line)))

           ;; Not a list item
           (t (setq type nil)))

          (if type
              ;; Process list item
              (progn
                ;; Close environments deeper than current level
                (while (and stack (> (cdar stack) indent))
                  (let ((env (pop stack)))
                    (push (format "\\end{%s}" (car env)) output-lines)))

                ;; Handle current level
                (if (and stack (= (cdar stack) indent))
                    ;; Same level - check if same type
                    (if (eq (caar stack) type)
                        ;; Same type - just add item
                        (push (concat "\\item " content) output-lines)
                      ;; Different type - close old, open new
                      (let ((old-env (pop stack)))
                        (push (format "\\end{%s}" (car old-env)) output-lines)
                        (push (format "\\begin{%s}" type) output-lines)
                        (push `(,type . ,indent) stack)
                        (push (concat "\\item " content) output-lines)))
                  ;; Deeper level or first list
                  (push (format "\\begin{%s}" type) output-lines)
                  (push `(,type . ,indent) stack)
                  (push (concat "\\item " content) output-lines)))

            ;; Not a list item - close all open lists and output line
            (while stack
              (let ((env (pop stack)))
                (push (format "\\end{%s}" (car env)) output-lines)))
            (push new-line output-lines)))))

    ;; Close any remaining open lists
    (while stack
      (let ((env (pop stack)))
        (push (format "\\end{%s}" (car env)) output-lines)))

    ;; Replace the region with converted text
    (delete-region start end)
    (insert (mapconcat 'identity (reverse output-lines) "\n"))))


(defun mooerslab-count-lines ()
  "Interactively count lines in an elisp or org-mode file.
Prompts for a file and returns annotated line counts."
  (interactive)
  (let* ((file (read-file-name "Select elisp or org-mode file: "
                               nil nil t nil
                               (lambda (f)
                                 (or (string-match "\\.el\\'" f)
                                     (string-match "\\.org\\'" f)))))
         (file-extension (file-name-extension file))
         (total-lines 0)
         (code-lines 0)
         (comment-lines 0)
         (blank-lines 0)
         (header-lines 0)
         (result-buffer (get-buffer-create "*Line Count Results*")))

    (if (not (file-exists-p file))
        (message "File does not exist: %s" file)

      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))

        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (setq total-lines (1+ total-lines))

            (cond
             ;; Blank line
             ((string-match "^[[:space:]]*$" line)
              (setq blank-lines (1+ blank-lines)))

             ;; For elisp files
             ((string= file-extension "el")
              (cond
               ;; Comment line
               ((string-match "^[[:space:]]*;" line)
                (setq comment-lines (1+ comment-lines)))
               ;; Code line
               (t
                (setq code-lines (1+ code-lines)))))

             ;; For org-mode files
             ((string= file-extension "org")
              (cond
               ;; Header line
               ((string-match "^\\*+ " line)
                (setq header-lines (1+ header-lines))
                (setq code-lines (1+ code-lines)))
               ;; Comment line
               ((string-match "^[[:space:]]*#\\+\\|^[[:space:]]*#[^+]" line)
                (setq comment-lines (1+ comment-lines)))
               ;; Content line
               (t
                (setq code-lines (1+ code-lines))))))

            (forward-line 1))))

      ;; Display results
      (with-current-buffer result-buffer
        (erase-buffer)
        (insert (format "Line Count Analysis for: %s\n" file))
        (insert (make-string 50 ?=))
        (insert "\n\n")
        (insert (format "File Type:        %s\n"
                       (if (string= file-extension "el") "Emacs Lisp" "Org-mode")))
        (insert (format "Total Lines:      %d\n" total-lines))
        (insert (format "Content Lines:    %d (%.1f%%)\n"
                       code-lines
                       (if (> total-lines 0)
                           (* 100.0 (/ (float code-lines) total-lines))
                         0)))
        (when (string= file-extension "org")
          (insert (format "  Header Lines:   %d\n" header-lines)))
        (insert (format "Comment Lines:    %d (%.1f%%)\n"
                       comment-lines
                       (if (> total-lines 0)
                           (* 100.0 (/ (float comment-lines) total-lines))
                         0)))
        (insert (format "Blank Lines:      %d (%.1f%%)\n"
                       blank-lines
                       (if (> total-lines 0)
                           (* 100.0 (/ (float blank-lines) total-lines))
                         0)))
        (insert "\n")
        (insert (make-string 50 ?-))
        (insert "\n")
        (insert (format "Effective Lines:  %d (non-blank)\n"
                       (- total-lines blank-lines))))

      ;; Show the result buffer
      (display-buffer result-buffer)

      ;; Return the annotated list
      (list :file file
            :type (if (string= file-extension "el") "elisp" "org-mode")
            :total-lines total-lines
            :content-lines code-lines
            :comment-lines comment-lines
            :blank-lines blank-lines
            :header-lines (if (string= file-extension "org") header-lines nil)))))


;; Function to create note and open PDF
(defun mooerslab-citar-open-org-roam-literature-note ()
  "Open or create an org-roam literature note and associated PDF. Specify type == 'paper' or 'book'."
  (interactive)
  (let* ((key (citar-select-ref))
         (entry (citar-get-entry key))
         (title (citar-get-value "title" entry))
         (author (citar-get-value "author" entry))
         (year (citar-get-value "year" entry))
         (journal (citar-get-value "journal" entry))
         (volume (citar-get-value "volume" entry))
         (pages (citar-get-value "pages" entry))
         (doi (citar-get-value "doi" entry))
         (url (citar-get-value "url" entry))
         ;; Get PDF file paths from both directories
         (pdf-paths (list (concat "~/0papersLabeled/" key ".pdf")
                         (concat "~/0booksLabeled/" key ".pdf")))
         ;; Find first existing PDF
         (pdf-path (car (seq-filter #'file-exists-p pdf-paths))))

    ;; Delete other windows to start fresh
    (delete-other-windows)

    ;; Create the note using org-roam's capture system
    (org-roam-capture-
     :templates
     '(("l" "literature type: paper or book" plain
        "\n* Source
:PROPERTIES:
:AUTHOR: ${author}
:YEAR: ${year}
:TITLE: ${title}
:JOURNAL: ${journal}
:VOLUME: ${volume}
:PAGES: ${pages}
:DOI: ${doi}
:URL: ${url}
:TYPE: ${=type=}
:END:

* Summary\n\n
* Table of Tibbits\n
|-------+-------------------------------------------------------------------------------------------------------+----------|\n
| Pages | Tibbits of knowledge, factoids, results, discussion points, or insights paraphrased in your own words | Keywords |\n
|-------+-------------------------------------------------------------------------------------------------------+----------|\n
|       |                                                                                                       |          |\n
|       |                                                                                                       |          |\n
|       |                                                                                                       |          |\n
|       |                                                                                                       |          |\n
|-------+-------------------------------------------------------------------------------------------------------+----------|\n\n
* Keywords
- \n- \n- \n- \n
* Abbreviations
- \n- \n- \n- \n
* Equations
- \n- \n- \n- \n
* Potential Uses
- \n- \n- \n- \n
* Where cited
- \n- \n- \n- \n
* Related bibtex entries\n
- \n- \n

"
        :if-new (file+head "literature/${citekey}.org"
                          "#+title: ${title}\n#+filetags: :literature:citation:\n#+roam_refs: [cite:@${citekey}]\n")
        :immediate-finish t
        :unnarrowed t))
     :info (list :citekey key
                 :title title
                 :author author
                 :year year
                 :journal journal
                 :volume volume
                 :pages pages
                 :doi doi
                 :url url
                 :type (if (string-match-p "book" (or (citar-get-value "type" entry) ""))
                          "book" "paper"))
     :node (org-roam-node-create :title title)
     :props '(:finalize find-file))

    ;; Split window and open PDF if found
    (when pdf-path
      (split-window-right)
      (other-window 1)
      (find-file pdf-path)
      (other-window -1))))  ; Return to note window


(defun mooerslab-wrap-book-pdf-filename-prefixes-as-org-links (beg end)
  "Transform a selection of book PDF filenames into org-mode links.
Each line in the region from BEG to END should contain a PDF filename without the extension pdf.
This function will transform each line into an org-mode link pointing to
~/0booksLabeled/filename.pdf."
  (interactive "r")  ; r means use the current region
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))  ; eobp = end of buffer predicate
        (beginning-of-line)
        (insert "- [[file:~/0booksLabeled/")
        (end-of-line)
        (insert ".pdf]]")
        (forward-line 1)))))

(defun mooerslab-wrap-article-pdf-filename-prefixes-as-org-links (beg end)
  "Transform a selection of article PDF filenames into org-mode links.
Each line in the region from BEG to END should contain a PDF filename without the extension pdf.
This function will transform each line into an org-mode link pointing to
~/0papersLabeled/filename.pdf."
  (interactive "r")  ; r means use the current region
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))  ; eobp = end of buffer predicate
        (beginning-of-line)
        (insert "- [[file:~/0papersLabeled/")
        (end-of-line)
        (insert ".pdf]]")
        (forward-line 1)))))


(defun mooerslab-bibtex-entry-add-file-field ()
  "Add a file field to the current BibTeX entry using the cite key.
The file field format is: file = {:<cite-key>.pdf:PDF}
The cursor should be positioned within a BibTeX entry.
It works when positioned in the title field."
  (interactive)
  (let (entry-start entry-end cite-key)
    ;; Find the start of the current entry
    (save-excursion
      (beginning-of-line)
      ;; Search backward for entry start, including current line
      (while (and (not (looking-at "^[[:space:]]*@\\w+[[:space:]]*{"))
                  (not (bobp)))
        (forward-line -1))
      (if (looking-at "^[[:space:]]*@\\w+[[:space:]]*{")
          (setq entry-start (point))
        (error "Cannot find start of BibTeX entry")))

    ;; Find the end of the current entry
    (save-excursion
      (goto-char entry-start)
      (let ((brace-count 0)
            (in-entry nil))
        ;; Move to the opening brace and start counting
        (re-search-forward "{")
        (setq brace-count 1)
        (setq in-entry t)
        ;; Count braces to find the matching closing brace
        (while (and (> brace-count 0) (not (eobp)))
          (cond
           ((looking-at "{")
            (setq brace-count (1+ brace-count)))
           ((looking-at "}")
            (setq brace-count (1- brace-count))))
          (when (> brace-count 0)
            (forward-char)))
        (if (= brace-count 0)
            (setq entry-end (1+ (point)))
          (error "Cannot find end of BibTeX entry"))))

    ;; Extract the cite key
    (save-excursion
      (goto-char entry-start)
      (if (re-search-forward "@\\w+[[:space:]]*{\\([^,}[:space:]]+\\)" entry-end t)
          (setq cite-key (match-string 1))
        (error "No cite key found in current entry")))

    ;; Check if file field already exists
    (save-excursion
      (goto-char entry-start)
      (when (re-search-forward "^[[:space:]]*file[[:space:]]*=" entry-end t)
        (error "File field already exists in this entry")))

    ;; Insert the file field before the closing brace
    (save-excursion
      (goto-char entry-end)
      (backward-char 1) ; Move before the closing brace
      (skip-chars-backward " \t\n\r") ; Skip whitespace
      ;; Add comma if the previous character isn't already a comma
      (unless (looking-back "," 1)
        (insert ","))
      (insert (format "\n  file = {:%s.pdf:PDF}" cite-key)))

    (message "Added file field with cite key: %s" cite-key)))

;; Bind the function to C-x a f
(global-set-key (kbd "C-x a f") 'mooerslab-bibtex-entry-add-file-field)


(defun mooerslab-bibtex-add-file-field-to-entry ()
  "Add a PDF file field to the current BibTeX entry when it is being generate after submission of the article's DOI.
  The file will be renamed using the citation key.
  The file is stored in ~/0papersLabeled.
  This desitnation for PDFs is set in the init.el.
  The file entry will be made even if the PDF was not automatically downloaded.
  You can add the PDF to your collection manually in this sitution."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((key (bibtex-key-in-head))
         (file-field (format ":%s.pdf:PDF" key)))
    (if (not (bibtex-search-forward-field "file" t))
        (save-excursion
          (bibtex-end-of-entry)
          (backward-char 1)  ;; Move before the closing brace
          (insert "  file = {" file-field "}")))))

;; Advise the doi-utils function to add the file field after creating an entry
(with-eval-after-load 'doi-utils
  (defun mooerslab-doi-utils-add-file-field (orig-fun &rest args)
    "Advice function that adds a file field after adding a BibTeX entry."
    (let ((result (apply orig-fun args)))
      (mooerslab-bibtex-add-file-field-to-entry)
      result))

  (advice-add 'doi-utils-add-bibtex-entry-from-doi
              :around #'mooerslab-doi-utils-add-file-field))


(defun mooerslab-replace-first-column-with-echo-region (start end)
  "Replace the first column in region with 'echo \"' + rest of column (minus first char).
For example: 'data1 value1' becomes 'echo \"ata1 value1'. Thus is a very common operation
that is normally handled in three steps starting with making a rectangular selection and
then replacing this selection."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (point)))
        ;; Skip empty lines
        (unless (looking-at-p "^\\s-*$")
          ;; Find the end of the first column (first whitespace)
          (skip-chars-forward "^ \t\n")
          (let* ((col-end (point))
                 (col-text (buffer-substring-no-properties line-start col-end))
                 (col-rest (if (> (length col-text) 1)
                              (substring col-text 1)
                            "")))
            ;; Delete the first column
            (delete-region line-start col-end)
            ;; Insert the replacement
            (insert (concat "echo \"" col-rest)))))
      ;; Move to next line
      (forward-line 1))))


(defun mooerslab-insert-quote-at-end-of-lines (start end)
  "Insert a double quote character at the end of each line in the selected region.
  This is often needed to close a quoted string."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (insert "\"")
      (forward-line 1))))






(defun mooerslab-github-markdown-table-package-functions ()
  "Create a GitHub markdown table of all functions in a package and their docstrings.
   Prompts the user for a package name in the minibuffer and ensures proper text formatting
   without excessive spaces between letters in the output."
  (interactive)
  (let* ((package-name (intern (completing-read "Package name: "
                                               (mapcar #'symbol-name features))))
         (package-symbols (apropos-internal (concat "^" (symbol-name package-name) "-") 'fboundp))
         (buffer (get-buffer-create (format "*%s-functions-markdown*" package-name)))
         (functions-list))

    ;; Create list of function symbols in the package
    (setq functions-list
          (sort package-symbols #'(lambda (a b)
                                   (string< (symbol-name a) (symbol-name b)))))

    ;; Switch to the output buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    (when (fboundp 'markdown-mode)
      (markdown-mode))

    ;; Insert header
    (insert (format "# Functions in package: %s\n\n" package-name))

    ;; Insert markdown table header
    (insert "| Function | Description |\n")
    (insert "| --- | --- |\n")

    ;; Insert functions and docstrings as table rows
    (if functions-list
        (dolist (func-symbol functions-list)
          (let* ((func-name (symbol-name func-symbol))
                 (raw-doc (or (documentation func-symbol) "No documentation available"))
                 ;; Clean the docstring of markdown table-breaking characters
                 (doc (replace-regexp-in-string "\n" " " raw-doc))
                 (doc (replace-regexp-in-string "|" "\\\\|" doc))
                 ;; Remove any carriage returns that might be present
                 (doc (replace-regexp-in-string "\r" "" doc))
                 ;; Ensure only a single space between words
                 (doc (replace-regexp-in-string "\\s-+" " " doc)))
            ;; Insert the row with function name in backticks and cleaned docstring
            (insert (format "| `%s` | %s |\n" func-name doc))))
      (insert "| No functions found in this package | |\n"))

    ;; Return to the beginning of the buffer
    (goto-char (point-min))

    ;; Message to user
    (message "Created GitHub markdown table of %d functions in package %s"
             (length functions-list) package-name)))


(defun mooerslab-format-authors-in-region (begin end)
  "Format author names in region from 'First M.N. Last' to 'Last, F.M.N.'
Works with various formats:
  - Regular names: 'Blaine Mooers' -> 'Mooers, B.'
  - With whitespace mulitple middle initials: 'Blaine H M Mooers' -> 'Mooers, B.H.M.'
  - With no whitespace mulitple middle initials: 'Blaine HM Mooers' -> 'Mooers, B.H.M.'
  - With dotted multiple initials: 'Blaine H.M. Mooers' -> 'Mooers, B.H.M.'
  - Multiple authors (comma-separated)

Select a region with author names and run this function to reformat them.

This is very useful during the preparation of grant progress reports and bibtex entries."
  (interactive "r")
  (when (use-region-p)
    (let* ((text (buffer-substring-no-properties begin end))
           (authors (split-string text "," t "[ \t\n\r]+"))
           (formatted-list '()))

      ;; Process each author
      (dolist (author authors)
        (let* ((parts (split-string (string-trim author) "[ \t]+" t))
               (last-name (car (last parts)))
               (first-names (butlast parts))
               (initials ""))

          ;; Process each first/middle name or initial
          (dolist (name first-names)
            (cond
             ;; Case 1: Already dotted initials (like "H.M.")
             ((string-match-p "\\." name)
              (let* ((split-initials (split-string name "\\." t))
                     (cleaned-initials
                      (mapcar (lambda (i) (concat i ".")) split-initials)))
                (setq initials (concat initials
                                      (mapconcat 'identity cleaned-initials "")))))

             ;; Case 2: Single letter (already an initial)
             ((= (length name) 1)
              (setq initials (concat initials name ".")))

             ;; Case 3: Full name (First letter capital, rest lowercase)
             ((string-match-p "^[A-Z][a-z]+$" name)
              (setq initials (concat initials (substring name 0 1) ".")))

             ;; Case 4: Multiple uppercase letters (like "HM")
             ((string-match-p "^[A-Z]+$" name)
              (dolist (c (string-to-list name))
                (setq initials (concat initials (char-to-string c) "."))))

             ;; Case 5: Anything else - just take first letter to be safe
             (t
              (setq initials (concat initials (substring name 0 1) ".")))))

          ;; Add formatted name to list
          (push (concat last-name ", " initials) formatted-list)))

      ;; Create final result and replace region
      (let ((result (mapconcat 'identity (nreverse formatted-list) ", ")))
        (delete-region begin end)
        (goto-char begin)
        (insert result)
        (message "Authors reformatted successfully")))))


;;; Works with the LaTeX imakeidx package in org-mode, which supports the use of two or more indices. Could work in Autex too.
(defun mooerslab-insert-main-index-entry ()
  "Insert a general index entry."
  (interactive)
  (let ((term (read-string "Index term: ")))
    (insert "\\index[main]{" term "}")))

(defun mooerslab-insert-author-index-entry ()
  "Insert an author index entry."
  (interactive)
  (let ((author (read-string "Author (Last, First): ")))
    (insert "\\index[authors]{" author "}")))

(defun mooerslab-insert-prompt-index-entry ()
  "Insert an writing prompt index entry."
  (interactive)
  (let ((prompt (read-string "Writing prompt: ")))
    (insert "\\index[prompt]{" prompt "}")))



(eval-after-load 'org-mode
      '(define-key org-mode-map (kbd "C-c w a") 'insert-author-index-entry))

(eval-after-load 'org-mode
        '(define-key org-mode-map (kbd "C-c w m") 'insert-main-index-entry))

(eval-after-load 'org-mode
        '(define-key org-mode-map (kbd "C-c w p") 'insert-prompt-index-entry))


(eval-after-load 'tex-mode
      '(define-key LaTeX-mode-map (kbd "C-c w a") 'insert-author-index-entry))

(eval-after-load 'tex-mode
       '(define-key LaTeX-mode-map (kbd "C-c w m") 'insert-main-index-entry))

(eval-after-load 'tex-mode
       '(define-key LaTeX-mode-map (kbd "C-c w p") 'insert-main-index-entry))

(defun mooerslab-LaTeX-narrow-to-section (&optional innermost)
 "Narrow to the LaTeX sectioning unit around point.
 By default the current section and its child subsections are shown.
 With a prefix argument C-u, show only from the current
 sectioning command to the next one; the children are excluded.
 Note that the recommended keybinding has to be mapped to the LaTeX keymap
 because the same keybinding in org-mode narrows to the subtree in an org-file."
(interactive "P")
(widen)
(save-mark-and-excursion
(LaTeX-mark-section innermost)
(narrow-to-region (region-beginning) (region-end))))

(with-eval-after-load 'latex
(keymap-set LaTeX-mode-map "C-x n s" #'mooerslab-LaTeX-narrow-to-section))


(defun mooerslab-mab-wrap-citar-citekey-and-create-abibnote-org ()
  "Replace the citekey under the cursor with LaTeX-wrapped text and create a
 corresponding empty citekey.org file in abibNotes folder in the home directory.
 Will work with citekeys in citar style or in LaTeX style or plain naked citekeys.
 The LaTeX code uses the bibentry package to inject a bibliographic entry into
 a section heading that is added in the table of contents. The function also adds
 file links to the PDF and org note files in a Notes drawer for quick access."

  (interactive)
  (let* ((bounds (or (save-excursion
                      ;; Check if we're inside a citation
                      (let ((start (re-search-backward "\\[cite:@" (line-beginning-position) t))
                            (end (re-search-forward "\\]" (line-end-position) t)))
                        (when (and start end)
                          (cons start end))))
                    ;; Otherwise just get the word at point
                    (bounds-of-thing-at-point 'word)))
         (citation-text (when bounds
                          (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (citekey (when citation-text
                    (if (string-match "\\[cite:@\\([^]]+\\)\\]" citation-text)
                        (match-string 1 citation-text)
                      citation-text))) ;; Plain word if not a citation

         ;; Extract directory from current buffer filename
         (current-file (buffer-file-name))
         (current-dir (when current-file (file-name-directory current-file)))

         ;; Try to determine default project number from filename
         (default-project-number
          (cond
           ;; First try to find "ab2156" pattern in the buffer file name
           ((and current-file
                 (string-match "ab\\([0-9]+\\).org" (file-name-nondirectory current-file)))
            (match-string 1 current-file))

           ;; Look for "2156" in the buffer file name
           ((and current-file
                 (string-match "\\([0-9]+\\)" (file-name-nondirectory current-file)))
            (match-string 1 current-file))

           ;; Default to empty string
           (t "")))

         ;; Prompt the user for project number with default from filename
         (project-number (read-string (format "Project number for BibTeX file [%s]: "
                                             default-project-number)
                                     nil nil default-project-number))

         ;; Construct file paths
         (mab-dir (concat "mab" project-number "/")) ;; New mab subfolder
         (mab-full-dir (and current-dir (concat current-dir mab-dir)))
         (bib-file-name (concat "mab" project-number ".bib"))
         (bib-file-path (and mab-full-dir (concat mab-full-dir bib-file-name)))
         (org-file-dir "/Users/blaine/abibNotes/") ;; Directory for the .org file
         (org-file-path (and citekey (concat org-file-dir citekey ".org"))) ;; Full path for the .org file

         ;; Updated wrapped text with file links inside a Notes drawer instead of COMMENT block
         (wrapped-text (and citekey
                           (format "#+LATEX: \\subsubsection*{\\bibentry{%s}}\n#+LATEX: \\addcontentsline{toc}{subsubsection}{%s}\n#+INCLUDE: %s\n:Notes:\nThe org-mode file is found [[file:~/abibNotes/%s.org][here]]\nThe PDF file is found [[file:~/0papersLabeled/%s.pdf][here]].\n\nAdd more prose. Add tables. Add figures.\n:END:"
                                  citekey citekey org-file-path citekey citekey))))

    ;; Debug message to check file paths
    (message "Using bibfile: %s" bib-file-path)

    (if (not citekey)
        (message "No citekey found under the cursor.")
      (progn
        ;; Delete the citation or word at cursor
        (when bounds
          (delete-region (car bounds) (cdr bounds)))
        ;; Insert the wrapped text in its place
        (insert wrapped-text)

        ;; Create a minimal .org file if it doesn't already exist - no header, no headline
        (if (file-exists-p org-file-path)
            (message "File %s already exists." org-file-path)
          (with-temp-file org-file-path
            (insert ""))) ;; Empty file

        ;; Create mab directory if it doesn't exist
        (when (and mab-full-dir (not (file-exists-p mab-full-dir)))
          (make-directory mab-full-dir t)
          (message "Created directory %s" mab-full-dir))

        ;; Append the BibTeX entry to the project-specific .bib file in mab subfolder
        (require 'bibtex)
        (when (and (featurep 'citar) mab-full-dir bib-file-path)  ;; Ensure we have all required paths
          ;; Get the bibtex entry using search in citar bibliography files
          (let* ((bib-files (citar--bibliography-files))
                 (bibtex-entry nil))

            ;; Look through each bibliography file for the entry
            (when bib-files
              (catch 'found
                (dolist (bib-file bib-files)
                  (with-temp-buffer
                    (insert-file-contents bib-file)
                    (bibtex-mode)
                    (bibtex-set-dialect 'BibTeX t)
                    (goto-char (point-min))
                    (when (re-search-forward (format "@[^{]+{%s," citekey) nil t)
                      (let ((beg (save-excursion
                                   (bibtex-beginning-of-entry)
                                   (point)))
                            (end (save-excursion
                                   (bibtex-end-of-entry)
                                   (point))))
                        (setq bibtex-entry (buffer-substring-no-properties beg end))
                        (throw 'found t)))))))

            (if bibtex-entry
                (progn
                  ;; Now append to the bib file
                  (with-temp-file bib-file-path
                    (when (file-exists-p bib-file-path)
                      (insert-file-contents bib-file-path))
                    (goto-char (point-max))
                    (unless (or (bobp) (bolp)) (insert "\n"))
                    (insert bibtex-entry "\n\n"))
                  (message "Added BibTeX entry to %s" bib-file-path))
              (message "Could not retrieve BibTeX entry for %s" citekey))))

        ;; Open the .org file in a new buffer
        (find-file org-file-path)
        (message "Replaced citekey, created .org file, and opened it: %s" org-file-path)))))


(defun mooerslab-abib-wrap-citar-citekey-and-create-abibnote-org ()
    "Replace the citekey under the cursor with LaTeX-wrapped text and create a
    corresponding empty citekey.org file in abibNotes folder in the home directory.
    Will work with citekeys in citar style or in LaTeX style or plain naked citekeys.
    The LaTeX code uses the bibentry package to inject a bibliographic entry into
    a section heading that is added in the table of contents. The function also adds
    file links to the PDF and org note files in a comment block for quick access."

    (interactive)
    (let* ((bounds (or (save-excursion
                      ;; Check if we're inside a citation
                      (let ((start (re-search-backward "\\[cite:@" (line-beginning-position) t))
                            (end (re-search-forward "\\]" (line-end-position) t)))
                        (when (and start end)
                          (cons start end))))
                    ;; Otherwise just get the word at point
                    (bounds-of-thing-at-point 'word)))
         (citation-text (when bounds
                          (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (citekey (when citation-text
                    (if (string-match "\\[cite:@\\([^]]+\\)\\]" citation-text)
                        (match-string 1 citation-text)
                      citation-text))) ;; Plain word if not a citation

         ;; Extract directory from current buffer filename
         (current-file (buffer-file-name))
         (current-dir (when current-file (file-name-directory current-file)))

         ;; Try to determine default project number from filename
         (default-project-number
          (cond
           ;; First try to find "ab2156" pattern in the buffer file name
           ((and current-file
                 (string-match "ab\\([0-9]+\\).org" (file-name-nondirectory current-file)))
            (match-string 1 current-file))

           ;; Look for "2156" in the buffer file name
           ((and current-file
                 (string-match "\\([0-9]+\\)" (file-name-nondirectory current-file)))
            (match-string 1 current-file))

           ;; Default to empty string
           (t "")))

         ;; Prompt the user for project number with default from filename
         (project-number (read-string (format "Project number for BibTeX file [%s]: "
                                             default-project-number)
                                     nil nil default-project-number))

         ;; Construct file paths
         (bib-file-name (concat "ab" project-number ".bib"))
         (bib-file-path (and current-dir (concat current-dir bib-file-name)))
         (org-file-dir "/Users/blaine/abibNotes/") ;; Directory for the .org file
         (org-file-path (and citekey (concat org-file-dir citekey ".org"))) ;; Full path for the .org file

         ;; Updated wrapped text with file links inside a comment block
         (wrapped-text (and citekey
                           (format "#+LATEX: \\subsubsection*{\\bibentry{%s}}\n#+LATEX: \\addcontentsline{toc}{subsubsection}{%s}\n#+INCLUDE: %s\n#+BEGIN_COMMENT\nfile:~/abibNotes/%s.org\nfile:~/0papersLabeled/%s.pdf\n#+END_COMMENT"
                                  citekey citekey org-file-path citekey citekey))))

    ;; Debug message to check file paths
    (message "Using bibfile: %s" bib-file-path)

    (if (not citekey)
        (message "No citekey found under the cursor.")
      (progn
        ;; Delete the citation or word at cursor
        (when bounds
          (delete-region (car bounds) (cdr bounds)))
        ;; Insert the wrapped text in its place
        (insert wrapped-text)

        ;; Create a minimal .org file if it doesn't already exist - no header, no headline
        (if (file-exists-p org-file-path)
            (message "File %s already exists." org-file-path)
          (with-temp-file org-file-path
            (insert ""))) ;; Empty file

        ;; Append the BibTeX entry to the project-specific .bib file in current directory
        (require 'bibtex)
        (when (and (featurep 'citar) current-dir bib-file-path)  ;; Ensure we have all required paths
          ;; Get the bibtex entry using search in citar bibliography files
          (let* ((bib-files (citar--bibliography-files))
                 (bibtex-entry nil))

            ;; Look through each bibliography file for the entry
            (when bib-files
              (catch 'found
                (dolist (bib-file bib-files)
                  (with-temp-buffer
                    (insert-file-contents bib-file)
                    (bibtex-mode)
                    (bibtex-set-dialect 'BibTeX t)
                    (goto-char (point-min))
                    (when (re-search-forward (format "@[^{]+{%s," citekey) nil t)
                      (let ((beg (save-excursion
                                   (bibtex-beginning-of-entry)
                                   (point)))
                            (end (save-excursion
                                   (bibtex-end-of-entry)
                                   (point))))
                        (setq bibtex-entry (buffer-substring-no-properties beg end))
                        (throw 'found t)))))))

            (if bibtex-entry
                (progn
                  ;; Now append to the bib file
                  (with-temp-file bib-file-path
                    (when (file-exists-p bib-file-path)
                      (insert-file-contents bib-file-path))
                    (goto-char (point-max))
                    (unless (or (bobp) (bolp)) (insert "\n"))
                    (insert bibtex-entry "\n\n"))
                  (message "Added BibTeX entry to %s" bib-file-path))
              (message "Could not retrieve BibTeX entry for %s" citekey))))

        ;; Open the .org file in a new buffer
        (find-file org-file-path)
        (message "Replaced citekey, created .org file, and opened it: %s" org-file-path)))))


(defvar mooerslab-global-bib-file (expand-file-name "~/Documents/global.bib")
"Path to the global BibTeX library searched by
`mooerslab-mab-return-matching-citar-citekeys-org'.")

(defun mooerslab-mab-return-matching-citar-citekeys-org (term)
"Prompt for a search TERM and insert citation keys for matching entries.
Searches `mooerslab-global-bib-file ' for BibTeX entries whose raw text contains
TERM (case-insensitive, plain substring match across all fields including
title, author, abstract, keywords, and the citekey itself).  For each
matching entry, insert one line of the form
[cite:@CITEKEY]
at point in the current buffer.  That is the org-cite/citar citation
format, so this command is intended for use inside an org-mode buffer
where org-cite is active.
Returns the list of raw citekeys (without the [cite:@...] wrapping) so
the function can also be called from other Lisp."
(interactive "sSearch term: ")
(unless (file-readable-p mooerslab-global-bib-file)
(user-error "Cannot read %s" mooerslab-global-bib-file ))
(let ((target-buffer (current-buffer))
(matches '()))
(with-temp-buffer
(insert-file-contents mooerslab-global-bib-file )
(bibtex-mode)
(let ((case-fold-search t))
(bibtex-map-entries
(lambda (key beg end)
(let ((entry-text (buffer-substring-no-properties beg end)))
(when (string-match-p (regexp-quote term) entry-text)
(push key matches)))))))
(setq matches (nreverse matches))
(with-current-buffer target-buffer
(if matches
(progn
(insert (mapconcat (lambda (k) (format "[cite:@%s]" k))
matches
"\n"))
(insert "\n")
(message "Inserted %d citation key(s) matching %S"
(length matches) term))
(message "No entries matched %S" term)))
matches))


(defun mooerslab-mab-wrap-citar-citekeys-in-region (start end)
  "Apply `mooerslab-mab-wrap-citar-citekey-and-create-abibnote-org' to every
citar citation in the region between START and END.

A citar citation is matched by the regexp \\[cite:@[^]]+\\].  Each match
in the region is processed in turn.  Citations are handled in reverse
order (from the end of the region toward the beginning) so the position
of an earlier citation is not invalidated when a later citation is
replaced by its much longer LaTeX expansion.

If the region contains more than 10 citations, a confirmation prompt
appears before any work begins, because each citation triggers file
creation, buffer opening, and an append to the project .bib file."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((source-buffer (current-buffer))
        (positions '()))
    ;; Phase 1.  Collect the start position of every citation in the region.
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[cite:@[^]]+\\]" end t)
        (push (match-beginning 0) positions)))
    (let ((n (length positions)))
      (cond
       ((zerop n)
        (message "No citar citations found in the region."))
       ((and (> n 10)
             (not (yes-or-no-p
                   (format "Region contains %d citations (more than 10). Proceed? "
                           n))))
        (message "Aborted by user."))
       (t
        ;; Phase 2.  Walk positions descending so each rewrite leaves the
        ;; remaining work undisturbed.  `with-current-buffer' keeps us in
        ;; the source buffer between iterations, even though the wrapped
        ;; function ends with `find-file' on the .org note.
        (dolist (pos (sort positions #'>))
          (with-current-buffer source-buffer
            (goto-char pos)
            (mooerslab-mab-wrap-citar-citekey-and-create-abibnote-org)))
        (pop-to-buffer source-buffer)
        (message "Wrapped %d citation(s)." n))))))

;% This function eases adding log files to the list of files for org-agenda to search for to-dos.
;% Another example of spending an hour to save a minute!
(defun mooerslab-append-log-to-org-agenda-files ()
  "Interactively append a log####.org file to org-agenda-files list.
Updates both the current org-agenda-files variable in memory and the setq statement in init.el.
Customize the path to your init.el file."
  (interactive)
  (let* ((home-dir (expand-file-name "~/"))
         ;; Find all log####.org files in the home directory and immediate sub-directories
         (org-files
          (delete-dups
           (append
            ;; Check home directory
            (directory-files home-dir t "^log[0-9]\\{4\\}\\.org$")
            ;; Check immediate subdirectories
            (apply #'append
                  (mapcar
                   (lambda (dir)
                     (when (and (file-directory-p dir)
                              (not (string-prefix-p "." (file-name-nondirectory dir))))
                       (directory-files dir t "^log[0-9]\\{4\\}\\.org$")))
                   (directory-files home-dir t directory-files-no-dot-files-regexp))))))
         ;; Remove home-dir prefix for cleaner display
         (relative-files (mapcar (lambda (f)
                                 (substring f (length home-dir)))
                               org-files))
         ;; Get current agenda files without home-dir prefix
         (current-agenda (mapcar (lambda (f)
                                 (substring f (length home-dir)))
                               org-agenda-files))
         ;; Only show log files not already in agenda-files
         (available-logs (cl-set-difference relative-files current-agenda
                                          :test 'string=))
         ;; Select file using completion
         (selected-file (completing-read
                        (format "Select log####.org file to append (current agenda files: %d): "
                                (length org-agenda-files))
                        (sort available-logs 'string<))))

    ;; Append the selected file with full path if one was selected
    (when (and selected-file (not (string-empty-p selected-file)))
      (let* ((full-path (expand-file-name selected-file home-dir))
             (init-file (expand-file-name "~/e30fewpackages/init.el")))
        (if (member full-path org-agenda-files)
            (message "File already in org-agenda-files: %s" selected-file)
          (progn
            ;; Update the current org-agenda-files
            (setq org-agenda-files (append org-agenda-files (list full-path)))

            ;; Update init.el
            (with-temp-buffer
              (insert-file-contents init-file)
              (goto-char (point-min))
              (when (re-search-forward "(setq org-agenda-files '(" nil t)
                (let ((start (point))
                      (end (save-excursion
                            (forward-sexp)
                            (1- (point)))))
                  ;; Replace the list content
                  (delete-region start end)
                  (insert "\n")
                  (dolist (file org-agenda-files)
                    (insert (format "                             \"%s\"\n" file)))
                  (insert "                         "))
                ;; Save the modified init.el
                (write-region (point-min) (point-max) init-file)))

            (message "Added to org-agenda-files and init.el: %s" selected-file)))))))




;;; org-insert-external-file
(defun mooerslab-org-insert-external-file (file-path)
  "Insert the contents of an external file into the current org-mode file.
Prompts for a file path via minibuffer and includes a timestamp in a comment."
  (interactive "fFile to be inserted: ")
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "#+BEGIN_COMMENT\n# File %s inserted on %s\n#+END_COMMENT\n\n" file-path timestamp))
    (insert-file-contents file-path)
    (goto-char (point-max))))
(global-set-key (kbd "C-c S") 'org-insert-external-file)


;;; org-insert-protocol-file
;% Insert the contents of a protocol file into the current org file.
;% I am using org-capture to do this in org-roam these days.
(defun mooerslab-org-insert-protocol-file (file-path)
  "Insert the contents of a protocol file from ~/org-roam/protocols into the current org-mode file.
Prompts for a file path via minibuffer and includes a timestamp in a comment."
  (interactive (list (read-file-name "Directory `~/org-roam/protocols`: " "~/" "org-roam/" "protocols/")))
  (let ((full-path (expand-file-name file-path))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "#+BEGIN_COMMENT\n# File %s inserted on %s\n#+END_COMMENT\n\n" full-path timestamp))
    (insert-file-contents full-path)
    (goto-char (point-max))))
(global-set-key (kbd "C-c P") 'org-insert-protocol-file)


(defun mooerslab-open-file-in-textmate ()
  "Open the current file or `dired' marked files in TextMate.
   This command is for macOS only. Modified from
   URL `http://xahlee.info/emacs/emacs/emacs_open_in_textedit.html'
   Version: 2017-11-21 2021-02-07 2023-06-26"
  (interactive)
  (when (not (eq system-type 'darwin)) (user-error "Error: TextMate only run in Mac"))
  (let* (
         (xFList (if (eq major-mode 'dired-mode) (dired-get-marked-files) (list buffer-file-name)))
         (xDoIt (if (<= (length xFList) 10) t (y-or-n-p "Open more than 10 files? "))))
    (when xDoIt
      (mapc (lambda (x) (shell-command (format "open -a TextMate.app \"%s\"" x))) xFList))))


(defun mooerslab-org-markmap-region (start end)
  "Export selected org region to a mindmap using markmap.
Requires markmap-cli (npm install -g markmap-cli)."
  (interactive "r")
  (let* ((org-content (buffer-substring-no-properties start end))
         (tmp-org (make-temp-file "org-markmap" nil ".org"))
         (tmp-md (make-temp-file "org-markmap" nil ".md"))
         (tmp-html (make-temp-file "org-markmap" nil ".html")))
    (with-temp-file tmp-org
      (insert org-content))
    ;; Export org to markdown
    (let ((org-export-show-temporary-export-buffer nil))
      (with-temp-buffer
        (insert org-content)
        (org-mode)
        (org-export-to-file 'md tmp-md nil nil nil nil nil)))
    ;; Run markmap-cli to generate HTML
    (let ((cmd (format "markmap %s -o %s" (shell-quote-argument tmp-md) (shell-quote-argument tmp-html))))
      (shell-command cmd))
    ;; Open in default browser
    (browse-url (concat "file://" tmp-html))
    (message "Markmap mindmap generated and opened in browser.")))
(global-set-key (kbd "C-x C-m") 'mooerslab-org-markmap-region)


(defun mooerslab-md-to-latex-region (start end)
  "Convert markdown in region between START and END to LaTeX format.
Uses direct pandoc conversion and carefully handles formatting issues."
  (interactive "r")
  (unless (executable-find "pandoc")
    (user-error "Pandoc not found. Please install pandoc first"))

  (let ((orig-content (buffer-substring-no-properties start end)))
    ;; Convert using pandoc
    (let ((latex-content
           (with-temp-buffer
             (insert orig-content)
             (if (zerop (call-process-region (point-min) (point-max)
                                           "pandoc" t t nil
                                           "-f" "markdown"
                                           "-t" "latex"
                                           "--wrap=preserve"))
                 (buffer-string)
               (message "Pandoc conversion failed")
               nil))))

      (if latex-content
          (progn
            ;; Replace the region with LaTeX content
            (delete-region start end)
            (goto-char start)
            (insert latex-content)

            ;; Clean up common LaTeX formatting issues
            (save-excursion
              ;; Mark the end of our working region
              (let ((end-marker (+ start (length latex-content))))
                (goto-char start)

                ;; Iterate through the buffer
                (while (< (point) end-marker)
                  ;; Handle itemize and enumerate environments
                  (if (looking-at "\\\\begin{\\(itemize\\|enumerate\\)}")
                      (let ((env-type (match-string 1))
                            (current-env-start (point)))

                        ;; Find the matching end of environment
                        (if (re-search-forward (format "\\\\end{%s}" env-type) end-marker t)
                            (progn
                              ;; Clean up double spacing in list environments
                              (goto-char current-env-start)
                              (while (and (< (point) end-marker)
                                          (re-search-forward "\n\n\\\\item" end-marker t))
                                (replace-match "\n\\\\item" nil nil))
                              ;; Move to end of this environment
                              (re-search-forward (format "\\\\end{%s}" env-type) end-marker t))
                          (goto-char end-marker)))
                    ;; Not in a list environment, just move to next line
                    (forward-line 1)))))

            (message "Markdown converted to LaTeX successfully"))

        ;; Restore original on failure
        (message "Failed to convert markdown to LaTeX")
        (delete-region start end)
        (goto-char start)
        (insert orig-content)))))


(defun mooerslab-md-to-org-region (start end)
  "Convert markdown in region between START and END to org-mode format.
Uses direct pandoc conversion and carefully removes blank lines between list items."
  (interactive "r")
  (unless (executable-find "pandoc")
    (user-error "Pandoc not found. Please install pandoc first"))

  (let ((orig-content (buffer-substring-no-properties start end)))
    ;; Convert using pandoc
    (let ((org-content
           (with-temp-buffer
             (insert orig-content)
             (if (zerop (call-process-region (point-min) (point-max)
                                           "pandoc" t t nil
                                           "-f" "markdown"
                                           "-t" "org"
                                           "--wrap=preserve"))
                 (buffer-string)
               (message "Pandoc conversion failed")
               nil))))

      (if org-content
          (progn
            ;; Replace the region with org content
            (delete-region start end)
            (goto-char start)
            (insert org-content)

            ;; Remove blank lines between list items
            (save-excursion
              ;; Mark the end of our working region
              (let ((end-marker (+ start (length org-content))))
                (goto-char start)

                ;; Iterate through the buffer
                (while (< (point) end-marker)
                  (if (looking-at "^\\([ \t]*\\)\\([-*+]\\|[0-9]+\\.\\)\\s-+")
                      (let ((indent-level (length (match-string 1)))
                            (list-type (if (member (match-string 2) '("-" "+" "*"))
                                          'unordered
                                        'ordered))
                            (current-line-start (point)))

                        ;; Move to the next line
                        (forward-line 1)

                        ;; Check if next line is blank followed by another list item of same type/level
                        (when (and (looking-at "^\\s-*$")
                                   (save-excursion
                                     (forward-line 1)
                                     (when (looking-at "^\\([ \t]*\\)\\([-*+]\\|[0-9]+\\.\\)\\s-+")
                                       (and
                                        ;; Same indent level
                                        (= (length (match-string 1)) indent-level)
                                        ;; Same list type
                                        (eq list-type (if (member (match-string 2) '("-" "+" "*"))
                                                        'unordered
                                                      'ordered))))))
                          ;; Delete the blank line
                          (delete-region (point) (progn (forward-line 1) (point)))
                          ;; Move back to check this list item again
                          (goto-char current-line-start)))
                    ;; Not a list item, just move to next line
                    (forward-line 1)))))

            (message "Markdown converted to org-mode successfully"))

        ;; Restore original on failure
        (message "Failed to convert markdown to org")
        (delete-region start end)
        (goto-char start)
        (insert orig-content)))))





;;; convert-init-el-to-init-org
;% The goal is to convert the init.el file to an org file is to be rendered on GitHub or locally in HTML or PDF.
;% This function is a work in progress.
(defun mooerslab-convert-init-el-to-org (input-file output-file)
  "Convert an Emacs init.el file to an Org-mode file."
  (with-temp-buffer
    (insert-file-contents input-file)
    (let ((in-src-block nil))
      (with-temp-file output-file
        (insert "#+TITLE: Emacs Configuration\n")
        (insert "#+AUTHOR: Blaine Mooers\n")
        (insert "#+OPTIONS: toc:nil\n\n")
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ;; Match comments and convert them to org-mode headings or prose
             ((string-match-p "^\\s-*;%+" line)
              (let ((prose (string-trim (replace-regexp-in-string "^\\s-*;%+" "" line))))
                (when in-src-block
                  (insert "#+END_SRC\n\n")
                  (setq in-src-block nil))
                (insert (format "%s\n\n" prose))))
             ((string-match-p "^\\s-*;" line)
              (let* ((level (length (match-string 0 line)))
                     (heading (string-trim (replace-regexp-in-string "^\\s-*;+" "" line))))
                (when in-src-block
                  (insert "#+END_SRC\n\n")
                  (setq in-src-block nil))
                (insert (format "%s %s\n" (make-string level ?*) heading))))
             (t
              (unless in-src-block
                (insert "#+BEGIN_SRC emacs-lisp\n")
                (setq in-src-block t))
              (insert (format "%s\n" line))))
            (forward-line 1)))
        (when in-src-block
          (insert "#+END_SRC\n"))))))
;% Example usage:
;% (convert-init-el-to-org "~/path/to/init.el" "~/path/to/init.org")


;;; Convert a selected latex list of items to an org-mode list
;%  To use this function, select the region containing the LaTeX list and run:
;%  M-x latex-to-org-list-region
(defun mooerslab-latex-to-org-list-region (start end)
  "Convert a LaTeX itemize list in the region to an Org-mode list."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\\\item" end t)
      (replace-match "-"))))

;;; mooerslab-region-csv-to-markdown-table
;% Convert selected rows in CSV format into a org-table.
;% It does not try to use these commas as field separators.
;% Respects the commas inside of strings.
;% The next three functions are part of the same functionality.
(defun mooerslab-region-csv-to-markdown-table ()
  "Convert CSV data in region to GitHub-style markdown table format.
Assumes first row contains headers. Properly handles quoted fields with commas."
  (interactive)
  (if (not (region-active-p))
      (message "No region selected")
    (let* ((start (region-beginning))
           (end (region-end))
           (csv-text (buffer-substring-no-properties start end))
           (parsed-rows (mooerslab-parse-csv-text csv-text)))
      ;; Delete region content
      (delete-region start end)
      ;; Insert markdown table header
      (when parsed-rows
        ;; Insert header row
        (insert "| "
                (mapconcat 'identity (car parsed-rows) " | ")
                " |")
        ;; Insert separator row with correct number of columns
        (insert "\n| ")
        (insert (mapconcat (lambda (_) "---") (car parsed-rows) " | "))
        (insert " |\n")
        ;; Insert data rows
        (dolist (row (cdr parsed-rows))
          (when row
            (insert "| "
                    (mapconcat 'identity row " | ")
                    " |\n")))))))

(defun mooerslab-parse-csv-text (text)
  "Parse CSV text into a list of rows, each row being a list of fields.
Properly handles quoted strings containing commas."
  (let ((rows (split-string text "\n" t)))
    (mapcar 'mooerslab-parse-csv-row rows)))

(defun mooerslab-parse-csv-row (row)
  "Parse a CSV row, properly handling quoted strings with commas."
  (let ((fields nil)
        (i 0)
        (len (length row))
        (current-field "")
        (in-quotes nil)
        (quote-char nil))

    (while (< i len)
      (let ((char (aref row i)))
        (cond
         ;; Handle quotes (both single and double)
         ((and (or (char-equal char ?\") (char-equal char ?\'))
               (or (not in-quotes) (char-equal char quote-char)))
          (if in-quotes
              (setq in-quotes nil)
            (setq in-quotes t
                  quote-char char)))

         ;; Handle commas - only treat as field separators when not in quotes
         ((and (char-equal char ?,) (not in-quotes))
          (push current-field fields)
          (setq current-field ""))

         ;; Add character to current field
         (t (setq current-field (concat current-field (string char))))))
      (setq i (1+ i)))

    ;; Add the last field
    (push current-field fields)

    ;; Cleanup: trim spaces and remove surrounding quotes
    (setq fields (nreverse fields))
    (mapcar (lambda (field)
              (setq field (string-trim field))
              ;; Remove surrounding quotes if present
              (when (and (> (length field) 1)
                         (or
                          (and (char-equal (aref field 0) ?\")
                               (char-equal (aref field (1- (length field))) ?\"))
                          (and (char-equal (aref field 0) ?\')
                               (char-equal (aref field (1- (length field))) ?\'))))
                (setq field (substring field 1 (1- (length field)))))
              field)
            fields)))


;;;; mooerslab-region-csv-to-org-table
;% Convert selected rows in CSV format into a org-table.
;% It does not try to use these commas as field separators.
;% Respects the commas inside of strings.
;% The next three functions are part of the same functionality.
(defun mooerslab-region-csv-to-org-table ()
  "Convert CSV data in region to org table format.
Assumes first row contains headers. Properly handles quoted fields with commas."
  (interactive)
  (if (not (region-active-p))
      (message "No region selected")
    (let* ((start (region-beginning))
           (end (region-end))
           (csv-text (buffer-substring-no-properties start end))
           (parsed-rows (mooerslab-parse-csv-text-org csv-text)))
      ;; Delete region content
      (delete-region start end)
      ;; Insert org table header
      (when parsed-rows
        (insert "| "
                (mapconcat 'identity (car parsed-rows) " | ")
                " |\n|-\n")
        ;; Insert data rows
        (dolist (row (cdr parsed-rows))
          (when row
            (insert "| "
                    (mapconcat 'identity row " | ")
                    " |\n"))))
      ;; Align the table
      (org-table-align))))

(defun mooerslab-parse-csv-text-org (text)
  "Parse CSV text into a list of rows, each row being a list of fields.
Properly handles quoted strings containing commas."
  (let ((rows (split-string text "\n" t)))
    (mapcar 'mooerslab-parse-csv-row-org rows)))

(defun mooerslab-parse-csv-row-org (row)
  "Parse a CSV row, properly handling quoted strings with commas."
  (let ((fields nil)
        (i 0)
        (len (length row))
        (current-field "")
        (in-quotes nil)
        (quote-char nil))

    (while (< i len)
      (let ((char (aref row i)))
        (cond
         ;; Handle quotes (both single and double)
         ((and (or (char-equal char ?\") (char-equal char ?\'))
               (or (not in-quotes) (char-equal char quote-char)))
          (if in-quotes
              (setq in-quotes nil)
            (setq in-quotes t
                  quote-char char)))

         ;; Handle commas - only treat as field separators when not in quotes
         ((and (char-equal char ?,) (not in-quotes))
          (push current-field fields)
          (setq current-field ""))

         ;; Add character to current field
         (t (setq current-field (concat current-field (string char))))))
      (setq i (1+ i)))

    ;; Add the last field
    (push current-field fields)

    ;; Cleanup: trim spaces and remove surrounding quotes
    (setq fields (nreverse fields))
    (mapcar (lambda (field)
              (setq field (string-trim field))
              ;; Remove surrounding quotes if present
              (when (and (> (length field) 1)
                         (or
                          (and (char-equal (aref field 0) ?\")
                               (char-equal (aref field (1- (length field))) ?\"))
                          (and (char-equal (aref field 0) ?\')
                               (char-equal (aref field (1- (length field))) ?\'))))
                (setq field (substring field 1 (1- (length field)))))
              field)
            fields)))

;;; create-org-table-with-caption
;%  This interactive function prompts the user to select the table's number of rows, columns, and caption.
(defun mooerslab-create-org-table-with-caption ()
  "This interactive function prompts the user for the number of rows. columns, and the caption of the table."
  (interactive)
  (let ((rows (read-number "Enter the number of rows: "))
        (cols (read-number "Enter the number of columns: "))
        (label (read-string "Enter the table label: "))
        (caption (read-string "Enter the table's caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert "|")
    (dotimes (_ cols)
      (insert "----+"))
    (insert "\n|")
    (dotimes (col cols)
      (insert (format " %c |" (+ ?A col))))
    (insert "\n|")
    (dotimes (_ cols)
      (insert "----+"))
    (insert "\n")
    (dotimes (_ rows)
      (insert "|")
      (dotimes (_ cols)
        (insert "     |"))
      (insert "\n"))
    (insert "|")
    (dotimes (_ cols)
      (insert "----+"))))


;;; mooerslab-org-table-to-markdown
;% This function is useful when preparing tables in org-mode and
;% then exporting them to a GitHub README.md file,
;% How to use it:
;%
;% 1. Place your cursor anywhere inside an org-mode table
;% 2. Run M-x mooerslab-org-table-to-markdown
;% 3. The org table will be replaced with a GitHub-style markdown table
;%
;% Key Technical Details
;%
;% The function uses org-table-get-rectangle, org-table-begin, and org-table-end to work with the current table.
;% It detects separator rows by checking if all cells in a row match the pattern ^[-+]+$ (contain only - or + characters).
;% The function preserves all cell content, including any formatting inside the cells.
;% It handles edge cases like missing trailing pipe characters at the end of rows.
(defun mooerslab-org-table-to-markdown ()
  "Convert an org-mode table at point to GitHub-style markdown table.
The function operates on the current org table, replacing it with GitHub markdown format."
  (interactive)
  (save-excursion
    (let ((table-bounds (org-table-get-rectangle))
          (table-begin (org-table-begin))
          (table-end (org-table-end))
          (first-row t)
          rows)

      ;; First collect all the rows from the org table
      (goto-char table-begin)
      (while (< (point) table-end)
        (when (looking-at org-table-line-regexp)
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 ;; Remove outer | characters and split by |
                 (cells (split-string
                         (substring line 1 (- (length line)
                                             (if (string-suffix-p "|" line) 1 0)))
                         "|"))
                 ;; Trim whitespace from each cell
                 (trimmed-cells (mapcar #'string-trim cells)))
            ;; Skip org-mode separator lines (containing only --- or ---)
            (unless (and (not first-row)
                         (cl-every (lambda (cell)
                                    (string-match-p "^[-+]+$" (string-trim cell)))
                                  cells))
              (push trimmed-cells rows)
              (when first-row
                (setq first-row nil)))))
        (forward-line 1))

      ;; Reverse the rows since we collected them in reverse order
      (setq rows (nreverse rows))

      ;; Delete the original table
      (delete-region table-begin table-end)

      ;; Insert the markdown table
      (goto-char table-begin)

      ;; Insert header row
      (when rows
        (insert "| " (mapconcat #'identity (car rows) " | ") " |")

        ;; Insert separator row
        (insert "\n| ")
        (insert (mapconcat (lambda (_) "---") (car rows) " | "))
        (insert " |")

        ;; Insert data rows
        (dolist (row (cdr rows))
          (insert "\n| " (mapconcat #'identity row " | ") " |"))

        ;; Add final newline
        (insert "\n")))))

;;;  count-non-blank-lines
;% Count the number of non-blank lines in the current buffer.
(defun mooerslab-count-non-blank-lines ()
  "Count the number of non-blank lines in the current buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (not (looking-at "^\\s-*$"))
          (setq count (1+ count)))
        (forward-line 1)))
    (message "Number of non-blank lines: %d" count)))


;;;## Convert CSV file into an org table. It will convert internal commas inside strings into pipes.
(defun mooerslab-csv2org (csv-file &optional caption)
  "Convert a CSV file to an Org-mode table.

Prompts for a CSV file and optionally a caption.  Creates a new
buffer containing the Org-mode table.  Does NOT handle CSV files with
quoted fields containing commas.

CSV-FILE: The path to the CSV file.
CAPTION: (Optional) A string to use as the table caption."
  (interactive "fCSV file: \nsCaption (optional): ")
  (let* ((output-buffer (generate-new-buffer (format "*%s-org*" (file-name-nondirectory csv-file))))
         (csv-data (with-temp-buffer
                     (insert-file-contents csv-file)
                     (split-string (buffer-string) "\n" t)))
         (header (pop csv-data))  ; Extract header and remove from csv-data
         (org-table-lines '()))

    (when (and caption (not (string-empty-p caption)))
      (push (concat "#+CAPTION: " caption) org-table-lines))

    ;; Process header
    (push (concat "|" (mapconcat (lambda (field)
                                  (format " %s " (replace-regexp-in-string "^\"\\(.*\\)\"$" "\\1" field)))
                                (split-string header "," t)
                                "|") "|")  ; Add pipes correctly
          org-table-lines)

    ;; Create separator line
    (push (concat "|" (mapconcat (lambda (field) (make-string (length field) ?-) )
                                (split-string header "," t)
                                "|") "|")
          org-table-lines)

    ;; Process data rows
    (dolist (row csv-data)
      (push (concat "|" (mapconcat (lambda (field)
                                  (format " %s " (replace-regexp-in-string "^\"\\(.*\\)\"$" "\\1" field)))
                                (split-string row "," t nil)
                                "|") "|") ; Add pipes correctly
            org-table-lines))

    ;; Create bottom rule (same as separator)
      (push (concat "|" (mapconcat (lambda (field) (make-string (length field) ?-) )
                                (split-string header "," t)
                                "|") "|")
          org-table-lines)

    ;; Insert into output buffer
    (with-current-buffer output-buffer
      (dolist (line (reverse org-table-lines))
        (insert (concat line "\n")))
      (org-mode) ; Switch to Org mode
      (goto-char (point-min))) ; Go to beginning of buffer

    (switch-to-buffer output-buffer)))


;;; export-csv-to-quiz-table
;% Usage example:
;% (export-csv-to-qiterm "~/6233iterm/qiterm.csv" "~/6233iterm/qiterm.db" "qiterm")
(with-eval-after-load 'sqlite
(defun mooerslab-export-csv-to-sqlite-table (csv-file db-file table-name)
  "Export selected rows from a CSV file to an SQLite database."
  (interactive "fCSV file: \nfSQLite DB file: \nsTable name: ")
  (let ((db (sqlite3-open db-file))
        (rows (with-temp-buffer
                (insert-file-contents csv-file)
                (split-string (buffer-string) "\n" t))))
    (dolist (row rows)
      (let ((values (split-string row ",")))
        (sqlite3-exec db
                      (format "INSERT INTO %s VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');"
                              table-name
                              (nth 0 values)
                              (nth 1 values)
                              (nth 2 values)
                              (nth 3 values)
                              (nth 4 values)
                              (nth 5 values)
                              (nth 6 values)
                              (nth 7 values)))))
    (sqlite3-close db)
    (message "Data successfully appended to %s" table-name)))
)


;;; export-csv-to-matched-sqlite-table
;% Usage example:
;% (export-csv-to-matched-sqlite-table "~/6233iterm/qiterm.csv" "~/6233iterm/qiterm.db" "qiterm")
(with-eval-after-load 'sqlite
(defun mooerslab-export-csv-to-matched-sqlite-table  (csv-file db-file table-name)
  "Export selected rows from a CSV file to an SQLite database.
Automatically determines column count and validates against table structure."
  (interactive "fCSV file: \nfSQLite DB file: \nsTable name: ")
  (let* ((db (sqlite3-open db-file))
         ;; Get table column count from database
         (table-info (sqlite3-exec db (format "PRAGMA table_info(%s)" table-name)))
         (db-column-count (length table-info))
         ;; Read and process CSV
         (rows (with-temp-buffer
                (insert-file-contents csv-file)
                (split-string (buffer-string) "\n" t)))
         ;; Get CSV column count from first row
         (csv-column-count (length (split-string (car rows) ","))))

    ;; Validate column counts match
    (if (not (= csv-column-count db-column-count))
        (progn
          (sqlite3-close db)
          (error "Column count mismatch: CSV has %d columns, table has %d columns"
                 csv-column-count db-column-count))

      ;; Process rows if validation passes
      (dolist (row rows)
        (let* ((values (split-string row ","))
               ;; Create placeholders for SQL query (?,?,?) based on column count
               (placeholders (mapconcat (lambda (_) "?")
                                      (make-list csv-column-count nil)
                                      ","))
               ;; Create SQL query with proper number of placeholders
               (query (format "INSERT INTO %s VALUES (%s)"
                            table-name placeholders)))
          ;; Execute the query with values
          (sqlite3-exec db query values)))

      (sqlite3-close db)
      (message "Data successfully appended to %s" table-name))))
)


;;; find file and go to line number
;%  interactively enter the file name and line number in the minibuffer
(defun mooerslab-find-file-at-line (file line)
  "Open FILE on LINE."
  (interactive "fFile: \nNLine: \n")
  (find-file file)
  (goto-line line))
;;; ffap: find file at point
;%  https://unix.stackexchange.com/questions/691444/how-do-i-open-a-file-at-specific-line-in-a-running-emacs
;%  have ffap pick up line number and goto-line
;%  found on emacswiki: https://www.emacswiki.org/emacs/FindFileAtPoint#h5o-6
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")
(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
    save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (forward-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))



;; M-x mooerslab-generate-tar-commands-with-chain after making a selection of a list of file paths to
;; generate the tar commands to tar the folders in the current directory from which the
;; tar command is issue. This is very helpful when the harddrive is full or read-only.
(defun mooerslab-generate-tar-commands (start end)
  "Generate tar commands for a list of paths.
   Each path's last component becomes the name of the tar file.
   START and END define the region containing the paths (one per line).
   If no region is active, operate on the entire buffer."
  (interactive "r")
  (unless (region-active-p)
    (setq start (point-min))
    (setq end (point-max)))

  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((result ""))
        (while (not (eobp))
          (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                 (path (string-trim line))
                 (path-without-trailing-slash (replace-regexp-in-string "/$" "" path)))

            (when (not (string= path ""))
              (let* ((dir-name (file-name-nondirectory path-without-trailing-slash))
                     (parent-dir (file-name-directory path-without-trailing-slash))
                     (tar-name (concat dir-name ".tar"))
                     (tar-command (format "tar cvf %s -C %s %s .\n"
                                         tar-name
                                         parent-dir
                                         dir-name)))
                (setq result (concat result tar-command)))))

          (forward-line 1))

        (delete-region (point-min) (point-max))
        (insert result)))))

(defun mooerslab-generate-tar-commands-with-chain (start end)
  "Generate tar commands for a list of paths with && between commands.
   Each path's last component becomes the name of the tar file.
   START and END define the region containing the paths (one per line).
   If no region is active, operate on the entire buffer."
  (interactive "r")
  (unless (region-active-p)
    (setq start (point-min))
    (setq end (point-max)))

  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((result ""))
        (while (not (eobp))
          (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                 (path (string-trim line))
                 (path-without-trailing-slash (replace-regexp-in-string "/$" "" path)))

            (when (not (string= path ""))
              (let* ((dir-name (file-name-nondirectory path-without-trailing-slash))
                     (parent-dir (file-name-directory path-without-trailing-slash))
                     (tar-name (concat dir-name ".tar"))
                     (is-last-line (save-excursion
                                     (forward-line 1)
                                     (eobp)))
                     (chain (if is-last-line "" " &&"))
                     (tar-command (format "tar cvf %s -C %s %s .%s\n"
                                         tar-name
                                         parent-dir
                                         dir-name
                                         chain)))
                    (setq result (concat result tar-command)))))

              (forward-line 1))

            (delete-region (point-min) (point-max))
            (insert result)))))


;;; get-citekeys-from-bibtex-file
;% used to work with annotated bibliography. Returns a list under the cursor in the current buffer.
(defun mooerslab-get-citekeys-from-bibtex-file ()
  "Prompt for a BibTeX filename in the minibuffer, extract citekeys, and insert an alphabetized itemized list into the current buffer at the cursor position."
  (interactive)
  (let* ((filename (read-file-name "BibTeX file: ")) ;; Prompt for the BibTeX file
         (citekeys '())) ;; Initialize an empty list to store citekeys
    (if (not (file-exists-p filename))
        (message "File does not exist: %s" filename)
      ;; Process the BibTeX file
      (with-temp-buffer
        (insert-file-contents filename) ;; Read the contents of the file into the buffer
        (goto-char (point-min)) ;; Move to the beginning of the buffer
        ;; Search for BibTeX entry keys (e.g., @article{citekey, ...)
        (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
          (let ((citekey (match-string 1))) ;; Extract the citekey from the match
            (push citekey citekeys))) ;; Add the citekey to the list
        ;; Sort the citekeys alphabetically
        (setq citekeys (sort citekeys #'string<))))
    ;; Insert the formatted list into the current buffer
    (let ((formatted-citekeys
           (mapconcat (lambda (key) (concat " " key)) citekeys "\n")))
      (insert formatted-citekeys "\n"))))


;;; wrap-citekey-and-create-tex-file
;% Used to convert a citekey into a section heading.
(defun mooerslab-wrap-citekey-and-create-abibnote-tex ()
  "Replace the citekey under the cursor with LaTeX-wrapped text, create a corresponding .tex file, and open it in a new buffer."
  (interactive)
  (let* ((citekey (thing-at-point 'word t)) ;; Get the citekey under the cursor
         (tex-file-dir "/Users/blaine/abibNotes/") ;; Directory for the .tex file
         (tex-file-path (concat tex-file-dir citekey ".tex")) ;; Full path for the .tex file
         (wrapped-text (format "\\subsection*{\\bibentry{%s}}\n\\Addcontentsline{toc}{subsection}{%s}\n\\input{%s}"
                               citekey citekey tex-file-path))) ;; LaTeX-wrapped text
    (if (not citekey)
        (message "No citekey found under the cursor.")
      (progn
        ;; Delete the citekey under the cursor
        (let ((bounds (bounds-of-thing-at-point 'word)))
          (delete-region (car bounds) (cdr bounds)))
        ;; Insert the wrapped text in its place
        (insert wrapped-text)
        ;; Create the .tex file if it doesn't already exist
        (if (file-exists-p tex-file-path)
            (message "File %s already exists." tex-file-path)
          (with-temp-file tex-file-path
            (insert (format "%% This is the .tex file for citekey: %s\n" citekey))))
        ;; Open the .tex file in a new buffer
        (find-file tex-file-path)
        (message "Replaced citekey, created .tex file, and opened it: %s" tex-file-path)))))


;;; insert-org-captioned-figure
;%  The function prompts the user for the image file path and name, the label, and the caption.
(defun mooerslab-insert-org-captioned-figure ()
  "Insert a captioned figure in Org-mode."
  (interactive)
  (let ((image-name (read-string "Enter the image file path: "))
        (label (read-string "Enter the figure label: "))
        (caption (read-string "Enter the figure caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert (format "[[file:%s]]\n" image-name))))


;;; launch-ithoughtsx
;% This is the best mind mapping software that I have encountered.
;% Probably better to make a bash alias to avoid tying up keybindings.
(defun mooerslab-launch-ithoughtsx ()
  "Launch iThoughtsX application."
  (interactive)
  (shell-command "open -a iThoughtsX"))
(global-set-key (kbd "C-c i") 'launch-ithoughtsx)


; ;;; launch-jabref
; ;% I favored the simplicity and power of JabRef for managing BibTeX entries.
;% Probably better to make a bash alias to avoid tying up keybindings.
;% The Emacs analog is ebib, which is awesome.
; (defun mooerslab-launch-jabref ()
;       "Launch jabRef application."
;       (interactive)
;       (shell-command "open -a JabRef"))
;     (global-set-key (kbd "C-c j") 'launch-jabref)


;;; launch-timesspent
;% This is a sqlite database where I track my effort.
;% Probably better to make a bash alias to avoid tying up keybindings.
;(defun mooerslab-launch-timesspent ()
;      "Launch timesspent database."
;     (interactive)
;      (shell-command "open /Users/blaine/6003TimeTracking/cb/mytime.db"))
;    (global-set-key (kbd "C-c e") 'launch-timesspent)


;;; Move the cursor to the minibuffer without using the mouse
;%  From video https://www.youtube.com/watch?v=X8c_TrGfYcM&t=15s using Emacs as a multiplexer."
;%  Derived from http://stackoverflow.com/a/4116113/446256.
(defun mooerslab-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key "\C-cm" 'switch-to-minibuffer) ;; Bind to `C-c m' for minibuffer.
;; (global-set-key (kbd ".") 'self-insert-command) ;; Unbind the settin below.
(global-set-key (kbd ".") 'repeat) ; analog of the Vim command.


;;; Open a file and move to a headline with a specific tag
;%  The default tag is restart-here.
;%  Example usage:
;%  (open-org-file-and-move-to-tag "~/path/to/your/file.org" "your-tag")
(defun mooerslab-open-org-file-and-move-to-tag (file &optional tag)
  "Open an Org file and move the cursor below a headline with a specific TAG.
If TAG is not provided, use a hardcoded default tag.
You have to adjust the headline level in the function.
The regular expression ^\\*\\* .*:%s: is used to search for second-level headlines (starting with **) with the specified tag."
  (interactive "fOrg file: \nsTag (leave empty for default): ")
  (let ((tag (if (string= tag "") "restart-here" tag)))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\*\\* .*:%s:" tag) nil t)
        (org-end-of-subtree)
      (message "Tag not found"))))


;;; Move cursor to line with tag
(defun mooerslab-org-move-to-tag (file &optional tag)
  "Move the cursor below a headline with a specific TAG.
If TAG is not provided, use a hardcoded default tag.
You have to adjust the headline level in the function.
The regular expression ^\\*\\* .*:%s: is used to search for second-level headlines (starting with **) with the specified tag."
  (interactive "fOrg file: \nsTag (leave empty for default): ")
  (let ((tag (if (string= tag "") "appendtodos" tag)))
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\*\\* .*:%s:" tag) nil t)
        (org-end-of-subtree)
      (message "Tag not found"))))


(defun mooerslab-org-append-todo-to-tagged-fourth-level-headline (new-todo &optional tag)
  "Append a new TODO item to the bottom of the TODO list under a 3rd level headline marked by TAG.
If TAG is not provided, it defaults to the :appendtodos: tag. This is for the writingLog.org file.
USAGE: M-x mooerslab-append-todo-to-tagged-headline. Answer the prompts. Works regardless of the position of the
point relative to the headline with the tag."
  (interactive "sNew TODO: \nsTag (default appendtodos): ")
  (let ((tag (if (string-empty-p tag) "appendtodos" tag)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*\\*\\*.*:" tag ":") nil t)
        (org-end-of-subtree)
        (insert (concat "\n**** TODO " new-todo))))))


;;; open-template-with-citekey
;% Open template file renamed with the citekey under the point.
;% This file is for use with an annotated bibliography.
;% Developed with the help of CoPilot.
(defun mooerslab-open-new-abibnote-on-citekey ()
  "Open a template file in Org-mode, rename it to the citekey under the cursor,
  and save it to '~/abibNotes/'. Citar has a function that will insert the citekey."
  (interactive)
  (let* ((citekey (thing-at-point 'word t))
         (template-file "~/abibNotes/templates/abib-template.org")
         (output-dir "~/abibNotes/")
         (output-file (concat output-dir citekey ".org")))
    (unless (file-exists-p output-dir)
      (make-directory output-dir t))
    (if (and citekey (file-exists-p template-file))
        (progn
          (copy-file template-file output-file t)
          (find-file output-file)
          (message "Template file saved as %s" output-file))
      (message "Citekey or template file not found"))))
(global-set-key (kbd "C-c z") 'open-new-abibnote-on-citekey)


;;; Play a YouTube video distraction-free with mpv
;%  You insert the YouTube url in the minibufffer.
;%  You have to install mpv with a package manager and another binary package.
;%  sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl
;%  sudo chmod a+rx /usr/local/bin/youtube-dl
;;;; play-youtube-video
(defun mooerslab-play-youtube-video (url)
  "Play a YouTube video with mpv."
  (interactive "sYouTube URL: ")
  (start-process "mpv" nil "mpv" URL))


;;; Reload the initialization file init.el.
(defun mooerslab-reload-init-e30mb ()
  "Reload the init.el file for e30mb. Edit the path to suit your needs."
  (interactive)
  (load-file "~/e30mb/init.el"))


;;; Open the init.org file for editing.
(defun mooerslab-open-initorg-e30mb ()
  "Open the init.org file for editing. Edit the path to suit your needs."
  (interactive)
  (find-file "~/e30mb/init.org"))


;;; Open the mooerslab.el file for editing.
(defun mooerslab-open-mooerslab-functions ()
  "Open the mooerslab.el file for editing. Edit the path to suit your needs."
  (interactive)
  (find-file "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab.el"))


;;; Reload the my-hydras file after editing it in Emacs.
(defun mooerslab-reload-my-hydras ()
    "Reload my-hydras.el. Edit the path to suit your needs."
    (interactive)
    (load-file "~/e30fewpackages/my-hydras/my-hydras.el"))


;;; Open the init.el my-hydras for editing.
(defun mooerslab-open-my-hydras ()
  "Open the init.el file for editing. Edit the path to suit your needs."
  (interactive)
  (find-file "~/e30fewpackages/my-hydras/my-hydras.el"))

;; send to claude
(defun mooerslab-send-to-claude (start end)
  "Send the selected (and copied) region to the Claude desktop application and submit it.
   Requires xdotool on Linux or osascript on macOS.
   The scope of the C-c C-c key binding is limited to the scratch buffer."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties start end)))
    ;; Copy the selected text to clipboard
    (kill-new selected-text)

    (cond
     ;; macOS implementation
     ((eq system-type 'darwin)
      (shell-command
       (concat
        "osascript -e '"
        "tell application \"Claude\" to activate\n"
        "delay 0.5\n"
        "tell application \"System Events\"\n"
        "  keystroke \"v\" using command down\n"
        "  delay 1.0\n"
        "  keystroke return\n"
        "end tell'")))

     ;; Linux implementation using xdotool
     ((eq system-type 'gnu/linux)
      (shell-command
       (concat
        "xdotool search --name \"Claude\" windowactivate --sync && "
        "xdotool key --clearmodifiers ctrl+v && "
        "sleep 1.0 && "
        "xdotool key Return")))

     ;; Windows implementation
     ((eq system-type 'windows-nt)
      (shell-command
       (concat
        "powershell -command \""
        "(New-Object -ComObject WScript.Shell).AppActivate('Claude') | Out-Null; "
        "Start-Sleep -Milliseconds 500; "
        "[System.Windows.Forms.SendKeys]::SendWait('^v'); "
        "Start-Sleep -Milliseconds 200; "
        "[System.Windows.Forms.SendKeys]::SendWait('{ENTER}')\"")))

     (t (message "Unsupported operating system")))))

;; Instead of a global key binding, use a local binding only in scratch buffer
(with-current-buffer "*scratch*"
  (local-set-key (kbd "C-c C-c") 'send-to-claude))

;; To ensure the binding is set every time scratch is created/visited
(defun setup-scratch-buffer-keys ()
  "Set up key bindings specific to the scratch buffer."
  (when (string= (buffer-name) "*scratch*")
    (local-set-key (kbd "C-c C-c") 'send-to-claude)))


;;; Spawn a new shell with the supplied title
(defun mooerslab-spawn-shell (name)
  "Invoke shell test"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer))
  (process-send-string nil "echo 'test1'\n")
  (process-send-string nil "echo 'test2'\n"))




(defun mooerslab-org-roam-list-titles-and-tags ()
  "Display the titles and tags of all org-roam notes in a buffer, one note per row.
Useful for feeding into Claude to make atomic notes from a chunk of prose."
  (interactive)
  (let* ((buffer-name "*Org-Roam Titles and Tags*")
         (buffer (get-buffer-create buffer-name))
         (all-nodes (org-roam-db-query [:select [id file title] :from nodes
                                         :where (= level 0)]))
         (all-tags (org-roam-db-query [:select [node-id tag] :from tags])))

    ;; Clear the buffer and set it up
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Org-Roam Notes: Titles and Tags\n\n")

      ;; Process each node
      (dolist (node all-nodes)
        (let* ((id (nth 0 node))
               (file (nth 1 node))
               (title (nth 2 node))
               (node-tags (mapcar #'cadr
                                 (seq-filter (lambda (tag-info)
                                               (string= (car tag-info) id))
                                             all-tags)))
               (tags-str (if node-tags
                             (concat " " (mapconcat (lambda (tag)
                                                      (format ":%s:" tag))
                                                    node-tags
                                                    " "))
                           "")))

          ;; Insert the title and tags
          (insert (format "* %s%s\n"
                          (or title (file-name-nondirectory file) "Untitled")
                          tags-str))))

      ;; Finalize buffer setup
      (goto-char (point-min))
      (org-overview))

    ;; Switch to the buffer
    (switch-to-buffer buffer)
    (message "Found %d org-roam notes" (length all-nodes))))


(defun mooerslab-org-roam-list-metadata-all-notes ()
  "Display the titles, tags, and backlinks of all org-roam notes in a buffer, one note per headline.
The list is a org-roam hierarchical tree of headlines with the backlinks listed below the headlines.
Each backlink is displayed with its full ID link format: [[id:NODE-ID][TITLE]].
Useful for feeding into Claude to make atomic notes from a chunk of prose and have Claude include the backlinks."
  (interactive)
  ;; Define a dummy flyover-mode if it doesn't exist
  (unless (fboundp 'flyover-mode)
    (defun flyover-mode (&optional arg)
      "Dummy function to prevent void function error.
This is a temporary placeholder for the missing flyover-mode."
      (interactive)
      nil))

  (let* ((buffer-name "*Org-Roam Titles, Tags, and Backlinks*")
         (buffer (get-buffer-create buffer-name))
         (all-nodes (org-roam-db-query [:select [id file title] :from nodes
                                         :where (= level 0)]))
         (all-tags (org-roam-db-query [:select [node-id tag] :from tags]))
         ;; Fixed query to use the correct column names
         (all-links (org-roam-db-query [:select [source dest] :from links
                                        :where (= type "id")])))

    ;; Clear the buffer and set it up
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Org-Roam Notes: Titles, Tags, and Backlinks\n\n")

      ;; Process each node
      (dolist (node all-nodes)
        (let* ((id (nth 0 node))
               (file (nth 1 node))
               (title (nth 2 node))
               (node-tags (mapcar #'cadr
                                 (seq-filter (lambda (tag-info)
                                               (string= (car tag-info) id))
                                             all-tags)))
               (tags-str (if node-tags
                             (concat " " (mapconcat (lambda (tag)
                                                      (format ":%s:" tag))
                                                    node-tags
                                                    " "))
                           ""))
               ;; Get backlinks (nodes that link to this node)
               (backlinks (seq-filter (lambda (link)
                                        (string= (cadr link) id))
                                      all-links))
               (backlink-ids (mapcar #'car backlinks)))

          ;; Insert the title and tags
          (insert (format "* %s%s\n"
                          (or title (file-name-nondirectory file) "Untitled")
                          tags-str))

          ;; Add backlinks section if there are any
          (when backlinks
            (insert "** Backlinks:\n")
            ;; For each backlink, find the node info and display title and tags
            (dolist (backlink-id backlink-ids)
              (let* ((backlink-node (car (seq-filter (lambda (n)
                                                       (string= (car n) backlink-id))
                                                     all-nodes)))
                     (backlink-title (and backlink-node (nth 2 backlink-node)))
                     (backlink-file (and backlink-node (nth 1 backlink-node)))
                     (backlink-tags (mapcar #'cadr
                                            (seq-filter (lambda (tag-info)
                                                         (string= (car tag-info) backlink-id))
                                                       all-tags)))
                     (backlink-tags-str (if backlink-tags
                                           (concat " " (mapconcat (lambda (tag)
                                                                    (format ":%s:" tag))
                                                                  backlink-tags
                                                                  " "))
                                         "")))
                (if backlink-node
                    (insert (format "   - [[id:%s][%s]]%s\n"
                                   backlink-id
                                   (or backlink-title
                                       (file-name-nondirectory backlink-file)
                                       "Untitled")
                                   backlink-tags-str))
                  (insert (format "   - Unknown node: %s\n" backlink-id))))))))

      ;; Finalize buffer setup
      (goto-char (point-min))
      (org-overview))

    ;; Switch to the buffer
    (switch-to-buffer buffer)
    (message "Found %d org-roam notes" (length all-nodes))))


(defun mooerslab-org-replace-in-directory (dir old-text new-text)
  "Replace OLD-TEXT with NEW-TEXT in all .org files in DIR and its subdirectories.
Arguments:
  DIR      - The directory to search for .org files. End path with a slash.
  OLD-TEXT - The text to be replaced
  NEW-TEXT - The text to replace with"
  (interactive
   (list
    (read-directory-name "Directory to process: " default-directory)
    (read-string "Text to replace: ")
    (read-string "Replace with: ")))

  ;; Ensure directory path ends with a slash
  (setq dir (file-name-as-directory dir))

  ;; Find all .org files in the directory and subdirectories
  (let* ((org-files (directory-files-recursively dir "\\.org$"))
         (count 0)
         (files-changed 0))

    (if (null org-files)
        (message "No .org files found in %s" dir)

      ;; Process each file
      (dolist (file org-files)
        (let ((file-changed nil))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))

            ;; Replace all occurrences in the file
            (let ((replacements 0))
              (while (search-forward old-text nil t)
                (replace-match new-text t t)
                (setq replacements (1+ replacements))
                (setq count (1+ count))
                (setq file-changed t))

              ;; Save the file if changes were made
              (when file-changed
                (setq files-changed (1+ files-changed))
                (write-region (point-min) (point-max) file)
                (message "Updated %s - %d replacements" file replacements)))))

      ;; Report the results
      (message "Replaced %d occurrences of '%s' with '%s' in %d out of %d files"
               count old-text new-text files-changed (length org-files))))))


(defun mooerslab/generate-reading-list-for-project (project)
  "Generate an Org-mode reading list for a specific project based on annotations in custom-made project field in ebib.
  Here is an example:
  (setq ebib-extra-fields
        '((keywords \"Keywords\" nil)
          (projects \"Projects\" nil)
          (readstatus \"ReadStatus\" nil)
          (priority \"Priority\" nil)
          (notes \"Notes\" t)))
  "
  (interactive "sProject name: ")
  (let ((buf (get-buffer-create (format "*Reading List: %s*" project))))
    (with-current-buffer buf
      (org-mode)
      (erase-buffer)
      (insert (format "#+TITLE: Reading List for %s\n\n" project))
      (insert "* Papers to Read\n\n")
      ;; Get entries from Ebib
      (ebib-db-list-keys (ebib--get-current-db))
      ;; Process each entry
      (dolist (key (ebib-db-list-keys (ebib--get-current-db)))
        (let* ((entry (ebib-db-get-entry key (ebib--get-current-db)))
               (projects (ebib-get-field-value "projects" entry (ebib--get-current-db)))
               (readstatus (ebib-get-field-value "readstatus" entry (ebib--get-current-db)))
               (title (ebib-get-field-value "title" entry (ebib--get-current-db)))
               (authors (ebib-get-field-value "author" entry (ebib--get-current-db))))
          (when (and projects (string-match project projects))
            (insert (format "** %s\n" title))
            (insert (format "   :PROPERTIES:\n"))
            (insert (format "   :AUTHORS: %s\n" authors))
            (insert (format "   :KEY: %s\n" key))
            (insert (format "   :STATUS: %s\n" (or readstatus "unread")))
            (insert (format "   :END:\n\n"))))))
    (switch-to-buffer buf)))


(defun mooerslab-update-tex-root-references (&optional content-dir)
  "Updates the first line of each .tex file in the specified directory
from '%!TEX root = ../main.tex' to '%!TEX root = ../main2113.tex'.

This first line enables the compiling of the whole document from an
individual file.

Optional argument CONTENT-DIR is the path to the Content directory.
If not provided, defaults to './Content'."
  (interactive)
  (let* ((content-dir (or content-dir "./Content"))
         (files-processed 0)
         (files-modified 0))

    ;; Make sure the directory exists
    (unless (file-directory-p content-dir)
      (error "Directory '%s' not found" content-dir))

    ;; Process all .tex files in the directory
    (dolist (file (directory-files content-dir t "\\.tex$"))
      (setq files-processed (1+ files-processed))

      ;; Read the file content
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))

        ;; Check if the first line needs to be modified
        (let ((first-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
          (if (string= first-line "%!TEX root = ../main.tex")
              (progn
                ;; Modify the first line
                (delete-region (line-beginning-position) (line-end-position))
                (insert "%!TEX root = ../main2113.tex")

                ;; Save the file
                (write-region (point-min) (point-max) file)
                (setq files-modified (1+ files-modified))
                (message "Updated: %s" file))
            (message "Skipped: %s (first line is: %s)" file first-line)))))

    ;; Display summary
    (message "Summary: Processed %d files, modified %d files"
             files-processed files-modified)
    (list files-processed files-modified)))

(defun mooerslab-region-to-org-dash-list (begin end)
  "Convert the active region to an org-mode dash list.
Each line in the region becomes a separate list item."
  (interactive "r") ; Get region boundaries
  (let ((lines nil)
        (dash-list ""))
    ;; Save each line from the region into a list
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (let ((line-text (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
          ;; Skip empty lines
          (unless (string-empty-p (string-trim line-text))
            (push line-text lines)))
        (forward-line 1)))

    ;; Build the dash list with the lines in reverse order (to maintain original order)
    (setq dash-list
          (mapconcat (lambda (line)
                       (concat "- " (string-trim line)))
                     (nreverse lines)
                     "\n"))

    ;; Replace the region with the dash list
    (delete-region begin end)
    (insert dash-list)))




;; Example usage:
;; (update-tex-root-references)
;; Or with a specific directory:
;; (update-tex-root-references "/Users/blaine/2113mlPrimer/Content")



; (defun mooerslab-org-unordered-list-to-latex-itemized-list ()
;   "Convert org-mode unordered list at point to LaTeX itemized list."
; Busted due to beg in org-mode
;   (interactive)
;   (save-excursion
;     (let ((beg (save-excursion
;                  (if (org-at-item-p)
;                      (progn (org-beginning-of-item-list) (point))
;                    (error "Not at an item"))))
;           (end (save-excursion
;                  (if (org-at-item-p)
;                      (progn (org-end-of-item-list) (point))
;                    (error "Not at an item"))))
;           (case-fold-search t))
;       (narrow-to-region beg end)
;       (goto-char (point-min))
;       ;; Insert \begin{itemize}
;       (insert "\\begin{itemize}\n")
;       ;; Convert each list item
;       (while (re-search-forward "^[ \t]*\\([-+*]\\)[ \t]+" nil t)
;         (replace-match "  \\\\item " t nil))
;       ;; Insert \end{itemize}
;       (goto-char (point-max))
;       (insert "\\end{itemize}\n")
;       (widen))))


;; ;;; widen-frame to the right. Enter period have first issue.
;; ;% Redundant with built-in commands.
;; (defun mooerslab-widen-frame ()
;;   "Increase the width of the current frame by 10 columns."
;;   (interactive)
;;   (set-frame-width (selected-frame) (+ (frame-width) 10)))
;; (global-set-key (kbd "C-c w") 'widen-frame)


;; ;; narrow-frame. Enter period have first issue.
;; (defun mooerslab-narrow-frame ()
;;   "Reduce the width of the current frame by 10 columns."
;;   (interactive)
;;   (set-frame-width (selected-frame) (- (frame-width) 10)))
;; (global-set-key (kbd "C-c n") 'narrow-frame)


; (transient-define-prefix pdb-transient-menu ()
;   "PDB Mode Commands"
;   [["Select"
;     ("c" "Select chain" pdb-select-chain)
;     ("C" "Select current chain" (lambda () (interactive) (pdb-select-chain "")))
;     ("r" "Select current residue" pdb-select-residue)
;     ("z" "Select zone of residues" pdb-select-zone)]
;    ["Navigate"
;     ("n" "Jump to next residue" pdb-forward-residue)
;     ("p" "Jump to previous residue" pdb-back-residue)
;     ("N" "Jump to next chain" pdb-forward-chain)
;     ("P" "Jump to previous chain" pdb-back-chain)]
;    ["Change Values"
;     ("a" "Set alternate conformer" pdb-change-alternate)
;     ("b" "Set B-factor" pdb-change-bfactor)]
;    ["Execution"
;     ("x" "Continue" pdb-continue)
;     ("s" "Step" pdb-step)
;     ("r" "Return" pdb-return)
;     ("u" "Until" pdb-until)
;     ("j" "Jump to line" pdb-jump)]
;    ["Breakpoints"
;     ("B" "Set breakpoint" pdb-break)
;     ("D" "Delete breakpoint" pdb-clear)
;     ("A" "Clear all breakpoints" pdb-clear-all)]
;    ["Expressions"
;     ("e" "Evaluate expression" pdb-eval-expression)
;     ("p" "Print expression" pdb-print-expression)]
;    ["Other"
;     ("q" "Quit debugger" pdb-quit)]])
; (with-eval-after-load 'pdb-mode
;   (define-key pdb-mode-map (kbd "C-c t") 'pdb-transient-menu)))

(provide 'mooerslab)
