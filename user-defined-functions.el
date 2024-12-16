
(defun ml/org-insert-external-file (file-path)
  "Insert the contents of an external file into the current org-mode file.
Prompts for a file path via minibuffer and includes a timestamp in a comment."
  (interactive "fFile to be inserted: ")
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "#+BEGIN_COMMENT\n# File %s inserted on %s\n#+END_COMMENT\n\n" file-path timestamp))
    (insert-file-contents file-path)
    (goto-char (point-max))))
(global-set-key (kbd "C-c S") 'org-insert-external-file)

;;; org-insert-protocol-file
;% Insert the contents of a protocol file into the current org file
(defun ml/org-insert-protocol-file (file-path)
  "Insert the contents of a protocol file from ~/protocols-org into the current org-mode file.
Prompts for a file path via minibuffer and includes a timestamp in a comment."
  (interactive (list (read-file-name "Directory `~/protocols-org/`: " "~/" "protocols-org/")))
  (let ((full-path (expand-file-name file-path))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "#+BEGIN_COMMENT\n# File %s inserted on %s\n#+END_COMMENT\n\n" full-path timestamp))
    (insert-file-contents full-path)
    (goto-char (point-max))))
(global-set-key (kbd "C-c P") 'org-insert-protocol-file)

;;; region-to-itemized-list-in-org
(defun ml/region-to-itemized-list-in-org ()
  "Convert the lines in a selected region into an itemized list."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (lines ())
        (str ""))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (setq lines (cons (buffer-substring (point) (progn (end-of-line) (point))) lines)))
    (dolist (line lines)
      (setq str (concat str (format "- %s\n" line))))
    (delete-region start end)
    (insert str))))
(global-set-key (kbd "C-c l") 'region-to-itemized-list)

;;; region-to-todos-in-org
(defun ml/region-to-todos-in-org ()
  "Convert each line in the region to a level 3 TODO heading."
  (interactive)
  (if (use-region-p)
      (save-excursion
        (let ((end (copy-marker (region-end)))
              (beg (region-beginning)))
          (goto-char beg)
          (while (< (point) end)
            (beginning-of-line)
            (insert "*** TODO ")
            (forward-line 1))
          (set-marker end nil)))
    (message "No region selected")))

;;; region-to-itemized-in-latex
(defun ml/region-to-itemized-in-latex (start end)
  "Converts the region between START and END to an itemized list in LaTeX"
  (interactive "r")  ; Use "r" to read region bounds automatically
  (let* ((text (buffer-substring-no-properties start end))
         (lines (split-string text "\n"))
         (latex-string "\\begin{itemize}\n"))
    (dolist (line lines)
      (if (string-empty-p (string-trim line))
          (setq latex-string (concat latex-string "\\item\n"))
        (setq latex-string (concat latex-string "\\item " (string-trim line) "\n"))))
    (setq latex-string (concat latex-string "\\end{itemize}\n"))
    (delete-region start end)
    (goto-char start)
    (insert latex-string)))
    
;;; region of csv list to latex
(defun convert-csv-to-latex-itemize (start end)  
  "Convert a comma-separated list in the selected region to a LaTeX itemized list."  
  (interactive "r")  
  (let ((csv-text (buffer-substring-no-properties start end)))  
    (delete-region start end)  
    (insert "\\begin{itemize}\n")  
    (dolist (item (split-string csv-text ","))  
      (insert (format "  \\item %s\n" (string-trim item))))  
    (insert "\\end{itemize}\n")))



;;; convert-init-el-to-init-org
;% The goal is to convert the init.el file to a org file so that it can be rendered on GitHub or rendered locally to html or PDF.
(defun ml/convert-init-el-to-org (input-file output-file)
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

;;; Convert a selected latex list of items to a org-mode list
;%  To use this function, select the region containing the LaTeX list and run:
;%  M-x latex-to-org-list-region
(defun ml/latex-to-org-list-region (start end)
  "Convert a LaTeX itemize list in the region to an Org-mode list."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\\\item" end t)
      (replace-match "-"))))

;;; create-org-table-with-caption
;%  This interactive function prompts the user for the number of rows, columns, and caption of the table.
(defun ml/create-org-table-with-caption ()
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

;;;  count-non-blank-lines
(defun ml/count-non-blank-lines ()
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

;;; find file and go to line number
;%  interactively enter the file name and line number in the minibuffer
(defun ml/find-file-at-line (file line)
  "Open FILE on LINE."
  (interactive "fFile: \nNLine: \n")
  (find-file file)
  (goto-line line))
;;; ffap: find file at point
;%  https://unix.stackexchange.com/questions/691444/how-do-i-open-a-file-at-specific-line-in-a-running-emacs
;%  have ffap pick up line number and goto-line
;%  found on emacswiki : https://www.emacswiki.org/emacs/FindFileAtPoint#h5o-6
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
;;; get-citekeys-from-bibtex-file
;% used to work with annotated bibliography. Returns a list under the cursor in the current buffer.
(defun ml/get-citekeys-from-bibtex-file ()  
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
(defun ml/wrap-citekey-and-create-tex-file ()  
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
(defun ml/insert-org-captioned-figure ()
  "Insert a captioned figure in Org-mode."
  (interactive)
  (let ((image-name (read-string "Enter the image file path: "))
        (label (read-string "Enter the figure label: "))
        (caption (read-string "Enter the figure caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert (format "[[file:%s]]\n" image-name))))
;;; launch-ithoughtsx
;% This is the best mindmapping software that I have encountered.
(defun ml/launch-ithoughtsx ()
  "Launch iThoughtsX application."
  (interactive)
  (shell-command "open -a iThoughtsX"))
(global-set-key (kbd "C-c i") 'launch-ithoughtsx)
;;; launch-jabref
;% I favored the simplicity and power of JabRef for mamanging BibTeX entries.
(defun ml/launch-jabref ()
      "Launch jabRef application."
      (interactive)
      (shell-command "open -a JabRef"))
    (global-set-key (kbd "C-c j") 'launch-jabref)
;;; launch-timesspent
;% This is a sqlite database where I track my effort.
(defun ml/launch-timesspent ()
      "Launch timesspent database."
      (interactive)
      (shell-command "open /Users/blaine/6003TimeTracking/cb/mytime.db"))
    (global-set-key (kbd "C-c e") 'launch-timesspent)

;;; Move the cursor to the minibuffer without using the mouse
;%  From video https://www.youtube.com/watch?v=X8c_TrGfYcM&t=15s using Emacs as a multiplexer."
;%  Derived from http://stackoverflow.com/a/4116113/446256.
(defun ml/switch-to-minibuffer ()
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
(defun ml/open-org-file-and-move-to-tag (file &optional tag)
  "Open an Org file and move the cursor below a headline with a specific TAG.
If TAG is not provided, use a hardcoded default tag.
You have have to adjust the headline level in the funciton.
The regular expression ^\\*\\* .*:%s: is used to search for second-level headlines (those starting with **) with the specified tag."
  (interactive "fOrg file: \nsTag (leave empty for default): ")
  (let ((tag (if (string= tag "") "restart-here" tag)))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\* .*:%s:" tag) nil t)
        (org-end-of-subtree)
      (message "Tag not found"))))
;;; Play a YouTube video with mpv
;%  You insert the YouTube url in the minibufffer.
;%  You have to install mpv with a package manager and another binary package.
;%  sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl
;%  sudo chmod a+rx /usr/local/bin/youtube-dl

;;; open-template-with-citekey
;% Open template file renamed with the citekey under the point.
;% This file is for use with an annotated bibliography.
;% Developed with the help of CoPilot.
(defun ml/open-new-abibnote-on-citekey ()
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


;;;; play-youtube-video
(defun ml/play-youtube-video (url)
  "Play a YouTube video with mpv."
  (interactive "sYouTube URL: ")
  (start-process "mpv" nil "mpv" URL))

;;; Reload the initialization file after editing it in Emacs
(defun ml/reload-init-e29f ()
  "Reload the init.el file for e29org. Edit the path to suite your needs."
  (interactive)
  (load-file "~/e29fewpackages/init.el"))
;;; Spawn a new shell with the supplied title
(defun ml/spawn-shell (name)
  "Invoke shell test"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer))
  (process-send-string nil "echo 'test1'\n")
  (process-send-string nil "echo 'test2'\n"))
;;; Split line with many sentences into one line per sentence.
(defun ml/split-sentences-into-lines (start end)
    "Move each sentence in the region to its own line."
    (interactive "r")
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([.!?]\\)\\s-+" end t)
        (replace-match "\\1\n"))))
;% Bind the function to a key combination, e.g., C-c s
(global-set-key (kbd "C-c s") 'split-sentences-into-lines)
;;; widen-frame to the right. Enter period have first issue.
(defun ml/widen-frame ()
  "Increase the width of the current frame by 10 columns."
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width) 10)))
(global-set-key (kbd "C-c w") 'widen-frame)
;; narrow-frame. Enter period have first issue.
(defun ml/narrow-frame ()
  "Reduce the width of the current frame by 10 columns."
  (interactive)
  (set-frame-width (selected-frame) (- (frame-width) 10)))
(global-set-key (kbd "C-c n") 'narrow-frame)

(transient-define-prefix pdb-transient-menu ()  
  "PDB Mode Commands"  
  [["Select"  
    ("c" "Select chain" pdb-select-chain)  
    ("C" "Select current chain" (lambda () (interactive) (pdb-select-chain "")))  
    ("r" "Select current residue" pdb-select-residue)  
    ("z" "Select zone of residues" pdb-select-zone)]  
   ["Navigate"  
    ("n" "Jump to next residue" pdb-forward-residue)  
    ("p" "Jump to previous residue" pdb-back-residue)  
    ("N" "Jump to next chain" pdb-forward-chain)  
    ("P" "Jump to previous chain" pdb-back-chain)]  
   ["Change Values"  
    ("a" "Set alternate conformer" pdb-change-alternate)  
    ("b" "Set B-factor" pdb-change-bfactor)]  
   ["Execution"  
    ("x" "Continue" pdb-continue)  
    ("s" "Step" pdb-step)  
    ("r" "Return" pdb-return)  
    ("u" "Until" pdb-until)  
    ("j" "Jump to line" pdb-jump)]  
   ["Breakpoints"  
    ("B" "Set breakpoint" pdb-break)  
    ("D" "Delete breakpoint" pdb-clear)  
    ("A" "Clear all breakpoints" pdb-clear-all)]  
   ["Expressions"  
    ("e" "Evaluate expression" pdb-eval-expression)  
    ("p" "Print expression" pdb-print-expression)]  
   ["Other"  
    ("q" "Quit debugger" pdb-quit)]])  
(with-eval-after-load 'pdb-mode  
  (define-key pdb-mode-map (kbd "C-c t") 'pdb-transient-menu))

(provide 'user-defined-functions)
