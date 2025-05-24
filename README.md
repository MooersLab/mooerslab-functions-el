![Version](https://img.shields.io/static/v1?label=mooerslab-functions-el&message=0.6&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


# Library of home-made Emacs Lisp functions

This is a library of homemade functions to customize Emacs to fit my workflow with Emacs 30.1 on Mac OSX.



## Installation with use-package and straight

Add this to your `init.el` file and reload Emacs or evaluate in the scratch buffer.
Straight will `git clone` this repo and store it in the `repos` subfolder of your `.emacs.d` folder.

```elisp
(use-package mooerslab-functions  
  :straight  
  '(:type git  
    :repo "https://github.com/MooersLab/mooerslab-functions-el.git"  
    :files ("mooerslab-functions.el")))
```
The functions will always be available.
There will be no need to use the `mooerslab-functions-load ()` function below.

## Usage

If you have the package **vertico** installed, enter `M-x mooerslab-` to see a list of functions in the minibuffer.
If you have installed the package **marginalia**, you will also see the document string's first line describing what the function does.
Use `C-n` repeatedly to navigate downward to the function that you want to select and execute.


## Alternate approach of loading the package

Add this function to your `init.el` file to load the file of home-made functions manually:

```elisp
;;;## mooerslab-functions-load
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun mooerslab-mooerslab-functions-load ()  
  "Load mooerslab-functions.el file."  
  (interactive)  
  (let ((file-path "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab-functions.el"))  
    (if (file-exists-p (expand-file-name file-path))  
        (load-file file-path)  
      (message "Cannot find mooerslab-functions.el file"))))
```

Enter `M-x ml/mooerslab-functions-load` to load the functions.


## Load the package into Emacs for editing

```elisp
;;;## user-functions-open
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun mooerslab-mooerslab-functions-open ()  
  "Open mooerslab-functions.el in the current buffer."  
  (interactive)  
  (let ((file-path "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab-functions.el"))  
    (if (file-exists-p (expand-file-name file-path))  
        (find-file file-path)  
      (message "Cannot find mooerslab-functions.el file"))))
```

Enter `M-x ml/mooerslab-functions-open` to edit the file.



## Functions in package: mooerslab

| Function | Description |
| --- | --- |
| `mooerslab-append-log-to-org-agenda-files` | Interactively append a log####.org file to org-agenda-files list. Updates both the current org-agenda-files variable in memory and the setq statement in init.el. Customize the path to your init.el file. |
| `mooerslab-beginning-of-list` | Move to the beginning of the current list. Handles org-mode lists, checklists, and LaTeX lists. |
| `mooerslab-carry-forward-todos` | Carry forward undone TODOs and unchecked items to Next Week while preserving categories. |
| `mooerslab-convert-init-el-to-org` | Convert an Emacs init.el file to an Org-mode file. |
| `mooerslab-convert-org-checklist-to-dash-list` | Convert org-mode checklist items to simple dash list items in the selected region. BEGIN and END define the boundaries of the region. Generated with Claude 3.7 Sonnet May 7, 2025. |
| `mooerslab-count-non-blank-lines` | Count the number of non-blank lines in the current buffer. |
| `mooerslab-create-org-table-with-caption` | This interactive function prompts the user for the number of rows. columns, and the caption of the table. |
| `mooerslab-csv2org` | Convert a CSV file to an Org-mode table. Prompts for a CSV file and optionally a caption. Creates a new buffer containing the Org-mode table. Does NOT handle CSV files with quoted fields containing commas. CSV-FILE: The path to the CSV file. CAPTION: (Optional) A string to use as the table caption. |
| `mooerslab-end-of-list` | Move to end of the current list. Handles org-mode lists, checklists, and LaTeX lists. |
| `mooerslab-export-csv-to-matched-sqlite-table` | Export selected rows from a CSV file to an SQLite database. Automatically determines column count and validates against the table structure. |
| `mooerslab-export-csv-to-sqlite-table` | Export selected rows from a CSV file to an SQLite database. |
| `mooerslab-find-file-at-line` | Open FILE on LINE. |
| `mooerslab-format-authors-in-region` | Format author names in region from ’First M.N. Last’ to ’Last, F.M.N.’ Works with various formats: - Regular names: ’Blaine Mooers’ -> ’Mooers, B.’ - With whitespace mulitple middle initials: ’Blaine H M Mooers’ -> ’Mooers, B.H.M.’ - With no whitespace mulitple middle initials: ’Blaine HM Mooers’ -> ’Mooers, B.H.M.’ - With dotted multiple initials: ’Blaine H.M. Mooers’ -> ’Mooers, B.H.M.’ - Multiple authors (comma-separated) Select a region with author names and run this function to reformat them. This is very useful during the preparation of grant progress reports and BibTeX entries. |
| `mooerslab-generate-tar-commands` | Generate tar commands for a list of paths. Each path’s last component becomes the name of the tar file. START and END define the region containing the paths (one per line). If no region is active, operate on the entire buffer. |
| `mooerslab-generate-tar-commands-with-chain` | Generate tar commands for a list of paths with && between commands. Each path’s last component becomes the name of the tar file. START and END define the region containing the paths (one per line). If no region is active, operate on the entire buffer. |
| `mooerslab-get-citekeys-from-bibtex-file` | Prompt for a BibTeX filename in the minibuffer, extract citekeys, and insert an alphabetized itemized list into the current buffer at the cursor position. |
| `mooerslab-github-markdown-table-package-functions` | Create a GitHub markdown table of all functions in a package and their docstrings. Prompts the user for a package name in the minibuffer. |
| `mooerslab-insert-author-index-entry` | Insert an author index entry |
| `mooerslab-insert-main-index-entry` | Insert a general index entry |
| `mooerslab-insert-org-captioned-figure` | Insert a captioned figure in Org-mode. |
| `mooerslab-latex-convert-csv-to-itemized-list` | Convert a comma-separated list in the selected region to a LaTeX itemized list. |
| `mooerslab-latex-region-to-itemized-list` | Converts the region between START and END to an itemized list in LaTeX |
| `mooerslab-latex-to-org-list-region` | Convert a LaTeX itemize list in the region to an Org-mode list. |
| `mooerslab-launch-ithoughtsx` | Launch iThoughtsX application. |
| `mooerslab-launch-timesspent` | Launch timesspent database. |
| `mooerslab-md-to-latex-region` | Convert markdown in region between START and END to LaTeX format. Uses direct pandoc conversion and carefully handles formatting issues. |
| `mooerslab-md-to-org-region` | Convert markdown in region between START and END to org-mode format. Uses direct pandoc conversion and carefully removes blank lines between list items. |
| `mooerslab-open-file-in-textmate` | Open the current file or ‘dired’ marked files in TextMate. This command is for macOS only. Modified from URL ‘http://xahlee.info/emacs/emacs/emacs_open_in_textedit.html’ Version: 2017-11-21 2021-02-07 2023-06-26 |
| `mooerslab-open-init-e30f` | Open the init.el file for editing. Edit the path to suit your needs. |
| `mooerslab-open-mooerslab-functions` | Open the init.el file for editing. Edit the path to suit your needs. |
| `mooerslab-open-my-hydras` | Open the init.el file for editing. Edit the path to suit your needs. |
| `mooerslab-open-new-abibnote-on-citekey` | Open a template file in Org-mode, rename it to the citekey under the cursor, and save it to ’~/abibNotes/’. Citar has a function that will insert the citekey. |
| `mooerslab-open-org-file-and-move-to-tag` | Open an Org file and move the cursor below a headline with a specific TAG. If TAG is not provided, use a hardcoded default tag. You have to adjust the headline level in the function. The regular expression ^\*\* .*:%s: is used to search for second-level headlines (starting with **) with the specified tag. |
| `mooerslab-org-add-periods-to-list-items` | Add periods to the end of all items in the selected org-mode list if missing. It operates only in the selected region between BEGIN and END. Preserves both checked and unchecked checkboxes and the initial dash. Suitable for preparing bullet lists for slides. |
| `mooerslab-org-append-todo-to-tagged-fourth-level-headline` | Append a new TODO item to the bottom of the TODO list under a 3rd level headline marked by TAG. If TAG is not provided, it defaults to the :appendtodos: tag. This is for the writingLog.org file. USAGE: M-x mooerslab-append-todo-to-tagged-headline. Answer the prompts. Works regardless of the position of the point relative to the headline with the tag. |
| `mooerslab-org-checklist-from-kill-ring` | Convert the latest kill-ring entry to org checklist format and put it back in the kill ring. |
| `mooerslab-org-convert-checkboxes-in-region-to-list` | Convert org-mode checkboxes to a regular dash/hyphen bullet list in region from START to END. |
| `mooerslab-org-convert-checklist-in-region-to-fourth-level-todos` | Convert selected region of org-mode checklist to fourth-order TODOs. Converts items starting with ’- [ ]’ to ’**** TODO’. Requires an active region selection. |
| `mooerslab-org-convert-itemized-list-in-region-to-checklist` | Convert an org-mode itemized list (starting with ’-’) to a checklist (starting with ’- [ ]’). |
| `mooerslab-org-convert-itemized-list-in-region-to-fourth-level-todos` | Convert selected region of org-mode itemized list to fourth-order TODOs. Requires an active region selection. |
| `mooerslab-org-convert-lines-to-org-checklist` | Convert lines in region to org-mode checklist items. Preserves existing checkboxes, indentation, and empty lines. If no region is active, operate on the current buffer. |
| `mooerslab-org-convert-list-in-region-to-checkboxes` | Convert a dash/hyphen bullet list to org-mode checkboxes in region from START to END. |
| `mooerslab-org-convert-region-to-fourth-level-todos` | Convert each line in the region to a level four heading. |
| `mooerslab-org-convert-unordered-to-ordered-list` | Convert unnumbered list items to numbered list items in the marked region. |
| `mooerslab-org-insert-external-file` | Insert the contents of an external file into the current org-mode file. Prompts for a file path via minibuffer and includes a timestamp in a comment. |
| `mooerslab-org-insert-protocol-file` | Insert the contents of a protocol file from ~/org-roam/protocols into the current org-mode file. Prompts for a file path via minibuffer and includes a timestamp in a comment. |
| `mooerslab-org-list-package-functions` | Return a dashed org-mode list of all functions in a package. Prompts the user for a package name in the minibuffer. |
| `mooerslab-org-markmap-region` | Export selected org region to a mindmap using markmap. Requires markmap-cli (npm install -g markmap-cli). |
| `mooerslab-org-move-to-tag` | Move the cursor below a headline with a specific TAG. If TAG is not provided, use a hardcoded default tag. You have to adjust the headline level in the function. The regular expression ^\*\* .*:%s: is used to search for second-level headlines (starting with **) with the specified tag. |
| `mooerslab-org-or-latex-add-periods-to-list` | Add a period to the end of each line in the current list if missing. Designed to work in both org and latex files. This is a massive problem with lists in slideshows. The absence of periods will upset some audience members. Works with: - org-mode lists (-, *, numbers) - org-mode checklists (- [ ], * [ ]) https://github.com/cursorless-everywhere/emacs-cursorless/issues- LaTeX \item lists - LaTeX \item checklists (\item [ ]) Usage: Place cursor anywhere in list. Enter M-x org-or-latex-add-periods-to-list or C-c p. Developed with the help of Claude 3.5 Sonnet. |
| `mooerslab-org-region-to-itemized-list` | Convert the lines in a selected region into an itemized list. |
| `mooerslab-play-youtube-video` | Play a YouTube video with mpv. |
| `mooerslab-region-csv-to-org-table` | Convert CSV data in region to org table format. Assumes first row contains headers and uses commas as delimiters. |
| `mooerslab-reload-init-e30f` | Reload the init.el file for e30fewpacakges. Edit the path to suit your needs. |
| `mooerslab-reload-my-hydras` | Reload my-hydras.el. Edit the path to suit your needs. |
| `mooerslab-remove-blank-lines-in-region` | Remove all blank lines in the region between START and END. |
| `mooerslab-spawn-shell` | Invoke shell test |
| `mooerslab-split-sentences-into-lines` | Move each sentence in the region to its own line, ignoring common titles and abbreviations. |
| `mooerslab-string-to-org-checklist` | Convert string TEXT to org-mode checklist format. Preserves existing checkboxes, indentation, and empty lines. |
| `mooerslab-switch-to-minibuffer` | Switch to minibuffer window. |
| `mooerslab-wrap-citar-citekey-and-create-abibnote-org` | Replace the citekey under the cursor with LaTeX-wrapped text and create a corresponding empty citekey.org file in abibNotes folder in the home directory. The LaTeX code uses the bibentry package to inject a bibliographic entry into a section heading that is added in the table of contents. The function still fails to automatically deduce the local bib file. To compensate, you are prompted for the project number. Because it is not automatic, this functin is alpha. |
| `mooerslab-wrap-citekey-and-create-abibnote-tex` | Replace the citekey under the cursor with LaTeX-wrapped text, create a corresponding .tex file, and open it in a new buffer. |


## Related

These functions were mentioned in the talk presented at emacsconf 2024 on metadata management in writing projects.

## Status

Ready to roll.

## Update history

|Version      | Changes                                                                                                                                  | Date                |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                              | 2024 December 12    |
| Version 0.2 |   Renamed the repository and the elisp file.  Updated the elisp functions in the README.md file.                                         | 2024 December 26    |
| Version 0.3 |   Added code for use straight to install this package.                                                                                    | 2025 February 16    |
| Version 0.4 |   Updated functions from emacs29 to emacs30.                                                                                              | 2025 March 3    |
| Version 0.5 |   Added function to add period to end of sentences in various kinds of lists in org and LaTeX files.                                      | 2025 March 4    |
| Version 0.6 |   Added table of functions and their docstrings.                                                                                         | 2025 May 17    |


## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
