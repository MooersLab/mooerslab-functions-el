![Version](https://img.shields.io/static/v1?label=matplotlib-voice-in&message=0.1&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


# Library of home-made Emacs Lisp functions

This is a library of homemade functions to customize Emacs to fit my workflow.

## Usage

Add this function to your `init.el` file to load the file of home-made functions:

```elisp
;;;## mooerslab-functions-load
;; ml is for mooerslab
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun ml/mooerslab-load ()
  "Open mooerslab-defined-function."
  (interactive)
  (load-file "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab-functions.el"))

;;;## user-functions-open
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun ml/mooerslab-functions-open ()
  "Open mooerslab-def-function."
  (interactive)
  (find-file "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab-functions.el"))
```

Enter `M-x ml/user-functions-open` to edit the file.
Enter `M-x ml/user-functions-load` to load the functions.
Enter `M-x ml/` to see a list of functions displayed in the minibuffer.
If you have installed the package marginalia, you will also see the document string's first line describing what the function does.

## Related

These functions were mentioned in the talk presented at emacsconf 2024 on metadata management in writing projects.

## Status

Ready to roll.

## Update history

|Version      | Changes                                                                                                                                  | Date                |
|:------------|:-----------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                              | 2024 December 12    |
| Version 0.2 |   Renamed the repository and the elisp file.  Updated the elisp functions in the README.md file.                                         | 2024 December 26    |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
