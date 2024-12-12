![Version](https://img.shields.io/static/v1?label=matplotlib-voice-in&message=0.1&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


# Library of home-made Emacs Lisp functions

This is a library of homemade functions that for the customized Emacs to fit my workflow.

## Usage

Add this function to your `init.el` file to load the file of home-made functions:

```elisp
;;;## user-functions-load
;; ml is for mooerslab
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun ml/user-functions-load ()
  "Open user-defined-function."
  (interactive)
  (load-file "~/~/6112MooersLabGitHubLabRepos/user-defined-funtions-el/user-defined-functions.el"))

;;;## user-functions-open
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun ml/user-functions-open ()
  "Open user-defined-function."
  (interactive)
  (find-file "~/6112MooersLabGitHubLabRepos/user-defined-funtions-el/user-defined-functions.el"))
```

Enter `M-x ml/user-functions-open` to edit the file.
Enter `M-x ml/user-functions-load` to load the functions.
Enter `M-x ml/`

## 



## Status

Ready to roll.

## Update history

|Version      | Changes                                                                                                                                  | Date                 |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                              | 2024 December 12  |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
