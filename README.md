![Version](https://img.shields.io/static/v1?label=matplotlib-voice-in&message=0.3&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


# Library of home-made Emacs Lisp functions

This is a library of homemade functions to customize Emacs to fit my workflow.



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


## Alternate approach of loading the package

Add this function to your `init.el` file to load the file of home-made functions manually:

```elisp
;;;## mooerslab-functions-load
;; ml is for mooerslab
;; Inspried https://sachachua.com/dotemacs/index.html#org4dd39d0
(defun ml/mooerslab-functions-load ()  
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
(defun ml/mooerslab-functions-open ()  
  "Open mooerslab-functions.el in the current buffer."  
  (interactive)  
  (let ((file-path "~/6112MooersLabGitHubLabRepos/mooerslab-functions-el/mooerslab-functions.el"))  
    (if (file-exists-p (expand-file-name file-path))  
        (find-file file-path)  
      (message "Cannot find mooerslab-functions.el file"))))
```

Enter `M-x ml/mooerslab-functions-open` to edit the file.

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
| Version 0.3 |   Added code for use straight to install this package.                                                                                    | 2025 February 16    |


## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
