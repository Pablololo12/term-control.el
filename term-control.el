;;; toggle-term.el --- Term-control allows to easily handle terminals for your projects  -*- lexical-binding:t -*-
;;
;; Author: pablololo12
;; URL: https://github.com/Pablololo12/term-control.el
;; Version: 0.1
;; Keywords: frames convenience terminals
;; Package-Requires: ((emacs "25.1"))
;;
;;; License
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; Term-control allows to handle and open terminals for all your different projects
;;
;;; Code:

(require 'cl-lib)

;; Configs

(defgroup term-control nil
  "Toggle a terminal buffer."
  :prefix "term-control-"
  :group 'applications)

(defcustom term-control-vsize 33
  "Percentage of the window that the toggle-term buffer occupies. Vertically"
  :type 'integer
  :group 'term-control)

(defcustom term-control-hsize 33
  "Percentage of the window that the toggle-term buffer occupies. Horizontally"
  :type 'integer
  :group 'term-control)

;; Structs and variables
(cl-defstruct term-control-terms
  name last-used)

(defvar term-control-active-terms nil
  "We hold a list of term-control-terms to know what to show")

;; Helper functions

(defun term-control-get-last-used ()
  "Gets the last used term name, or nil if none found."
  (let ((term (cl-find-if #'term-control-terms-last-used term-control-active-terms)))
    (when term
      (term-control-terms-name term))))

(defun term-control-get-all-terms ()
  (mapcar #'term-control-terms-name term-control-active-terms))

(defun term-control-set-last-used (target-name value)
  "Sets the last-used slot to whatever we choose"
  (dolist (term term-control-active-terms)
    (when (string= (term-control-terms-name term) target-name)
      (setf (term-control-terms-last-used term) value))))

(defun term-control-display-buffer (name &optional sidescreen)
  "Displays the buffer and moves focus to it."
  (let ((size (/ term-control-hsize 100.0)))
    (let ((window
           (display-buffer-in-side-window
            (get-buffer name)
            `((side . ,(or sidescreen 'bottom))
              (window-height . ,size)
              (preserve-size . (t . nil))))))
      (set-window-dedicated-p window nil)
      (select-window window)))) ; 🔥 switch focus here

;; User Interface

(defun term-control-switch-to-term-ver ()
    "Switch to or creates a terminal from `term-control-active-terms`"
    (interactive)
    (term-control-switch-to-term 'left))

(defun term-control-switch-to-term (&optional side)
  "Switch to or create a terminal from `term-control-active-terms`."
  (interactive)
  (let* ((choices (term-control-get-all-terms))
         (selected (completing-read "Choose terminal: " choices nil nil)))
    ;; If buffer doesn't exist, create and register it
    (unless (get-buffer selected)
      (let ((buf (get-buffer-create selected)))
        (with-current-buffer buf
          (vterm-mode))
        (push (make-term-control-terms :name (buffer-name buf) :last-used nil)
              term-control-active-terms)))
    ;; Update last-used status
    (term-control-set-last-used selected t)
    (dolist (term term-control-active-terms)
      (unless (string= (term-control-terms-name term) selected)
        (setf (term-control-terms-last-used term) nil)))
    ;; Display the buffer in side window and switch to it
    (term-control-display-buffer selected (or side 'bottom))))

(defun term-control-toggle-ver ()
  "Toggle the last used terminal buffer occupying all the vertical space"
  (interactive)
  (term-control-toggle 'left))

(defun term-control-toggle (&optional side)
  "Toggle the last used terminal buffer: show it if hidden, hide it if visible."
  (interactive)
  (let* ((term-buffer (term-control-get-last-used))           ; last used terminal buffer (from the struct)
         (term-window (when term-buffer (get-buffer-window term-buffer))))
    (cond
     ((null term-buffer)
      (message "No terminal buffer to toggle."))
     (term-window
      ;; Terminal is currently visible: hide it by closing its window
      (bury-buffer term-buffer)               ; hide the buffer but keep it alive
      (delete-window term-window))            ; delete only the terminal's window (not the current one)
     (t
      (term-control-display-buffer term-buffer (or side 'bottom))))))

(provide 'term-control)

;;; toggle-term.el ends here.
