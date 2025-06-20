;;; term-control.el --- Allows to easily handle pop-up vterm windows in your emacs -*- lexical-binding:t -*-
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
;; Term-control allows to easily handle pop-up vterm windows in your emacs.
;;
;;; Code:

(require 'cl-lib)
(require 'vterm)

;; Configs

(defgroup term-control nil
  "Toggle a terminal buffer."
  :prefix "term-control-"
  :group 'applications)

(defcustom term-control-vsize 33
  "Percentage of the window that the toggle-term buffer occupies.  Vertically."
  :type 'integer
  :group 'term-control)

(defcustom term-control-hsize 50
  "Percentage of the window that the toggle-term buffer occupies.  Horizontally."
  :type 'integer
  :group 'term-control)

;; Structs and variables

(cl-defstruct term-control-terms
  name)

(cl-defstruct term-control-tabs
  name term-name)

(defvar term-control-active-terms nil
  "We hold a list of term-control-terms to know what to show.")

(defvar term-control-active-tabs nil
  "We hold a list of each tab which terminal it has")

;; Helper functions
(defun term-control-get-tab ()
  "Returns the tab identifier, we have it here for the future in case getting tab name changes"
  (cdr (assoc 'name (tab-bar--current-tab))))

(defun term-control-get-last-used ()
  "Gets the last used term name, or nil if none found."
  (let ((term (cl-find (term-control-get-tab) term-control-active-tabs :key #'term-control-tabs-name :test #'string=)))
    (when term
      (term-control-tabs-term-name term))))

(defun term-control-get-all-terms ()
  "It gets all the term names open."
  (mapcar #'term-control-terms-name term-control-active-terms))

(defun term-control-set-last-used (target-name)
  "Sets the last-used term for the current tab to TARGET-NAME."
  (let* ((tab-name (term-control-get-tab))
         (tab (cl-find tab-name term-control-active-tabs
                       :key #'term-control-tabs-name
                       :test #'string=)))
    (if tab
        (setf (term-control-tabs-term-name tab) target-name)
      (push (make-term-control-tabs :name tab-name
                                    :term-name target-name)
            term-control-active-tabs))))

(defun term-control-display-buffer (name &optional sidescreen)
  "Displays the buffer and moves focus to it.
Argument NAME Name of the buffer we want to display.
Optional argument SIDESCREEN Where in the screen we want to show the terminal."
  (let* ((control-size (if (eq 'bottom (or sidescreen 'bottom))
                           term-control-vsize
                         term-control-hsize))
         (size (/ control-size 100.0)))
    (let ((window
           (display-buffer-in-side-window
            (get-buffer name)
            `((side . ,(or sidescreen 'bottom))
              (window-height . ,size)
              (window-width . ,size)
              (preserve-size . (t . nil))))))
      (set-window-dedicated-p window nil)
      (select-window window)))) ; switch focus here

;; User Interface

(defun term-control-switch-to-term-ver ()
  "Switch to or creates a terminal from `term-control-active-terms`."
  (interactive)
  (term-control-switch-to-term 'left))

(defun term-control-switch-to-term (&optional side)
  "Switch to or create a terminal from `term-control-active-terms`.
Optional argument SIDE Where in the screen we want to show the terminal."
  (interactive)
  (let* ((choices (term-control-get-all-terms))
         (selected (completing-read "Choose terminal: " choices nil nil)))
    ;; If buffer doesn't exist, create and register it
    (unless (get-buffer selected)
      (let ((buf (get-buffer-create selected)))
        (with-current-buffer buf
          (vterm-mode)
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      ;; Remove from term-control-active-terms
                      (setq term-control-active-terms
                            (cl-remove (buffer-name) term-control-active-terms
                                       :key #'term-control-terms-name
                                       :test #'string=))
                      ;; Remove from term-control-active-tabs
                      (setq term-control-active-tabs
                            (cl-remove-if (lambda (tab)
                                            (string= (term-control-tabs-term-name tab)
                                                     (buffer-name)))
                                          term-control-active-tabs)))
                    nil t)) ; Local hook
        (unless (cl-find selected term-control-active-terms
                         :key #'term-control-terms-name :test #'string=)
          (push (make-term-control-terms :name selected)
                term-control-active-terms))))
    ;; Update last-used status
    (term-control-set-last-used selected)
    ;; Display the buffer in side window and switch to it
    (term-control-display-buffer selected (or side 'bottom))))

(defun term-control-toggle-ver ()
  "Toggle the last used terminal buffer occupying all the vertical space."
  (interactive)
  (term-control-toggle 'left))

(defun term-control-toggle (&optional side)
  "Toggle the last used terminal buffer: show it if hidden, hide it if visible.
Optional argument SIDE Where in the screen we want to show the terminal."
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

;;; term-control.el ends here.
