;;; auxtex-custom.el --- Additional functions to auctex

;; Copyright (C) 2014-2017 Gaby Launay

;; Author: Gaby Launay <gaby.launay@tutanota.com>
;; URL: https://github.com/galaunay/auctex-custom
;; Keywords: LaTeX
;; Package-Requires: ((auctex "11.90.2.2017-07-25"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some additional functions for auctex

;;; Code:

(require 'projectile)
(require 'tex)

(defvar TeX-compilation-on-idle-timer nil
  "Timer for compilation on idle.")

(defun TeX-save-buffer-or-project ()
  "Save the current file or project."
  (interactive)
  (if (projectile-project-p)
      (projectile-save-project-buffers)
    (save-buffer)))

(defun TeX-error-overview-quit-help ()
  "Quit error overview and associated help buffers."
  (interactive)
  (let ((help-buffer-name "*TeX Help*")
        (error-buffer-name "*TeX errors*"))
    ;; get error buffer and close it
    (when (get-buffer-window help-buffer-name)
      (condition-case nil
          (delete-window (get-buffer-window help-buffer-name))
        (error nil)))
    (when (get-buffer help-buffer-name)
      (kill-buffer help-buffer-name))
    ;; close Tex error overview
    (when (get-buffer-window error-buffer-name)
      (condition-case nil
          (delete-window (get-buffer-window error-buffer-name))
        (error nil)))
    (when (get-buffer error-buffer-name)
      (kill-buffer error-buffer-name))))

(defun TeX-save-and-command (NAME FILE &optional OVERRIDE-CONFIRM)
  "Same as ‘TeX-command’, but save the file beforehand.

NAME is a command name.

FILE is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.

Use the information in ‘TeX-command-list’ to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.


Save project buffers,and run the wanted command.
Launch NAME command on FILE
quit error overview windows if present."
  (interactive)
  (TeX-error-overview-quit-help)
  (TeX-save-buffer-or-project)
  (TeX-command NAME FILE OVERRIDE-CONFIRM))

(defun TeX-save-and-command-on-buffer-or-region (NAME FILE &optional OVERRIDE-CONFIRM)
  "Same as ‘TeX-command’, but use the current region.

NAME is a command name.

FILE is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.

Use the information in ‘TeX-command-list’ to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.


Save project buffers,and run the wanted command.
Launch NAME command on FILE
quit error overview windows if present."
  (interactive)
  (when (not (use-region-p)) (TeX-select-beamer-frame))
  (let ((TeX-command-region-begin (if (use-region-p) (region-beginning) (point-min)))
        (TeX-command-region-end (if (use-region-p) (region-end) (point-max))))
    (TeX-error-overview-quit-help)
    (TeX-save-buffer-or-project)
    (deactivate-mark)
    (TeX-region-update)
    (TeX-command NAME FILE OVERRIDE-CONFIRM)))

(defun TeX-command-and-show (NAME FILE &optional OVERRIDE-CONFIRM)
  "Same as ‘TeX-command’, but show the compiled documents afterwards.

NAME is a command name.

FILE is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.

Use the information in ‘TeX-command-list’ to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.


Save project buffers,and run the wanted command.
Launch NAME command on FILE
quit error overview windows if present."
  (interactive)
  (TeX-error-overview-quit-help)
  (TeX-save-buffer-or-project)
  (TeX-command-sequence (list NAME "View") FILE OVERRIDE-CONFIRM))

(defun TeX-command-and-show-on-buffer-or-region (NAME FILE &optional OVERRIDE-CONFIRM)
  "Same as ‘TeX-command-and-show’, but use the current region.

NAME is a command name.

FILE is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.

Use the information in ‘TeX-command-list’ to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.


Save project buffers,and run the wanted command.
Launch NAME command on FILE
quit error overview windows if present."
  (interactive)
  (when (not (use-region-p)) (TeX-select-beamer-frame))
  (let ((TeX-command-region-begin (if (use-region-p) (region-beginning) (point-min)))
        (TeX-command-region-end (if (use-region-p) (region-end) (point-max))))
    (TeX-error-overview-quit-help)
    (TeX-save-buffer-or-project)
    (deactivate-mark)
    (TeX-region-update)
    (TeX-command-sequence (list NAME "View") FILE OVERRIDE-CONFIRM)))

(defun TeX-select-beamer-frame ()
  "Select the current beamer frame."
  (interactive)
  (while (not (looking-at-p "\\\\begin *{frame}"))
    (LaTeX-find-matching-begin))
  (forward-char)
  (LaTeX-mark-environment))

(defun TeX-find-figure-by-number (fig-number)
  "Goto the latex figure associated with the number FIG-NUMBER."
  (interactive "nFigure number: ")
  (let ((fig-pos nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^[ ]*\\\\includegraphics" (point-max) t fig-number)
          (setq fig-pos (point))
        (message "No figure associated to this number (%s)" fig-number)))
    (when fig-pos
      (evil--jumps-push)
      (xref-push-marker-stack (point-marker))
      (goto-char fig-pos)
      (re-search-forward "\\\\caption" nil t 1))))

(defun TeX-update-pdf-quietly ()
  "Recompile the current associated pdf quielty.

Use LatexMk by default."
  (let ((output-name (expand-file-name
                      (TeX-active-master (TeX-output-extension))))
        (TeX-debug-bad-boxes nil)
        (TeX-debug-warnings nil))
    (when (and (find-buffer-visiting output-name)
               (derived-mode-p 'latex-mode))
      (TeX-command-and-show "LatexMk" 'TeX-active-master))))

(defun TeX-toggle-compilation-on-save ()
  "Toggle automatic re-compilation on save."
  (interactive)
  (if (member 'TeX-update-pdf-quietly after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'TeX-update-pdf-quietly)
        (message "Deactivated autocompilation on save."))
    (add-hook 'after-save-hook 'TeX-update-pdf-quietly)
    (message "Activated autocompilation on save.")
    (let ((TeX-debug-bad-boxes nil)
          (TeX-debug-warnings nil))
           (TeX-command "LatexMk" 'TeX-master-file))))

;; useless...
(defun TeX-toggle-compilation-on-idle ()
  "Toggle automatic recompilation on idle."
  (interactive)
  (if TeX-compilation-on-idle-timer
      (cancel-timer TeX-compilation-on-idle-timer)
    (setq TeX-compilation-on-idle-timer
          (run-with-idle-timer 2 t 'TeX-update-pdf-quietly))))


(provide 'auctex-custom)
;;; auctex-custom.el ends here
