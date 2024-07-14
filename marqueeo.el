;;; marqueeo.el --- Mario in the header-line.               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: inconvenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)

(defgroup marqueeo nil "Mario in the header-line." :group 'applications :prefix "marqueeo-")
(defcustom marqueeo-tick-interval (/ 1 30.0) "Rate at which to update header-line." :type 'float)
(defvar-local marqueeo--previous-header-line nil)
(defvar-local marqueeo-frames
    (let ((default-directory (file-name-directory (or load-file-name (buffer-file-name)))))
      (mapcar (lambda (f) (create-image f 'png nil :scale 0.25 :ascent 100 :heuristic-mask t))
              (directory-files "./frames" t ".*.png"))))
(defvar-local marqueeo-timer nil)
(defvar-local marqueeo--frame-counter -1)
(defun marqueeo-animation-function ()
  "Animate."
  (when (> (cl-incf marqueeo--frame-counter) (window-width))
    (setq marqueeo--frame-counter 0))
  (concat
   (when (> marqueeo--frame-counter 1)
     (propertize " " 'display `(space :align-to ,marqueeo--frame-counter)))
   (propertize " " 'display (nth (mod marqueeo--frame-counter (length marqueeo-frames))
                                 marqueeo-frames))))

(defun marqueeo-update-header-line (buffer)
  "Update the header-line in BUFFER."
  (with-current-buffer buffer
    (setq header-line-format (marqueeo-animation-function))))

(define-minor-mode marqueeo-mode
  "Minor mode to display an animation in the current buffer's `header-line'."
  :lighter " marqueeo"
  (if marqueeo-mode
      (setq marqueeo--previous-header-line header-line-format
            marqueeo-timer
            (run-at-time nil marqueeo-tick-interval
                         (apply-partially #'marqueeo-update-header-line (current-buffer))))
    (setq header-line-format marqueeo--previous-header-line)
    (cancel-timer marqueeo-timer)))

(defalias 'its-a-him-marqueeo #'marqueeo-mode)
(provide 'marqueeo)
;;; marqueeo.el ends here
