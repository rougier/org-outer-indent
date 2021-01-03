;;; org-outer-indent.el --- Outer indent mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicolas P. Rougier

;; Author: Nicolas Rougier <nicolas.rougier@inria.fr>
;; Keywords: outlines, hypermedia

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an outer indent mode for org headlines that
;; is compatible with org-num-mode. Use
;;
;;     <M-x org-outer-indent-mode>
;;
;; to activate it:


;; org-indent-mode       |* Headline level 1
;;                       |  * Headline level 2

;; org-outer-indent-mode |  * Headline level 1
;;  + org-num-mode OFF   | ** Headline level 2

;; org-outer-indent-mode |   1 Headline level 1
;;  + org-num-mode ON    | 1.1 Headline level 2
;;
;; Internally, the library advises the `org-indent--compute-prefixes`
;; function to compute line prefix depending on the desired mode and
;; adds a hook on org-num-mode to hide/show stars.
;;
;;
;; Known bugs:
;;
;;  - org-beginning-of-line get confused when headline stars are hidden
;;
;; Known limitations:
;;
;; - org-num-mode can selectively un-number a given headline. In such case
;;   indentaton will be wrong and stars will be hidden, which might be
;;   confusing. It might be possible to solve the problem by examining
;;   org-num-mode overlays but this might be too much work.
;; 
;; - When used in conjunction woth org-num-mode, outer indentation works if
;;   the number of sections at a given level is < 10 and uses a regular
;;   numbering format (such as the default one).


;;; Code

(require 'org)
(require 'org-num)
(require 'org-indent)


;;; Internal variables

(defvar-local org-outer-indent--overlays nil
  "List of overlays used for showing/hiding stars.")


;;; Internal functions

(defun org-outer-indent--prefix-length (level)
  "Compute headline prefix length at a given LEVEL."
  (if org-num-mode
      (if (or (not org-num-max-level) (<= level org-num-max-level))
          (length (funcall org-num-format-function (make-list level 1)))
        (+ 1 level))
    (+ 1 level)))

(defun org-outer-indent--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."

  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))

  ;; Find the lowest headline level
  (let* ((max-level (seq-max (org-element-map
                              (org-element-parse-buffer) 'headline
                              #'(lambda (item)
                                  (org-element-property :level item)))))
         ;; We could also iterate over each evel to get maximum length
         ;; Instead, we take the length of the deepest numbered level.
         (line-indentation (org-outer-indent--prefix-length
                            (if (and org-num-mode org-num-max-level)
                                (min org-num-max-level max-level)
                              max-level)))
         (headline-indentation))
    (dotimes (level org-indent--deepest-level)
      (setq headline-indentation
            (max 0 (- line-indentation
                      (org-outer-indent--prefix-length level))))
      (aset org-indent--inlinetask-line-prefixes level
            (make-string line-indentation ?\s))
      (aset org-indent--text-line-prefixes level
            (make-string line-indentation ?\s))
      (aset org-indent--heading-line-prefixes level
                (make-string headline-indentation ?\s)))))


(defun org-outer-indent--clear ()
  "Remove all stars hiding overlays in current buffer."
  (mapc #'delete-overlay org-outer-indent--overlays)
  (setq org-outer-indent--overlays nil))
  

(defun org-outer-indent--hideshow-stars ()
  "Remove all stars hiding overlays in current buffer."

  (org-outer-indent--clear)
  (when org-num-mode
     (let ((num-stars (if org-num-max-level
                          (format "\\(^*\\{1,%d\\} \\)" org-num-max-level)
                        "\\(^*+ \\)")))
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward num-stars nil t)
           (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
             (overlay-put overlay 'display "")
             (push overlay org-outer-indent--overlays))))))

  ;; Force recomputation of indentations
  (if org-indent-mode
       (org-indent-mode t)))


;;; Public functions

(define-minor-mode org-outer-indent-mode
  "Outer indent mode for org buffer."
  :init-value nil
  
  (cond
   ;; Minor mode ON
   (org-outer-indent-mode
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot activate headline numbering outside Org mode"))
    (advice-add 'org-indent--compute-prefixes :override
                #'org-outer-indent--compute-prefixes)
    (add-hook 'org-num-mode-hook
              #'org-outer-indent--hideshow-stars)
    (org-indent-mode t))
   
   ;; Minor mode OFF
   (t
    (org-outer-indent--clear)
    (advice-remove 'org-indent--compute-prefixes
                   #'org-outer-indent--compute-prefixes)
    (remove-hook 'org-num-mode-hook
                 #'org-outer-indent--hideshow-stars)
    (org-indent-mode t))))
  
(provide 'org-outer-indent)

;;; org-outer-indent.el ends here
