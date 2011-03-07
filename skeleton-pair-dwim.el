;;; skeleton-pair-dwim.el --- Auto pairs insert do what I mean.

;; Copyright (C) 2011  Yuuki Arisawa

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; Keywords: convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;(setq skeleton-pair-alist '((?| _ ?|) ) )

;;; Code:

;; requires
(require 'skeleton)
(setq skeleton-pair t)

;; not to deactivate mark
(defadvice skeleton-insert
  (around skeleton-insert-dwim-advice)
  (if (and transient-mark-mode (not mark-active))
      ad-do-it
    (progn
      (save-excursion ad-do-it)
      (setq deactivate-mark nil)
      (if (< (mark) (point))
	  (forward-char 1);;for undo
	(set-mark (1+(mark)))
      )
      )))

(defun skeleton-pair-dwim-closep (char)
  (let ((skeleton
	 (or (rassoc `(_ ,char) skeleton-pair-alist)
	     (rassoc `(_ ,char) skeleton-pair-default-alist))))
    (if (and skeleton (eq (car skeleton) (nth 2 skeleton)))
	nil
      skeleton)))

(defun skeleton-pair-dwim-openp (char)
  (and
   (not (skeleton-pair-dwim-closep char))
   (let ((skeleton
	  (or (assq char skeleton-pair-alist)
	      (assq char skeleton-pair-default-alist))))
     (if (and skeleton (eq (car skeleton) (nth 2 skeleton)))
	 nil
       skeleton))))

(defun skeleton-pair-dwim-default-alist-to-keys()
  (mapcar 'char-to-string
	  (append (mapcar 'car
			  skeleton-pair-alist)
		  (mapcar 'car
			  skeleton-pair-default-alist))))

;;interactive function
;;ok "" "aaa" over "aa aaa"
(defun skeleton-pair-insert-dwim (arg)
  (interactive "*p")
  (if (not skeleton-pair)
      (self-insert-command (prefix-numeric-value arg))
    (let* ((char last-command-event)
	   (closep (skeleton-pair-dwim-closep char))
	   (mark (and skeleton-autowrap
		      (or (eq last-command 'mouse-drag-region)
			  (and transient-mark-mode mark-active))))
	   (skeleton-end-hook))
      (cond ((skeleton-pair-dwim-openp char)
	     (skeleton-pair-insert-maybe nil);;40(
	     ;;(message "m1")
	     )
	    ((and (not mark)
		  (eq (char-after) char)
		  (eq this-command real-last-command));;41) & 34"
	     (forward-char 1)
	     ;;(message "m4")
	     )
	    ((not closep)
	     (skeleton-pair-insert-maybe nil);;34"
	     ;;(message "m2")
	     )
	    ((and mark closep)
	     (skeleton-insert (cons nil closep) (if mark -1));;41)
	     ;;(message "m3")
	     )
	    (t ;;closep
	     (self-insert-command (prefix-numeric-value arg));;41)
	     ;;(message "m5")
	     )
	    ))))

(defun skeleton-pair-dwim-inside-stringp ()
  (nth 3 (parse-partial-sexp (point-min) (point))))

(defun skeleton-pair-dwim-after-wordp ()
  (if (not (skeleton-pair-dwim-openp last-command-event))
      (= (char-syntax (preceding-char)) ?w)))

;;looking-back for )"
(defun skeleton-pair-dwim-after-same-as-charp ()
  (if (not (skeleton-pair-dwim-openp last-command-event))
      (= last-command-event (preceding-char))))

;;local set key
(defun skeleton-pair-dwim-local-set-key(keys)
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (skeleton-pair-dwim-define-key map keys)))
;;(global-set-key)
(defun skeleton-pair-dwim-parse-key (keys)
  (if (not (listp keys))
      (setq keys (list keys)))
  ;;(setq keys
	(append keys
		(delq nil
		      (mapcar
		       (lambda (key)
			 (let ((openp (skeleton-pair-dwim-openp
				       (string-to-char key))))
			   (if openp
			       (char-to-string(nth 2 openp))))
			 )keys)))
  )

(defun skeleton-pair-dwim-define-key(maps keys)
  (if (not (listp keys))
      (setq keys (list keys)))
  (if (keymapp maps)
      (setq maps (list maps)))
  (setq keys (skeleton-pair-dwim-parse-key keys))
  (mapc
   (lambda (map)
     (mapc
      (lambda (key)
	(or (vectorp key) (stringp key)
		(signal 'wrong-type-argument (list 'arrayp key)))
	(define-key (if(keymapp map) map (eval map))
	  key 'skeleton-pair-insert-dwim);;kbd?
	)keys)
     )maps))

;;user-setting
;;(skeleton-pair-dwim-define-key
 ;;'(global-map emacs-lisp-mode-map) '("{" "(" "\"" "'"))
;;global-map '("{" "(" "\"" "'"))
 ;;global-map '("{" "}" "(" ")" ))
 ;;'(global-map emacs-lisp-mode-map) ")")
 ;;emacs-lisp-mode-map "\"")
;; emacs-lisp-mode-map '("{" "(" "\"" "'"))

;;(define-key global-map "(" 'skeleton-pair-insert-dwim)

;; region

(defun skeleton-pair-dwim-load-default ()
  ;;defalut setting
  (ad-activate-regexp 'skeleton-insert-dwim-advice)

  ;;default setting
  (setq skeleton-pair-filter-function
	'(lambda()
	   (or (skeleton-pair-dwim-inside-stringp)
	       (skeleton-pair-dwim-after-wordp)
	       (skeleton-pair-dwim-after-same-as-charp)
	       )))

  ;; default
  (skeleton-pair-dwim-define-key
   global-map '("{" "(" "\"" "'"))

  ;; (skeleton-pair-dwim-local-set-key
  ;;  (skeleton-pair-dwim-default-alist-to-keys))
  )

(provide 'skeleton-pair-dwim)
;;; skeleton-pair-dwim.el ends here
