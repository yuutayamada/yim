;;; yim.el --- skk(Simple Kana to Kanji) support program -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2012, 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/yim
;; Version: 0.0.1
;; Keywords: keyword

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is support program for skk.el.
;; you can use SKK without shiftkey

;;; Requirements
;; This program require skk.el.
;; You can download from below link
;; http://openlab.ring.gr.jp/skk/ddskk-ja.html

;;; Usage
;; (require 'yim)
;; (autoload 'yim-convert "yim" "yim" t nil)
;; (autoload 'yim-abbrev "yim" "yim" t nil)
;; (define-key global-map [(henkan)] 'yim-convert)
;; (define-key global-map [(hiragana-katakana)] 'yim-abbrev)

;;; Code:

(require 'cl-lib)
(require 'skk-vars)
(with-no-warnings (require 'skk))

(defvar yim-default-skip-character '())
(defvar yim-ignore-kanji-list '())
(defvar yim-before-convert-hook nil)
(defvar yim-before-turn-on-latan-mode-hook nil)
(defvar yim-skip-WO t)

(defvar yim-regexps '((:alpha . "[a-z;:']")
                      (:ALPHA . "[a-zA-Z;:']")
                      (:hiragana . "[ぁ-ん]")
                      (:katakana . "[ァ-ンー]")))

(defun yim-get-regexp (symbol)
  (assoc-default symbol yim-regexps))

(defadvice skk-latin-mode
  (around ad-turn-on-auto-capitalize activate)
  (run-hook-with-args 'yim-before-turn-on-latan-mode-hook)
  ad-do-it)

;;;###autoload
(defun yim-convert ()
  (interactive)
  (run-hook-with-args 'yim-before-convert-hook)
  ;;henkan-mode active=▼
  (cl-case skk-henkan-mode
    (active
     (if (equal (point) (mark))
         (progn (skk-start-henkan 4)
                (set-mark-command nil))
       (when (> (point) (mark))
         (skk-kakutei)
         (yim-convert-core))))
    (t (yim-convert-core)
       (set-mark-command nil))))

(defun yim-collect-character-p ()
  (goto-char (point-min))
  (re-search-forward "[a-zA-Z]" nil t))

(defun yim-get-char (type input-alphabet)
  "Return converted Hiragana or Katakana from alphabet"
  (interactive)
  (let*
      ((rule-list (append skk-rom-kana-rule-list skk-rom-kana-base-rule-list))
       (record    (assoc-default input-alphabet rule-list))
       (katakana-hiragana-pair (nth 1 record)))
    (cl-typecase katakana-hiragana-pair
      (list (cl-case type
              (:hiragana (cdr katakana-hiragana-pair))
              (:katakana (car katakana-hiragana-pair))))
      ;; single character
      (string katakana-hiragana-pair))))

(defun yim-convert-alphabet-in-region (start end &optional type)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (while (yim-collect-character-p)
        (yim-convert-from-alphabet-to (or type :hiragana))))))

(defun yim-convert-from-alphabet-to (hiragana-or-katakana)
  (cl-loop
   with match = ""
   with ALPHA = (yim-get-regexp :ALPHA)
   with alpha = (yim-get-regexp :alpha)
   with alpha1 = (concat ALPHA alpha alpha)
   with alpha2 = (concat ALPHA alpha)
   with order = `(,alpha1 ,alpha2 ,ALPHA)
   for regexp in order do
   (goto-char (point-min))
   (when (re-search-forward regexp nil t)
     (setq match (downcase (match-string 0)))
     (if (and (yim-skip-character-p match)
              (yim-get-char hiragana-or-katakana match))
         (replace-match (yim-get-char hiragana-or-katakana match))))))

(defun yim-skip-character-p (input-alphabet)
  (and skk-use-azik
       (not (equal "n" input-alphabet))))

(defun yim-convert-core (&optional hiragana-or-katakana)
  (let ((convert
         (lambda (&optional type)
           (skk-kakutei)
           (save-excursion
             (set-mark-command nil)
             (yim-backward)
             (yim-convert-alphabet-in-region (point) (mark) type)
             (unless (equal :katakana hiragana-or-katakana)
               (yim-kanji-henkan-point))))))
    (funcall convert hiragana-or-katakana)
    (skk-latin-mode-on)
    ;;ミニバッファで無入力で変換すると▽より前にありますのエラーでるので
    (if (minibufferp)
        (re-search-forward "▽" nil t))
    (if (looking-at "▽")
        (forward-char)
      ;;変換時に１回SPC押した状態に
      (unless (equal :katakana hiragana-or-katakana)
        (skk-start-henkan 4)))))

(defun yim-convert-to-katakana ()
  (interactive)
  (when (equal 'active skk-henkan-mode)
    (while (and (eq skk-henkan-mode 'active)
                 (not (eq skk-henkan-mode 'on)))
      (skk-undo)))
  (if (string-match "[a-zA-Z]" (char-to-string (char-before)))
      (yim-convert-core :katakana)
    (when (string-match "[ぁ-ん]" (char-to-string (char-before)))
      (save-excursion
        (set-mark-command nil)
        (yim-backward)
        (skk-katakana-region (point) (mark))))))

(defun yim-asterisk-spc-insert ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\*+[^ ].+")
        (progn
          (while (looking-at "\*")
            (forward-char))
          (unless (looking-at " ")
            (insert " "))))))

(defun yim-ajust-position ()
  (cond
   ((not (or (bobp) (bolp)))
    (backward-char))
   ((and (looking-at " ") (minibufferp))
    (forward-char)
    (insert " "))
   ((yim-ignore-character-p)
    (forward-char)))
  t)

(defun yim-ignore-character-p ()
  (looking-at "[、。上-黑#]"))

(defun yim-backward ()
  (condition-case error
      (let ((move (lambda (action characters &optional times)
                   (let ((case-fold-search nil)
                         (num (or times 1)))
                     (while (looking-at characters)
                       (when (or (bolp) (bobp))
                         (error "Reached beginning of line or buffer"))
                       (when buffer-read-only
                         (error "Reached read only point"))
                       (cl-case action
                         (:fwd  (forward-char  num))
                         (:back (backward-char num))))))))
        (when (yim-ajust-position)
          (funcall move :back "[ー@]")
          (funcall move :fwd (yim-get-regexp :katakana))
          (cond
           ;; backward char if point was hiragana chunk
           ((looking-at (yim-get-regexp :hiragana))
            (funcall move :back "[ぁ-ん0-9ー〜]")
            (forward-char))
           ;; backward char if point was alphabetical chunk
           ((looking-at "[a-z\-\.\,]")
            (funcall move :back "[a-z0-9;:\\-/']"))
           ((looking-at "[0-9;:%\\$\\^\\&'`\\!\"/\\\\]")
            (unless (or (bobp) (bolp))
              (yim-backward))))))
    (error error)))

(defun yim-kanji-henkan-point ()
  (let ((ignore-chars "[\\\\(\[「!?~%#\*\\$上-黑ァ-ン]\\|\\]"))
    (while (looking-at ignore-chars)
      (forward-char))
    (yim-skip-WO)
    (skk-set-henkan-point-subr)))

(defun yim-skip-WO ()
  (when (and yim-skip-WO (looking-at "を"))
    (forward-char)))

;;;*skk-henkan-abbrev
;;abbrev変換の▽を後からつける
;;skk-backward-and-set-henkan-pointのabbrev-mode版
;;;###autoload
(defun yim-abbrev ()
  "abbrev変換の▽を後からつける"
  (interactive)
  (if (eq skk-henkan-mode 'active)
      (cond ((equal (point) (mark))
             (skk-start-henkan 4))
            ((> (point) (mark))
             (skk-kakutei)
             (yim-abbrev)))
    (save-excursion
      (skip-chars-backward "a-z")
      (skk-kakutei)
      (skk-abbrev-mode t))
    (if (looking-at "▽")
        (forward-char)
      (skk-start-henkan 4))))

(defun yim-adjust-string (str)
  (replace-regexp-in-string "\\[.+\\]" " " str))

(defun yim-ignore-match-string (disregar action &optional kanji last)
  (let* ((non-hiragana (or kanji "[^ぁ-ん]"))
         (disregar-last (concat "[^んをっぁぃぅぇぉゃゅょー" last "]"))
         (candidate (concat non-hiragana disregar disregar-last))
         (data (yim-adjust-string disregar))
         (times
          (if (string-match "\n" data)
              (- (length data) 1)
            (length data))))
    (if (looking-at candidate)
        (cl-case action
          (:fwd (forward-char times))
          (:back (backward-char times))))))

(defun yim-convert-list-for-skip-kanji (kanji-list)
  ""
  (cl-loop with kanji and hiragana
           with store = '()
           for record in kanji-list do
           (setq kanji (car (split-string record " "))
                 hiragana (cdr (split-string record " ")))
           (setq store (append store (list (cons kanji hiragana))))
           finally return store))


(provide 'yim)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; yim.el ends here
