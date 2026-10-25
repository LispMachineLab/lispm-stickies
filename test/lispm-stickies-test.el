;;; lispm-stickies-test.el --- Tests for lispm-stickies  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-stickies
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run with `make test'.  They need no window system nor LispM Desktop:
;; notes are read, written, shown in windows of a batch Emacs's frame and
;; thrown away.

;;; Code:

(require 'ert)
(require 'lispm-stickies)

(defconst lispm-stickies-test--directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The repository's root.")

(defmacro lispm-stickies-test--with-file (&rest body)
  "Run BODY with the notes kept in a file of their own, Stickies closed."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "lispm-stickies-test" t))
          (lispm-stickies-file (expand-file-name "stickies.org" dir))
          (lispm-stickies--notes nil)
          (lispm-stickies--loaded nil))
     (unwind-protect (progn ,@body)
       (let ((lispm-stickies--quitting t))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p lispm-stickies--notes)))
       (delete-directory dir t))))

(defun lispm-stickies-test--note (text &rest properties)
  "Return a new note's buffer holding TEXT, one of the notes, with
PROPERTIES as `lispm-stickies--parse' returns them."
  (lispm-stickies--load)
  (let ((buffer (lispm-stickies--make-note
                 (append properties
                         (list :id (lispm-stickies--new-id) :color 'yellow :text text
                               :geometry '(60 60 240 160))))))
    (setq lispm-stickies--notes (append lispm-stickies--notes (list buffer)))
    buffer))

(ert-deftest lispm-stickies-test-knows-no-desktop ()
  "Stickies stands alone: none of its files but the tests names LispM
Desktop's code, which shows the notes its own way from its side."
  (dolist (file (directory-files lispm-stickies-test--directory t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (and (re-search-forward "lispm-desktop\\|platinum" nil t)
                       (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)))))))

(ert-deftest lispm-stickies-test-loaded ()
  "Loaded, Stickies has its icon, opens from its command and writes its
notes as Emacs quits; it shows them in windows of the selected frame."
  (should (file-readable-p lispm-stickies-icon))
  (should (commandp #'lispm-stickies))
  (should (memq #'lispm-stickies-save kill-emacs-hook))
  (should (eq lispm-stickies-show-note-function #'lispm-stickies-show-note-in-window)))

(ert-deftest lispm-stickies-test-colors ()
  "The Color menu lists Stickies' seven colours in its order, the note in
front's checked."
  (should (equal (mapcar #'cadr lispm-stickies-colors)
                 '("Yellow" "Blue" "Green" "Pink" "Purple" "Gray" "Black & White")))
  (should (equal (lispm-stickies-color-value 'yellow 'paper) "#FFFF99"))
  (should (equal (lispm-stickies-color-value 'unknown 'edge) "#FFCC00"))
  (should (equal (lispm-stickies--blend "#FFFF99" "#FF9900" 0.5) "#FFCC4C"))
  (lispm-stickies-test--with-file
    (let ((note (lispm-stickies-test--note "Hello"))
          (changed 0))
      (with-current-buffer note
        (add-hook 'lispm-stickies-changed-hook (lambda () (cl-incf changed)) nil t)
        (lispm-stickies-set-color 'pink)
        (should (eq (lispm-stickies-note-color note) 'pink))
        (should (= changed 1))
        (should (equal (seq-some (lambda (spec) (and (consp spec) (plist-get spec :background)))
                                 (alist-get 'default face-remapping-alist))
                       "#FFCCCC"))
        ;; The menu bar's Color menu has them all, the note's checked.
        (let ((items (seq-filter #'consp (cdr (keymap-lookup lispm-stickies-note-mode-map
                                                             "<menu-bar> <stickies> <Color>")))))
          (should (= (length items) 7))
          (should (equal (seq-keep (lambda (item)
                                     (and (eval (cdr (plist-get (nthcdr 4 item) :button)) t)
                                          (nth 2 item)))
                                   items)
                         '("Pink"))))))))

(ert-deftest lispm-stickies-test-title ()
  "A note is titled by its first line that is not blank, shortened."
  (should (equal (lispm-stickies--title "\n  Buy milk  \nand eggs") "Buy milk"))
  (should (equal (lispm-stickies--title " \n\n") "Note"))
  (should (= (length (lispm-stickies--title (make-string 100 ?a))) 60)))

(ert-deftest lispm-stickies-test-file-round-trip ()
  "Notes written to the org file read back as they were: a line that
would start a heading is a level deeper in the file, and back again."
  (let* ((created (encode-time '(0 30 15 21 10 2026 nil -1 nil)))
         (notes (list (list :id "a" :color 'pink :geometry '(10 20 240 160) :rolled t
                            :created created :modified created
                            :text "* Not a note\n** Nor this\n*bold* stays\nλ and ü")
                      (list :id "b" :color 'black-and-white :geometry '(30 40 200 100) :rolled nil
                            :created created :modified created :text "")
                      (list :id "c" :color 'gray :geometry '(1 2 3 4) :rolled nil
                            :created created :modified created :text "ends in a newline\n")))
         (file (lispm-stickies--format notes)))
    (should (string-match-p "^\\* \\* Not a note$" file))
    (should (string-match-p "^\\*\\* Not a note$" file))
    (should (string-match-p "^\\*\\*\\* Nor this$" file))
    (should (string-match-p "^\\*bold\\* stays$" file))
    (should (string-match-p "^:CREATED:  \\[2026-10-21 Wed 15:30\\]$" file))
    (should (equal (lispm-stickies--parse file) notes))))

(ert-deftest lispm-stickies-test-file-written-by-hand ()
  "A note written by hand in the file, without properties or with ones
unknown, still reads: a colour unknown is the new notes' colour."
  (let ((notes (lispm-stickies--parse "#+title: Stickies\n\n* Plain\nJust text\n* Odd\n:PROPERTIES:\n:COLOR: mauve\n:GEOMETRY: 1 2\n:END:\nMore\n")))
    (should (equal (mapcar (lambda (note) (plist-get note :text)) notes) '("Just text" "More")))
    (should (equal (mapcar (lambda (note) (plist-get note :color)) notes)
                   (list lispm-stickies-color lispm-stickies-color)))
    (should-not (plist-get (cadr notes) :geometry))
    (should (stringp (plist-get (car notes) :id)))))

(ert-deftest lispm-stickies-test-notes-written-and-read ()
  "The notes are written to the file and come back from it, after Stickies
quit, with their text, colour, place and dates."
  (lispm-stickies-test--with-file
    (let ((note (lispm-stickies-test--note "Buy milk\nand eggs")))
      (with-current-buffer note
        (lispm-stickies-set-color 'green)
        (goto-char (point-max))
        (insert "\n* and bread")))
    (lispm-stickies-test--note "Call Joie")
    (lispm-stickies-quit)
    (should-not lispm-stickies--notes)
    (should-not lispm-stickies--loaded)
    (should (string-match-p "^\\*\\* and bread$"
                            (with-temp-buffer (insert-file-contents lispm-stickies-file)
                                              (buffer-string))))
    (lispm-stickies--load)
    (should (equal (mapcar (lambda (buffer) (with-current-buffer buffer (buffer-string)))
                           lispm-stickies--notes)
                   '("Buy milk\nand eggs\n* and bread" "Call Joie")))
    (with-current-buffer (car lispm-stickies--notes)
      (should (eq lispm-stickies--note-color 'green))
      (should (equal lispm-stickies--geometry '(60 60 240 160)))
      (should (eq major-mode 'lispm-stickies-note-mode)))))

(ert-deftest lispm-stickies-test-quitting-keeps-every-note ()
  "Quitting writes every note, even when a write pending runs while the
notes' windows close, as timers do."
  (lispm-stickies-test--with-file
    (dolist (text '("One" "Two" "Three"))
      (lispm-stickies-test--note text))
    (let ((lispm-stickies-hide-note-function (lambda (_buffer) (lispm-stickies-save))))
      (lispm-stickies-quit))
    (should (equal (mapcar (lambda (note) (plist-get note :text))
                           (lispm-stickies--parse (with-temp-buffer
                                                    (insert-file-contents lispm-stickies-file)
                                                    (buffer-string))))
                   '("One" "Two" "Three")))))

(ert-deftest lispm-stickies-test-throwing-a-note-away ()
  "Killing a note's buffer throws the note away; one holding text only
once `lispm-stickies-confirm-function' says so, an empty one at once,
and what shows it is taken away."
  (lispm-stickies-test--with-file
    (let ((empty (lispm-stickies-test--note " \n"))
          (full (lispm-stickies-test--note "Keep me"))
          (asked 0)
          (hidden nil))
      (let ((lispm-stickies-hide-note-function (lambda (buffer) (push buffer hidden))))
        (let ((lispm-stickies-confirm-function (lambda (&rest _) (cl-incf asked) nil)))
          (lispm-stickies-close-note empty)
          (should (= asked 0))
          (should-not (buffer-live-p empty))
          (should (equal hidden (list empty)))
          (lispm-stickies-close-note full)
          (should (= asked 1))
          (should (buffer-live-p full)))
        (let ((lispm-stickies-confirm-function #'always))
          (lispm-stickies-close-note full))
        (should (equal hidden (list full empty))))
      (should-not (buffer-live-p full))
      (should-not lispm-stickies--notes)
      (should-not (lispm-stickies--parse (with-temp-buffer (insert-file-contents lispm-stickies-file)
                                                           (buffer-string)))))))

(ert-deftest lispm-stickies-test-for-programs ()
  "Programs list the notes, read from the file while Stickies is closed,
which changes nothing; find one by its identity, a note written by hand
too; change its text; and make one of a colour."
  (lispm-stickies-test--with-file
    (with-temp-file lispm-stickies-file
      (insert "#+title: Stickies\n\n* Plain\nJust text\n"
              "* Kept\n:PROPERTIES:\n:ID: k1\n:COLOR: blue\n:END:\nKept\n"))
    (let ((notes (lispm-stickies-notes)))
      (should (equal (mapcar (lambda (note) (plist-get note :text)) notes) '("Just text" "Kept")))
      (should (equal (plist-get (cadr notes) :id) "k1"))
      (should (equal (plist-get (car notes) :id) (plist-get (car (lispm-stickies-notes)) :id)))
      (should-not (lispm-stickies-find-note "none"))
      (should-not lispm-stickies--loaded)
      (let ((plain (lispm-stickies-find-note (plist-get (car notes) :id))))
        (should lispm-stickies--loaded)
        (should (equal (with-current-buffer plain (buffer-string)) "Just text"))
        (lispm-stickies-set-text plain "Call Sam at 5")
        (should (equal (plist-get (car (lispm-stickies-notes)) :text) "Call Sam at 5"))))
    (let ((lispm-stickies-show-note-function #'ignore))
      (should (eq (lispm-stickies-note-color (lispm-stickies-new-note "Parked on level 3" 'pink))
                  'pink))
      (should-error (lispm-stickies-new-note "Mauve" 'mauve)))
    (lispm-stickies-quit)
    (should (equal (mapcar (lambda (note) (list (plist-get note :text) (plist-get note :color)))
                           (lispm-stickies-notes))
                   '(("Call Sam at 5" yellow) ("Kept" blue) ("Parked on level 3" pink))))))

(ert-deftest lispm-stickies-test-note-buffer ()
  "A note's buffer is its paper, its fringes its edges, named after its
first line, as that changes."
  (lispm-stickies-test--with-file
    (let ((note (lispm-stickies-test--note "Hello" :color 'blue)))
      (with-current-buffer note
        (should (equal (seq-some (lambda (spec) (and (consp spec) (plist-get spec :background)))
                                 (alist-get 'default face-remapping-alist))
                       "#99FFFF"))
        (should (equal (seq-some (lambda (spec) (and (consp spec) (plist-get spec :background)))
                                 (alist-get 'fringe face-remapping-alist))
                       "#00CCFF"))
        (should (equal (buffer-name) "*Note: Hello*"))
        (goto-char (point-min))
        (insert "Buy milk\n")
        (should (equal (buffer-name) "*Note: Buy milk*"))
        (should (equal (lispm-stickies-note-title note) "Buy milk"))
        (should (eq (keymap-lookup lispm-stickies-note-mode-map "C-c C-n") #'lispm-stickies-new-note))))))

(ert-deftest lispm-stickies-test-shown-in-windows ()
  "Without a frontend, a note is shown in a window of the selected frame:
the one showing a note, another one otherwise; the next and previous
notes come in turn, and Stickies opens on the note in front."
  (lispm-stickies-test--with-file
    (save-window-excursion
      (delete-other-windows)
      (let* ((one (lispm-stickies-test--note "One"))
             (two (lispm-stickies-test--note "Two"))
             (other (selected-window)))
        (should-not (lispm-stickies-running-p))
        (let ((window (lispm-stickies-show-note one)))
          (should (eq (window-buffer window) one))
          (should (eq window (selected-window)))
          (should (lispm-stickies-running-p))
          (should (eq (funcall lispm-stickies-front-note-function) one))
          ;; From a note, the next takes its window.
          (lispm-stickies-next-note)
          (should (eq (window-buffer window) two))
          (lispm-stickies-next-note)
          (should (eq (window-buffer window) one))
          (lispm-stickies-previous-note)
          (should (eq (window-buffer window) two))
          (select-window other)
          (should (eq (funcall lispm-stickies-front-note-function) two))
          (with-temp-buffer
            (lispm-stickies)
            (should (eq (window-buffer) two))))))))

(ert-deftest lispm-stickies-test-frontend ()
  "A frontend shows the notes its own way: Stickies asks it to show a
note, where the notes are before writing them, and tells it of changes."
  (lispm-stickies-test--with-file
    (let* ((shown nil)
           (changed nil)
           (lispm-stickies-show-note-function (lambda (buffer) (push buffer shown) 'frame))
           (lispm-stickies-shown-function (lambda (buffer) (memq buffer shown)))
           (lispm-stickies-front-note-function (lambda () (car shown)))
           (lispm-stickies-place-function (lambda (_buffer) '((10 20 300 200) . t)))
           (lispm-stickies-changed-hook (list (lambda () (push (current-buffer) changed))))
           (note (lispm-stickies-new-note "Hello")))
      (should (equal shown (list note)))
      (should (eq (lispm-stickies-show-note note) 'frame))
      (should (lispm-stickies-running-p))
      (should (eq (lispm-stickies--current) note))
      (with-current-buffer note
        (insert "! "))
      (should (equal changed (list note)))
      (should (equal (lispm-stickies-note-place note) '(nil)))
      (let ((plist (car (lispm-stickies-notes))))
        (should (equal (plist-get plist :geometry) '(10 20 300 200)))
        (should (plist-get plist :rolled)))
      (should (equal (lispm-stickies-note-place note) '((10 20 300 200) . t)))
      (lispm-stickies-set-note-place note '(1 2 3 4) nil)
      (should (equal (lispm-stickies-note-place note) '((1 2 3 4))))
      (should (equal (lispm-stickies-note-id note) (plist-get (car (lispm-stickies-notes)) :id)))
      (should (equal (lispm-stickies-note-buffers) (list note)))
      (lispm-stickies-quit)
      (should-not (lispm-stickies-note-buffers))
      (should (equal (mapcar #'lispm-stickies-note-title (lispm-stickies-note-buffers t)) '("! Hello"))))))

(ert-deftest lispm-stickies-test-unload ()
  "Unloaded, Stickies quits, its notes written."
  (lispm-stickies-test--with-file
    (lispm-stickies-test--note "Bye")
    (lispm-stickies-unload-function)
    (unwind-protect
        (progn
          (should-not lispm-stickies--notes)
          (should-not (memq #'lispm-stickies-save kill-emacs-hook))
          (should (equal (mapcar (lambda (note) (plist-get note :text))
                                 (lispm-stickies--parse (with-temp-buffer
                                                          (insert-file-contents lispm-stickies-file)
                                                          (buffer-string))))
                         '("Bye"))))
      (add-hook 'kill-emacs-hook #'lispm-stickies-save))))

(provide 'lispm-stickies-test)
;;; lispm-stickies-test.el ends here
