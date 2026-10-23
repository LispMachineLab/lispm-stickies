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

;; Run with `make test', LispM Desktop's lisp/ on the load path.  They need
;; no window system: notes are read, written and thrown away without their
;; windows, and the drawings are read pixel by pixel.

;;; Code:

(require 'ert)
(require 'dom)
(require 'lispm-stickies)

(defun lispm-stickies-test--pixel (svg x y)
  "Return the colour of pixel X, Y of SVG: the last rectangle over it."
  (let (color)
    (dolist (rect (dom-by-tag svg 'rect))
      (let ((rx (dom-attr rect 'x)) (ry (dom-attr rect 'y))
            (w (dom-attr rect 'width)) (h (dom-attr rect 'height)))
        (when (and (<= rx x) (< x (+ rx w)) (<= ry y) (< y (+ ry h)))
          (setq color (upcase (dom-attr rect 'fill))))))
    color))

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

(ert-deftest lispm-stickies-test-defined-on-load ()
  "Loaded, Stickies is one of the desktop's applications, with its icon and
the menus of Mac OS 9's Stickies, running while a note is open."
  (let ((props (alist-get 'stickies lispm-desktop-apps)))
    (should (equal (plist-get props :name) "Stickies"))
    (should (file-readable-p (plist-get props :icon)))
    (should (eq (plist-get props :activate) #'lispm-stickies))
    (should (equal (mapcar #'cadr (funcall (plist-get props :menus)))
                   '("File" "Edit" "Note" "Color")))
    (let ((lispm-stickies--notes nil))
      (should-not (funcall (plist-get props :running)))))
  (should (memq #'lispm-stickies--moved lispm-desktop-platinum-frame-moved-functions))
  (should (memq #'lispm-stickies-save kill-emacs-hook)))

(ert-deftest lispm-stickies-test-colors ()
  "The Color menu lists Stickies' seven colours in its order, the note in
front's checked."
  (should (equal (mapcar #'cadr lispm-stickies-colors)
                 '("Yellow" "Blue" "Green" "Pink" "Purple" "Gray" "Black & White")))
  (should (equal (lispm-stickies--color 'yellow 'paper) "#FFFF99"))
  (should (equal (lispm-stickies--color 'unknown 'edge) "#FFCC00"))
  (lispm-stickies-test--with-file
    (let ((note (lispm-stickies-test--note "Hello")))
      (with-current-buffer note
        (lispm-stickies-set-color 'pink))
      (cl-letf (((symbol-function 'lispm-stickies--front-frame) (lambda () 'frame))
                ((symbol-function 'lispm-stickies--note) (lambda (_) note)))
        (should (equal (mapcar (lambda (item) (plist-get item :label))
                               (seq-filter (lambda (item) (plist-get item :checked))
                                           (lispm-stickies--color-items)))
                       '("Pink")))))))

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
    (cl-letf (((symbol-function 'lispm-desktop-app-window-close)
               (lambda (_kind) (lispm-stickies-save))))
      (lispm-stickies-quit))
    (should (equal (mapcar (lambda (note) (plist-get note :text))
                           (lispm-stickies--parse (with-temp-buffer
                                                    (insert-file-contents lispm-stickies-file)
                                                    (buffer-string))))
                   '("One" "Two" "Three")))))

(ert-deftest lispm-stickies-test-throwing-a-note-away ()
  "Killing a note's buffer throws the note away; one holding text only
once the alert says so, an empty one at once."
  (lispm-stickies-test--with-file
    (let ((empty (lispm-stickies-test--note " \n"))
          (full (lispm-stickies-test--note "Keep me"))
          (asked 0))
      (cl-letf (((symbol-function 'lispm-desktop-popup-alert)
                 (lambda (&rest _) (cl-incf asked) 'keep)))
        (lispm-stickies-close-note empty)
        (should (= asked 0))
        (should-not (buffer-live-p empty))
        (lispm-stickies-close-note full)
        (should (= asked 1))
        (should (buffer-live-p full)))
      (cl-letf (((symbol-function 'lispm-desktop-popup-alert) (lambda (&rest _) 'throw)))
        (lispm-stickies-close-note full))
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
    (cl-letf (((symbol-function 'lispm-stickies--show) #'ignore)
              ((symbol-function 'lispm-stickies--new-geometry) (lambda (_) '(60 60 240 160))))
      (should (eq (buffer-local-value 'lispm-stickies--note-color
                                      (lispm-stickies-new-note "Parked on level 3" 'pink))
                  'pink))
      (should-error (lispm-stickies-new-note "Mauve" 'mauve)))
    (lispm-stickies-quit)
    (should (equal (mapcar (lambda (note) (list (plist-get note :text) (plist-get note :color)))
                           (lispm-stickies-notes))
                   '(("Call Sam at 5" yellow) ("Kept" blue) ("Parked on level 3" pink))))))

(ert-deftest lispm-stickies-test-note-buffer ()
  "A note's buffer is its paper: coloured, edged by its fringes, a title
bar and a bottom drawn for it, no scroll bar, kept out of the windows'
buffer menus."
  (lispm-stickies-test--with-file
    (with-current-buffer (lispm-stickies-test--note "Hello" :color 'blue)
      (should (equal (seq-some (lambda (spec) (and (consp spec) (plist-get spec :background)))
                               (alist-get 'default face-remapping-alist))
                     "#99FFFF"))
      (should (equal (seq-some (lambda (spec) (and (consp spec) (plist-get spec :background)))
                               (alist-get 'fringe face-remapping-alist))
                     "#00CCFF"))
      (should (equal tab-line-format '(:eval (lispm-stickies--title-bar))))
      (should (equal mode-line-format '(:eval (lispm-stickies--bottom))))
      (should (= left-fringe-width 1))
      (should-not lispm-desktop-platinum-listed)
      (should (eq (lispm-stickies--scroll-bar (current-buffer)) 'no))
      (should (eq (keymap-lookup lispm-stickies-note-mode-map "C-c C-n") #'lispm-stickies-new-note)))
    (with-temp-buffer
      (should-not (lispm-stickies--scroll-bar (current-buffer))))))

(ert-deftest lispm-stickies-test-title-bar-as-stickies-drew-it ()
  "The front note's title bar is Mac OS 9.0.4's Stickies', pixel for pixel:
pinstripes of every other pixel in the edge colour, the close box five
pixels in from the left edge and the zoom box from the right, two pixels
down; another note's is plain paper, lit along its top."
  (let* ((width 102)
         (svg (lispm-stickies-draw-title-bar 'yellow width 10 t))
         (inactive (lispm-stickies-draw-title-bar 'yellow width 10 nil)))
    (cl-flet ((at (x y) (lispm-stickies-test--pixel svg x y)))
      (should (equal (list (at 0 0) (at 50 0) (at 0 5) (at (1- width) 5)) (make-list 4 "#FFCC00")))
      ;; The pinstripes.
      (should (equal (list (at 1 1) (at 2 1) (at 1 2) (at 2 2) (at 50 9) (at 51 9))
                     '("#FFCC00" "#FFFF99" "#FFFF99" "#FFCC00" "#FFFF99" "#FFCC00")))
      ;; The close box, from column 5 to 11, rows 2 to 8.
      (should (equal (mapcar (lambda (x) (at x 2)) '(5 6 10 11 12))
                     '("#FFFF99" "#FFFF99" "#FFFF99" "#FF9900" "#FFCC00")))
      (should (equal (mapcar (lambda (x) (at x 3)) '(5 6 9 10 11))
                     '("#FFFF99" "#FF9900" "#FF9900" "#FFFF99" "#FF9900")))
      (should (equal (mapcar (lambda (y) (at 6 y)) '(4 6 7)) '("#FF9900" "#FF9900" "#FFFF99")))
      (should (equal (mapcar (lambda (x) (at x 8)) '(4 5 11)) '("#FFCC00" "#FF9900" "#FF9900")))
      ;; The zoom box, its diagonal and its right and bottom lines.
      (let ((right (- width 6)))
        (should (equal (mapcar (lambda (y) (at right y)) '(2 5 8)) (make-list 3 "#FF9900")))
        (should (equal (list (at (- width 8) 3) (at (- width 9) 4) (at (- width 10) 5) (at (- width 11) 6))
                       (make-list 4 "#FF9900")))
        (should (equal (list (at (- width 12) 8) (at (- width 12) 7) (at (- width 7) 3))
                       '("#FF9900" "#FFFF99" "#FFFF99")))))
    (cl-flet ((at (x y) (lispm-stickies-test--pixel inactive x y)))
      (should (equal (list (at 1 0) (at 50 0) (at 8 3) (at 50 5) (at (1- width) 5) (at 0 5))
                     '("#FFFFFF" "#FFFFFF" "#FFFF99" "#FFFF99" "#FFCC00" "#FFCC00"))))))

(ert-deftest lispm-stickies-test-boxes-clear-of-resize-corners ()
  "On macOS, which resizes an undecorated frame from a square at each
corner, the boxes stand past the squares whose rows they cross, so a
press reaches them: the top ones, and on a note rolled up the bottom ones
too; elsewhere they stand where Stickies drew them."
  (let ((width 160)
        (height 160))
    (cl-letf (((symbol-function 'framep) (lambda (_) 'ns))
              ((symbol-function 'frame-parameter) (lambda (_ parameter) (eq parameter 'undecorated)))
              ((symbol-function 'frame-pixel-height) (lambda (_) height)))
      (should (equal (lispm-stickies--box-insets 'note 10) '(12 . 12)))
      (setq height 17)
      (should (equal (lispm-stickies--box-insets 'note 17) '(12 . 15))))
    (cl-letf (((symbol-function 'framep) (lambda (_) 'x))
              ((symbol-function 'frame-pixel-height) (lambda (_) 17)))
      (should (equal (lispm-stickies--box-insets 'note 17) '(5 . 5))))
    (let ((svg (lispm-stickies-draw-title-bar 'yellow width 10 t nil nil '(12 . 12))))
      (cl-flet ((at (x y) (lispm-stickies-test--pixel svg x y)))
        ;; The close box from column 12, the zoom box to column width - 13.
        (should (equal (list (at 13 3) (at 18 2) (at 12 8) (at 11 7))
                       '("#FF9900" "#FF9900" "#FF9900" "#FFCC00")))
        (should (equal (list (at (- width 13) 2) (at (- width 19) 8) (at (- width 12) 8))
                       '("#FF9900" "#FF9900" "#FFCC00")))))))

(ert-deftest lispm-stickies-test-rolled-note-made-taller ()
  "A note rolled up that the window system makes taller, as macOS does when
its edge is dragged, is rolled down; its own sizes, rolled up or down,
reported late, are not taken so."
  (let ((rolled-down 0)
        (height nil))
    (cl-letf (((symbol-function 'window-frame) (lambda (_) 'note))
              ((symbol-function 'lispm-desktop-app-window-rolled-p) #'always)
              ((symbol-function 'lispm-desktop-platinum-rolled-up-height) (lambda (_) 144))
              ((symbol-function 'lispm-stickies--rolled-height) (lambda (_) 17))
              ((symbol-function 'frame-text-height) (lambda (_) height))
              ((symbol-function 'lispm-desktop-app-window-roll-down) (lambda (_) (cl-incf rolled-down)))
              ((symbol-function 'lispm-stickies--remember) #'ignore)
              ((symbol-function 'lispm-stickies--save-soon) #'ignore))
      (dolist (h '(17 144))
        (setq height h)
        (lispm-stickies--resized 'window))
      (should (= rolled-down 0))
      (setq height 119)
      (lispm-stickies--resized 'window)
      (should (= rolled-down 1)))))

(ert-deftest lispm-stickies-test-bottom-as-stickies-drew-it ()
  "The front note's bottom shows Stickies' grow box over the corner, its
edges in ink; another note's is paper over the edge."
  (let ((svg (lispm-stickies-draw-bottom 'gray 100 t))
        (inactive (lispm-stickies-draw-bottom 'gray 100 nil)))
    (cl-flet ((at (x y) (lispm-stickies-test--pixel svg x y)))
      (should (equal (list (at 0 3) (at 50 3) (at 50 7)) '("#BBBBBB" "#EEEEEE" "#BBBBBB")))
      (should (equal (mapcar (lambda (x) (at x 0)) '(95 96 99)) '("#EEEEEE" "#777777" "#777777")))
      (should (equal (mapcar (lambda (x) (at x 4)) '(92 96 97 99)) '("#777777" "#777777" "#EEEEEE" "#777777")))
      (should (equal (mapcar (lambda (x) (at x 7)) '(91 92 99)) '("#BBBBBB" "#777777" "#777777"))))
    (cl-flet ((at (x y) (lispm-stickies-test--pixel inactive x y)))
      (should (equal (list (at 96 0) (at 99 3) (at 95 7)) '("#EEEEEE" "#BBBBBB" "#BBBBBB"))))))

(ert-deftest lispm-stickies-test-rolled-up-title-bar ()
  "A note rolled up has a taller title bar, edged at its bottom, its boxes
in the middle."
  (let ((svg (lispm-stickies-draw-title-bar 'green 120 17 t)))
    (cl-flet ((at (x y) (lispm-stickies-test--pixel svg x y)))
      (should (equal (at 60 16) "#66FF66"))
      ;; The close box is rows 5 to 11: its top inner line on row 6.
      (should (equal (list (at 6 6) (at 6 4) (at 11 11)) '("#00EE00" "#66FF66" "#00EE00"))))))

(ert-deftest lispm-stickies-test-unload ()
  "Unloaded, Stickies quits and leaves the desktop."
  (lispm-stickies-test--with-file
    (lispm-stickies-test--note "Bye")
    (lispm-stickies-unload-function)
    (unwind-protect
        (progn
          (should-not (assq 'stickies lispm-desktop-apps))
          (should-not lispm-stickies--notes)
          (should-not (memq #'lispm-stickies--moved lispm-desktop-platinum-frame-moved-functions))
          (should (equal (mapcar (lambda (note) (plist-get note :text))
                                 (lispm-stickies--parse (with-temp-buffer
                                                          (insert-file-contents lispm-stickies-file)
                                                          (buffer-string))))
                         '("Bye"))))
      (lispm-stickies--setup))))

(provide 'lispm-stickies-test)
;;; lispm-stickies-test.el ends here
