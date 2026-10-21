;;; lispm-stickies.el --- Stickies, Mac OS 9's notes, on LispM Desktop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-stickies
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, outlines
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

;; Stickies, Mac OS 9's notes, as an application of LispM Desktop that its
;; App Store installs: each note a small window of its own over the
;; desktop, in one of Stickies' seven colours, drawn as Stickies drew it.
;; The note in front wears a title bar of pinstripes with a close box and
;; a zoom box, and a grow box in its corner; the others are plain paper.
;;
;; Loading this file makes Stickies one of the desktop's applications
;; (`lispm-desktop-define-app'); unloading it, as the App Store does when
;; it removes it, takes it away again.  Opening Stickies brings its notes
;; back where they were.  A note is written as it is typed.  Its close
;; box throws it away, as Stickies' did, asking first when it holds text;
;; quitting Stickies only puts the notes away.  A double click on a title
;; bar rolls the note up into it, its first line for a title; the zoom
;; box sizes the note to its text, and back.
;;
;; The notes are kept in one org file, `lispm-stickies-file', a heading
;; each: its first line for a title, its colour, place and dates in its
;; properties, its text under them.  Agents read it as any org file.
;;
;;   * Buy milk
;;   :PROPERTIES:
;;   :ID:       20261021T153012-3f2a
;;   :COLOR:    yellow
;;   :GEOMETRY: 120 80 240 160
;;   :CREATED:  [2026-10-21 Wed 15:30]
;;   :MODIFIED: [2026-10-21 Wed 15:31]
;;   :END:
;;   Buy milk
;;   and eggs
;;
;; A line of a note that would start a heading there is written a level
;; deeper, so it stays the note's own.  Stickies writes the file while it
;; runs: edit it while Stickies is closed.
;;
;; From the keyboard, in a note:
;;
;;   C-c C-n  a new note             C-c C-f  the next note
;;   C-c C-k  throw the note away    C-c C-b  the previous note
;;   C-c C-l  its colour             C-c C-r  roll it up, or down
;;   C-c C-z  zoom it                C-c C-d  when it was made and changed
;;   C-c C-q  quit Stickies

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'face-remap)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-popup)

(defconst lispm-stickies-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package, holding its icon.")

(defconst lispm-stickies-icon (expand-file-name "icons/stickies.svg" lispm-stickies-directory)
  "Stickies' icon, a stack of notes.")

(defconst lispm-stickies-colors
  '((yellow "Yellow" "#FFFF99" "#FFCC00" "#FF9900")
    (blue "Blue" "#99FFFF" "#00CCFF" "#0099FF")
    (green "Green" "#CCFFCC" "#66FF66" "#00EE00")
    (pink "Pink" "#FFCCCC" "#FF9999" "#FF66FF")
    (purple "Purple" "#CCCCFF" "#9999FF" "#9999FF")
    (gray "Gray" "#EEEEEE" "#BBBBBB" "#777777")
    (black-and-white "Black & White" "#FFFFFF" "#000000" "#000000"))
  "The colours of notes, in the order of Stickies' Color menu:
\(KEY NAME PAPER EDGE INK).  PAPER is the note, EDGE its outline and the
pinstripes of its title bar, INK its boxes, as Mac OS 9.0.4's Stickies
drew them.")

(defgroup lispm-stickies nil
  "Stickies, Mac OS 9's notes, on LispM Desktop."
  :group 'lispm-desktop
  :prefix "lispm-stickies-")

(defcustom lispm-stickies-file (locate-user-emacs-file "stickies.org")
  "The org file the notes are kept in, a heading each."
  :type 'file)

(defcustom lispm-stickies-color 'yellow
  "The colour of a new note, a key of `lispm-stickies-colors'."
  :type (cons 'choice (mapcar (lambda (color) (list 'const :tag (nth 1 color) (car color)))
                              lispm-stickies-colors)))

(defcustom lispm-stickies-face 'variable-pitch
  "Face the notes are written in, over their paper, or nil for Emacs's
own default.  Stickies wrote in Geneva, a font of varying widths."
  :type '(choice (const :tag "Emacs's default" nil) face))

(defconst lispm-stickies-size '(240 . 160)
  "Size in pixels of a new note: (WIDTH . HEIGHT).")

(defconst lispm-stickies-title-height 10
  "Height in pixels of a note's title bar, its top edge included.")

(defconst lispm-stickies-bottom-height 8
  "Height in pixels of a note's bottom, its edge and the grow box above it.")

(defconst lispm-stickies-padding 4
  "Pixels between a note's left edge and its text.")

(defconst lispm-stickies-box-inset 5
  "Pixels from a note's side edges to its title bar boxes, Stickies'.")

;;;; Colours

(defun lispm-stickies--color (key part)
  "Return PART of the colour KEY: `name', `paper', `edge' or `ink'.
A colour unknown is yellow."
  (let ((color (or (alist-get key lispm-stickies-colors)
                   (alist-get 'yellow lispm-stickies-colors))))
    (nth (pcase part ('name 0) ('paper 1) ('edge 2) ('ink 3)) color)))

;;;; The file

(defun lispm-stickies--escape (text)
  "Return TEXT with each line that would start a heading a level deeper."
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " text t))

(defun lispm-stickies--unescape (text)
  "Return TEXT with each heading a level higher, as it was before
`lispm-stickies--escape'."
  (replace-regexp-in-string "^\\*\\(\\*+\\) " "\\1 " text t))

(defun lispm-stickies--title (text)
  "Return the title of a note holding TEXT: its first line that is not
blank, shortened, or \"Note\"."
  (let ((line (seq-find (lambda (line) (not (string-blank-p line))) (split-string text "\n"))))
    (if line (truncate-string-to-width (string-trim line) 60 nil nil "…") "Note")))

(defun lispm-stickies--timestamp (time)
  "Return TIME as an inactive org timestamp."
  (let ((system-time-locale "C"))
    (format-time-string "[%Y-%m-%d %a %H:%M]" time)))

(defun lispm-stickies--parse-timestamp (string)
  "Return the time of STRING, an inactive org timestamp, or nil."
  (when (and string (string-match "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[^]0-9]*\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)" string))
    (encode-time (list 0 (string-to-number (match-string 5 string))
                       (string-to-number (match-string 4 string))
                       (string-to-number (match-string 3 string))
                       (string-to-number (match-string 2 string))
                       (string-to-number (match-string 1 string))
                       nil -1 nil))))

(defun lispm-stickies--parse (text)
  "Return the notes of TEXT, the contents of `lispm-stickies-file', as
plists: :id, :color, :geometry, :rolled, :created, :modified and :text."
  (let (notes)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (let ((end (save-excursion
                     (forward-line)
                     (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max))))
              (properties nil))
          (forward-line)
          (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
            (forward-line)
            (while (and (< (point) end) (not (looking-at "[ \t]*:END:[ \t]*$")))
              (when (looking-at "[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$")
                (push (cons (upcase (match-string 1)) (match-string 2)) properties))
              (forward-line))
            (forward-line))
          (let* ((body (buffer-substring-no-properties (min (point) end) end))
                 (geometry (mapcar #'string-to-number
                                   (split-string (or (cdr (assoc "GEOMETRY" properties)) ""))))
                 (color (intern-soft (or (cdr (assoc "COLOR" properties)) ""))))
            (push (list :id (or (cdr (assoc "ID" properties)) (lispm-stickies--new-id))
                        :color (if (assq color lispm-stickies-colors) color lispm-stickies-color)
                        :geometry (and (= (length geometry) 4) geometry)
                        :rolled (equal (cdr (assoc "ROLLED" properties)) "t")
                        :created (lispm-stickies--parse-timestamp (cdr (assoc "CREATED" properties)))
                        :modified (lispm-stickies--parse-timestamp (cdr (assoc "MODIFIED" properties)))
                        :text (lispm-stickies--unescape (string-remove-suffix "\n" body)))
                  notes))
          (goto-char end))))
    (nreverse notes)))

(defun lispm-stickies--format (notes)
  "Return the contents of `lispm-stickies-file' holding NOTES, plists as
`lispm-stickies--parse' returns them."
  (concat
   "#+title: Stickies\n"
   "# The notes of LispM's Stickies, a heading each.  Stickies writes this\n"
   "# file while it runs: edit it while Stickies is closed.\n\n"
   (mapconcat
    (lambda (note)
      (concat "* " (lispm-stickies--title (plist-get note :text)) "\n"
              ":PROPERTIES:\n"
              (format ":ID:       %s\n" (plist-get note :id))
              (format ":COLOR:    %s\n" (plist-get note :color))
              (when-let* ((geometry (plist-get note :geometry)))
                (format ":GEOMETRY: %s\n" (mapconcat #'number-to-string geometry " ")))
              (when (plist-get note :rolled) ":ROLLED:   t\n")
              (when-let* ((time (plist-get note :created)))
                (format ":CREATED:  %s\n" (lispm-stickies--timestamp time)))
              (when-let* ((time (plist-get note :modified)))
                (format ":MODIFIED: %s\n" (lispm-stickies--timestamp time)))
              ":END:\n"
              (lispm-stickies--escape (plist-get note :text)) "\n"))
    notes "")))

;;;; Notes

(defvar lispm-stickies--notes nil
  "The notes' buffers, the oldest first, while Stickies runs.")

(defvar lispm-stickies--loaded nil
  "Non-nil while the notes are read: from opening Stickies to quitting it.")

(defvar lispm-stickies--quitting nil
  "Non-nil while Stickies quits, its notes' buffers killed but kept.")

(defvar-local lispm-stickies--id nil
  "This note's identity, a string.")

(defvar-local lispm-stickies--note-color nil
  "This note's colour, a key of `lispm-stickies-colors'.")

(defvar-local lispm-stickies--created nil
  "When this note was made.")

(defvar-local lispm-stickies--modified nil
  "When this note's text last changed.")

(defvar-local lispm-stickies--geometry nil
  "Where this note's window was last: (LEFT TOP WIDTH HEIGHT) in pixels,
its text area's size, over the desktop.")

(defvar-local lispm-stickies--rolled nil
  "Non-nil while this note is rolled up into its title bar.")

(defvar-local lispm-stickies--unzoomed nil
  "This note's size before its zoom box sized it to its text, or nil.")

(defvar-local lispm-stickies--remaps nil
  "The face remappings colouring this note.")

(defun lispm-stickies--new-id ()
  "Return a new note's identity."
  (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random #x10000)))

(defun lispm-stickies--kind (buffer)
  "Return the kind of window of the note BUFFER, see
`lispm-desktop-app-window-define'."
  (intern (concat "lispm-stickies-" (buffer-local-value 'lispm-stickies--id buffer))))

(defun lispm-stickies--frame (buffer)
  "Return the window of the note BUFFER while it is open."
  (lispm-desktop-app-window-frame (lispm-stickies--kind buffer)))

(defun lispm-stickies--note (frame)
  "Return the note FRAME shows, or nil when FRAME is no note's window."
  (let ((buffer (frame-parameter frame 'lispm-stickies-note)))
    (and (buffer-live-p buffer) buffer)))

(defun lispm-stickies--frames ()
  "Return the notes' open windows, the oldest note's first."
  (delq nil (mapcar #'lispm-stickies--frame lispm-stickies--notes)))

(defun lispm-stickies--front-frame ()
  "Return the window of the note in front, or nil when none is open."
  (let ((frames (lispm-stickies--frames)))
    (or (seq-find (lambda (frame) (memq frame frames))
                  (lispm-desktop-apps-windows-in-order (lispm-desktop-apps-screen)))
        (car (last frames)))))

(defun lispm-stickies--running-p ()
  "Return non-nil while a note is open."
  (and (lispm-stickies--frames) t))

(defun lispm-stickies--make-note (note)
  "Return the buffer of NOTE, a plist as `lispm-stickies--parse' returns."
  (let ((buffer (generate-new-buffer "*Stickies Note*")))
    (with-current-buffer buffer
      (let ((buffer-undo-list t))
        (insert (plist-get note :text)))
      (goto-char (point-min))
      (lispm-stickies-note-mode)
      (setq lispm-stickies--id (plist-get note :id)
            lispm-stickies--note-color (plist-get note :color)
            lispm-stickies--created (or (plist-get note :created) (current-time))
            lispm-stickies--modified (or (plist-get note :modified) lispm-stickies--created)
            lispm-stickies--geometry (or (plist-get note :geometry) (lispm-stickies--new-geometry nil))
            lispm-stickies--rolled (plist-get note :rolled))
      (lispm-stickies--paint)
      (set-buffer-modified-p nil))
    buffer))

(defun lispm-stickies--new-geometry (front)
  "Return where a new note opens: a little right of and below FRONT, the
window of the note in front, or near the desktop's top left corner."
  (if (frame-live-p front)
      (pcase-let ((`(,left ,top . ,_) (lispm-stickies--frame-geometry front)))
        (list (+ left 20) (+ top 20) (car lispm-stickies-size) (cdr lispm-stickies-size)))
    (list 60 (+ 40 (lispm-desktop-app-window-menu-bar-height (lispm-desktop-apps-screen)))
          (car lispm-stickies-size) (cdr lispm-stickies-size))))

(defun lispm-stickies--frame-geometry (frame)
  "Return where FRAME, a note's window, is: (LEFT TOP WIDTH HEIGHT), as
`lispm-stickies--geometry' holds it.  A window out on the computer's
screen counts from its desktop's corner; one rolled up keeps the height
it rolls down to."
  (pcase-let ((`(,left . ,top) (lispm-desktop-platinum-window-position frame)))
    (when-let* ((screen (and (not (lispm-desktop-platinum-window-parent frame))
                             (lispm-desktop-apps-out-p frame))))
      (pcase-let ((`(,x ,y . ,_) (frame-edges screen 'native-edges)))
        (setq left (- left x) top (- top y))))
    (list left top (frame-text-width frame)
          (or (lispm-desktop-platinum-rolled-up-height frame) (frame-text-height frame)))))

(defun lispm-stickies--remember (frame)
  "Remember where FRAME, a note's window, is and whether it is rolled up."
  (when-let* ((buffer (lispm-stickies--note frame)))
    (with-current-buffer buffer
      (setq lispm-stickies--geometry (lispm-stickies--frame-geometry frame)
            lispm-stickies--rolled (lispm-desktop-app-window-rolled-p frame)))))

(defun lispm-stickies--plist (buffer)
  "Return the note BUFFER as a plist, as `lispm-stickies--parse' returns."
  (when-let* ((frame (lispm-stickies--frame buffer)))
    (lispm-stickies--remember frame))
  (with-current-buffer buffer
    (list :id lispm-stickies--id :color lispm-stickies--note-color
          :geometry lispm-stickies--geometry :rolled lispm-stickies--rolled
          :created lispm-stickies--created :modified lispm-stickies--modified
          :text (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Reading and writing

(defvar lispm-stickies--save-timer nil
  "Pending timer writing the notes, if any.")

(defun lispm-stickies--load ()
  "Read the notes from `lispm-stickies-file', unless they are read."
  (unless lispm-stickies--loaded
    (setq lispm-stickies--notes
          (mapcar #'lispm-stickies--make-note
                  (when (file-readable-p lispm-stickies-file)
                    (lispm-stickies--parse
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8))
                         (insert-file-contents lispm-stickies-file))
                       (buffer-string)))))
          lispm-stickies--loaded t)))

(defun lispm-stickies-save ()
  "Write the notes to `lispm-stickies-file', while Stickies runs."
  (interactive)
  (when (timerp lispm-stickies--save-timer)
    (cancel-timer lispm-stickies--save-timer))
  (setq lispm-stickies--save-timer nil)
  (when lispm-stickies--loaded
    (let ((text (lispm-stickies--format
                 (mapcar #'lispm-stickies--plist (seq-filter #'buffer-live-p lispm-stickies--notes))))
          (coding-system-for-write 'utf-8-unix))
      (make-directory (file-name-directory (expand-file-name lispm-stickies-file)) t)
      (unless (and (file-readable-p lispm-stickies-file)
                   (equal text (with-temp-buffer
                                 (let ((coding-system-for-read 'utf-8))
                                   (insert-file-contents lispm-stickies-file))
                                 (buffer-string))))
        (write-region text nil lispm-stickies-file nil 'silent)))))

(defun lispm-stickies--save-soon ()
  "Write the notes once Emacs is idle for a second."
  (unless (timerp lispm-stickies--save-timer)
    (setq lispm-stickies--save-timer (run-with-idle-timer 1 nil #'lispm-stickies-save))))

;;;; The windows

(defun lispm-stickies--define-kind (buffer)
  "Make the window of the note BUFFER a kind of floating window, which
draws its own frame, see `lispm-desktop-app-window-define'."
  (lispm-desktop-app-window-define
   (lispm-stickies--kind buffer)
   :app 'stickies :name "Stickies"
   :geometry (lambda (_screen) (buffer-local-value 'lispm-stickies--geometry buffer))
   :buffer (lambda () buffer)
   :params `((lispm-desktop-platinum-own-frame . t) (lispm-stickies-note . ,buffer)
             (child-frame-border-width . 0) (left-fringe . 1) (right-fringe . 1)
             (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
             (right-divider-width . 0) (bottom-divider-width . 0)
             (cursor-color . "#000000"))
   :roll #'lispm-stickies--roll
   :refocus-p #'ignore
   :on-close #'lispm-stickies--save-soon))

(defun lispm-stickies--forget (buffer)
  "Forget the kind of window of the note BUFFER."
  (setf (alist-get (lispm-stickies--kind buffer) lispm-desktop-app-window-kinds nil t) nil))

(defun lispm-stickies--show (buffer)
  "Open the note BUFFER in its window over the desktop, in front, rolled up
when it was; return the window."
  (let ((open (lispm-stickies--frame buffer)))
    (lispm-stickies--define-kind buffer)
    (let ((frame (lispm-desktop-app-window-show (lispm-stickies--kind buffer))))
      (when (and (not open) (buffer-local-value 'lispm-stickies--rolled buffer))
        (lispm-desktop-app-window-roll-up frame))
      frame)))

(defun lispm-stickies--rolled-height (frame)
  "Return the height of FRAME's title bar while it is rolled up: a pixel
more than a line, as Emacs keeps the tab line of no window shorter."
  (1+ (frame-char-height frame)))

(defun lispm-stickies--roll (frame)
  "Leave FRAME's note its title bar alone, for rolling it up; return the
title bar's height.  This is the :roll of a note's kind of window."
  (set-window-parameter (frame-root-window frame) 'mode-line-format 'none)
  (lispm-stickies--rolled-height frame))

(defun lispm-stickies--resized (window)
  "Take the note in WINDOW as rolled down when the window system made it
taller while it was rolled up, as macOS does when its edge is dragged:
its title bar would be drawn rolled up over its text.  A size that is
the note's own, rolled up or down, is not taken so, as macOS reports a
size Emacs set a moment late.
This is one of `window-size-change-functions' of a note."
  (let ((frame (window-frame window)))
    (when (and (lispm-desktop-app-window-rolled-p frame)
               (not (memql (frame-text-height frame)
                           (list (lispm-desktop-platinum-rolled-up-height frame)
                                 (lispm-stickies--rolled-height frame)))))
      (lispm-desktop-app-window-roll-down frame)
      (lispm-stickies--remember frame)
      (lispm-stickies--save-soon))))

(defun lispm-stickies--moved (frame)
  "Remember where FRAME is when it is a note's window, moved or grown.
This is one of `lispm-desktop-platinum-frame-moved-functions'."
  (when (lispm-stickies--note frame)
    (lispm-stickies--remember frame)
    (lispm-stickies--save-soon)))

(defun lispm-stickies--refocus ()
  "Give the note in front the keyboard back after a menu bar menu closes,
while Stickies is in front.
This is one of `lispm-desktop-platinum-menu-closed-hook'."
  (when-let* (((eq (lispm-desktop-apps-frontmost) 'stickies))
              (frame (lispm-stickies--front-frame))
              ((frame-visible-p frame))
              ((not (eq frame (selected-frame)))))
    (select-frame-set-input-focus frame)))

(defun lispm-stickies--scroll-bar (buffer)
  "Give a note no scroll bar, as Stickies gave none.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (buffer-local-value 'lispm-stickies--id buffer) 'no))

;;;; Drawing

(defconst lispm-stickies--close-box
  '("......x" ".xxxx.x" ".x....x" ".x....x" ".x....x" "......x" "xxxxxxx")
  "The close box, a row of pixels a string: x is ink, the rest paper.")

(defconst lispm-stickies--zoom-box
  '("......x" "....x.x" "...x..x" "..x...x" ".x....x" "......x" "xxxxxxx")
  "The zoom box, a row of pixels a string: x is ink, the rest paper.")

(defconst lispm-stickies--grow-box
  '("....xxxx" "....x..x" "....x..x" "....x..x" "xxxxx..x" "x......x" "x......x" "xxxxxxxx")
  "The grow box over a note's bottom right corner, edges included.")

(defun lispm-stickies--draw-box (svg rows x y paper ink pressed)
  "Draw the box of ROWS on SVG at X, Y in PAPER and INK; PRESSED fills it
with the paper's edge colour instead."
  (lispm-desktop-platinum-svg-fill svg x y (length (car rows)) (length rows) (or pressed paper))
  (lispm-desktop-platinum-svg-pixel-map svg x y rows `((?x . ,ink))))

(defun lispm-stickies-draw-title-bar (color width height active &optional pressed label insets)
  "Return an SVG of a note's title bar, WIDTH by HEIGHT pixels, in COLOR.
Its top row is the note's top edge, and its ends lie over the note's side
edges.  ACTIVE draws it as the note in front: pinstripes of every other
pixel in the edge colour, as Stickies drew them, the close box on the
left and the zoom box on the right, PRESSED naming the box held down,
`close' or `zoom'.  INSETS, (LEFT . RIGHT) pixels, part the boxes from
the side edges, by default `lispm-stickies-box-inset' each.  An inactive
bar is plain paper, lit along its top.  LABEL, the note's first line, is
drawn in the middle of a bar taller than `lispm-stickies-title-height',
as a rolled-up note's."
  (let* ((svg (svg-create width height))
         (paper (lispm-stickies--color color 'paper))
         (edge (lispm-stickies--color color 'edge))
         (ink (lispm-stickies--color color 'ink))
         (rolled (> height lispm-stickies-title-height))
         (left (or (car insets) lispm-stickies-box-inset))
         (right (or (cdr insets) lispm-stickies-box-inset))
         (box-top (lispm-stickies--box-top height)))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height paper)
    (if (not active)
        (progn (lispm-desktop-platinum-svg-fill svg 0 0 (1- width) 1 "#FFFFFF")
               (lispm-desktop-platinum-svg-fill svg (1- width) 0 1 height edge))
      (lispm-desktop-platinum-svg-fill svg 0 0 width 1 edge)
      (let ((bottom (if rolled (- height 2) (1- height))))
        (cl-loop for y from 1 to bottom
                 do (cl-loop for x from (if (cl-oddp y) 1 2) below (1- width) by 2
                             do (lispm-desktop-platinum-svg-fill svg x y 1 1 edge))))
      (lispm-desktop-platinum-svg-fill svg (1- width) 0 1 height edge)
      (lispm-stickies--draw-box svg lispm-stickies--close-box left box-top paper ink
                                (and (eq pressed 'close) edge))
      (lispm-stickies--draw-box svg lispm-stickies--zoom-box (- width right 7) box-top paper ink
                                (and (eq pressed 'zoom) edge)))
    ;; The left edge goes on down the fringe, in the edge colour.
    (lispm-desktop-platinum-svg-fill svg 0 (if active 0 1) 1 height edge)
    (when rolled
      (lispm-desktop-platinum-svg-fill svg 0 (1- height) width 1 edge)
      (when label
        (let* ((room (- width 2 (* 2 (+ (max left right) 7 6))))
               (label (lispm-desktop-platinum-fit label room))
               (label-width (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-title))
               (x (/ (- width label-width) 2)))
          (when (> room 0)
            (when active
              (lispm-desktop-platinum-svg-fill svg (- x 4) 1 (+ label-width 8) (- height 2) paper))
            (lispm-desktop-platinum-svg-content svg label x 0 height (if active "#000000" "#555555")
                                                'lispm-desktop-platinum-title)))))
    svg))

(defun lispm-stickies-draw-bottom (color width active)
  "Return an SVG of a note's bottom, WIDTH pixels wide, in COLOR: paper
over its bottom edge, which runs under the side edges' ends.  ACTIVE
draws the grow box in the corner, over the edges, as Stickies drew it."
  (let* ((height lispm-stickies-bottom-height)
         (svg (svg-create width height))
         (edge (lispm-stickies--color color 'edge)))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height (lispm-stickies--color color 'paper))
    (lispm-desktop-platinum-svg-fill svg 0 0 1 height edge)
    (lispm-desktop-platinum-svg-fill svg (1- width) 0 1 height edge)
    (lispm-desktop-platinum-svg-fill svg 0 (1- height) width 1 edge)
    (when active
      (lispm-desktop-platinum-svg-pixel-map svg (- width 8) 0 lispm-stickies--grow-box
                                            `((?x . ,(lispm-stickies--color color 'ink)))))
    svg))

(defun lispm-stickies--window-width (window)
  "Return the width in pixels a note's title bar and bottom take in WINDOW."
  (- (window-pixel-width window) (window-right-divider-width window)
     (window-scroll-bar-width window)))

(defun lispm-stickies--slice (image x width &rest properties)
  "Return a string showing the columns of IMAGE from X, WIDTH pixels wide,
with PROPERTIES."
  (apply #'propertize " " 'display (list (list 'slice x 0 width 1.0) image) properties))

(defvar lispm-stickies--title-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'lispm-stickies--title-press)
    (keymap-set map "<tab-line> <mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <drag-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-mouse-1>" #'lispm-stickies--title-double-click)
    map)
  "Keymap of a note's title bar outside its boxes: a press moves the note
by its outline, a double click rolls it up or down.")

(defun lispm-stickies--title-press (event)
  "Move the note whose title bar EVENT pressed by its outline, as Mac OS 9
moved a window by its title bar."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-desktop-platinum-drag-outline (window-frame window) 'move))))

(defun lispm-stickies--title-double-click (event)
  "Roll the note whose title bar EVENT double-clicked up, or down."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-stickies-roll (window-buffer window)))))

(defun lispm-stickies--box-button (string target command help)
  "Return STRING as the title bar box TARGET, which runs COMMAND with the
note's buffer once released over it, as Mac OS 9's boxes did; HELP
describes it."
  (propertize string
              'help-echo help
              'lispm-desktop-platinum-target target
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>"
                                    (lambda (event)
                                      (interactive "e")
                                      (let ((start (event-start event)))
                                        (lispm-desktop-platinum-press
                                         start (lispm-desktop-platinum-same-target-p start)
                                         (lambda () (funcall command (window-buffer (posn-window start))))))))
                        (keymap-set map "<tab-line> <mouse-1>" #'ignore)
                        map)))

(defun lispm-stickies--box-top (height)
  "Return the row a title bar HEIGHT pixels tall stands its boxes on:
Stickies' second, or the middle of a rolled-up note's taller bar."
  (if (> height lispm-stickies-title-height) (/ (- height 7) 2) 2))

(defun lispm-stickies--box-insets (frame height)
  "Return how far in from the side edges of FRAME, a note's window, the
boxes of its title bar HEIGHT pixels tall stand: (LEFT . RIGHT) pixels.
They are `lispm-stickies-box-inset', or past each corner whose
rows they cross where macOS takes a press as resizing an undecorated
frame, see `lispm-desktop-platinum-resize-corners': the top ones always,
the bottom ones too on a note rolled up."
  (let ((top (lispm-stickies--box-top height))
        (frame-height (frame-pixel-height frame)))
    (cl-flet ((inset (side)
                (if (and (eq (framep frame) 'ns) (frame-parameter frame 'undecorated))
                    (seq-reduce (pcase-lambda (inset `(,corner ,end . ,square))
                                  (if (and (eq corner side)
                                           (if (eq end 'top)
                                               (< top square)
                                             (> (+ top 7) (- frame-height square))))
                                      (max inset square)
                                    inset))
                                lispm-desktop-platinum-resize-corners
                                lispm-stickies-box-inset)
                  lispm-stickies-box-inset)))
      (cons (inset 'left) (inset 'right)))))

(defun lispm-stickies--title-bar ()
  "The tab line of a note's window: its title bar, see
`lispm-stickies-draw-title-bar'."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (active (mode-line-window-selected-p))
         (width (lispm-stickies--window-width window))
         (rolled (lispm-desktop-app-window-rolled-p frame))
         (height (if rolled (lispm-stickies--rolled-height frame) lispm-stickies-title-height))
         (label (and rolled (lispm-stickies--title (buffer-string))))
         (pressed (and active (window-parameter window 'lispm-desktop-platinum-pressed)))
         (color lispm-stickies--note-color)
         (insets (lispm-stickies--box-insets frame height))
         (left (car insets))
         (right (cdr insets))
         (image (lispm-desktop-platinum-image
                 (list 'lispm-stickies-title color width height active pressed label insets)
                 (lambda () (lispm-stickies-draw-title-bar color width height active pressed label
                                                           insets)))))
    (if (or (not active) (< width (* 2 (+ left right 14))))
        (lispm-stickies--slice image 0 width 'keymap lispm-stickies--title-map)
      (let ((plain (lambda (x w) (lispm-stickies--slice image x w 'keymap lispm-stickies--title-map))))
        (concat (funcall plain 0 left)
                (lispm-stickies--box-button (lispm-stickies--slice image left 7) 'close
                                            #'lispm-stickies-close-note "Throw this note away")
                (funcall plain (+ left 7) (- width left right 14))
                (lispm-stickies--box-button (lispm-stickies--slice image (- width right 7) 7) 'zoom
                                            #'lispm-stickies-zoom "Size this note to its text, or back")
                (funcall plain (- width right) right))))))

(defvar lispm-stickies--grow-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <down-mouse-1>" #'lispm-stickies--grow-press)
    (keymap-set map "<mode-line> <mouse-1>" #'ignore)
    map)
  "Keymap of a note's grow box.")

(defun lispm-stickies--grow-press (event)
  "Resize the note whose grow box EVENT pressed by its outline, as Mac OS 9's
GrowWindow did."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-desktop-platinum-drag-outline (window-frame window) 'grow))))

(defun lispm-stickies--bottom ()
  "The mode line of a note's window: its bottom, see
`lispm-stickies-draw-bottom'."
  (let* ((window (selected-window))
         (active (mode-line-window-selected-p))
         (width (lispm-stickies--window-width window))
         (color lispm-stickies--note-color)
         (image (lispm-desktop-platinum-image
                 (list 'lispm-stickies-bottom color width active)
                 (lambda () (lispm-stickies-draw-bottom color width active)))))
    (if (or (not active) (< width 16))
        (lispm-stickies--slice image 0 width)
      (concat (lispm-stickies--slice image 0 (- width 8))
              (lispm-stickies--slice image (- width 8) 8 'keymap lispm-stickies--grow-map
                                     'help-echo "Drag to resize this note")))))

(defun lispm-stickies--paint ()
  "Colour the current note's buffer as its colour says: its paper, the
fringes as its side edges, and its title bar and bottom lines no taller
than what they show."
  (mapc #'face-remap-remove-relative (delq nil lispm-stickies--remaps))
  (let* ((color lispm-stickies--note-color)
         (paper (lispm-stickies--color color 'paper))
         (line (list :background paper :height 10 :box nil :overline nil :underline nil)))
    (setq lispm-stickies--remaps
          (list (and lispm-stickies-face (face-remap-add-relative 'default lispm-stickies-face))
                ;; Over the font, which some themes colour.
                (face-remap-add-relative 'default :background paper :foreground "#000000")
                (face-remap-add-relative 'hl-line :background paper)
                (face-remap-add-relative 'fringe :background (lispm-stickies--color color 'edge))
                (face-remap-add-relative 'region :foreground "#000000"
                                         :background (lispm-desktop-platinum-blend
                                                      paper (lispm-stickies--color color 'ink) 0.5))
                (face-remap-add-relative 'tab-line line)
                (face-remap-add-relative 'mode-line line)
                (face-remap-add-relative 'mode-line-active line)
                (face-remap-add-relative 'mode-line-inactive line))))
  (force-mode-line-update))

;;;; The mode

(defvar-keymap lispm-stickies-note-mode-map
  :doc "Keymap of a note."
  "C-c C-n" #'lispm-stickies-new-note
  "C-c C-k" #'lispm-stickies-close-note
  "C-c C-l" #'lispm-stickies-set-color
  "C-c C-r" #'lispm-stickies-roll
  "C-c C-z" #'lispm-stickies-zoom
  "C-c C-f" #'lispm-stickies-next-note
  "C-c C-b" #'lispm-stickies-previous-note
  "C-c C-d" #'lispm-stickies-note-info
  "C-c C-q" #'lispm-stickies-quit)

(defun lispm-stickies--changed (&rest _)
  "Note the time the text changed, and write the notes soon.
This is one of `after-change-functions' of a note."
  (setq lispm-stickies--modified (current-time))
  (when (lispm-desktop-app-window-rolled-p (selected-frame))
    (force-mode-line-update))
  (lispm-stickies--save-soon))

(defun lispm-stickies--may-kill ()
  "Return non-nil when this note may be thrown away: Stickies quits, the
note holds no text, or the alert asking says so.
This is one of `kill-buffer-query-functions' of a note."
  (or lispm-stickies--quitting
      (string-blank-p (buffer-string))
      (eq (lispm-desktop-popup-alert 'caution "Throw this note away?"
                                     '(("Cancel" keep cancel) ("Throw Away" throw default))
                                     "Its text will be lost.")
          'throw)))

(defun lispm-stickies--killed ()
  "Throw the note away when its buffer is killed, unless Stickies quits.
This is one of `kill-buffer-hook' of a note."
  (unless lispm-stickies--quitting
    (let ((buffer (current-buffer)))
      (setq lispm-stickies--notes (delq buffer lispm-stickies--notes))
      (when-let* ((frame (lispm-stickies--frame buffer)))
        (lispm-desktop-app-window-close (lispm-stickies--kind buffer))
        (when (frame-live-p frame)
          (delete-frame frame)))
      (lispm-stickies--forget buffer)
      (lispm-stickies--save-soon))))

(define-derived-mode lispm-stickies-note-mode text-mode "Note"
  "Major mode of a note of Stickies: text on coloured paper, written to
`lispm-stickies-file' as it is typed.

\\{lispm-stickies-note-mode-map}"
  (visual-line-mode 1)
  (let ((padding (propertize " " 'display `(space :width (,lispm-stickies-padding)))))
    (setq-local tab-line-format '(:eval (lispm-stickies--title-bar))
                mode-line-format '(:eval (lispm-stickies--bottom))
                header-line-format nil
                lispm-desktop-platinum-listed nil
                cursor-type 'bar
                cursor-in-non-selected-windows nil
                left-fringe-width 1
                right-fringe-width 1
                right-margin-width 1
                fringes-outside-margins t
                indicate-empty-lines nil
                indicate-buffer-boundaries nil
                fringe-indicator-alist (mapcar (lambda (entry) (list (car entry))) fringe-indicator-alist)
                line-prefix padding
                wrap-prefix padding
                show-trailing-whitespace nil))
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (add-hook 'after-change-functions #'lispm-stickies--changed nil t)
  (add-hook 'window-size-change-functions #'lispm-stickies--resized nil t)
  (add-hook 'kill-buffer-query-functions #'lispm-stickies--may-kill nil t)
  (add-hook 'kill-buffer-hook #'lispm-stickies--killed nil t))

;;;; Commands

(defun lispm-stickies--current ()
  "Return the note to act on: the current buffer when it is a note, else
the note in front."
  (cond (lispm-stickies--id (current-buffer))
        ((lispm-stickies--front-frame) (lispm-stickies--note (lispm-stickies--front-frame)))
        (t (user-error "No note is open"))))

;;;###autoload
(defun lispm-stickies ()
  "Open Stickies: its notes come back over the desktop where they were,
the note in front with the keyboard; a first note opens when there is
none."
  (interactive)
  (lispm-stickies--load)
  (if (null lispm-stickies--notes)
      (lispm-stickies-new-note)
    (let ((front (lispm-stickies--front-frame)))
      (dolist (buffer lispm-stickies--notes)
        (let ((frame (lispm-stickies--frame buffer)))
          (if frame
              (lispm-desktop-platinum-raise-window frame)
            (lispm-stickies--show buffer))))
      (let ((front (or front (lispm-stickies--front-frame))))
        (lispm-desktop-platinum-raise-window front)
        (select-frame-set-input-focus front)))
    (lispm-desktop-apps-set-frontmost 'stickies)))

(defun lispm-stickies-new-note (&optional text)
  "Open a new note, in front of the others; it holds TEXT, if given.
Return its buffer."
  (interactive)
  (lispm-stickies--load)
  (let* ((now (current-time))
         (buffer (lispm-stickies--make-note
                  (list :id (lispm-stickies--new-id) :color lispm-stickies-color
                        :text (or text "") :created now :modified now
                        :geometry (lispm-stickies--new-geometry (lispm-stickies--front-frame))))))
    (setq lispm-stickies--notes (append lispm-stickies--notes (list buffer)))
    ;; A note opened before Stickies, as from the keyboard, opens the rest.
    (unless (lispm-stickies--running-p)
      (dolist (other lispm-stickies--notes)
        (unless (eq other buffer)
          (lispm-stickies--show other))))
    (lispm-stickies--show buffer)
    (lispm-stickies--save-soon)
    buffer))

(defun lispm-stickies-close-note (&optional buffer)
  "Throw the note BUFFER away, by default this one, as Stickies' close box
did: asked first when it holds text."
  (interactive)
  (when (kill-buffer (or buffer (lispm-stickies--current)))
    (lispm-stickies-save)))

(defun lispm-stickies--read-color ()
  "Read a colour of `lispm-stickies-colors' by its name."
  (let* ((names (mapcar (lambda (color) (cons (nth 1 color) (car color))) lispm-stickies-colors))
         (name (completing-read "Color: " names nil t)))
    (alist-get name names nil nil #'equal)))

(defun lispm-stickies-set-color (color &optional buffer)
  "Colour the note BUFFER, by default this one, COLOR, a key of
`lispm-stickies-colors'."
  (interactive (list (lispm-stickies--read-color)))
  (with-current-buffer (or buffer (lispm-stickies--current))
    (setq lispm-stickies--note-color color)
    (lispm-stickies--paint))
  (lispm-stickies--save-soon))

(defun lispm-stickies-roll (&optional buffer)
  "Roll the note BUFFER, by default this one, up into its title bar, or
down again."
  (interactive)
  (let ((buffer (or buffer (lispm-stickies--current))))
    (when-let* ((frame (lispm-stickies--frame buffer)))
      (lispm-desktop-app-window-toggle-roll frame)
      (lispm-stickies--remember frame)
      (lispm-stickies--save-soon))))

(defun lispm-stickies--fit-size (frame)
  "Return the size FRAME's note takes to show its text unwrapped, as far as
the screen allows: (WIDTH . HEIGHT) of its text area."
  (let* ((window (frame-root-window frame))
         (screen (lispm-desktop-apps-window-screen frame))
         (buffer (window-buffer window))
         (lines (split-string (with-current-buffer buffer (buffer-string)) "\n"))
         (text (seq-max (cons 0 (mapcar (lambda (line) (string-pixel-width line buffer)) lines))))
         (chrome (- (frame-text-width frame) (window-body-width window t)))
         (width (min (max (+ text chrome lispm-stickies-padding (frame-char-width frame))
                          (car lispm-desktop-platinum-window-min-size))
                     (/ (frame-inner-width screen) 2))))
    (cons width
          (let ((frame-resize-pixelwise t))
            (set-frame-width frame width nil t)
            (redisplay t)
            (min (max (cdr (window-text-pixel-size window nil nil nil nil t))
                      (cdr lispm-desktop-platinum-window-min-size))
                 (/ (* 2 (frame-inner-height screen)) 3))))))

(defun lispm-stickies-zoom (&optional buffer)
  "Size the note BUFFER, by default this one, to its text, as far as the
screen allows, or back to the size it had, as Stickies' zoom box did."
  (interactive)
  (let ((buffer (or buffer (lispm-stickies--current))))
    (when-let* ((frame (lispm-stickies--frame buffer)))
      (lispm-desktop-app-window-roll-down frame)
      (let ((frame-resize-pixelwise t))
        (with-current-buffer buffer
          (if-let* ((size lispm-stickies--unzoomed))
              (progn (setq lispm-stickies--unzoomed nil)
                     (set-frame-size frame (car size) (cdr size) t))
            (setq lispm-stickies--unzoomed (cons (frame-text-width frame) (frame-text-height frame)))
            (pcase-let ((`(,width . ,height) (lispm-stickies--fit-size frame)))
              (set-frame-size frame width height t)))))
      (lispm-desktop-apps-keep-on-desktop frame)
      (lispm-stickies--remember frame)
      (lispm-stickies--save-soon))))

(defun lispm-stickies--select (step)
  "Bring the note STEP notes after this one in front, wrapping around."
  (let* ((frames (lispm-stickies--frames))
         (index (or (seq-position frames (selected-frame)) -1)))
    (when frames
      (let ((frame (nth (mod (+ index step) (length frames)) frames)))
        (lispm-desktop-platinum-raise-window frame)
        (select-frame-set-input-focus frame)))))

(defun lispm-stickies-next-note ()
  "Bring the next note in front, wrapping around."
  (interactive)
  (lispm-stickies--select 1))

(defun lispm-stickies-previous-note ()
  "Bring the previous note in front, wrapping around."
  (interactive)
  (lispm-stickies--select -1))

(defun lispm-stickies-note-info (&optional buffer)
  "Say when the note BUFFER, by default this one, was made and last changed."
  (interactive)
  (with-current-buffer (or buffer (lispm-stickies--current))
    (message "Created %s, modified %s, %d characters"
             (format-time-string "%e %B %Y %H:%M" lispm-stickies--created)
             (format-time-string "%e %B %Y %H:%M" lispm-stickies--modified)
             (buffer-size))))

(defun lispm-stickies-import-text (file)
  "Open a new note holding the text of FILE."
  (interactive "fImport text from: ")
  (lispm-stickies-new-note (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))

(defun lispm-stickies-export-text (file &optional buffer)
  "Write the text of the note BUFFER, by default this one, to FILE."
  (interactive "FExport text to: ")
  (with-current-buffer (or buffer (lispm-stickies--current))
    (write-region nil nil file)))

(defun lispm-stickies-quit ()
  "Quit Stickies: its notes are written and put away, to come back where
they were the next time it opens."
  (interactive)
  (when lispm-stickies--loaded
    (lispm-stickies-save)
    (let ((notes lispm-stickies--notes)
          (lispm-stickies--quitting t))
      ;; Written: the windows closing run timers, and a write pending
      ;; would find some notes gone.
      (setq lispm-stickies--notes nil
            lispm-stickies--loaded nil)
      (dolist (buffer notes)
        (when (buffer-live-p buffer)
          (lispm-desktop-app-window-close (lispm-stickies--kind buffer))
          (lispm-stickies--forget buffer)
          (kill-buffer buffer))))))

;;;; Menus

(defun lispm-stickies--key (command)
  "Return the key of COMMAND in a note, or of Emacs, as a menu shows it."
  (when-let* ((keys (where-is-internal command (list lispm-stickies-note-mode-map global-map) t)))
    (key-description keys)))

(defun lispm-stickies--item (label command &rest properties)
  "Return the menu item LABEL running COMMAND in the note in front, and
PROPERTIES.  The note comes forward first: a menu bar click focuses the
desktop under it."
  `(:label ,label
    :action ,(lambda ()
               (when-let* ((frame (lispm-stickies--front-frame)))
                 (select-frame-set-input-focus frame))
               (call-interactively command))
    ,@(when-let* ((key (lispm-stickies--key command)))
        (list :key key))
    ,@properties))

(defun lispm-stickies--file-items ()
  "Return the items of Stickies' File menu."
  (let ((open (and (lispm-stickies--front-frame) t)))
    (list (lispm-stickies--item "New Note" #'lispm-stickies-new-note)
          (lispm-stickies--item "Close Note" #'lispm-stickies-close-note :enabled open)
          'separator
          (lispm-stickies--item "Import Text…" #'lispm-stickies-import-text)
          (lispm-stickies--item "Export Text…" #'lispm-stickies-export-text :enabled open)
          'separator
          (lispm-stickies--item "Quit Stickies" #'lispm-stickies-quit))))

(defun lispm-stickies--edit-items ()
  "Return the items of Stickies' Edit menu."
  (let ((open (and (lispm-stickies--front-frame) t)))
    (list (lispm-stickies--item "Undo" #'undo :enabled open)
          (lispm-stickies--item "Redo" #'undo-redo :enabled open)
          'separator
          (lispm-stickies--item "Cut" #'kill-region :enabled open)
          (lispm-stickies--item "Copy" #'kill-ring-save :enabled open)
          (lispm-stickies--item "Paste" #'yank :enabled open)
          (lispm-stickies--item "Select All" #'mark-whole-buffer :enabled open))))

(defun lispm-stickies--note-items ()
  "Return the items of Stickies' Note menu."
  (let* ((frame (lispm-stickies--front-frame))
         (open (and frame t)))
    (list (lispm-stickies--item (if (and frame (lispm-desktop-app-window-rolled-p frame))
                                    "Roll Down" "Roll Up")
                                #'lispm-stickies-roll :enabled open)
          (lispm-stickies--item "Zoom" #'lispm-stickies-zoom :enabled open)
          'separator
          (lispm-stickies--item "Next Note" #'lispm-stickies-next-note :enabled open)
          (lispm-stickies--item "Previous Note" #'lispm-stickies-previous-note :enabled open)
          'separator
          (lispm-stickies--item "Note Info" #'lispm-stickies-note-info :enabled open))))

(defun lispm-stickies--color-items ()
  "Return the items of Stickies' Color menu: the colours, the note in
front's checked."
  (let* ((frame (lispm-stickies--front-frame))
         (current (and frame (buffer-local-value 'lispm-stickies--note-color
                                                 (lispm-stickies--note frame)))))
    (mapcar (pcase-lambda (`(,key ,name . ,_))
              (lispm-stickies--item name (lambda () (interactive) (lispm-stickies-set-color key))
                                    :checked (eq key current) :enabled (and frame t)))
            lispm-stickies-colors)))

(defun lispm-stickies--menus ()
  "Return Stickies' menus, for the menu bar while it is in front."
  '((lispm-stickies-file "File" lispm-stickies--file-items)
    (lispm-stickies-edit "Edit" lispm-stickies--edit-items)
    (lispm-stickies-note "Note" lispm-stickies--note-items)
    (lispm-stickies-color "Color" lispm-stickies--color-items)))

;;;; Taking part

(defun lispm-stickies--setup ()
  "Make Stickies one of the desktop's applications."
  (lispm-desktop-define-app 'stickies :name "Stickies" :icon lispm-stickies-icon
                            :menus #'lispm-stickies--menus :activate #'lispm-stickies
                            :running #'lispm-stickies--running-p)
  (add-hook 'kill-emacs-hook #'lispm-stickies-save)
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-stickies--moved)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-stickies--refocus)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-stickies--scroll-bar))

(defun lispm-stickies-unload-function ()
  "Quit Stickies and take it away from the desktop, as unloading it does.
Return nil, for the rest to be unloaded as usual."
  (lispm-stickies-quit)
  (lispm-desktop-remove-app 'stickies)
  (remove-hook 'kill-emacs-hook #'lispm-stickies-save)
  (remove-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-stickies--moved)
  (remove-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-stickies--refocus)
  (remove-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-stickies--scroll-bar)
  nil)

(lispm-stickies--setup)

(provide 'lispm-stickies)
;;; lispm-stickies.el ends here
