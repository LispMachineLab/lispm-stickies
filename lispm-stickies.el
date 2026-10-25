;;; lispm-stickies.el --- Stickies, Mac OS 9's notes  -*- lexical-binding: t; -*-

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

;; Stickies, Mac OS 9's notes: short notes on paper of one of Stickies'
;; seven colours, written as they are typed, kept for the next time.
;; In any Emacs a note is a buffer on its paper, shown in a window of the
;; selected frame.  A frontend may show the notes its own way through the
;; variables of "How notes are shown" below: LispM Desktop, whose App
;; Store installs Stickies, shows each note in a small window of its own
;; over the desktop, drawn as Stickies drew it.
;;
;; Opening Stickies brings its notes back.  A note is written as it is
;; typed.  Throwing a note away asks first when it holds text; quitting
;; Stickies only puts the notes away.
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
;; deeper, so it stays the note's own.  GEOMETRY and ROLLED are where a
;; frontend that shows notes in windows of their own last showed it, and
;; whether rolled up into its title bar; Stickies keeps them as they are.
;; Stickies writes the file while it runs: edit it while Stickies is
;; closed.
;;
;; Programs, such as an agent's skill, list the notes with
;; `lispm-stickies-notes', find one by its identity with
;; `lispm-stickies-find-note', make one with `lispm-stickies-new-note', change
;; it with `lispm-stickies-set-text' and `lispm-stickies-set-color', and
;; bring it in front with `lispm-stickies-show-note'.
;;
;; From the keyboard, in a note:
;;
;;   C-c C-n  a new note             C-c C-f  the next note
;;   C-c C-k  throw the note away    C-c C-b  the previous note
;;   C-c C-l  its colour             C-c C-d  when it was made and changed
;;   C-c C-q  quit Stickies

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'easymenu)
(require 'face-remap)

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
  "Stickies, Mac OS 9's notes."
  :group 'convenience
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

;;;; Colours

(defun lispm-stickies-color-value (key part)
  "Return PART of the colour KEY, a key of `lispm-stickies-colors': `name',
`paper', `edge' or `ink'.  A colour unknown is yellow."
  (let ((color (or (alist-get key lispm-stickies-colors)
                   (alist-get 'yellow lispm-stickies-colors))))
    (nth (pcase part ('name 0) ('paper 1) ('edge 2) ('ink 3)) color)))

(defun lispm-stickies--blend (from to fraction)
  "Return the colour FRACTION of the way from FROM to TO, both #RRGGBB."
  (apply #'format "#%02X%02X%02X"
         (cl-loop for start from 1 below 7 by 2
                  collect (let ((a (string-to-number (substring from start (+ start 2)) 16))
                                (b (string-to-number (substring to start (+ start 2)) 16)))
                            (round (+ a (* fraction (- b a))))))))

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
plists: :id, :color, :geometry, :rolled, :created, :modified and :text.
A note written by hand without an identity gets one made of its place
and text, the same each time the file is read."
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
          (let* ((body (lispm-stickies--unescape
                        (string-remove-suffix "\n" (buffer-substring-no-properties (min (point) end) end))))
                 (geometry (mapcar #'string-to-number
                                   (split-string (or (cdr (assoc "GEOMETRY" properties)) ""))))
                 (color (intern-soft (or (cdr (assoc "COLOR" properties)) ""))))
            (push (list :id (or (cdr (assoc "ID" properties))
                                (format "note-%s" (substring (md5 (format "%d\n%s" (length notes) body))
                                                             0 12)))
                        :color (if (assq color lispm-stickies-colors) color lispm-stickies-color)
                        :geometry (and (= (length geometry) 4) geometry)
                        :rolled (equal (cdr (assoc "ROLLED" properties)) "t")
                        :created (lispm-stickies--parse-timestamp (cdr (assoc "CREATED" properties)))
                        :modified (lispm-stickies--parse-timestamp (cdr (assoc "MODIFIED" properties)))
                        :text body)
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
  "Where a frontend last showed this note in a window of its own:
\(LEFT TOP WIDTH HEIGHT) in pixels, its text area's size, or nil.")

(defvar-local lispm-stickies--rolled nil
  "Non-nil while this note is rolled up into its title bar.")

(defvar-local lispm-stickies--remaps nil
  "The face remappings colouring this note.")

(defvar-local lispm-stickies--named nil
  "The title this note's buffer is named after.")

;;;; How notes are shown
;;
;; A frontend shows the notes its own way by setting these; the defaults
;; show each in a window of the selected frame.

(defvar lispm-stickies-show-note-function #'lispm-stickies-show-note-in-window
  "Function bringing a note in front, with the keyboard.
It is called with the note's buffer, and returns what shows it, a
window or a frame.  By default a window of the selected frame shows
it, see `lispm-stickies-show-note-in-window'.")

(defvar lispm-stickies-hide-note-function #'ignore
  "Function taking away what shows a note, called with its buffer when
Stickies puts the note away or throws it away, before the buffer is
killed.  By default nothing is done: killing the buffer takes it out of
its windows.")

(defvar lispm-stickies-shown-function #'lispm-stickies-shown-in-window-p
  "Function returning non-nil while the note of the buffer it is called
with is shown.")

(defvar lispm-stickies-front-note-function #'lispm-stickies-front-note-in-window
  "Function returning the buffer of the note in front, or nil when no
note is shown.")

(defvar lispm-stickies-confirm-function #'lispm-stickies-yes-or-no-p
  "Function asking whether to throw a note holding text away.
It is called with the question and a sentence saying what it means, and
returns non-nil to throw the note away.")

(defvar lispm-stickies-place-function #'ignore
  "Function returning where the note of the buffer it is called with is
shown now, (GEOMETRY . ROLLED) as `lispm-stickies-note-place' returns
it, or nil when the note has no place of its own.  Stickies asks it
before it writes the notes.")

(defvar lispm-stickies-changed-hook nil
  "Hook run in a note's buffer once its text or its colour changed.")

(defun lispm-stickies-show-note-in-window (buffer)
  "Show the note BUFFER in a window of the selected frame, selected: the
selected window when it shows a note, another one otherwise; return the
window.  This is `lispm-stickies-show-note-function' by default."
  (pop-to-buffer buffer (and (lispm-stickies-note-p (window-buffer))
                             '(display-buffer-same-window (inhibit-same-window . nil))))
  (selected-window))

(defun lispm-stickies-shown-in-window-p (buffer)
  "Return a window showing the note BUFFER, or nil.
This is `lispm-stickies-shown-function' by default."
  (get-buffer-window buffer 'visible))

(defun lispm-stickies-front-note-in-window ()
  "Return the note shown that was selected last, or nil.
This is `lispm-stickies-front-note-function' by default."
  (seq-find (lambda (buffer) (and (memq buffer lispm-stickies--notes)
                                  (funcall lispm-stickies-shown-function buffer)))
            (buffer-list)))

(defun lispm-stickies-yes-or-no-p (question detail)
  "Ask QUESTION, DETAIL after it, with `yes-or-no-p'.
This is `lispm-stickies-confirm-function' by default."
  (yes-or-no-p (format "%s %s " question detail)))

;;;; A note's buffer

(defun lispm-stickies--new-id ()
  "Return a new note's identity."
  (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random #x10000)))

(defun lispm-stickies--name ()
  "Name the current note's buffer after its title, when that changed."
  (let ((title (lispm-stickies--title (buffer-string))))
    (unless (equal title lispm-stickies--named)
      (setq lispm-stickies--named title)
      (rename-buffer (format "*Note: %s*" title) t))))

(defun lispm-stickies--make-note (note)
  "Return the buffer of NOTE, a plist as `lispm-stickies--parse' returns."
  (let ((buffer (generate-new-buffer "*Note*")))
    (with-current-buffer buffer
      (let ((buffer-undo-list t))
        (insert (plist-get note :text)))
      (goto-char (point-min))
      (lispm-stickies-note-mode)
      (setq lispm-stickies--id (plist-get note :id)
            lispm-stickies--note-color (plist-get note :color)
            lispm-stickies--created (or (plist-get note :created) (current-time))
            lispm-stickies--modified (or (plist-get note :modified) lispm-stickies--created)
            lispm-stickies--geometry (plist-get note :geometry)
            lispm-stickies--rolled (plist-get note :rolled))
      (lispm-stickies--name)
      (lispm-stickies--paint)
      (set-buffer-modified-p nil))
    buffer))

(defun lispm-stickies--plist (buffer)
  "Return the note BUFFER as a plist, as `lispm-stickies--parse' returns."
  (pcase (funcall lispm-stickies-place-function buffer)
    (`(,geometry . ,rolled)
     (with-current-buffer buffer
       (setq lispm-stickies--geometry geometry
             lispm-stickies--rolled rolled))))
  (with-current-buffer buffer
    (list :id lispm-stickies--id :color lispm-stickies--note-color
          :geometry lispm-stickies--geometry :rolled lispm-stickies--rolled
          :created lispm-stickies--created :modified lispm-stickies--modified
          :text (buffer-substring-no-properties (point-min) (point-max)))))

(defun lispm-stickies-note-p (buffer)
  "Return non-nil when BUFFER is a live note's."
  (and (buffer-live-p buffer) (buffer-local-value 'lispm-stickies--id buffer) t))

(defun lispm-stickies-note-buffers (&optional read)
  "Return the notes' buffers, the oldest first: none while Stickies is
closed, unless READ is non-nil, which reads the notes first."
  (when read
    (lispm-stickies--load))
  (seq-filter #'buffer-live-p lispm-stickies--notes))

(defun lispm-stickies-note-id (buffer)
  "Return the identity of the note BUFFER, a string."
  (buffer-local-value 'lispm-stickies--id buffer))

(defun lispm-stickies-note-color (buffer)
  "Return the colour of the note BUFFER, a key of `lispm-stickies-colors'."
  (buffer-local-value 'lispm-stickies--note-color buffer))

(defun lispm-stickies-note-title (buffer)
  "Return the title of the note BUFFER: its first line that is not blank,
shortened, or \"Note\"."
  (with-current-buffer buffer
    (lispm-stickies--title (buffer-string))))

(defun lispm-stickies-note-place (buffer)
  "Return where a frontend last showed the note BUFFER, (GEOMETRY . ROLLED):
GEOMETRY (LEFT TOP WIDTH HEIGHT) in pixels, or nil, and ROLLED non-nil
when it was rolled up into its title bar."
  (with-current-buffer buffer
    (cons lispm-stickies--geometry lispm-stickies--rolled)))

(defun lispm-stickies-set-note-place (buffer geometry rolled)
  "Remember that the note BUFFER is shown at GEOMETRY, ROLLED up or not,
as `lispm-stickies-note-place' returns them; it is written soon after."
  (with-current-buffer buffer
    (setq lispm-stickies--geometry geometry
          lispm-stickies--rolled (and rolled t)))
  (lispm-stickies-save-soon))

(defun lispm-stickies-running-p ()
  "Return non-nil while a note is shown."
  (and (seq-some lispm-stickies-shown-function (seq-filter #'buffer-live-p lispm-stickies--notes))
       t))

;;;; Reading and writing

(defvar lispm-stickies--save-timer nil
  "Pending timer writing the notes, if any.")

(defun lispm-stickies--read-file ()
  "Return the notes `lispm-stickies-file' holds, plists as
`lispm-stickies--parse' returns them."
  (when (file-readable-p lispm-stickies-file)
    (lispm-stickies--parse
     (with-temp-buffer
       (let ((coding-system-for-read 'utf-8))
         (insert-file-contents lispm-stickies-file))
       (buffer-string)))))

(defun lispm-stickies--load ()
  "Read the notes from `lispm-stickies-file', unless they are read."
  (unless lispm-stickies--loaded
    (setq lispm-stickies--notes (mapcar #'lispm-stickies--make-note (lispm-stickies--read-file))
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

(defun lispm-stickies-save-soon ()
  "Write the notes once Emacs is idle for a second."
  (unless (timerp lispm-stickies--save-timer)
    (setq lispm-stickies--save-timer (run-with-idle-timer 1 nil #'lispm-stickies-save))))

;;;; Paper

(defun lispm-stickies--paint ()
  "Colour the current note's buffer as its colour says: its paper, and its
fringes its edges."
  (mapc #'face-remap-remove-relative (delq nil lispm-stickies--remaps))
  (let* ((color lispm-stickies--note-color)
         (paper (lispm-stickies-color-value color 'paper)))
    (setq lispm-stickies--remaps
          (list (and lispm-stickies-face (face-remap-add-relative 'default lispm-stickies-face))
                ;; Over the font, which some themes colour.
                (face-remap-add-relative 'default :background paper :foreground "#000000")
                (face-remap-add-relative 'hl-line :background paper)
                (face-remap-add-relative 'fringe :background (lispm-stickies-color-value color 'edge))
                (face-remap-add-relative 'region :foreground "#000000"
                                         :background (lispm-stickies--blend
                                                      paper (lispm-stickies-color-value color 'ink) 0.5)))))
  (force-mode-line-update))

;;;; The mode

(defvar-keymap lispm-stickies-note-mode-map
  :doc "Keymap of a note."
  "C-c C-n" #'lispm-stickies-new-note
  "C-c C-k" #'lispm-stickies-close-note
  "C-c C-l" #'lispm-stickies-set-color
  "C-c C-f" #'lispm-stickies-next-note
  "C-c C-b" #'lispm-stickies-previous-note
  "C-c C-d" #'lispm-stickies-note-info
  "C-c C-q" #'lispm-stickies-quit)

(easy-menu-define lispm-stickies-note-mode-menu lispm-stickies-note-mode-map
  "Stickies' menu."
  `("Stickies"
    ["New Note" lispm-stickies-new-note]
    ["Close Note" lispm-stickies-close-note]
    "---"
    ["Next Note" lispm-stickies-next-note]
    ["Previous Note" lispm-stickies-previous-note]
    ["Note Info" lispm-stickies-note-info]
    ("Color"
     ,@(mapcar (pcase-lambda (`(,key ,name . ,_))
                 (vector name `(lispm-stickies-set-color ',key)
                         :style 'radio :selected `(eq lispm-stickies--note-color ',key)))
               lispm-stickies-colors))
    "---"
    ["Import Text…" lispm-stickies-import-text]
    ["Export Text…" lispm-stickies-export-text]
    "---"
    ["Quit Stickies" lispm-stickies-quit]))

(defun lispm-stickies--changed (&rest _)
  "Note the time the text changed, and write the notes soon.
This is one of `after-change-functions' of a note."
  (setq lispm-stickies--modified (current-time))
  (lispm-stickies--name)
  (run-hooks 'lispm-stickies-changed-hook)
  (lispm-stickies-save-soon))

(defun lispm-stickies--may-kill ()
  "Return non-nil when this note may be thrown away: Stickies quits, the
note holds no text, or `lispm-stickies-confirm-function' says so.
This is one of `kill-buffer-query-functions' of a note."
  (or lispm-stickies--quitting
      (string-blank-p (buffer-string))
      (funcall lispm-stickies-confirm-function "Throw this note away?" "Its text will be lost.")))

(defun lispm-stickies--killed ()
  "Throw the note away when its buffer is killed, unless Stickies quits.
This is one of `kill-buffer-hook' of a note."
  (unless lispm-stickies--quitting
    (let ((buffer (current-buffer)))
      (setq lispm-stickies--notes (delq buffer lispm-stickies--notes))
      (funcall lispm-stickies-hide-note-function buffer)
      (lispm-stickies-save-soon))))

(define-derived-mode lispm-stickies-note-mode text-mode "Note"
  "Major mode of a note of Stickies: text on coloured paper, written to
`lispm-stickies-file' as it is typed.

\\{lispm-stickies-note-mode-map}"
  (visual-line-mode 1)
  (setq-local show-trailing-whitespace nil
              indicate-empty-lines nil)
  (add-hook 'after-change-functions #'lispm-stickies--changed nil t)
  (add-hook 'kill-buffer-query-functions #'lispm-stickies--may-kill nil t)
  (add-hook 'kill-buffer-hook #'lispm-stickies--killed nil t))

;;;; Commands

(defun lispm-stickies--current ()
  "Return the note to act on: the current buffer when it is a note, else
the note in front."
  (cond (lispm-stickies--id (current-buffer))
        ((funcall lispm-stickies-front-note-function))
        (t (user-error "No note is open"))))

;;;###autoload
(defun lispm-stickies ()
  "Open Stickies: its notes come back, the note in front with the
keyboard; a first note opens when there is none."
  (interactive)
  (lispm-stickies--load)
  (if-let* ((notes (seq-filter #'buffer-live-p lispm-stickies--notes)))
      (lispm-stickies-show-note (or (funcall lispm-stickies-front-note-function) (car (last notes))))
    (lispm-stickies-new-note)))

(defun lispm-stickies-new-note (&optional text color)
  "Open a new note, in front of the others; it holds TEXT, if given, on
paper of COLOR, a key of `lispm-stickies-colors', by default
`lispm-stickies-color'.  Return its buffer."
  (interactive)
  (when (and color (not (assq color lispm-stickies-colors)))
    (error "No colour %s; Stickies has %s" color
           (mapconcat (lambda (entry) (symbol-name (car entry))) lispm-stickies-colors ", ")))
  (lispm-stickies--load)
  (let* ((now (current-time))
         (buffer (lispm-stickies--make-note
                  (list :id (lispm-stickies--new-id) :color (or color lispm-stickies-color)
                        :text (or text "") :created now :modified now))))
    (setq lispm-stickies--notes (append lispm-stickies--notes (list buffer)))
    (lispm-stickies-show-note buffer)
    (lispm-stickies-save-soon)
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
    (lispm-stickies--paint)
    (run-hooks 'lispm-stickies-changed-hook))
  (lispm-stickies-save-soon))

(defun lispm-stickies--select (step)
  "Bring the note STEP notes after this one in front, wrapping around."
  (let* ((notes (seq-filter #'buffer-live-p lispm-stickies--notes))
         (current (if lispm-stickies--id (current-buffer) (funcall lispm-stickies-front-note-function)))
         (index (or (seq-position notes current) -1)))
    (when notes
      (lispm-stickies-show-note (nth (mod (+ index step) (length notes)) notes)))))

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
      ;; Written: what shows the notes going away may run timers, and a
      ;; write pending would find some notes gone.
      (setq lispm-stickies--notes nil
            lispm-stickies--loaded nil)
      (dolist (buffer notes)
        (when (buffer-live-p buffer)
          (funcall lispm-stickies-hide-note-function buffer)
          (kill-buffer buffer))))))

;;;; For programs

(defun lispm-stickies-notes ()
  "Return the notes, the oldest first, as plists: :id, a string; :color,
a key of `lispm-stickies-colors'; :text; :created and :modified, times;
:geometry and :rolled, where its window was and whether it was rolled
up.  They are Stickies' own while it has them read, else
`lispm-stickies-file' is read, and nothing changes."
  (if lispm-stickies--loaded
      (mapcar #'lispm-stickies--plist (seq-filter #'buffer-live-p lispm-stickies--notes))
    (lispm-stickies--read-file)))

(defun lispm-stickies-find-note (id)
  "Return the buffer of the note whose identity is ID, a string, or nil
when there is none.  The notes are read first, when there is one."
  (when (or lispm-stickies--loaded
            (seq-find (lambda (note) (equal (plist-get note :id) id)) (lispm-stickies--read-file)))
    (lispm-stickies--load)
    (seq-find (lambda (buffer) (equal (buffer-local-value 'lispm-stickies--id buffer) id))
              (seq-filter #'buffer-live-p lispm-stickies--notes))))

(defun lispm-stickies-show-note (buffer)
  "Bring the note BUFFER in front, with the keyboard, as
`lispm-stickies-show-note-function' shows notes; return what shows it."
  (funcall lispm-stickies-show-note-function buffer))

(defun lispm-stickies-set-text (buffer text)
  "Make TEXT the text of the note BUFFER, which is written soon after."
  (with-current-buffer buffer
    (unless (equal text (buffer-string))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))))

;;;; Loading and unloading

(add-hook 'kill-emacs-hook #'lispm-stickies-save)

(defun lispm-stickies-unload-function ()
  "Quit Stickies, as unloading it does; return nil, for the rest to be
unloaded as usual."
  (lispm-stickies-quit)
  (remove-hook 'kill-emacs-hook #'lispm-stickies-save)
  nil)

(provide 'lispm-stickies)
;;; lispm-stickies.el ends here
