;;; pdf-tools-points.el --- Offline annotation with pdf-tools and tikz

;; Copyright (C) 2016-2017 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/pdf-tools-points.git
;; Keywords:

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

;; Open a pdf file with pdf-tools and enable the minor mode
;; pdf-tools-points. Click at the locations you want and call
;; `pdf-tools-points-visit'.

;;; Code:

(require 'pdf-tools)
(require 'dash)

(defvar-local pdf-tools-points-points-list nil
  "List of recorded points.

It is a list of elements of the form (PAGE X Y) where PAGE is the
page number of the point and X and Y its coordinates in that
page.")

(defun pdf-tools-points-register-current-point (event)
  "Add current click to the list of points."
  (interactive "e")
  (unless (and (eventp event)
               (mouse-event-p event))
    (signal 'wrong-type-argument (list 'mouse-event-p event)))
  (pdf-util-assert-pdf-window)
  (let* ((page (pdf-view-current-page (selected-window)))
         (posn (event-start event))
         (isize (pdf-view-image-size))
         (pos (posn-object-x-y posn))
         (x (car pos))
         (y (cdr pos))
         (xx (/ x (float (car isize))))
         (yy (/ y (float (cdr isize)))))
    (setq pdf-tools-points-points-list (cons (list page xx yy) pdf-tools-points-points-list))
    (minibuffer-message "Added point (%f, %f) on page %d, total of %d points"
                        xx yy page (length pdf-tools-points-points-list))))

(defvar pdf-tools-points-template "\
\\documentclass[a4paper]{scrartcl}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[francais]{babel}
\\usepackage{pifont}
\\usepackage{grffile}   %% Dots in pdf filename
\\usepackage{pdfpages}
\\usepackage{etoolbox}
\\usepackage{tikz}
\\usetikzlibrary{fit}
\\usetikzlibrary{calc}

\\let\\oldding\\ding%% Store old \\ding in \\oldding
\\renewcommand{\\ding}[2][1]{\\scalebox{#1}{\\oldding{#2}}}%% Scale \\oldding via optional argument

\\newif\\ifpoints
\\pointstrue

\\newcommand\\tickati[2][1]{%%
  \\ifnum\\therealpage=\\csname #2\\endcsname
  \\node[inner sep=0pt] at (#2) {\\ding[#1]{51}};
  \\fi
}

\\newcommand\\tickat[2][1]{%%
  \\forcsvlist{\\tickati[#1]}{#2}
}

\\newcommand\\crossati[2][1]{%%
  \\ifnum\\therealpage=\\csname #2\\endcsname
  \\node[inner sep=0pt] at (#2) {\\ding[#1]{55}};
  \\fi
}

\\newcommand\\crossat[2][1]{%%
  \\forcsvlist{\\crossati[#1]}{#2}
}

\\newcommand\\picat[3][4cm]{%%
  \\ifnum\\therealpage=\\csname #2\\endcsname
  \\node[inner sep=0pt] at (#2) {\\includegraphics[width=#1]{#3}};
  \\fi
}

\\newcommand\\textbetween[3]{%%
  \\ifnum\\therealpage=\\csname #2\\endcsname
  \\path let \\p1=($(#2)-(#1)$), \\n1 = {veclen(\\x1,0)}, \\n2 = {veclen(0,\\y1)} in
  node[fit={(#1) (#2)}, inner sep=0pt] (rect) {}
  node[inner sep=0pt] at (rect) {%%
    \\begin{minipage}[h][\\n2][t]{\\n1}
        #3
      \\end{minipage}};
  \\fi
}

\\newcommand\\textat[2]{%%
  \\ifnum\\therealpage=\\csname #1\\endcsname
  \\node[inner sep=0pt, anchor=base west] at (#1) {#2};
  \\fi
}

\\tikzset{page cs/.cd, x/.store in=\\px, y/.store in=\\py}
\\tikzdeclarecoordinatesystem{page}{%%
  \\tikzset{page cs/.cd, #1}%%
  \\pgfpointadd{\\pgfpointanchor{current page}{south west}}%%
  {\\pgfpoint{\\px*\\paperwidth}{\\paperheight-\\py*\\paperheight}}%%
}

%% For some reason, \\makeatletter does not work, \\csname does
\\newcommand\\therealpage{
  \\csname AM@page\\endcsname
}

%s

\\begin{document}

\\newcommand\\mycommand{
  \\thispagestyle{empty}
%s
}
\\includepdf[pages={-}, pagecommand=\\mycommand]{\\detokenize{%s}}

\\end{document}
"
  "Template to create the LaTeX file.

There is 3 placeholder: the first to link points to the page they
belong to, the second to create tikz coordinates et nodes and the
last is the filename of the pdf file." )

(defvar pdf-tools-points-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'pdf-tools-points-register-current-point)
    map))

(define-minor-mode pdf-tools-points-minor-mode
  ""
  nil nil nil
  (setq pdf-tools-points-points-list nil)
  :group 'pdf-tools-points)

(defun pdf-tools-points-visit (&optional filename)
  (interactive)
  (unless filename
    (setq filename (concat (file-name-sans-extension (pdf-view-buffer-file-name)) "_tikz.tex")))
  (let ((body (pdf-tools-points-to-tikz)))
    (with-current-buffer (find-file filename)
      (insert body))))

(defun pdf-tools-points-to-tikz ()
  (let* ((points (-map-indexed (lambda (idx e)
                                 (cons idx e))
                               (sort pdf-tools-points-points-list
                                     (lambda (e f)
                                       (if (equal (car e) (car f))
                                           (if (< (abs (- (nth 2 e) (nth 2 f))) 10)
                                               (< (nth 1 e) (nth 1 f))
                                             (< (nth 2 e) (nth 2 f)))
                                         (< (car e) (car f)))))))
         (pageref (apply 'concat (mapcar (lambda (e)
                                           (format "\\def\\%s{%d}"
                                                   (pdf-tools-decimal-to-base26 (car e))
                                                   (nth 1 e)))
                                         points)))
         (groups (--group-by (nth 1 it) points))
         (tikz-begin "\\begin{tikzpicture}[overlay, remember picture]")
         (tikz-end "\\end{tikzpicture}")
         (fis (apply 'concat (make-list (length groups) "\\fi")))
         (command
          (mapconcat
           'identity
           (-map-indexed
            (lambda (idx group)
              (let* ((page (car group))
                     (ifpage (if (= idx 0)
                                 (format "\\ifnum\\therealpage=%d" page)
                               (format "\\else\\ifnum\\therealpage=%d" page)))
                     (coords (mapconcat (lambda (e)
                                          (format "\\coordinate (%s) at (page cs:x=%f, y=%f);"
                                                  (pdf-tools-decimal-to-base26 (car e))
                                                  (nth 2 e)
                                                  (nth 3 e)))
                                        (cdr group)
                                        "\n"))
                     (labels (mapconcat (lambda (e)
                                          (prog1 (let ((b26 (pdf-tools-decimal-to-base26 (car e))))
                                                   (format "\\draw[shift=(%s)] plot[mark=x] coordinates{(0,0)} node[right] {\\texttt{%s}};"
                                                           b26 b26))))
                                        (cdr group)
                                        "\n")))
                (concat ifpage "\n" coords "\n\\ifpoints\n" labels "\n\\fi")))
            groups)
           "\n")))
    (format pdf-tools-points-template
            pageref
            (concat
             "  " tikz-begin "\n"
             (org-ascii--indent-string command 4)
             "\n"
             "    " fis "\n\n\n\n"
             "  " tikz-end "\n")
            (pdf-view-buffer-file-name))))

;; Taken from org
(defun org-ascii--indent-string (s width)
  "Indent string S by WIDTH white spaces.
Empty lines are not indented."
  (when (stringp s)
    (replace-regexp-in-string
     "\\(^\\)[ \t]*\\S-" (make-string width ?\s) s nil nil 1)))

;;; Adapted from http://git.informatimago.com/viewgit/index.php?a=viewblob&p=public/emacs&h=94650b080e8a38e9620687b9abd3fcb3e24c6561&hb=e57067965e5def38d5fa18dab0aa75cff3d049b9&f=pjb-utilities.el
(defun pdf-tools-decimal-to-base26 (number)
  "Convert number NUMBER to a letter encoded string like so:
A .. Z AA .. ZZ AAA .. ZZZ .."
  (let* ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (buffer (make-string 32 ?A))
         (n (floor (log (1+ (/ (* 25 number) 26)) 26)))
         (number (- number (* 26 (/ (1- (expt 26 n)) 25))))
         (b 31))
    (while (< 0 number)
      (let ((digit (% number 26)))
        (aset buffer b (aref letters digit))
        (setq number (/ (- number digit) 26)
              b (- b 1))))
    (substring buffer (- 31 n))))

(provide 'pdf-tools-points)

;;; pdf-tools-points.el ends here
