;;;  -*- lexical-binding: t -*-
(require 'dired)
(require 'image-utility)

(defvar dired-externally-file-ext-regex
  (rx bos
      (or (and "do" (or ?c ?t) (? ?x))
          (and "ppt" (? ?x))
          "pdf"
          "mp4"
          "dwg"
          "dxf"
          "DXF"
          (and "xls" (? ?x)))
      eos))

(defvar-keymap dired-space-map
  "p" #'dired-convert-image-to-pdf
  "i" #'dired-convert-pdf-to-image
  "m" #'dired-merge-pdf-files)

(define-keymap
  :keymap dired-mode-map
  ;; 折叠子目录
  "TAB" #'dired-hide-subdir
  "C-k" #'dired-kill-subdir
  "M-p" #'dired-prev-subdir
  "M-n" #'dired-next-subdir
  ;; `f' 进入目录或文件
  ;; `b' 返回上级目录
  "b" #'dired-up-directory
  "e" #'dired-find-file-externally
  "E" #'dired-toggle-read-only
  "/ u" #'dired-upcase
  "/ l" #'dired-downcase
  "/ d" #'dired-flag-files-regexp
  "/ g" #'dired-mark-files-containing-regexp
  "/ m" #'dired-mark-files-regexp
  "/ r" #'dired-do-rename-regexp
  "/ C" #'dired-do-copy-regexp
  "/ H" #'dired-do-hardlink-regexp
  "/ R" #'dired-do-rename-regexp
  "/ S" #'dired-do-symlink-regexp
  "/ Y" #'dired-do-relsymlink-regexp
  "/ &" #'dired-flag-garbage-files
  "SPC" dired-space-map)

(defun dired-find-file-externally (&optional arg)
  "Open marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (let ((file (dired-get-file-for-visit)))
     (if (or (file-directory-p file)
             (string-match-p dired-externally-file-ext-regex
                             (file-name-extension file)))
         (find-file-externally file)))
   arg))

(defun dired-merge-pdf-files (name)
  "将 `image' 文件及 `pdf' 合并为一个 `pdf' 文件"
  (interactive "sOutput file name: ")
  (let ((files (dired-get-marked-files))
        (default-directory (dired-current-directory)))
    (if (length< files 2)
        (user-error "Less files to merge"))
    (apply #'image-utility--merge-files name
           (mapcar (lambda (file)
                     (pcase (file-name-extension file)
                       ((or "png" "pdf") file)
                       (_ (image-utility--convert file))))
                   files))))

(defun dired-convert-image-to-pdf (&optional arg)
  "将 `image' 文件转化为 pdf 文件"
  (interactive "P")
  (let ((default-directory (dired-current-directory)))
    (dired-map-over-marks
     (image-utility--convert (dired-get-filename) "pdf")
     arg)))

(defun dired-convert-pdf-to-image (&optional arg)
  "将 `pdf' 文件转化为 `image' 文件"
  (interactive "P")
  (let ((default-directory (dired-current-directory)))
    (dired-map-over-marks
     (image-utility--convert (dired-get-filename) "png")
     arg)))

(provide 'dired+)
