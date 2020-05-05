;;; projectiny.el --- Simple project bookmarking for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Arthur Colombini Gusmao

;; Author: Arthur Colombini Gusmao
;; Maintainer: Arthur Colombini Gusmao

;; URL: https://github.com/arthurcgusmao/projectiny
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides very basic functionalities for bookmarking and
;; searching projects. Its goals are minimalism and use of Emacs' built-in
;; libraries such as `project.el`.

;;; Code:

(require 'project)


(defcustom projectiny-bookmarks-file
  (expand-file-name "projectiny-bookmarks"
                    user-emacs-directory)
  "File that references bookmarked projects.")


(defun projectiny-add-bookmark ()
  "Add a project (with completion) to the bookmarks file.

The completion defaults to the root of the current project, which
uses `project-current' to provide a smart suggestion."
  (interactive)
  (let* ((default-dir (cdr (project-current nil)))
         (proj-dir
          (expand-file-name
           (read-directory-name "Choose the project directory: "
                                default-dir nil t)))
         (known-projects (projectiny--read-known-projects)))
    ;; Add new project to list
    (add-to-list 'known-projects proj-dir)
    ;; Write modified list to file
    (with-temp-buffer
      (insert (mapconcat 'identity known-projects "\n"))
      (write-file projectiny-bookmarks-file))))

(defun projectiny-edit-bookmarks ()
  "Open the `projectiny-bookmarks-file' for editing.

Each line of the file should contain a directory path that
correspond to root directory of a project the user want to
bookmark."
  (interactive)
  (find-file projectiny-bookmarks-file))

(defun projectiny--read-known-projects ()
  "Read the list of known projects from
`projectiny-bookmarks-file'."
  (when (file-exists-p projectiny-bookmarks-file)
    (with-temp-buffer
      (insert-file-contents projectiny-bookmarks-file)
      (split-string (buffer-string) "\n" t))))

(defun projectiny--choose-project ()
  "Prompt the user to choose a project from the known list, and
return its root directory path. Shown paths are abbreviated to
increase readability."
  (let ((default-directory ""))
    (expand-file-name
     (completing-read
      "Choose project: "
      (mapcar #'abbreviate-file-name
              (projectiny--read-known-projects))))))

(defun projectiny--project-get-instance (dir)
  "Return the project instance in DIR. If that directory is not a
part of a detectable project, return a `transient' project
instance rooted in it."
  (or (project--find-in-directory dir)
      (progn
        (message "Using `%s' as a transient project root" dir)
        (setq pr (cons 'transient dir)))))

(defun projectiny-find-file-in ()
  "Visit a file (with completion) in a project's roots.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((default-directory (projectiny--choose-project)) ; Set default-directory to make further use of this variable meaningful
         (pr (projectiny--project-get-instance default-directory))
         (dirs (project-roots pr)))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

(defun projectiny-find-file-all ()
  "Visit a file (with completion) in all known projects.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((proj-instances
          (mapcar #'projectiny--project-get-instance
                  (projectiny--read-known-projects)))
         (all-files
          (mapcan #'project-files proj-instances)))
    (find-file (completing-read "Find file: " all-files))))


(provide 'projectiny)

;;; projectiny.el ends here
