;;; projectiny.el --- Basic project management for Emacs -*- lexical-binding: t -*-

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

;; This package provides basic functionalities for creating a list of known
;; projects and quickly opening files in them. Its goals are minimalism,
;; modularity, and the use of Emacs' built-in libraries (`project.el`).

;;; Code:

(require 'project)


(defcustom projectiny-known-projects-file
  (expand-file-name "projectiny-known-projects"
                    user-emacs-directory)
  "File that references known projects.")


(defun projectiny-add-project ()
  "Add a project (with completion) to the known projects file.

The completion provides a smart suggestion that defaults to the
root of the project where the user is (when applicable), by
leveraging on `project-current'."
  (interactive)
  (let* ((default-directory (cdr (project-current nil)))
         ;; Here I found that not using `default-directory' in the let function
         ;; makes its value be incorrectly modified on the buffer where the
         ;; user issued the command, due to how `read-directory-name' works.
         (proj-dir
          (expand-file-name
           (read-directory-name "Choose the project directory: "
                                default-directory nil t)))
         (known-projects (projectiny--read-known-projects)))
    ;; Add new project to list
    (add-to-list 'known-projects proj-dir)
    ;; Write modified list to file
    (with-temp-buffer
      (insert (mapconcat 'identity known-projects "\n"))
      (write-file projectiny-known-projects-file))
    ;; Find file in newly added project. (Uses the value of default-directory,
    ;; so no need to pass the project root as a parameter)
    (let ((default-directory proj-dir))
      (projectiny-find-file))))

(defun projectiny-edit-known-projects ()
  "Open `projectiny-known-projects-file' for editing.

Each line of the file should contain a directory path that
correspond to the root directory of a project the user wants
projectiny to remember."
  (interactive)
  (find-file projectiny-known-projects-file))

(defun projectiny-clean-known-projects ()
  "Remove unexisting paths from `projectiny-known-projects-file'."
  (interactive)
  (let ((known-projects (list)))
    ;; Loop over known projects and store existing ones in list
    (dolist (proj-dir (projectiny--read-known-projects))
      (when (file-directory-p proj-dir)
        (add-to-list 'known-projects proj-dir)))
    ;; Write cleaned list to file
    (with-temp-buffer
      (insert (mapconcat 'identity known-projects "\n"))
      (write-file projectiny-known-projects-file))))


(defun projectiny--read-known-projects ()
  "Read the list of known projects from
`projectiny-known-projects-file'."
  (when (file-exists-p projectiny-known-projects-file)
    (with-temp-buffer
      (insert-file-contents projectiny-known-projects-file)
      (split-string (buffer-string) "\n" t))))

(defun projectiny--choose-project ()
  "Prompt the user to choose a project from the known list, and
return its root directory path.

Shown paths are abbreviated to increase readability."
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


(defun projectiny-find-file ()
  "Wrapper around `project-find-file'. Visit a file (with
completion) in the current project’s roots.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  (project-find-file))

(defun projectiny-find-file-in ()
  "Visit a file (with completion) in a project's roots.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  ;; Deliberately modify `default-directory' to enable later use of this variable
  (let* ((default-directory (projectiny--choose-project))
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
