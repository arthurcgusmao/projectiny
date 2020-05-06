# projectiny.el

This package provides basic functionalities for creating a list of known
projects and quickly opening files in them. Its goals are minimalism,
modularity, and the use of Emacs' built-in libraries (`project.el`).

## Overview

List of projectiny's interactive functions (mostly are self-descriptive, but
please see check their docstrings for details):

- `projectiny-add-project`
- `projectiny-edit-known-projects`
- `projectiny-find-file`
- `projectiny-find-file-in`
- `projectiny-find-file-all`

By design, projectiny doesn't automatically search for existing projects as you
use Emacs. You have to explicitly add to the list of known projects by either
using the provided commands or editing the known projects file. (Notice you can
still add some sort of automatic project recognition by creating hooks on the
functions you desire, though not being the aim of the project)

You can configure the file that projectiny uses to save its known projects via
the custom variable `projectiny-known-projects-file`.

## Installation

### Manual install

Clone the repository somewhere in your machine, add its location to the Emacs
path and require it:

```lisp
(add-to-list 'load-path "/path/to/projectiny/")
(require 'projectiny)
```

### Install using [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package):

```lisp
(use-package projectiny
  :straight (:host github :repo "arthurcgusmao/projectiny"))
```
