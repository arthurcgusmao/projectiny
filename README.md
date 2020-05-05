# projectiny.el

This package provides very basic functionalities for bookmarking and searching
in projects. Its goals are minimalism and use of Emacs' built-in libraries such
as `project.el`.

## Overview

Projectiny interactive functions are (mostly are self-descriptive, but please
see their documentation for further details):

- `projectiny-add-bookmark`
- `projectiny-edit-bookmarks`
- `projectiny-find-file-in`
- `projectiny-find-file-all`

You can configure the file that projectiny uses to save its bookmarks via the custom variable `projectiny-bookmarks-file`.

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
