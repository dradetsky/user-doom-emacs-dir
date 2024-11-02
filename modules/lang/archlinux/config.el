;;; lang/archlinux/config.el -*- lexical-binding: t; -*-

(after! pkgbuild-mode
  (when +file-templates-inhibit
    (setq pkgbuild-template "")
    (setq pkgbuild-initialize nil)))
