<div align="center">

# Zyxir's Emacs Configuration

Personal Emacs configuration for everyday use.

[Install] • [設計理念] • [Screenshots] • [Changelog]

[Install]: #install
[設計理念]: abc
[Screenshots]: abc
[Changelog]: abc

![Minimum Emacs version supported: 27.2](https://img.shields.io/badge/Emacs-27.2+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Latest commit](https://img.shields.io/github/last-commit/zyxir/dot-emacs/develop?style=flat-square)
![Windows](https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue)
![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)

IMAGE PLACEHOLDER

</div>

<hr>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Zyxir's Emacs Configuration](#zyxirs-emacs-configuration)
    - [Introduction](#introduction)
    - [Features](#features)
    - [Install](#install)
    - [Roadmap](#roadmap)
    - [Changelog](#changelog)

<!-- markdown-toc end -->


## Introduction

## Features

A LOT OF FEATURES

## Install

> This configuration is **tailored for my personal workflow**. Therefore it is not suggested for you to use it directly. However, you can make it a start point of your own configuration.

1. Install [git], and clone the repo as `.emacs.d`.
2. Init and update all git submodules.
3. Write `custom.el` based on `example-custom.el`.
4. For Microsoft Windows:
   - Make sure [msys2] or [mingw] is installed on Windows, as a C compiler is required to compile some packages.
   - If [smart-input-source] should be enabled, add `./3rd-party/im-select/im-select.exe` to path.
5. One the first run, do `M-x all-the-icons-install-fonts` for icon support.
7. Other optional modules:
   - Install [opencc] for Chinese conversion support.
   - Install [pandoc] for document conversion support, which is needed by many features, such as org-mode PDF export.
   - Install [vmd] for real-time Markdown preview (toggled with <kbd>C-c C-c p</kbd>).

[msys2]: https://www.msys2.org/
[mingw]: https://www.mingw-w64.org/
[smart-input-source]: https://github.com/laishulu/emacs-smart-input-source/
[opencc]: https://github.com/BYVoid/OpenCC/
[pandoc]: https://pandoc.org/
[git]: https://git-scm.com/
[grip]: https://github.com/joeyespo/grip/

## Roadmap

THIS IS ROADMAP

## Changelog

THIS IS CHANGELOG