<div align="center">

# Zyxir's Emacs Configuration

Personal Emacs configuration for everyday use.

[Install] • [Documents] • [Screenshots] • [Changelog]

[Install]: #install
[Documents]: #documents
[Screenshots]: screenshots.md
[Changelog]: #changelog

![Minimum Emacs version supported: 27.2](https://img.shields.io/badge/Emacs-27.2+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Latest commit](https://img.shields.io/github/last-commit/zyxir/dot-emacs/master?style=flat-square)
![Windows](https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue)
![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)

</div>

<hr>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Zyxir's Emacs Configuration](#zyxirs-emacs-configuration)
    - [Introduction](#introduction)
    - [Features](#features)
    - [Documents](#documents)
    - [Install](#install)
    - [Changelog](#changelog)
        - [Version 2.X](#version-2x)
            - [[2.0.1] - 2021-10-09](#201---2021-10-09)
            - [[2.0.0] - 2021-10-09](#200---2021-10-09)
        - [Version 1.X](#version-1x)
            - [[1.0.0] - 2021-09-30](#100---2021-09-30)

<!-- markdown-toc end -->

## Introduction

This is my configuration of Emacs, which is mainly for GTD (getting things done), PKM (personal knowledge management), document writing and casual coding.

There are mature tools for GTD (like Google Keep), PKM (like Notion), document writing (like Microsoft Word) and coding (like Sublime Text), then why use Emacs? Aside from Emacs being an unified, highly configurable environment, there is another reason, which could be explained by Carsten Dominik's words on Google Tech Talks 2008:

> Text files are the only truly portable format for files, so we can write things down and read it anywhere, read it on a small device, read it on a big device, on any computer, Linux, Windows, on a Macintosh, whatever. You can read it truly anywhere, that data will never get lost. So, 20 years from now, you will still be able to read these files, which you wrote.

## Features

- Efficient text editing features, like LSP and ripgrep.
- Org-mode powered GTD, PKM and diary system.
- Tailored for different file formats: Markdown, Python, etc..
- Other handy features like OpenCC and PlantUML support.

## Documents

This is like my dev blog on this repository.

| Date | Title |
| -- | -- |
| `2021-06-28` | [在 Windows 下使用 opencc.el](documents/20210628-opencc-windows-conf.org) |
| `2021-09-30` | [Next Iteration Plan](documents/20210930-next-iteration-plan.org) |

## Install

> This configuration is **tailored for my personal workflow**. Therefore it is not suggested for you to use it directly. However, you can make it a start point of your own configuration.

1. Install [git], and clone the repo as `.emacs.d`.
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

## Changelog

### Version 2.X

#### [2.0.1] - 2021-10-09

The README is polished, as the "changelog" and "screenshots" sections are added.

#### [2.0.0] - 2021-10-09

The whole thing has been reworked, from a org-based literate configuration, to a complex collection of emacs-lisp scripts. Most of the original functionalities is kept or improved.

### Version 1.X

#### [1.0.0] - 2021-09-30

The configuration is almost stable for everyday usage, but a rewrite is planned.
