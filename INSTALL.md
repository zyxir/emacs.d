# INSTALL

Instructions to install this configuration on a new machine. Supported platforms:

- **Windows** native compiled Emacs.
- **WSL** (Windows Subsystem for Linux). My distro is Ubuntu 20.04.
- **GNU/Linux**. My distro is Arch Linux.

## Third-party Software

Some third-party software is required for this configuration:

- [git] is **required** to clone this configuration.
- If you would like to use the built-in Rime input method, [librime] is **required** to compile it.
- [ripgrep] is **recommended** for fast text search.
- [opencc] is **recommended** for simplified/traditional Chinese conversion.
- [pandoc] is **strongly recommended** for document conversion. It is vital in many applications, e.g. org export to PDF.

For native Windows Emacs, [msys2] or [mingw] is required to compile some packages.

[msys2]: https://www.msys2.org/
[mingw]: https://www.mingw-w64.org/
[smart-input-source]: https://github.com/laishulu/emacs-smart-input-source/
[ripgrep]: https://github.com/BurntSushi/ripgrep/
[opencc]: https://github.com/BYVoid/OpenCC/
[pandoc]: https://pandoc.org/
[git]: https://git-scm.com/
[librime]: https://github.com/rime/librime

## Windows Specific Preparations

Turn on "Beta: Use Unicode UTF-8 for worldwide language support". Otherwise compatibility issues may be encountered.

## WSL Specific Preparations

You can input Chinese with the built-in Rime input method. Read [this article](documents/20220303-switch-state-with-lshift-for-emacs-rime.org).

GWSL and OpenInWSL provide good integration with Windows for WSL apps.

## Inside Emacs

1. Write `custom.el` based on `example-custom.el`.
1. Run `M-x all-the-icons-install-fonts` for icon support.
