# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Mar 22 17:43:02 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 2320322 1.7.3.0 (WIP) TODO espeak ffmpeg

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{9..11} )
inherit distutils-r1 pypi

DESCRIPTION="Python/C library and a set of tools to automagically synchronize audio and text (aka forced alignment)."
HOMEPAGE="
	http://www.readbeyond.it/aeneas/
	https://github.com/readbeyond/aeneas/
	https://pypi.python.org/pypi/aeneas/
"
LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64 ~x86"

# SRC_URI=""
#cp -apiv /f/mirrors-gtk/files.pythonhosted.org/packages/e5/92/88d421001bb257588df4864ceca24d570e2e822db4f40f48737a78b648b2/aeneas-1.7.3.0.tar.gz /gentoo/distfiles

RDEPEND="
	>=dev-python/beautifulsoup4-4.5.1[${PYTHON_USEDEP}]
	>=dev-python/lxml-3.6.0[${PYTHON_USEDEP}]
	>=dev-python/numpy-1.9[${PYTHON_USEDEP}]
	app-accessibility/espeak
	media-video/ffmpeg
"
