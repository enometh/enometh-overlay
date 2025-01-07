# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2025-01-07 08:40:22 IST>
#   Touched: Tue Jan 07 08:32:49 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250107 5.6.2 - ship a patch which add --render--online and
# renders offline by default.

EAPI=8

PYTHON_COMPAT=( pypy3 python3_{9..12} )
DISTUTILS_USE_PEP517=setuptools
inherit distutils-r1 pypi

DESCRIPTION="Preview GitHub Markdown files like Readme locally before committing them"
HOMEPAGE="https://github.com/joeyespo/grip"
LICENSE="MIT"
RESTRICT="test"

SLOT="0"

KEYWORDS="amd64"

RDEPEND="
	!media-sound/grip
	>=dev-python/docopt-0.4.0[${PYTHON_USEDEP}]
	>=dev-python/flask-0.10.1[${PYTHON_USEDEP}]
	>=dev-python/markdown-2.5.1[${PYTHON_USEDEP}]
	>=dev-python/path-and-address-2.0.1[${PYTHON_USEDEP}]
	>=dev-python/pygments-1.6[${PYTHON_USEDEP}]
	>=dev-python/requests-2.4.1[${PYTHON_USEDEP}]
"

PATCHES+=( ${FILESDIR}/grip-4.6.2-render-offline.patch )