# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Apr 21 20:16:38 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 250421 1.9.3

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{10..13} )
PYPI_PN=python-osc
inherit distutils-r1 pypi

DESCRIPTION="Open Sound Control server and client implementations in pure Python
"
HOMEPAGE="https://pypi.org/project/python-osc/"
LICENSE="Unlicense"
SLOT="0"
KEYWORDS="~amd64 ~x86"
DEPEND="
"
DOCS=( README.rst )

# RESTRICT="test"
