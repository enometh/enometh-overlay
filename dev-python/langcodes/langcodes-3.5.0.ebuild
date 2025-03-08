# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 04 23:45:57 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230404    3.3.0
# ;madhu 250308    3.5.0

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{10..13} )
inherit distutils-r1 pypi

DESCRIPTION="Langcodes: a library for language codes"
HOMEPAGE="https://pypi.org/project/langcodes/#files"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"

RESTRICT="test"
