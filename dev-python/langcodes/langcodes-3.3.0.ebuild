# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 04 23:45:57 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230404    3.3.0

EAPI=8

DISTUTILS_USE_PEP517=poetry
PYTHON_COMPAT=( python3_{9..11} )
inherit distutils-r1 pypi

DESCRIPTION="Langcodes: a library for language codes"
HOMEPAGE="https://pypi.org/project/langcodes/#files"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
