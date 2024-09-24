# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Sep 24 14:54:14 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240924 1.4

EAPI=8

DISTUTILS_USE_PEP517=hatchling
PYTHON_COMPAT=( python3_{11..12} )

inherit distutils-r1 pypi

DESCRIPTION="Client-side protocol for Lichat (shirakumo.github.io/lichat-protocol)"
HOMEPAGE="https://github.com/shirakumo/py-lichat"

LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
