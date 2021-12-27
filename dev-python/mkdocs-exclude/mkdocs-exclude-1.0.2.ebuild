# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-12-27 17:22:47 IST>
#   Touched: Mon Dec 27 17:22:35 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# https://data.gpo.zugaina.org/guru/dev-python/mkdocs-exclude/mkdocs-exclude-1.0.2.ebuild
#
# ;madhu 211227

EAPI=8

PYTHON_COMPAT=( python3_{8..10} )

inherit distutils-r1

DESCRIPTION="Lets you exclude files or trees from your output"
HOMEPAGE="https://github.com/apenwarr/mkdocs-exclude"
SRC_URI="mirror://pypi/${P:0:1}/${PN}/${P}.tar.gz"

LICENSE="Apache-2.0"
KEYWORDS="~amd64 ~x86"
SLOT="0"

RDEPEND="dev-python/mkdocs[${PYTHON_USEDEP}]"