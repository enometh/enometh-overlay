# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Feb 04 13:46:56 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.

# ;madhu 250204 - 6.0.0 (WIP)

EAPI=8

DISTUTILS_USE_PEP517=setuptools

PYTHON_COMPAT=( python3_{9..11} )
inherit distutils-r1 pypi

DESCRIPTION="Library Provides script conversion (a.k.a transliteration) between various script"
HOMEPAGE="github.com:YOURUSERNAME/conllu
https://pypi.org/project/conllu/"

# ;madhu 250204 using pypi requires now javascript and does fastly bs
# https://files.pythonhosted.org/packages/0c/53/177d029cdae086c245b1875264d1f736d1909743b8b8e81ffcf7ab43cc48/conllu-6.0.0.tar.gz

RDEPEND="
"

LICENSE="AGPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
RESTRICT="test"