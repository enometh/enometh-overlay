# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Apr 04 23:47:36 2023 +0530  <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230404   1.1.0
# ;madhu 250308   1.3.0

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{10..13} )
inherit distutils-r1 pypi

DESCRIPTION="language_data: a supplement to langcodes"
HOMEPAGE="https://pypi.org/project/language-data
https://github.com/rspeer/language_data"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
DEPEND="
	dev-python/langcodes[${PYTHON_USEDEP}]
	dev-python/marisa-trie[${PYTHON_USEDEP}]
"
DOCS=( README.md CHANGELOG.md )

RESTRICT="test"

src_prepare()
{
	default
	sed -i -e 's|, "CHANGELOG.md"||g' pyproject.toml
}
