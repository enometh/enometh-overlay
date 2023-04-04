# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Apr 05 01:08:46 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230405 - 2.1.2 (WIP)


EAPI=8

DISTUTILS_USE_PEP517=setuptools

PYTHON_COMPAT=( python3_{9..11} )
inherit distutils-r1 pypi

DESCRIPTION="Library Provides script conversion (a.k.a transliteration) between various script"
HOMEPAGE="https://github.com/virtualvinodh/aksharamukha-python
https://pypi.org/project/aksharamukha"

## ;madhu 230405 TODO OMITTED -- WILL CAUSE SOME RUNTIME FAILURES
# pykakasi>=2.0.6
# Flask>=2.0.3
# Flask-CORS>=3.0.6
# Requests>=2.20.1


RDEPEND="
	>=dev-python/pyyaml-5.4.1[${PYTHON_USEDEP}]
	>=dev-python/langcodes-3.1.0[${PYTHON_USEDEP}]
	dev-python/language_data[${PYTHON_USEDEP}]
	>=dev-python/regex-2021.8.3[${PYTHON_USEDEP}]
	dev-python/lxml[${PYTHON_USEDEP}]
	>=dev-python/fonttools-4.27[${PYTHON_USEDEP}]
"

LICENSE="AGPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
