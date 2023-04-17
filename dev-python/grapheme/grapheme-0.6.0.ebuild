# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Apr 17 12:45:07 2023 +0530  <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230417 - 0.6.0

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYTHON_COMPAT=( python3_{9..11} )
inherit distutils-r1 pypi

DESCRIPTION="For working with grapheme clusters as defined by the Unicode Standard Annex #29"
HOMEPAGE="https://pypi.org/project/grapheme https://github.com/alvinlindstam/grapheme"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
