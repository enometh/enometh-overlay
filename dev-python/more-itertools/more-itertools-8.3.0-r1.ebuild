# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Oct 21 12:40:58 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221021 more-itertools-8.3.0 py3_7 compatibility

EAPI=7

_PYTHON_ALLOW_PY37=1
#;madhu 221021 set PYTHON_ALLOW_PY31 in python-any-r1.eclass:(_python_any_set_globals_ and use it in python-utils-r1.eclass:(_python_set_impls) to set up supp
PYTHON_COMPAT_NO_STRICT=true
PYTHON_COMPAT=( python3_{7..10} pypy3 )

inherit distutils-r1

DESCRIPTION="More routines for operating on iterables, beyond itertools"
HOMEPAGE="https://pypi.org/project/more-itertools/"
SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"

LICENSE="MIT"
SLOT="46"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~mips ~ppc ~ppc64 ~s390 ~sparc ~x86 ~sparc-solaris ~sparc64-solaris ~x64-solaris ~x86-solaris"

BDEPEND="
	=dev-python/setuptools-46.3.0-r2:46[${PYTHON_USEDEP}]
"

distutils_enable_sphinx docs \
	dev-python/sphinx_rtd_theme
distutils_enable_tests unittest
