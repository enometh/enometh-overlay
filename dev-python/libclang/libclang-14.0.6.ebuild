# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Nov 22 22:29:58 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 231122 14.0.6

EAPI=8

DISTUTILS_USE_SETUPTOOLS=
PYTHON_COMPAT=( python3_{10..12} pypy3 )
inherit pypi distutils-r1

SLOT=0
LICENSE=Apache-2.0
KEYWORDS="~amd64 ~x86"

DESCRIPTION="libclang-for-pip"
HOMEPAGE=https://pypi.org/project/libclang/

#  SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"
# "https://files.pythonhosted.org/packages/9d/97/a87a352058772f30c39cace0992c807c1d2a0c3a9fc673434b011ac54538/libclang-14.0.6.tar.gz
