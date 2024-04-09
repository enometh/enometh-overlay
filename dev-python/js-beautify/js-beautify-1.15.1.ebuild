# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Oct 29 23:29:41 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019 Madhu.  All Rights Reserved.
#
# [Tue Oct 29 23:41:43 2019 +0530]
# ;madhu 191029 1.10.2
# ;madhu 240409 1.15.1

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYPI_NO_NORMALIZE=1
PYTHON_COMPAT=( python3_{9..12} )

inherit distutils-r1 pypi

DESCRIPTION="JS Beautify - python only"
HOMEPAGE="https://github.com/beautify-web/js-beautify"
#SRC_URI="https://github.com/beautify-web/js-beautify/archive/v${PV}.tar.gz -> ${P}.tar.gz"

MY_PN="jsbeautifier"

SRC_URI="$(pypi_sdist_url --no-normalize "${MY_PN}" "${PV}" .tar.gz)"

LICENSE="MIT"
SLOT="0"
KEYWORDS="alpha amd64 arm arm64 hppa ia64 ~mips ppc ppc64 s390 sparc x86 ~amd64-fbsd ~x64-macos"

DEPEND=">=dev-python/six-1.13.0[${PYTHON_USEDEP}]
>=dev-python/editorconfig-0.12.2[${PYTHON_USEDEP}]"

#S="${WORKDIR}/${P}/python" # 1.10.2
S=${WORKDIR}/${MY_PN}-${PV}/
