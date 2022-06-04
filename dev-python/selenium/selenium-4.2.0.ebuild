# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Nov 04 16:35:01 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211104 selenium-3.141.0-r1 -> 4.0.0
# ;madhu 220604 4.2.0

EAPI=7

DISTUTILS_USE_SETUPTOOLS=manual
PYTHON_COMPAT=( python3_{8..10} )

inherit distutils-r1

DESCRIPTION="Python language binding for Selenium Remote Control"
HOMEPAGE="https://www.seleniumhq.org"
SRC_URI="
	https://files.pythonhosted.org/packages/py3/${PN::1}/${PN}/${P}-py3-none-any.whl
		-> ${P}-py3-none-any.whl.zip"

KEYWORDS="~alpha amd64 arm arm64 hppa ~ia64 ppc ppc64 ~riscv sparc x86 ~x64-macos"
LICENSE="Apache-2.0"
SLOT="0"
S=${WORKDIR}

RDEPEND="
	dev-python/urllib3[${PYTHON_USEDEP}]"

# do not use any build system to avoid circular deps
python_compile() { :; }

python_install() {
	python_domodule selenium  *.dist-info
}
