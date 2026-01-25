# Copyright 2021-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue Oct 05 16:17:08 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 211005 2021.8 2021.7-54-g6db3697
# ;madhu 250120 2024.2 - 2024.1-36-g905ce4f gentoo SRC_URI is busted.
# ;madhu 260125 2026.1 - 2026.1-6-g1f4e51a

EAPI=8

USE_GIT=true

DISTUTILS_SINGLE_IMPL=1
DISTUTILS_USE_PEP517=setuptools
PYPI_NO_NORMALIZE=1
PYTHON_COMPAT=( python3_{10..13} )

inherit distutils-r1 pypi

DESCRIPTION="A documentation generator for GObject-based libraries"
HOMEPAGE="
	https://gitlab.gnome.org/GNOME/gi-docgen
	https://pypi.org/project/gi-docgen/
"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI=https://gitlab.gnome.org/GNOME/gi-docgen.git
	EGIT_BRANCH=main
	SRC_URI=""
else
	#SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"
	SRC_URI="https://download.gnome.org/sources/${PN}/$(ver_cut 1)/${P}.tar.xz"
	#;madhu 250120 404 curl --head 'https://files.pythonhosted.org/packages/source/g/gi-docgen/gi-docgen-2024.1.tar.gz'
	#https://files.pythonhosted.org/packages/d1/86/d17f162d174b6340031fc96474405f13d50ceda4b6bf6588593cf31eb84b/gi_docgen-2024.1.tar.gz
fi

LICENSE="|| ( Apache-2.0 GPL-3+ ) CC0-1.0 OFL-1.1 MIT"
SLOT="0"
KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~loong ~m68k ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86"
REQUIRED_USE="${PYTHON_REQUIRED_USE}"

RDEPEND="
	${PYTHON_DEPS}
	$(python_gen_cond_dep '
		>=dev-python/markdown-3.2[${PYTHON_USEDEP}]
		>=dev-python/markupsafe-1[${PYTHON_USEDEP}]
		>=dev-python/pygments-2[${PYTHON_USEDEP}]
		>=dev-python/jinja2-2[${PYTHON_USEDEP}]
		>=dev-python/typogrify-2[${PYTHON_USEDEP}]
	')
"

distutils_enable_tests pytest
