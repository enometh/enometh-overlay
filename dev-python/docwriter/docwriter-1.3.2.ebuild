# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=7

PYTHON_COMPAT=( python3_{8..10} pypy3 )

inherit distutils-r1

DESCRIPTION="API documentation generator for the FreeType Library that extracts and builds Markdown docs from the FreeType header files"
HOMEPAGE="https://gitlab.freedesktop.org/freetype/docwriter https://pypi.org/project/docwriter/"
SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"

LICENSE="FTL"
SLOT="0"
KEYWORDS="~amd64 ~arm ~arm64 ~x86"

RDEPEND="
	>=dev-python/mistune-0.8.4[${PYTHON_USEDEP}]
	>=dev-python/mkdocs-1.2.1[${PYTHON_USEDEP}]
	>=dev-python/mkdocs-material-7.1.9[${PYTHON_USEDEP}]
	>=dev-python/pyyaml-5.3.1[${PYTHON_USEDEP}]
"
