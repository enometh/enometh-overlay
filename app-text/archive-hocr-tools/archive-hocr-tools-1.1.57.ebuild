# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Tue May 28 12:11:16 2024 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2024 Madhu.  All Rights Reserved.
#
# ;madhu 240528 1.1.57 - TODO sphinx-doc

EAPI=8

DISTUTILS_USE_PEP517=setuptools
PYPI_NO_NORMALIZE=1
PYTHON_COMPAT=( python3_{9..12} )

inherit distutils-r1 pypi

DESCRIPTION="hOCR (streaming) parsers and writers"
HOMEPAGE="https://github.com/internetarchive/archive-hocr-tools"
SRC_URI="$(pypi_sdist_url --no-normalize "${PN}" "${PV}" .tar.gz)"

LICENSE="AGPL-3"
SLOT="0"
KEYWORDS="~amd64 ~x86"
