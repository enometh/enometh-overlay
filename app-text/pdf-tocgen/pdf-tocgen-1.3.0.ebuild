# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Oct 13 04:43:16 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221013 1.3.0 - leave DISTUTILS_USE_PEP517 empty to default to "legacy" with the dev-python/pyproject2setuppy hack instead of poetry

EAPI=8

PYTHON_COMPAT=( python3_{8..11} )
inherit distutils-r1

MY_PN="pdf.tocgen"
S=${WORKDIR}/${MY_PN}-${PV}

RDEPEND=">=app-text/PyMuPDF-1.20.2"

DESCRIPTION="a set of command-line tools for automatically extracting and generating the table of contents (ToC) of a PDF file"
HOMEPAGE="https://krasjet.com/voice/pdf.tocgen/ https://github.com/Krasjet/pdf.tocgen/"

SRC_URI="https://Krasjet/pdf.tocgen/releases/download/v${PV}/pdf.tocgen-${PV}.tar.gz"
LICENSE="GPL-3 AGPL-3"
SLOT="0"
KEYWORDS="amd64 x86"
