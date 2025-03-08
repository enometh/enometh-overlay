# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Apr 05 00:26:03 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230405   0.8.0 -  SRC_URI
# ;madhu 250308   1.2.1

EAPI=8

PYPI_NO_NORMALIZE=1
PYPI_PN=${PN/-/_}

#DISTUTILS_USE_PEP517=setuptools
#;madhu 230405 cannot use DISTUTILS_USE_PEP517  because we want to call setup.py build and not build_ext (which fails)
DISTUTILS_USE_SETUPTOOLS=bdepend

PYTHON_COMPAT=( python3_{10..13} )
inherit distutils-r1 pypi

DESCRIPTION="Static memory-efficient Trie-like structures based on C++ marisa-trie"
HOMEPAGE="https://github.com/kmike/marisa-trie
https://pypi.org/project/marisa-trie/"

# ;madhu 230405  - filename doesn't follow pypi underscore?
#SRC_URI="https://files.pythonhosted.org/packages/source/m/marisa-trie/marisa-trie-${PV}.tar.gz"
#S="${WORKDIR}/${P}"

LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64 ~x86"
