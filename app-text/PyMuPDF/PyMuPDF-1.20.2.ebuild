# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Oct 13 04:43:16 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221013 1.20.2 targets mupdf-1.20.3

EAPI=8

DISTUTILS_USE_SETUPTOOLS=bdepend
PYTHON_COMPAT=( python3_{8..11} )
inherit distutils-r1

RESTRICT="test"

DESCRIPTION="Python bindings and abstractions to MuPDF"
HOMEPAGE="https://github.com/pymupdf/PyMuPDF"

SLOT="0"
IUSE="+system-mupdf"

KEYWORDS="amd64 x86"
SRC_URI="https://github.com/pymupdf/PyMuPDF/archive/refs/tags/${PV}.tar.gz -> ${P}.tar.gz"

# target mupdf version
MUPDF_PV="1.20.3"

# build mupdf from local git repo
USE_GIT_FOR_MUPDF=true

DEPEND="system-mupdf? ( =app-text/mupdf-${MUPDF_PV} )"

if ${USE_GIT_FOR_MUPDF}; then
	inherit git-r3
	EGIT_REPO_URI="file:///build/git-mirror/mupdf.git"
	EGIT_CLONE_TYPE="shallow"
	EGIT_BRANCH="madhu-${MUPDF_PV}"
	EGIT_SUBMODULES=()
	EGIT_CHECKOUT_DIR="${WORKDIR}/mupdf-${MUPDF_PV}-source"
else
	SRC_URI+=" https://mupdf.com/downloads/archive/mupdf-${MUPDF_PV}-source.tar.gz "
fi

reexport_env_vars() {
	if use system-mupdf; then
		export PYMUPDF_SETUP_MUPDF_BUILD=""
		export PYMUPDF_SETUP_TGZ=""
	else
		export PYMUPDF_SETUP_MUPDF_BUILD="${WORKDIR}/mupdf-${MUPDF_PV}-source"
		export PYMUPDF_SETUP_MUPDF_TGZ=""
	fi
}

src_prepare() {
	default
	if use system-mupdf; then
		eapply ${FILESDIR}/PyMuPDF-1.20.2-no-third-party.patch
	elif ${USE_GIT_FOR_MUPDF}; then
		eapply ${FILESDIR}/PyMuPDF-1.20.2-revert-mupdf-1.18-Makefile.patch
	fi
	reexport_env_vars
	distutils-r1_src_prepare
}

src_unpack() {
	unpack ${P}.tar.gz

	if use system-mupdf; then
		:
	else
		if ${MUPDF_USE_GIT}; then
			git-r3_src_unpack
		else
			unpack ${A}
		fi
	fi
}

python_compile() {
	reexport_env_vars
	distutils-r1_python_compile -j1
}

python_install() {
	reexport_env_vars
	distutils-r1_python_install
}

#:JUNK AT EOF
#
# NOTES PyMuPDF-1.20.2/setup.py
#
#	PYMUPDF_SETUP_MUPDF_BUILD=""
#
#        If set, overrides location of mupdf when building PyMuPDF:
#            Empty string:
#
# ;madhu 221013 actually empty string actually fetches a tgz. I don't
# ;think you can build with a system mupdf anymore
#
#                Build PyMuPDF with the system mupdf.
#
#            Otherwise:
#                Location of mupdf directory.
#
#	PYMUPDF_SETUP_MUPDF_TGZ="file:${DISTDIR}/mupdf-1.20.3-source.tar.gz"
#
#         If set, overrides location of MuPDF .tar.gz file:
#             Empty string:
#                 Do not download MuPDF .tar.gz file. Sdist's will not contain
#                 MuPDF.
#
#             A string containing '://':
#                 The URL from which to download the MuPDF .tar.gz file. Leaf
#                 must match mupdf-*.tar.gz.
#
#             Otherwise:
#                 The path of local mupdf git checkout. We put all files in this
#                 checkout known to git into a local tar archive.

#  ;madhu 221013 can't DEPEND="app-text/mupdf::gentoo" because gentoo doesn't build mupdf-third. use MUPDF_USE_GIT=false to use upstream tarball via SRC_URI. without overriding env vars "ebuild compile" calls setup.py which will download a tarball in the compile phase (ignoring FEATURES=network-sandbox). have to set env vars in both src_compile and src_install because portage/distutils-r1 can't set up the environment in src_prepare.
