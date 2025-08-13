# Copyright 2017-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Aug 13 17:39:06 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# echo dev-util/cargo-c-0.10.15 > /etc/portage/profile/package.provided/cargoc
# ln -svf $(pwd)/github.com/lu-zero/cargo-c/releases/download/v0.10.15/cargo-c-x86_64-unknown-linux-musl.tar.gz /gentoo/distfiles/cargo-c-dist-bin-0.10.15-x86_64-unknown-linux-musl.tar.gz
#
# ;madhu 250813 - 0.15.10  installs the x86_64-unknown-linux-musl binaries from the project's github releases under /usr/bin/ (requires  musl compatible files, which is not readily provided by gentoo)

EAPI=8

DESCRIPTION="Helper program to build and install c-like libraries"
HOMEPAGE="https://github.com/lu-zero/cargo-c"
LICENSE="MIT"
SLOT="0"
KEYWORDS="~amd64"
IUSE=""
S="${WORKDIR}"

SRC_URI="https://github.com/lu-zero/cargo-c/releases/download/v${PV}/cargo-c-x86_64-unknown-linux-musl.tar.gz -> ${P}-x86_64-unknown-linux-musl.tar.gz"
RESTRICT="strip"
QA_PRESTRIPPED="usr/bin/cargo-capi usr/bin/cargo-cbuild usr/bin/cargo-ctest usr/bin/cargo-cinstall"

src_install ()
{
	exeinto /usr/bin
	doexe cargo-capi cargo-cbuild cargo-cinstall cargo-ctest
	einstalldocs
}