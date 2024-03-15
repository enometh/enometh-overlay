# Copyright 2017-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Dec 15 12:49:51 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221215 0.3.3 installs the x86_64-unknown-linux-musl binary from the project's github releases under /usr/bin/sccache-dist. (requires a musl compatible files, which is not readily provided by gentoo)
#
# ;madhu 240315 0.7.7

EAPI=8

DESCRIPTION="ccache/distcc like tool with support for rust and cloud storage"
HOMEPAGE="https://github.com/mozilla/sccache/"

LICENSE="Apache-2.0"

SRC_URI="https://github.com/mozilla/sccache/releases/download/v${PV}/sccache-dist-v${PV}-x86_64-unknown-linux-musl.tar.gz"
SLOT="0"
KEYWORDS="~amd64"
IUSE=""

S=${WORKDIR}/sccache-dist-v${PV}-x86_64-unknown-linux-musl
DOCS=("README.md" "LICENSE")

src_install ()
{
	exeinto /usr/bin
	doexe sccache-dist
	einstalldocs
}