# Copyright 2019-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2020-07-02 19:12:25 IST>
#   Touched: Sun Mar 24 15:32:24 2019 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2022 Madhu.  All Rights Reserved.
#
# ;madhu 200702 0.4.3 -> 0.5.0  040cd8fba91 2020-07-02
# ;madhu 221128 0.5.1 v0.5.1-7-g39767bf

EAPI=8

inherit autotools multilib-minimal toolchain-funcs

MY_COMMIT="39767bf4564391a09aaa5f7088229eb73622352f"
USE_GIT=true

DESCRIPTION="C library offering replacements for parts of POSIX whose behaviour is inconsistent across *NIX flavours."
HOMEPAGE="https://github.com/sionescu/libfixposix"

if ${USE_GIT}; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/sionescu/libfixposix"
	EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
	EGIT_COMMIT="$MY_COMMIT"
	EGIT_BRANCH=master
#	EGIT_CLONE_TYPE=shallow
else
#	SRC_URI="https://github.com/sionescu/libfixposix/archive/refs/tags/v${PN}.tar.gz -> ${P}.tar.gz"}
	SRC_URI="https://github.com/sionescu/libfixposix/archive/${MY_COMMIT}.tar.gz -> ${P}.tar.gz"
	S="${WORKDIR}/${PN}-${MY_COMMIT}"
fi

LICENSE=BSD
SLOT="0"
KEYWORDS="amd64 x86"

src_prepare() {
	default
	eautoreconf
}

multilib_src_configure() {
	ECONF_SOURCE=${S} econf
}