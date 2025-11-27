# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Nov 27 07:34:21 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 251127 2.14.9 -- static, lua, sc-doc manpages, are all
# non-optional for now. other combinations need more testing

EAPI=8
USE_GIT=false
LUA_COMPAT=( lua5-{1,3,4} )

inherit lua-single toolchain-funcs

DESCRIPTION="Alpine Package Keeper (apk) is a package manager developed for Alpine Linux"
HOMEPAGE="git://git.alpinelinux.org/apk-tools"

if "$USE_GIT" ; then
	inherit git-r3
	# https://github.com/alpinelinux/apk-tools
	# https://gitlab.alpinelinux.org/alpine/apk-tools
	# "git://git.alpinelinux.org/apk-tools"  # HARD FAIL
	EGIT_REPO_URI="https://github.com/alpinelinux/apk-tools"
	EGIT_COMMIT="v2.14.9"
	EGIT_CLONE_TYPE="shallow"

	#   for the github egit_repo_uri:
	#	EGIT_OVERRIDE_REPO_ALPINELINUX_APK_TOOLS=/build/git-mirror/apk-tools.git
else
	SRC_URI="https://github.com/alpinelinux/apk-tools/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
fi

IUSE="+doc +static +lua"
LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64 ~x86"

REQUIRED_USE="lua? ( ${LUA_REQUIRED_USE} )"

#  doc? ( app-text/scdoc )
BDEPEND="
  app-text/scdoc
 lua? ( $(lua_gen_cond_dep 'dev-lua/lua-zlib[${LUA_USEDEP}]') )
"

DEPEND="${LUA_DEPS}
 dev-libs/openssl:0=
 static? ( dev-libs/openssl[static-libs(+)] )
 sys-libs/zlib:0=
 static?  ( sys-libs/zlib[static-libs(+)] )
"

RDEPEND="${DEPEND}"

src_compile() {
	MAKEARGS=(
		AR="$(tc-getAR)"
		CC="$(tc-getCC)"
		V=1
	)
	# ;madhu 251127 Makefile implicitly uses LUA which should be path to
	# lua, it uses LUA_VERSION which does not correspond to what is set
	# in lua-utils.eclass.
	#
	# v3 rc uses meson and doesn't build, even if it's fixed the future
	# looks bleak.
	#
	if use lua; then
		MAKEARGS+=(
			LUA=${LUA}
			LUA_VERSION="${ELUA:3}"
		)
	else
		MAKEARGS+=( LUA=no )
	fi

	emake ${MAKEARGS[@]} compile
	use static && emake ${MAKEARGS[@]} static
}

src_install() {
#	if ! use doc; then MAKEARGS+=( SCDOC=/bin/true ); fi
	emake ${MAKEARGS[@]} \
		  DESTDIR="${D}" \
		  LIBDIR="${EPREFIX}/usr/$(get_libdir)" \
		  PKGCONFIGDIR="${EPREFIX}/usr/$(get_libdir)/pkg-config" \
		  SBINDIR="${EPREFIX}/sbin" \
		  INCLUDEDIR="${EPREFIX}/usr/include" \
		  MANDIR="${EPREFIX}/usr/share/man" \
		  CONFDIR="${EPREFIX}/etc/apk" \
		  DOCDIR="${EPREFIX}/usr/share/doc/${PF}" \
		  LUA_LIBDIR="${EPREFIX}/$(lua_get_lmod_dir)" \
		  install

	use static && dosbin src/apk.static
}
