# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Thu Nov 27 11:49:20 2025 +0000 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
#
# ;madhu 251127 fix for prefix

EAPI=8

LUA_COMPAT=( lua5-{1..4} luajit )

inherit cmake lua prefix

DESCRIPTION="Simple streaming interface to zlib for Lua"
HOMEPAGE="https://github.com/brimworks/lua-zlib"
SRC_URI="https://github.com/brimworks/${PN}/archive/v${PV}.tar.gz -> ${P}.tar.gz"

LICENSE="MIT"
SLOT="0"
KEYWORDS="amd64 arm arm64 ~hppa ~ppc ~ppc64 ~sparc x86"
REQUIRED_USE="${LUA_REQUIRED_USE}"

RDEPEND="
	sys-libs/zlib
	${LUA_DEPS}

"
DEPEND="${RDEPEND}"

# ;madhu 251127
#
# lua-utils.eclass _lua_export inexplicably strips out SYSROOT from
# correct values returned by pkg-config on prefix.
#
#				;;
#			LUA_CMOD_DIR)
#				local val
#
#				val=$($(tc-getPKG_CONFIG) --variable INSTALL_CMOD ${impl}) || die
#				val="${val#${ESYSROOT#${SYSROOT}}}"
#
#				export LUA_CMOD_DIR=${val}
#				debug-print "${FUNCNAME}: LUA_CMOD_DIR = ${LUA_CMOD_DIR}"
#
# +++ export PKG_CONFIG=x86_64-pc-linux-gnu-pkg-config
# ++++ PKG_CONFIG=x86_64-pc-linux-gnu-pkg-config
# ++++ echo x86_64-pc-linux-gnu-pkg-config
# +++ x86_64-pc-linux-gnu-pkg-config --variable INSTALL_CMOD lua5.4
# ++ val=/var/tmp/gentoo/usr/lib64/lua/5.4
# ++ val=/usr/lib64/lua/5.4
# ...
# so reintroduce them..

lua_src_configure() {

	# avoid an error where if EPREFIX is empty, retard cmake thinks that
	# "//usr/lib64/lua5.x" is a network destination and refuses to
	# install it according to the cmake directive INSTALL (TARGETS
	# cmod_zlib DESTINATION ${INSTALL_CMOD})
	local cmod_dest="${EPREFIX}/$(lua_get_cmod_dir)"
	cmod_dest=${cmod_dest##/}

	local mycmakeargs=(
		-DINSTALL_CMOD="${cmod_dest}" #"$(get_cmod_dest)"
		-DLUA_INCLUDE_DIR="${EPREFIX}/$(lua_get_include_dir)"
		-DUSE_LUA_VERSION="$(lua_get_version)"
		# ;madhu 251127 this is ignored with impunity.
		-DCMAKE_INSTALL_PREFIX="${EPREFIX}/usr"
	)

	if [[ ${ELUA} == luajit ]]; then
		mycmakeargs+=( -DUSE_LUAJIT="ON" )
	fi

	cmake_src_configure
}

src_configure() {
	lua_foreach_impl lua_src_configure
}

src_compile() {
	lua_foreach_impl cmake_src_compile
}

src_install() {
	lua_foreach_impl cmake_src_install
}
