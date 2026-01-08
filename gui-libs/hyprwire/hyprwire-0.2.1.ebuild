#
#   Time-stamp: <>
#   Touched: Thu Jan 08 23:22:08 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260108 0.2.1  should be dev-libs/hyprwire but since the ebuild is from guru (look ma! no inherit cmake) gui-libs/hyprwire

# timings:
# drop_caches () {for i in 1 2 3 ; do echo $i | sudo tee /proc/sys/vm/drop_caches ; done }
# for CXX in /usr/bin/g++-14 /opt/gcc-15.2/bin/g++ ; do drop_caches; time $CXX -DHT_HIDDEN=public -DHYPRWIRE_VERSION=\"0.2.1\" -Dhyprwire_EXPORTS -I/dev/shm/hyprwire-0.2.1/./include -I/dev/shm/hyprwire-0.2.1/./src -I/dev/shm/hyprwire-0.2.1/build-14 -isystem /usr/lib64/libffi/include -std=gnu++23 -fPIC -Wall -Wextra -Wno-unused-parameter -Wno-unused-value -Wno-missing-field-initializers -Wpedantic -O3  -c /dev/shm/hyprwire-0.2.1/src/core/client/ClientObject.cpp; done
# gcc version 14.2.1 20250104 (GCC)
# 2.32s user 0.17s system 98% cpu 2.526 total
# gcc version 15.2.1 20260103
# 13.95s user 0.24s system 99% cpu 14.200 total

EAPI=8

inherit toolchain-funcs

DESCRIPTION="Small C++ library for utilities used across the Hypr* ecosystem"
HOMEPAGE="https://github.com/hyprwm/hyprutils"

if [[ "${PV}" = *9999 ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/hyprwm/${PN}.git"
else
	SRC_URI="https://github.com/hyprwm/${PN}/archive/refs/tags/v${PV}.tar.gz -> ${P}.tar.gz"
	KEYWORDS="~amd64"
fi

LICENSE="BSD"
SLOT="0/9"

DEPEND="${RDEPEND}"
BDEPEND="
	|| ( >=sys-devel/gcc-14:* >=llvm-core/clang-18:* )
	dev-build/cmake
	virtual/pkgconfig
"

pkg_setup() {
	[[ ${MERGE_TYPE} == binary ]] && return

	if tc-is-gcc && ver_test $(gcc-version) -lt 14; then
		die "GCC version is too old"
	elif tc-is-clang && ver_test $(clang-version) -lt 18; then
		die "Clang version is too old"
	fi
}

src_configure() {
	CXX=/opt/gcc-15.2/bin/g++ \
	cmake --no-warn-unused-cli \
		-DCMAKE_BUILD_TYPE:STRING=Release \
		-DCMAKE_INSTALL_PREFIX:PATH="${EPREFIX}/usr" \
		-DCMAKE_INSTALL_LIBDIR:PATH="$(get_libdir)" \
		-DCMAKE_VERBOSE_MAKEFILE=On \
		-S . -B ./build || die
}

src_compile() {
	cmake --build ./build --verbose --config Release --target all || die
}

src_install() {
	DESTDIR="${D}" cmake --install ./build || die
}
