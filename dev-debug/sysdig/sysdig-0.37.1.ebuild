# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2023-07-04 11:06:15 IST>
#   Touched: Tue Jul 04 11:06:15 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023-24 Madhu.  All Rights Reserved.
#
# ;madhu 230704 0.29.3-r1 -> 0.32.0 -- Note that tar size has bloated
# from 3mb to 25mb, most of it in 70Mb yaml under scripts/.  cmake
# downloads the driver anyway, so bundle it and build it here.  this
# ebuild should depend on jsoncpp.  c-ares should be configured with
# cmake to find cmake files at c-ares_DIR:PATH /usr/lib64/cmake/c-ares.
# /var/tmp/ccache/ccache.conf should have hard_link = false and
# inode_ccache = false to build the kernel module.  symlink amd64->x86
# on /usr/src/linux/arch and on /usr/build/linux/arch.  don't use
# IUSE=modules, build both driver and bpf for the current kernel and ship
# both under /usr/lib64/sysdig so the user can use it as they please.
#
# ;madhu 240607 0.37.1, set KBUILD_OUTPUT before building, have to
# reexport ARCH in src_compile, make sure ccache.conf doesn't have
# hard_link=false and inode_ccache=false when building the module.
# TODO. pkgconfig are not fixed.
#
EAPI=8

LUA_COMPAT=( luajit  )

# ;240607 0.37.1 SRC_URI uses sysdig-${PV}.7z instead of the github
# release tarball, which can be prepared without scripts/config, have to
# use unpacker.
FUDGE_SRC_URI=true

# ;240607 0.37.1 both the libs-0.16.0 and libs-7.1.0-driver tarballs are
# indentical except for the dates and one github workflow file. we can
# omit the driver download
OMIT_DRIVER_DOWNLOAD=true

# ;madhu 240609, if sysdig will use a bundled copy of
# LuaJit2.1.0-beta3.tar.gz

USE_BUNDLED_LUA=false

# don't use USE=modules
#MODULES_OPTIONAL_USE=modules

# inherit linux-mod-r1 for tc-get-kernel and KV_FULL
inherit bash-completion-r1 cmake lua-single linux-info

if ${FUDGE_SRC_URI}; then
	inherit unpacker
fi

DESCRIPTION="A system exploration and troubleshooting tool"
HOMEPAGE="https://sysdig.com/"

# For now we need to bump this version of falcosecurity/libs manually;
# check the used git revision in <src>/cmake/modules/falcosecurity-libs.cmake
LIBS_COMMIT="0.16.0"

if ! ${OMIT_DRIVER_DOWNLOAD}; then
DRIVER_VER="7.1.0"
DRIVER_DST="libs-${DRIVER_VER}-driver"
fi

if ${FUDGE_SRC_URI}; then
	SRC_URI=SRC_URI="mirror://gentoo/${P}.7z"
else
	SRC_URI="https://github.com/draios/sysdig/archive/${PV}.tar.gz -> ${P}.tar.gz"
fi

SRC_URI+=" https://github.com/falcosecurity/libs/archive/${LIBS_COMMIT}.tar.gz -> falcosecurity-libs-${LIBS_COMMIT}.tar.gz"

if ! ${OMIT_DRIVER_DOWNLOAD}; then
   SRC_URI+=" https://github.com/falcosecurity/libs/archive/${DRIVER_VER}+driver.tar.gz -> sysdig-${DRIVER_VER}+driver.tar.gz"
fi

if ${USE_BUNDLED_LUA}; then
LUAGIT_PV=2.1.0-beta3
SRC_URI+=" https://github.com/LuaJIT/LuaJIT/archive/v{LUAJIT_PV}.tar.gz -> LuaJIT-${LUAGIT_PV}.tar.gz"
fi

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~amd64 ~x86"
##IUSE="+modules"
REQUIRED_USE="${LUA_REQUIRED_USE}"

RDEPEND="${LUA_DEPS}
	app-misc/jq
	dev-cpp/tbb:=
	dev-cpp/yaml-cpp:=
	dev-libs/jsoncpp
	dev-libs/libb64:=
	dev-libs/openssl:=
	dev-libs/protobuf:=
	net-dns/c-ares:=
	net-libs/grpc:=
	net-misc/curl
	sys-libs/ncurses:=
	sys-libs/zlib:=
	virtual/libelf:="

DEPEND="${RDEPEND}
	dev-cpp/nlohmann_json
	dev-cpp/valijson
	virtual/os-headers"

## for now pin the driver to the same ebuild version
##PDEPEND="modules? ( =dev-debug/scap-driver-${PV}* )"

src_unpack() {
	if ${FUDGE_SRC_URI} ; then
		unpacker ${P}.7z
	fi

	unpack falcosecurity-libs-${LIBS_COMMIT}.tar.gz

	if ! ${OMIT_DRIVER_DOWNLOAD}; then
	   unpack sysdig-${DRIVER_VER}+driver.tar.gz
	fi

	if ${USE_BUNDLED_LUA}; then
		unpack LuaJIT-${LUAGIT_PV}.tar.gz
	fi
}

PATCHES=(
	${FILESDIR}/sysdig-0.37.1-fix-SOVERSION.patch
	${FILESDIR}/sysdig-0.37.1-fix-shared-build.patch
	${FILESDIR}/sysdig-0.37.1-link-with-jsconcpp.patch
)

if ${USE_BUNDLED_LUA}; then
	PATCHES+=( ${FILESDIR}/sysdig-0.37.1-use-bundled-luajit.patch )
fi

src_prepare() {

	if ! ${OMIT_DRIVER_DOWNLOAD}; then
		die untested
	else
		pushd "${WORKDIR}"/libs-${LIBS_COMMIT} || die
		for i in libs-0.16.0-fix-gentoo-compile-driver-bpf.patch \
					 libs-0.16.0-fix-gentoo-compile-driver.patch ; do
			eapply ${FILESDIR}/$i
		done
		popd || die
	fi

	# avoid gentoo tweaks
 if false; then
	# manually apply patch to falcosecurity-libs dependency
	sed -i -e 's:-ggdb::' CMakeLists.txt || die

	# force C++14 standard for libs & main
	sed -i -e 's:-std=c++..:-std=c++14:' "${WORKDIR}"/libs-${LIBS_COMMIT}/cmake/modules/CompilerFlags.cmake || die
	sed -i -e 's:-std=c++..:-std=c++14:' -e 's:-ggdb::'  CMakeLists.txt || die
 fi

	cmake_src_prepare
}

src_configure() {
	local mycmakeargs=(
#		# don't not build driver
		-DBUILD_DRIVER=ON
		-DBUILD_BPF=ON

		# unbundle the deps
		-DUSE_BUNDLED_DEPS=OFF

		-DUSE_BUNDLED_B64=OFF
		-DUSE_BUNDLED_JSONCPP=OFF
		-DUSE_BUNDLED_RE2=OFF
		-DUSE_BUNDLED_TBB=OFF
		-DUSE_BUNDLED_VALIJSON=OFF
		-DUSE_BUNDLED_UTHASH=OFF
		-DUSE_BUNDLED_ZLIB=OFF

		-DBUILD_SHARED_LIBS=ON # needs patching

#		-DUSE_BUNDLED_DRIVER=ON

		-DCREATE_TEST_TARGETS=OFF

		# libscap examples are not installed or really useful
		-DBUILD_LIBSCAP_EXAMPLES=OFF

		# point to driver libs-{DRIVER_VER}-driver/driver
		# -DDRIVER_SOURCE_DIR=${WORKDIR}/${DRIVER_DST}/driver
		# see OMIT_DRIVER_DOWNLOAD below:

		# point to the falcosecurity-libs tree
		-DFALCOSECURITY_LIBS_SOURCE_DIR="${WORKDIR}"/libs-${LIBS_COMMIT}

		# explicitly set version
		-DSYSDIG_VERSION=${PV}

		# add valijson include path to prevent downloading
		-DVALIJSON_INCLUDE="${ESYSROOT}"/usr/include

		# enable chisels
		-DWITH_CHISEL=ON
	)

	if ${OMIT_DRIVER_DOWNLOAD}; then
		mycmakeargs+=( -DDRIVER_SOURCE_DIR="${WORKDIR}"/libs-${LIBS_COMMIT}/driver)
	else
		mycmakeargs+=( -DDRIVER_SOURCE_DIR=${WORKDIR}/${DRIVER_DST}/driver )
	fi

	if ${USE_BUNDLED_LUA}; then
		mycmakeargs+=( -DUSE_BUNDLED_LUAJIT=ON )
	else
		mycmakeargs+=( -DUSE_BUNDLED_LUAJIT=OFF )
	fi

	cmake_src_configure

	# TODO: this should be in src_prepare but src_configure creates this
	# directory.
	if ${USE_BUNDLED_LUA}; then
		cp -apiv ${DISTDIR}/LuaJit-${LUAGIT_PV}.tar.gz ${BUILD_DIR}/luajit-prefix/v${LUAGIT_PV}.tar.gz || die
	fi
}

src_compile() {
	# ;madhu 240607 since we are dont USE=modules portage defaults to
	# ARCH=amd64 which means linux headers cannot be found.
	export ARCH="$(tc-arch-kernel)"

	echo src_compile CCACHE_DIR=$CCACHE_DIR KBUILD_OUTPUT=$KBUILD_OUTPUT ARCH=$ARCH
	cmake_src_compile
}

src_install() {
	cmake_src_install

	# don't' remove driver headers
	# rm -r "${ED}"/usr/src || die
	#
	# even if the kernel version changes user can copy
	# /usr/src/scap-0.0.0 to /tmp/scap and run make -C
	# /lib/modules/$(uname -r)/build M=/tmp/scap and insmod the
	# module. build the probe under bpf/
	#
	# see man sysdig. sysdig -B downloads stuff if it cant find the bpf
	# module in ~/.scap/scap-bpf.o or env var SYDIG_BPF_PROBE.

	mkdir -pv ${ED}/usr/lib64/sysdig/${KV_FULL}
	cp -apfv ${BUILD_DIR}/driver/bpf/probe.o ${ED}/usr/lib64/sysdig/
	cp -apfv ${BUILD_DIR}/driver/scap.ko ${ED}/usr/lib64/sysdig/${KV_FULL}

	dostrip -x ${ED}/usr/lib64/sysdig/probe.o

	# move bashcomp to the proper location
	dobashcomp "${ED}"/usr/etc/bash_completion.d/sysdig || die
	rm -r "${ED}"/usr/etc || die
}
