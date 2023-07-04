# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2023-07-04 11:06:15 IST>
#   Touched: Tue Jul 04 11:06:15 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 230704 0.29.3-r1 -> 0.32.0 -- Note that tar size has bloated
# from 3mb to 25mb, most of it in 70Mb yaml under scripts/.  cmake
# downloads the driver anyway, so bundle it and build it here.  this
# ebuild should depend on jsoncpp.  c-ares should be configured with
# cmake to find cmake files at c-ares_DIR:PATH /usr/lib64/cmake/c-ares.
# /var/tmp/ccache/ccache.conf should have hard_link = false and
# inode_ccache = false to build the kernel module.  symlink amd64->x86
# on /usr/src/linux/arch and on /usr/build/linux/arch.  don't use
# IUSE=modules, build both driver and bpf for the current kerneland ship
# both under /usr/lib64/sysdig so the user can use it as they please.

EAPI=8

LUA_COMPAT=( luajit )

#MODULES_OPTIONAL_USE=modules
#inherit linux-mod-r1

inherit bash-completion-r1 cmake lua-single

DESCRIPTION="A system exploration and troubleshooting tool"
HOMEPAGE="https://sysdig.com/"

# For now we need to bump this version of falcosecurity/libs manually;
# check the used git revision in <src>/cmake/modules/falcosecurity-libs.cmake
LIBS_COMMIT="0.11.3"

# ;madhu 230704 build downloads 'https://github.com/falcosecurity/libs/archive/5.0.1+driver.tar.gz' which unpacks to libs-5.0.1-driver/
# portage cannot handle a SRC_URI line with "... -> sysdig-5.0.1+driver.tar.gz" it cannot find sysdig-5.0.1%2Bdriver.tar.gz in distdir.
DRIVER_VER="5.0.1"
DRIVER_DST="libs-${DRIVER_VER}-driver"

SRC_URI="https://github.com/draios/sysdig/archive/${PV}.tar.gz -> ${P}.tar.gz
	https://github.com/falcosecurity/libs/archive/${LIBS_COMMIT}.tar.gz -> falcosecurity-libs-${LIBS_COMMIT}.tar.gz
	https://github.com/falcosecurity/libs/archive/${DRIVER_VER}+driver.tar.gz -> sysdig-${DRIVER_VER}+driver.tar.gz
"

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~amd64 ~x86"
##IUSE="+modules"
REQUIRED_USE="${LUA_REQUIRED_USE}"

RDEPEND="${LUA_DEPS}
	app-misc/jq
	dev-cpp/tbb:=
	dev-cpp/yaml-cpp:=
	dev-libs/libb64:=
	dev-libs/openssl:=
	dev-libs/protobuf:=
	net-dns/c-ares:=
	net-libs/grpc:=
	net-misc/curl
	sys-libs/ncurses:=
	sys-libs/zlib:="

DEPEND="${RDEPEND}
	dev-cpp/nlohmann_json
	dev-cpp/valijson
	virtual/os-headers"

## for now pin the driver to the same ebuild version
##PDEPEND="modules? ( =dev-util/scap-driver-${PV}* )"

src_prepare() {
	# manually apply patch to falcosecurity-libs dependency
	sed -i -e 's:-ggdb::' CMakeLists.txt || die
	cmake_src_prepare
}

src_configure() {
	local mycmakeargs=(
#		# don't not build driver
		-DBUILD_DRIVER=ON
		-DBUILD_BPF=ON

		-DUSE_BUNDLED_B64=OFF
		-DUSE_BUNDLED_JSOCNCPP=OFF
		-DUSE_BUNDLED_RE2=OFF
		-DUSE_BUNDLED_TBB=OFF
		-DUSE_BUNDLED_VALIJSON=OFF
		-DBUILD_SHARED_LIBS=OFF
		-DCREATE_TEST_TARGETS=OFF

		# libscap examples are not installed or really useful
		-DBUILD_LIBSCAP_EXAMPLES=OFF

		# point to driver libs-{DRIVER_VER}-driver/driver
		-DDRIVER_SOURCE_DIR=${WORKDIR}/${DRIVER_DST}/driver

		# point to the falcosecurity-libs tree
		-DFALCOSECURITY_LIBS_SOURCE_DIR="${WORKDIR}"/libs-${LIBS_COMMIT}

		# explicitly set version
		-DSYSDIG_VERSION=${PV}

		# unbundle the deps
		-DUSE_BUNDLED_DEPS=OFF

		# add valijson include path to prevent downloading
		-DVALIJSON_INCLUDE="${ESYSROOT}"/usr/include

		# enable chisels
		-DWITH_CHISEL=ON
	)

	cmake_src_configure
}

src_install() {
	cmake_src_install

	# don't' remove driver headers
	# rm -r "${ED}"/usr/src || die
	mkdir ${ED}/usr/lib64/sysdig
	cp -apfv ${BUILD_DIR}/driver/scap.ko ${BUILD_DIR}/driver/bpf/probe.o ${ED}/usr/lib64/sysdig

	# move bashcomp to the proper location
	dobashcomp "${ED}"/usr/etc/bash_completion.d/sysdig || die
	rm -r "${ED}"/usr/etc || die
}
