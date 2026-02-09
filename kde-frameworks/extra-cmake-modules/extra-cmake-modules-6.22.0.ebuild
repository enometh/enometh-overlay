# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 08:14:30 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 231211 5.65.0
# ;madhu 260209 6.22.0 Qt5 + Qt6

EAPI=8

PYTHON_COMPAT=( python3_{11..14} )

inherit cmake frameworks.kde.org python-any-r1

DESCRIPTION="Extra modules and scripts for CMake"
HOMEPAGE="https://invent.kde.org/frameworks/extra-cmake-modules"

LICENSE="BSD"
KEYWORDS="~amd64 ~arm ~arm64 ~hppa ~loong ~ppc ~ppc64 ~riscv ~x86"
IUSE="doc test"

RESTRICT="!test? ( test )"

RDEPEND="app-arch/libarchive[bzip2]"
DEPEND="
	test? (
		dev-qt/qtbase[dbus,gui]
		dev-qt/qtdeclarative
	)
"
BDEPEND="
	doc? (
		${PYTHON_DEPS}
		$(python_gen_any_dep 'dev-python/sphinx[${PYTHON_USEDEP}]')
		dev-qt/qttools
	)
	test? (
		dev-qt/qtbase
		dev-qt/qttools[linguist]
	)
"

python_check_deps() {
	python_has_version "dev-python/sphinx[${PYTHON_USEDEP}]"
}

pkg_setup() {
	use doc && python-any-r1_pkg_setup
}

src_configure() {
	local mycmakeargs=(
		-DDOC_INSTALL_DIR=/usr/share/doc/"${PF}"
		-DBUILD_QTHELP_DOCS=$(usex doc)
		-DBUILD_HTML_DOCS=$(usex doc)
		-DBUILD_MAN_DOCS=$(usex doc)
		-DBUILD_TESTING=$(usex test)
	)
	if use test; then
		mycmakeargs+=( -DQT_MAJOR_VERSION=6 ) # bug 938316
	fi

	cmake_src_configure
}

src_test() {
	local CMAKE_SKIP_TESTS=(
		# passes, but then breaks src_install
		ECMToolchainAndroidTest
		# broken, bug #627806
		ECMPoQmToolsTest
		# can not possibly succeed in releases, bug #764953
		KDEFetchTranslations
	)
	# possible race condition with multiple jobs, bug #701854
	cmake_src_test -j1
}
