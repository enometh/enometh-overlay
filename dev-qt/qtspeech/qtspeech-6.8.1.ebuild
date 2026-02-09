# Copyright 2023-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Feb 09 17:23:46 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
# ;madhu 260209 6.8.1 (Alt Qt). have to workaround qt6-build_src_prepare which checks for qt-declarative by removing the dep...

EAPI=8
QTVER=6.8.1

# tests are kind of flaky, sometimes hang, and also fail with clang
# (not that it's unusable with clang) -- may be worth revisiting
# eventually given qtspeech is still somewhat new (added in 6.4.0)
QT6_RESTRICT_TESTS=1

inherit qt6-build

DESCRIPTION="Text-to-speech library for the Qt6 framework"

if [[ ${QT6_BUILD_TYPE} == release ]]; then
	KEYWORDS="amd64 arm arm64 ~loong ppc ppc64 ~riscv x86"
fi

IUSE="flite qml +speechd"

RDEPEND="
	dev-qt/qtbase:6
	dev-qt/qtmultimedia:6
	flite? ( app-accessibility/flite )
	qml? ( dev-qt/qtdeclarative )
	speechd? ( app-accessibility/speech-dispatcher )
"
DEPEND="${RDEPEND}"

# ;madhu 260209 qt6-src-prepare without the qml check..
src_prepare() {
	local CMAKE_QA_COMPAT_SKIP=1

	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	cmake_src_prepare

	if [[ -e CMakeLists.txt ]]; then
		# throw an error rather than skip if *required* conditions are not met
		sed -e '/message(NOTICE.*Skipping/s/NOTICE/FATAL_ERROR/' \
			-i CMakeLists.txt || die
	fi

	if in_iuse test && use test && [[ -e tests/auto/CMakeLists.txt ]]; then
		# .cmake files tests causing a self-dependency in many modules,
		# and that sometimes install additional test junk
		sed -i '/add_subdirectory(cmake)/d' tests/auto/CMakeLists.txt || die
	fi

	_qt6-build_prepare_env
	if use !custom-cflags; then
		_qt6-build_sanitize_cpu_flags
		# lto+gcc used to break a lot of tests, but this has improved so
		# tentatively allow again for Qt >=6.10 + GCC >=15.2 (bug #955531)
		if ver_test ${PV} -lt 6.10 ||
			{ tc-is-gcc && ver_test $(gcc-version) -lt 15.2; };
		then
			filter-lto
		fi
	fi
	[[ ${QT6_HAS_STATIC_LIBS} ]] && lto-guarantee-fat
}

src_configure() {
	local mycmakeargs=(
		$(cmake_use_find_package qml Qt6Qml)
		$(qt_feature flite)
		$(qt_feature speechd)
		-DCMAKE_INSTALL_PREFIX=/opt/Qt${QTVER}
	)
	CMAKE_PREFIX_PATH=/opt/Qt${QTVER} \
	qt6-build_src_configure
}
