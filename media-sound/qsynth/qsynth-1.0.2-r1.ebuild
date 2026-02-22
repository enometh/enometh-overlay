# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Fri Jul 16 09:10:10 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210716 0.9.4 STACKTRACE, no TRANSLATIONS
# ;madhu 260222 1.0.2-r1  stay with Qt5..

EAPI=8

inherit cmake desktop xdg

DESCRIPTION="Qt application to control FluidSynth"
HOMEPAGE="https://qsynth.sourceforge.io/"

if [[ ${PV} == *9999* ]]; then
	EGIT_REPO_URI="https://git.code.sf.net/p/qsynth/code"
	inherit git-r3
else
	SRC_URI="https://downloads.sourceforge.net/qsynth/${P}.tar.gz"
	KEYWORDS="amd64 ppc ppc64 x86"
fi

LICENSE="GPL-2"
SLOT="0"
IUSE="+alsa debug jack pulseaudio"

REQUIRED_USE="|| ( alsa jack pulseaudio )"

BDEPEND="
	dev-qt/linguist-tools:5
"
DEPEND="
	dev-qt/qtcore:5
	dev-qt/qtgui:5
	dev-qt/qtnetwork:5
	dev-qt/qtwidgets:5
	media-sound/fluidsynth:=[jack?,alsa?,pulseaudio?]
"
RDEPEND="${DEPEND}"

# PATCHES=( "${FILESDIR}/${PN}-0.9.1-cmake-no-git-version.patch" )

src_prepare() {
	cmake_src_prepare

	# get rid of translations
	sed -e '/translations\/.*ts/d' -i src/CMakeLists.txt || die
	sed -e "/^find_package.*QT/s/Qt6 //" -i CMakeLists.txt || die
	sed -e 's|target_link_libraries (${PROJECT_NAME} PRIVATE asan)|target_link_libraries (${PROJECT_NAME} PRIVATE)|g' -i CMakeLists.txt || die
}

src_configure() {
	local mycmakeargs=(
#		-DCONFIG_DEBUG=$(usex debug 1 0)
		-DCONFIG_STACKTRACE=1
		-DCONFIG_QT6=no
	)
	cmake_src_configure
}

src_install() {
	cmake_src_install

	# The desktop file is invalid, and we also change the command
	# depending on useflags
	rm "${D}/usr/share/applications/org.rncbc.qsynth.desktop" || die

	local args
	if use pulseaudio; then
		args="-a pulseaudio"
	elif use alsa; then
		args="-a alsa"
	else
		args="-a oss"
	fi

	make_desktop_entry --eapi9 qsynth -a "${args}" -n Qsynth -i org.rncbc.qsynth
}
