# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun May 07 19:57:18 2023 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2023 Madhu.  All Rights Reserved.
#
# ;madhu 200611 5.0.413237.0524
# ;madhu 200617 5.1.412382.0614
# ;madhu 210302 5.5.7938.0228
# ;madhu 211007 zoom-5.8.0.16
# ;madhu 200302 zoom-5.5.7938.0228
# ;madhu 220605 zoom-5.10.7.3311
# ;madhu 230507 5.14.7.2928
# ;madhu 240505 6.0.2.4680
# ;madhu 260923 7.0.0.1666-r2 implicit bundled-qt as we're still on qt5, USE=-zoom-symlink, SLOT=7 installs under /opt/zoom7.

# ln -sv /cache/mirrors/cdn.zoom.us/prod/7.0.0.1666/zoom_x86_64.tar.xz /gentoo/distfiles/zoom-7.0.0.1666_x86_64.tar.xz
# PKGDIR=/tmp/packages or FEATURES=-buildpkg to avoid building the package

EAPI=8

inherit desktop linux-info readme.gentoo-r1 xdg-utils

DESCRIPTION="Video conferencing and web conferencing service"
HOMEPAGE="https://www.zoom.com/"
SRC_URI="https://zoom.us/client/${PV}/${PN}_x86_64.tar.xz -> ${P}_x86_64.tar.xz"
S="${WORKDIR}/${PN}"

LICENSE="all-rights-reserved"
SLOT="7"
KEYWORDS="-* ~amd64"
IUSE="opencl pulseaudio wayland zoom-symlink"
RESTRICT="mirror bindist strip"

if [ ${SLOT} != "0" ];then ZOOMSLOT=$SLOT; fi

# 	pulseaudio? ( media-libs/libpulse )

# qt6
#	dev-libs/quazip
#	dev-qt/qt5compat:6[qml]
#	dev-qt/qtbase:6[opengl]
#	dev-qt/qtdeclarative:6[opengl]
#	dev-qt/qtsvg:6

RDEPEND="!games-engines/zoom
	>=app-accessibility/at-spi2-core-2.46.0:2
	app-crypt/mit-krb5
	dev-libs/expat
	dev-libs/glib:2
	dev-libs/icu
	dev-libs/nspr
	dev-libs/nss
	media-libs/alsa-lib
	media-libs/fdk-aac:0/2
	media-libs/fontconfig
	media-libs/freetype
	media-libs/mesa[gbm(+)]
	media-sound/mpg123
	net-print/cups
	sys-apps/dbus
	sys-apps/util-linux
	sys-libs/glibc
	virtual/glu
	virtual/libudev
	virtual/opengl
	x11-libs/cairo
	x11-libs/libdrm
	x11-libs/libICE
	x11-libs/libSM
	x11-libs/libX11
	x11-libs/libxcb
	x11-libs/libXcomposite
	x11-libs/libXdamage
	x11-libs/libXext
	x11-libs/libXfixes
	x11-libs/libxkbcommon[X]
	x11-libs/libXrandr
	x11-libs/libXrender
	x11-libs/libxshmfence
	x11-libs/libXtst
	x11-libs/pango
	x11-libs/xcb-util-cursor
	x11-libs/xcb-util-image
	x11-libs/xcb-util-keysyms
	x11-libs/xcb-util-renderutil
	x11-libs/xcb-util-wm
	opencl? ( virtual/opencl )
	wayland? ( dev-libs/wayland )"

BDEPEND="dev-util/bbe"

CONFIG_CHECK="~USER_NS ~PID_NS ~NET_NS ~SECCOMP_FILTER"
QA_PREBUILT="opt/zoom$ZOOMSLOT/*"

src_prepare() {
	default

	# The tarball doesn't contain an icon, so extract it from the binary
	bbe -s -b '/<svg width="32" height="32"/:/<\x2fsvg>\n/' -e 'J 1;D' zoom \
		>videoconference-zoom.svg && [[ -s videoconference-zoom.svg ]] \
		|| die "Extraction of icon failed"

	if ! use pulseaudio; then
		# For some strange reason, zoom cannot use any ALSA sound devices if
		# it finds libpulse. This causes breakage if media-sound/apulse[sdk]
		# is installed. So, force zoom to ignore libpulse.
		bbe -e 's/libpulse.so/IgNoRePuLsE/' zoom >zoom.tmp || die
		mv zoom.tmp zoom || die
	fi
}

src_install() {
	insinto /opt/zoom$ZOOMSLOT
	exeinto /opt/zoom$ZOOMSLOT
	doins -r calendar cef diagnostic email imjs js json ringtone sip \
		timezones translations
	# implicit bundled-qt
	doins -r Qt
	find Qt -type f '(' -name '*.so' -o -name '*.so.*' ')' \
		 -printf "$(printf /opt/zoom$ZOOMSLOT/)%p\0" | \
		xargs -0 -r fperms 0755 || die
	(     cd "${ED}"/opt/zoom$ZOOMSLOT/Qt || die
		  # Remove libs and plugins with unresolved soname dependencies.
		  # Why does the upstream package contain such garbage? :-(
		  junk=(
			  lib/libQt6QmlLocalStorage.so.6
			  plugins/egldeviceintegrations/libqeglfs-emu-integration.so
			  plugins/egldeviceintegrations/libqeglfs-kms-egldevice-integration.so
			  plugins/egldeviceintegrations/libqeglfs-kms-integration.so
			  plugins/egldeviceintegrations/libqeglfs-x11-integration.so
			  plugins/platforminputcontexts/libfcitx5platforminputcontextplugin.so
			  plugins/platforms/libqeglfs.so
			  qml/Qt/labs/animation/liblabsanimationplugin.so
			  qml/Qt/labs/lottieqt/liblottieqtplugin.so
			  qml/Qt/labs/qmlmodels/liblabsmodelsplugin.so
			  qml/Qt/labs/settings/libqmlsettingsplugin.so
			  qml/Qt/labs/sharedimage/libsharedimageplugin.so
			  qml/Qt/labs/wavefrontmesh/libqmlwavefrontmeshplugin.so
			  qml/Qt/test/controls/libquickcontrolstestutilsprivateplugin.so
			  qml/QtQml/XmlListModel/libqmlxmllistmodelplugin.so
			  qml/QtQuick/Controls/FluentWinUI3/impl/libqtquickcontrols2fluentwinui3styleimplplugin.so
			  qml/QtQuick/Controls/FluentWinUI3/libqtquickcontrols2fluentwinui3styleplugin.so
			  qml/QtQuick/Controls/Imagine/impl/libqtquickcontrols2imaginestyleimplplugin.so
			  qml/QtQuick/Controls/Imagine/libqtquickcontrols2imaginestyleplugin.so
			  qml/QtQuick/Controls/Material/impl/libqtquickcontrols2materialstyleimplplugin.so
			  qml/QtQuick/Controls/Material/libqtquickcontrols2materialstyleplugin.so
			  qml/QtQuick/Controls/Universal/impl/libqtquickcontrols2universalstyleimplplugin.so
			  qml/QtQuick/Controls/Universal/libqtquickcontrols2universalstyleplugin.so
			  qml/QtQuick/Effects/libeffectsplugin.so
			  qml/QtQuick/LocalStorage/libqmllocalstorageplugin.so
			  qml/QtQuick/Particles/libparticlesplugin.so
			  qml/QtQuick/VectorImage/libqquickvectorimageplugin.so)
		  rm -fv "${junk[@]}"
	)

	doins *.pcm Embedded.properties version.txt unifywebview_config.zip
	doexe zoom zopen ZoomClips ZoomLauncher ZoomWebviewHost *.sh \
		aomhost cpthost libaomagent.so libcml.so libdvf.so libmkldnn.so \
		libavcodec.so* libavformat.so* libavutil.so* libswresample.so* \
		libquazip.so

	fperms a+x /opt/zoom$ZOOMSLOT/cef/chrome_sandbox
	dosym  {"/usr/$(get_libdir)",/opt/zoom$ZOOMSLOT}/libmpg123.so
	dosym  "/usr/$(get_libdir)/libfdk-aac.so.2" /opt/zoom$ZOOMSLOT/libfdkaac2.so
	#;madhu 260930 no qt6 dosym -r "/usr/$(get_libdir)/libquazip1-qt6.so" /opt/zoom$ZOOMSLOT/libquazip.so
	# no glvnd, only mesa
	dosym /usr/$(get_libdir)/libGL.so.1 /opt/zoom$ZOOMSLOT/libGLX.so.0
	dosym /usr/$(get_libdir)/libGL.so.1 /opt/zoom$ZOOMSLOT/libOpenGL.so.0

	if use opencl; then
		doexe libclDNN64.so
		dosym  {"/usr/$(get_libdir)",/opt/zoom$ZOOMSLOT}/libOpenCL.so.1
	fi

	if ! use wayland; then
		# Soname dependency on libwayland-client.so.0
		rm -fv "${ED}"/opt/zoom$ZOOMSLOT/cef/libGLESv2.so || die
	fi

	if [ ${SLOT} = "0" ] ; then
	use zoom-symlink && dosym  /opt/zoom$ZOOMSLOT/ZoomLauncher /usr/bin/zoom

	make_desktop_entry "${EPREFIX}/opt/zoom$ZOOMSLOT/ZoomLauncher %U" Zoom$ZOOMSLOT \
		videoconference-zoom$ZOOMSLOT "Network;VideoConference;" \
		"MimeType=$(printf '%s;' \
			x-scheme-handler/zoommtg \
			x-scheme-handler/zoomus \
			application/x-zoom)"
	doicon videoconference-zoom.svg
	doicon -s scalable videoconference-zoom.svg
	fi

	local DOC_CONTENTS="Some of Zoom's screen share features (e.g.
		the whiteboard) require display compositing. If you encounter
		a black window when sharing the screen, then one of the following
		actions should help:
		\\n- Enable compositing in your window manager if it is supported
		\\n- Alternatively, run the xcompmgr command (from x11-misc/xcompmgr)"
	readme.gentoo_create_doc
}

pkg_postinst() {
	xdg_desktop_database_update
	xdg_icon_cache_update
	readme.gentoo_print_elog
}

pkg_postrm() {
	xdg_desktop_database_update
	xdg_icon_cache_update
}
