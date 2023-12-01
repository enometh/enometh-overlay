# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Dec 19 12:52:47 2022 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2022 Madhu.  All Rights Reserved.
#
# ;madhu 221219 4.3.4 -> 4.4.3 with patches for py9, drop the gentoo
# ;test patch
#
# ;madhu 230429 4.4.4
# ;madhu 231007 5.0.6 -- use PYTHON_TARGETS=python3_9 PYTHON_SINGLE_TARGET=python3_9
# ;madhu 231201 5.0.4 - cython update.

EAPI=8
USE_GIT=true

if ${USE_GIT} ||  [[ ${PV} = 9999* ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/Xpra-org/xpra.git"
#	EGIT_REPO_URI="file:///7/gtk/xpra/.git"
# copy shallow manually
#sudo -u portage cp /7/gtk/xpra/.git/shallow /gentoo/git3-src/7_gtk_xpra_.git/shallow
	EGIT_BRANCH=madhu-5.0.4
	KEYWORDS="~amd64 ~x86"
else
#	SRC_URI="mirror://pypi/${PN:0:1}/${PN}/${P}.tar.gz"
#	SRC_URI="https://xpra.org/src/${P}.tar.xz"
	inherit pypi
	KEYWORDS="~amd64 ~x86"
fi

PYTHON_COMPAT=( python3_{9..12} )
DISTUTILS_USE_PEP517=setuptools
DISTUTILS_SINGLE_IMPL=yes
DISTUTILS_EXT=1

inherit xdg xdg-utils distutils-r1 multibuild prefix tmpfiles udev

DESCRIPTION="X Persistent Remote Apps (xpra) and Partitioning WM (parti) based on wimpiggy"
HOMEPAGE="https://xpra.org/"
LICENSE="GPL-2 BSD"
SLOT="0"
IUSE="brotli +client +clipboard crypt csc cups dbus doc ffmpeg jpeg html ibus +lz4 lzo minimal oauth opengl pinentry pulseaudio +server sound systemd test +trayicon udev vpx webcam webp xdg xinerama"

REQUIRED_USE="${PYTHON_REQUIRED_USE}
	|| ( client server )
	cups? ( dbus )
	oauth? ( server )
	opengl? ( client )
	test? ( client clipboard crypt dbus html server sound xdg xinerama )
"

# ;madhu 231007 pulseaudio <- libpulse
TEST_DEPEND="
	$(python_gen_cond_dep '
		dev-python/netifaces[${PYTHON_USEDEP}]
		dev-python/pillow[jpeg?,webp?,${PYTHON_USEDEP}]
		dev-python/rencode[${PYTHON_USEDEP}]
		dbus? ( dev-python/dbus-python[${PYTHON_USEDEP}] )
		xdg? ( dev-python/pyxdg[${PYTHON_USEDEP}] )
	')
	html? ( www-apps/xpra-html5 )
	server? (
		x11-base/xorg-server[-minimal,xvfb]
		x11-drivers/xf86-input-void
	)
	xinerama? ( x11-libs/libfakeXinerama )
"
DEPEND="
	${PYTHON_DEPS}
	$(python_gen_cond_dep '
		dev-python/pygobject:3[${PYTHON_USEDEP},cairo]
		opengl? ( dev-python/pyopengl[${PYTHON_USEDEP}] )
		sound? ( dev-python/gst-python:1.0[${PYTHON_USEDEP}] )
	')
	x11-libs/gtk+:3[introspection]
	x11-libs/libX11
	x11-libs/libXcomposite
	x11-libs/libXdamage
	x11-libs/libXfixes
	x11-libs/libXrandr
	x11-libs/libXres
	x11-libs/libXtst
	x11-libs/libxkbfile
	brotli? ( app-arch/brotli )
	csc? ( >=media-video/ffmpeg-1.2.2:0= )
	ffmpeg? ( >=media-video/ffmpeg-3.2.2:0=[x264,x265] )
	jpeg? ( media-libs/libjpeg-turbo )
	pulseaudio? (
		media-sound/pulseaudio
		media-plugins/gst-plugins-pulse:1.0
	)
	sound? (
		media-libs/gstreamer:1.0
		media-libs/gst-plugins-base:1.0
	)
	vpx? ( media-libs/libvpx media-video/ffmpeg )
	webp? ( media-libs/libwebp )
"
RDEPEND="
	${DEPEND}
	${TEST_DEPEND}
	$(python_gen_cond_dep '
		crypt? ( dev-python/cryptography[${PYTHON_USEDEP}] )
		cups? ( dev-python/pycups[${PYTHON_USEDEP}] )
		lz4? ( dev-python/lz4[${PYTHON_USEDEP}] )
		lzo? ( >=dev-python/python-lzo-0.7.0[${PYTHON_USEDEP}] )
		oauth? ( dev-python/oauthlib[${PYTHON_USEDEP}] )
		opengl? ( dev-python/pyopengl_accelerate[${PYTHON_USEDEP}] )
		webcam? (
			dev-python/numpy[${PYTHON_USEDEP}]
			dev-python/pyinotify[${PYTHON_USEDEP}]
			media-libs/opencv[${PYTHON_USEDEP},python]
		)
	')
	acct-group/xpra
	virtual/ssh
	x11-apps/xauth
	x11-apps/xmodmap
	ibus? ( app-i18n/ibus )
	pinentry? ( app-crypt/pinentry )
	trayicon? ( dev-libs/libayatana-appindicator )
	udev? ( virtual/udev )
"
DEPEND+="
	test? ( ${TEST_DEPEND} )
"

#;madhu 231007 virtual-pandoc
BDEPEND="
	$(python_gen_cond_dep '
		>=dev-python/cython-3.0.6[${PYTHON_USEDEP}]
	')
	virtual/pkgconfig
	doc? ( app-text/pandoc )
"

RESTRICT="!test? ( test )"

if ${USE_GIT}; then
	:
else
PATCHES=(
	"${FILESDIR}"/${PN}-9999-xdummy.patch
)
fi

python_prepare_all() {
	if use minimal; then
		sed -r -e '/pam_ENABLED/s/DEFAULT/False/' \
			-e 's/^(xdg_open)_ENABLED = .*/\1_ENABLED = False/' \
			-i setup.py || die
		PATCHES+=( "${FILESDIR}"/${PN}-4.4.6_xpra-4.4.6_minimal-features.patch )
	fi

	distutils-r1_python_prepare_all

	hprefixify xpra/scripts/config.py

	sed -r -e "/\bdoc_dir =/s:/${PN}/\":/${PF}/html\":" \
		-i setup.py || die
}

python_configure_all() {
	sed -e "/'pulseaudio'/s:DEFAULT_PULSEAUDIO:$(usex pulseaudio True False):" \
		-i setup.py || die

	DISTUTILS_ARGS=(
		--without-PIC
		--without-Xdummy
		$(use_with client)
		$(use_with clipboard)
		$(use_with csc csc_swscale)
		--without-csc_libyuv
		--without-cuda_rebuild
		--without-cuda_kernels
		$(use_with cups printing)
		--without-debug
		$(use_with dbus)
		$(use_with doc docs)
		$(use_with ffmpeg dec_avcodec2)
		$(use_with ffmpeg enc_ffmpeg)
		$(use_with ffmpeg enc_x264)
		--with-gtk3
		$(use_with jpeg jpeg_encoder)
		$(use_with jpeg jpeg_decoder)
		--without-mdns
		--without-sd_listen
		--without-service
		$(use_with opengl)
		$(use_with server shadow)
		$(use_with server)
		$(use_with sound audio)
		--without-strict
		$(use_with vpx)
		--with-warn
		$(use_with webcam)
		$(use_with webp)
		--with-x11
	)

	export XPRA_SOCKET_DIRS="${EPREFIX}/run/xpra"
}

python_test() {
	export XAUTHORITY=${HOME}/.Xauthority
	touch "${XAUTHORITY}" || die

	distutils_install_for_testing
	xdg_environment_reset

	env -u WAYLAND_DISPLAY -u XDG_SESSION_TYPE \
	PYTHONPATH="${S}/tests/unittests:${BUILD_DIR}/test/lib" \
	XPRA_SYSTEMD_RUN=$(usex systemd) XPRA_TEST_COVERAGE=0 \
		"${PYTHON}" "${S}"/tests/unittests/unit/run.py || die
}

python_install_all() {
	distutils-r1_python_prepare_all

	# Switching to PEP517 gives /usr/etc. Previously, setup.py hardcodes
	# if root_prefix.endswith("/usr"):
	#     root_prefix = root_prefix[:-4]
	# But now setuptools uses data/* to represent out-of-sitedir files.
	# The upstream hack no longer works. We are on our own.

	mv -v "${ED}"/usr/etc "${ED}"/ || die

	# Move udev dir to the right place if necessary.
	if use udev; then
		local dir=$(get_udevdir)
		if [[ ! ${ED}/usr/lib/udev -ef ${ED}${dir} ]]; then
			dodir "${dir%/*}"
			mv -vnT "${ED}"/usr/lib/udev "${ED}${dir}" || die
		fi
	else
		rm -vr "${ED}"/usr/lib/udev || die
		rm -v "${ED}"/usr/libexec/xpra/xpra_udev_product_version || die
	fi
}

pkg_postinst() {
	tmpfiles_process xpra.conf
	xdg_pkg_postinst
	use udev && udev_reload
}

pkg_postrm() {
	xdg_pkg_postinst
	use udev && udev_reload
}
