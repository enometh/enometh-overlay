# Copyright 1999-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2024-10-02 08:09:15 IST>
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
# ;madhu 241002 5.0.11 - v5.0.10-19-gb33340f29, "beta", python3.9 only, (vs 6.0ebuild): ffmpeg not gone, "test" untested, todo test minimal, no avif_decoder, todo: disable asan, xdummy forced
EAPI=8
USE_GIT=true

if ${USE_GIT} ||  [[ ${PV} = 9999* ]]; then
	inherit git-r3
	EGIT_REPO_URI="https://github.com/Xpra-org/xpra.git"
#	EGIT_REPO_URI="file:///7/gtk/xpra/.git"
# copy shallow manually
#sudo -u portage cp /7/gtk/xpra/.git/shallow /gentoo/git3-src/7_gtk_xpra_.git/shallow
	EGIT_BRANCH=madhu-5.0.10
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

inherit cuda xdg xdg-utils distutils-r1 multibuild prefix tmpfiles udev

DESCRIPTION="X Persistent Remote Apps (xpra) and Partitioning WM (parti) based on wimpiggy"
HOMEPAGE="https://xpra.org/"
LICENSE="GPL-2 BSD"
SLOT="0"
IUSE="+X avif brotli +client +clipboard crypt cuda csc cups dbus debug doc examples gstreamer +gtk3  jpeg html ibus +lz4 lzo mdns minimal oauth opengl openh264 pinentry qrcode pulseaudio +server sound systemd test +trayicon udev vpx webcam webp x264 xdg xinerama ffmpeg"
IUSE+=" video_cards_nvidia"
RESTRICT="!test? ( test )"

REQUIRED_USE="${PYTHON_REQUIRED_USE}
	|| ( client gtk3 server )
	cups? ( dbus )
	oauth? ( server )
	opengl? ( client )
	clipboard? ( gtk3 )
	gtk3? ( client )
	test? ( client clipboard crypt dbus gstreamer html server sound xdg xinerama )
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
		x11-drivers/xf86-video-dummy
	)
	webcam? ( media-video/v4l2loopback )
	xinerama? ( x11-libs/libfakeXinerama )

	ffmpeg? ( >=media-video/ffmpeg-3.2.2:0=[x264,x265] )
"

#	csc? ( >=media-video/ffmpeg-1.2.2:0= )

DEPEND="
	${PYTHON_DEPS}
	$(python_gen_cond_dep '
		dev-python/pygobject:3[${PYTHON_USEDEP}]
		opengl? ( dev-python/pyopengl[${PYTHON_USEDEP}] )
		sound? ( dev-python/gst-python:1.0[${PYTHON_USEDEP}] )
		gtk3? (
			dev-python/pygobject:3[cairo]
		)
	')
	dev-libs/xxhash
	avif? ( media-libs/libavif )
	brotli? ( app-arch/brotli )
	client? (
			x11-libs/gtk+:3[X?,introspection]
		)
	jpeg? ( media-libs/libjpeg-turbo )
	mdns? ( dev-libs/mdns )
	openh264? ( media-libs/openh264:= )
	pulseaudio? (
		media-sound/pulseaudio
		media-plugins/gst-plugins-pulse:1.0
		media-plugins/gst-plugins-opus
	)
	sound? (
		media-libs/gstreamer:1.0
		media-libs/gst-plugins-base:1.0
	)
	vpx? ( media-libs/libvpx )
	webp? ( media-libs/libwebp )
	X? (
		x11-apps/xrandr
		x11-libs/libXcomposite
		x11-libs/libXdamage
		x11-libs/libXfixes
		x11-libs/libXrandr
		x11-libs/libXres
		x11-libs/libXtst
		x11-libs/libxkbfile
	)
	x264? ( media-libs/x264 )
"
# nvenc? ( amd64? ( media-libs/nv-codec-headers ) )
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
	test? (
		${TEST_DEPEND}
		$(python_gen_cond_dep '
			dev-python/paramiko[${PYTHON_USEDEP}]
		')
	)
"

#;madhu 231007 virtual-pandoc
BDEPEND="
	$(python_gen_cond_dep '
		>=dev-python/cython-3.0.6[${PYTHON_USEDEP}]
		dev-python/pip[${PYTHON_USEDEP}]
	')
	virtual/pkgconfig
	doc? ( app-text/pandoc )
"

if ${USE_GIT}; then
	:
else
PATCHES=(
	"${FILESDIR}"/${PN}-9999-xdummy.patch
)
fi

PATCHES+=(
#	"${FILESDIR}/${PN}-9999-pep517.patch"
)

src_prepare() {
	default

	sed \
		-e 's#UNITTESTS_DIR=`dirname $(readlink -f $0)`#: "${UNITTESTS_DIR:=`dirname $(readlink -f $0)`}"#' \
		-e 's#INSTALL_ROOT="$SRC_DIR/dist/python${PYTHON_VERSION}"#: "${INSTALL_ROOT:=$SRC_DIR/dist/python${PYTHON_VERSION}}"#' \
		-e '/setup.py install/d' \
		-i "${S}/tests/unittests/run" || die
}

python_prepare_all() {
	distutils-r1_python_prepare_all

	hprefixify xpra/scripts/config.py

	sed -r -e "/\bdoc_dir =/s:/${PN}/\":/${PF}/html\":" \
		-i setup.py || die

	if use minimal; then
		sed -r -e '/pam_ENABLED/s/DEFAULT/False/' \
			-e 's/^(xdg_open)_ENABLED = .*/\1_ENABLED = False/' \
			-i setup.py || die
		PATCHES+=( "${FILESDIR}"/${PN}-4.4.6_xpra-4.4.6_minimal-features.patch )
	fi
}

python_configure_all() {
	sed -e "/'pulseaudio'/s:DEFAULT_PULSEAUDIO:$(usex pulseaudio True False):" \
		-i setup.py || die

	DISTUTILS_ARGS=(
		--with-PIC
		"$(use_with avif)"
		"$(use_with brotli)"
		"$(use_with sound audio)"
		"$(use_with client)"
		"$(use_with clipboard)"
		"$(use_with csc csc_cython)"
		--without-csc_libyuv
		# "$(use_with csc csc_libyuv)" # https://chromium.googlesource.com/libyuv/libyuv
		"$(use_with cuda cuda_rebuild)"
		"$(use_with cuda cuda_kernels)"
		"$(use_with dbus)"
		"$(use_with debug)"
		"$(use_with doc docs)"
		--without-evdi
		# "$(use_with evdi)" x11-drivers/evdi::guru
		"$(use_with examples example)"
		"$(use_with gstreamer)"
		"$(use_with gstreamer gstreamer_audio)"
		"$(use_with gstreamer gstreamer_video)"
		"$(use_with gtk3)"
		"$(use_with html http)"
		"$(use_with mdns)"
		"$(use_with video_cards_nvidia nvidia)"
		--without-nvdec
		--without-nvenc
		--without-nvfbc
		# "$(use_with nvenc nvdec)" # NVIDIA Video Codec SDK
		# "$(use_with nvenc nvenc)" # NVIDIA Video Codec SDK
		# "$(use_with nvenc nvfbc)" # NVIDIA Capture SDK
		"$(use_with opengl)"
		"$(use_with openh264)"
		"$(use_with cups printing)"
		--without-pandoc_lua
		"$(use_with qrcode qrencode)"
		--without-quic
		# "$(use_with quic)" # https://github.com/aiortc/aioquic
		"$(use_with systemd sd_listen)"
		"$(use_with server)"
		"$(use_with systemd service)"
		"$(use_with server shadow)"
		"$(use_with vpx)"
		"$(use_with webcam)"
		"$(use_with webp)"
		"$(use_with X x11)"
		"$(use_with X Xdummy)"

		$(use_with ffmpeg dec_avcodec2)
		$(use_with ffmpeg enc_ffmpeg)
		$(use_with ffmpeg enc_x264)

		"$(use_with test tests)"
		--with-strict
		# --with-verbose
		# --with-warn
		# --with-cythonize_more

		--pkg-config-path="${S}/fs/lib/pkgconfig"
	)

	if use server; then
		DISTUTILS_ARGS+=(
			"$(use_with jpeg jpeg_encoder)"
			"$(use_with vpx vpx_encoder)"
			"$(use_with openh264 openh264_encoder)"
			"$(use_with cuda nvjpeg_encoder)"
			"$(use_with webp webp_encoder)"
			--without-spng_encoder
			# "$(use_with spng spng_encoder)" # https://github.com/randy408/libspng
		)
	else
		DISTUTILS_ARGS+=(
			--without-jpeg_encoder
			--without-vpx_encoder
			--without-openh264_encoder
			--without-nvjpeg_encoder
			--without-webp_encoder
			--without-spng_encoder
		)
	fi

	if use client || use gtk3; then
		DISTUTILS_ARGS+=(
			"$(use_with vpx vpx_decoder)"
			"$(use_with openh264 openh264_decoder)"
			"$(use_with cuda nvjpeg_decoder)"
			"$(use_with jpeg jpeg_decoder)"
			"$(use_with webp webp_decoder)"
			--without-spng_decoder
			# "$(use_with spng spng_decoder)" # https://github.com/randy408/libspng
		)
	else
		DISTUTILS_ARGS+=(
			--without-jpeg_decoder
			--without-vpx_decoder
			--without-openh264_decoder
			--without-nvjpeg_decoder
			--without-webp_decoder
			--without-spng_decoder
		)
	fi

	DISTUTILS_ARGS+=(
		# Arguments from user
		"${MYDISTUTILS_ARGS[@]}"
	)

	export XPRA_SOCKET_DIRS="${EPREFIX}/var/run/xpra"
}

python_compile() {
	if use cuda; then
		export NVCC_PREPEND_FLAGS="-ccbin $(cuda_gccdir)/g++"
	fi

	PYTHONPATH="${S}" distutils-r1_python_compile
}

python_test() {
	einfo "${BUILD_DIR}/install/$(python_get_sitedir)"

	use cuda && cuda_add_sandbox -w
	addwrite /dev/dri/renderD128

	addpredict /dev/dri/card0
	addpredict /dev/fuse
	addpredict /dev/tty0
	addpredict /dev/vga_arbiter
	addpredict /proc/mtrr
	addpredict /var/run/utmp

	addpredict "$(python_get_sitedir)"

	if [[ -d "/sys/devices/virtual/video4linux" ]]; then
		local devices
		readarray -t devices <<<"$(find /sys/devices/virtual/video4linux -mindepth 1 -maxdepth 1 -type d -name 'video*' )"
		for device in "${devices[@]}"; do
			addwrite "/dev/$(basename "${device}" || die )"
		done
	fi

	xdg_environment_reset

	env -u WAYLAND_DISPLAY -u XDG_SESSION_TYPE \
	PYTHONPATH="${S}/tests/unittests:${BUILD_DIR}/test/lib" \
	XPRA_SYSTEMD_RUN=$(usex systemd) XPRA_TEST_COVERAGE=0 \
		"${PYTHON}" "${S}"/tests/unittests/unit/run.py || die
}

python_install() {
	# remove test file
	rm -vrf "${BUILD_DIR}/install/usr/share/xpra/www"

	# Switching to PEP517 gives /usr/etc. Previously, setup.py hardcodes
	# if root_prefix.endswith("/usr"):
	#     root_prefix = root_prefix[:-4]
	# But now setuptools uses data/* to represent out-of-sitedir files.
	# The upstream hack no longer works. We are on our own.
	distutils-r1_python_install
}

python_install_all() {
	distutils-r1_python_install_all

	mv -v "${ED}"/usr/etc "${ED}"/ || die

	sed -e "s#/.*data/etc#/etc#g" \
		-i "${ED}/etc/xpra/conf.d/"* || die

	# Move udev dir to the right place if necessary.
	if use udev; then
		local dir
		dir=$(get_udevdir)
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