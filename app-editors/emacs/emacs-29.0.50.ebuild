# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <2021-11-28 17:09:52 IST>
#   Touched: Wed Jan 30 18:34:45 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2021 Madhu.  All Rights Reserved.
#
# emacs from git
#
#
# MULTIVARIANT BUILDS: include multivariant in use flags. This will then
# build emacs binaries for all of gtk athena nox and motif use flags
# which are set.  primary_variant_{$variant} can be used to denote the
# main build. TODO - eslect ebrowse aren't handled correctly. NOTE:
# Requires a patch to emacs to handle process other pdmp files even if a
# signature fails.
#
# ;madhu 200105 28.0.50
# ;madhu 200213 sync with gentoo-portage
# ;madhu 200927 sync with gentoo-portage
# ;madhu 211128 29.0.50 jit, xinput2, webp
#

EAPI=8

inherit autotools elisp-common flag-o-matic multilib readme.gentoo-r1 toolchain-funcs

USE_GIT=true

if [[ ${PV##*.} = 9999 ]] || ${USE_GIT} ; then
	inherit git-r3
	EGIT_REPO_URI="https://git.savannah.gnu.org/git/emacs.git"
	EGIT_BRANCH="madhu-tip"
	EGIT_SUBMODULES=()
	EGIT_CHECKOUT_DIR="${WORKDIR}/${P}"
	EGIT_CLONE_TYPE="shallow"
	S="${EGIT_CHECKOUT_DIR}"

#	SLOT="${PV%%.*}-vcs"
	SLOT="${PV%%.*}"

#EGIT_OVERRIDE_REPO_EMACS="file:///build/git-mirror/emacs.git/"
#EGIT_OVERRIDE_BRANCH_EMACS="madhu-tip"

	KEYWORDS="~alpha ~amd64 ~arm ~hppa ~ia64 ~mips ~ppc ~ppc64 ~s390 ~sh ~sparc ~x86 ~amd64-fbsd ~x86-fbsd ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~x86-macos"
	FULL_VERSION="${PV%%_*}"
	S="${WORKDIR}/emacs-${FULL_VERSION}"
	[[ ${FULL_VERSION} != ${PV} ]] && S="${WORKDIR}/emacs"
else
	# FULL_VERSION keeps the full version number, which is needed in
	# order to determine some path information correctly for copy/move
	# operations later on
	FULL_VERSION="${PV%%_*}"
	SRC_URI="mirror://gnu/emacs/${P}.tar.xz"
	S="${WORKDIR}/emacs-${FULL_VERSION}"
	# PV can be in any of the following formats:
	# 27.1                 released version (slot 27)
	# 27.1_rc1             upstream release candidate (27)
	# 27.0.9999            live ebuild (slot 27-vcs)
	# 27.0.90              upstream prerelease snapshot (27-vcs)
	# 27.0.50_pre20191223  snapshot by Gentoo developer (27-vcs)
	if [[ ${PV} == *_pre* ]]; then
		SRC_URI="https://dev.gentoo.org/~ulm/distfiles/${P}.tar.xz"
		S="${WORKDIR}/emacs"
	elif [[ ${PV//[0-9]} != "." ]]; then
		SRC_URI="https://alpha.gnu.org/gnu/emacs/pretest/${PN}-${PV/_/-}.tar.xz"
	fi
	SLOT="${PV%%.*}"
	[[ ${PV} == *.*.* ]] && SLOT+="-vcs"
	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86 ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos"
fi

DESCRIPTION="The extensible, customizable, self-documenting real-time display editor"
HOMEPAGE="https://www.gnu.org/software/emacs/"

LICENSE="GPL-3+ FDL-1.3+ BSD HPND MIT W3C unicode PSF-2"
SLOT="29"


IUSE="acl alsa aqua athena cairo dbus dynamic-loading games gconf gfile gif +gmp gpm gsettings gtk gui gzip-el harfbuzz imagemagick +inotify jit jpeg json kerberos lcms libxml2 livecd m17n-lib mailutils motif png selinux sound source ssl svg systemd +threads tiff toolkit-scroll-bars webp wide-int Xaw3d xinput2 xft +xpm xwidgets zlib nox multivariant"

VARIANTS="gtk athena nox motif"
PRIMARY_VARIANT="${VARIANTS}"
for variant in ${VARIANTS}; do
	IUSE+=" primary_variant_${variant}"
done

variants=()
primary_variant=

setvariants() {
	#set `primary_variant' and `variant' variables from USE
	if use multivariant; then
		for i in ${VARIANTS} ; do
			if use primary_variant_${i}; then
				primary_variant=$i
				break
			fi
		done
		for i in ${VARIANTS}; do
			if use ${i}; then
				if [ -z "${primary_variant}" ]; then
					primary_variant=${i}
				elif [ "${primary_variant} != ${variant}" ]; then
					variants+=(${i})
				fi
			fi
		done
		[ -n ${primary_variant} ] || die "no primary_variant for multivariant"
	else
		[ -z ${primary_variant} ] || die "primary_variant for non multivariant"
	fi
}


RESTRICT="test"

RDEPEND="app-emacs/emacs-common[games?,gui(-)?]
	sys-libs/ncurses:0=
	acl? ( virtual/acl )
	alsa? ( media-libs/alsa-lib )
	dbus? ( sys-apps/dbus )
	games? ( acct-group/gamestat )
	gmp? ( dev-libs/gmp:0= )
	gpm? ( sys-libs/gpm )
	!inotify? ( gfile? ( >=dev-libs/glib-2.28.6 ) )
	jit? ( sys-devel/gcc:=[jit(-)] )
	json? ( dev-libs/jansson:= )
	kerberos? ( virtual/krb5 )
	lcms? ( media-libs/lcms:2 )
	libxml2? ( >=dev-libs/libxml2-2.2.0 )
	mailutils? ( net-mail/mailutils[clients] )
	!mailutils? ( acct-group/mail net-libs/liblockfile )
	selinux? ( sys-libs/libselinux )
	ssl? ( net-libs/gnutls:0= )
	systemd? ( sys-apps/systemd )
	zlib? ( sys-libs/zlib )
	gui? ( !aqua? (
		x11-libs/libICE
		x11-libs/libSM
		x11-libs/libX11
		x11-libs/libXext
		x11-libs/libXfixes
		x11-libs/libXinerama
		x11-libs/libXrandr
		x11-libs/libxcb
		x11-misc/xbitmaps
		gconf? ( >=gnome-base/gconf-2.26.2 )
		gsettings? ( >=dev-libs/glib-2.28.6 )
		gif? ( media-libs/giflib:0= )
		jpeg? ( virtual/jpeg:0= )
		png? ( >=media-libs/libpng-1.4:0= )
		svg? ( >=gnome-base/librsvg-2.0 )
		tiff? ( media-libs/tiff:0 )
		xpm? ( x11-libs/libXpm )
		imagemagick? ( >=media-gfx/imagemagick-6.6.2:0= )
		xft? (
			media-libs/fontconfig
			media-libs/freetype
			x11-libs/libXft
			x11-libs/libXrender
			cairo? ( >=x11-libs/cairo-1.12.18 )
			harfbuzz? ( media-libs/harfbuzz:0= )
			m17n-lib? (
				>=dev-libs/libotf-0.9.4
				>=dev-libs/m17n-lib-1.5.1
			)
		)
		gtk? (
			x11-libs/gtk+:3
			xwidgets? (
				net-libs/webkit-gtk:4=
				x11-libs/libXcomposite
			)
		)
		!gtk? (
			motif? (
				>=x11-libs/motif-2.3:0
				x11-libs/libXpm
				x11-libs/libXmu
				x11-libs/libXt
			)
			!motif? (
				Xaw3d? (
					x11-libs/libXaw3d
					x11-libs/libXmu
					x11-libs/libXt
				)
				!Xaw3d? ( athena? (
					x11-libs/libXaw
					x11-libs/libXmu
					x11-libs/libXt
				) )
			)
		)
	) )"

DEPEND="${RDEPEND}
	gui? ( !aqua? ( x11-base/xorg-proto ) )"

BDEPEND="sys-apps/texinfo
	virtual/pkgconfig
	gzip-el? ( app-arch/gzip )"

EMACS_SUFFIX="${PN/emacs/emacs-${SLOT}}"
SITEFILE="20${PN}-${SLOT}-gentoo.el"

src_prepare() {
	if [[ ${PV##*.} = 9999 ]] || ${USE_GIT} ; then
		FULL_VERSION=$(sed -n 's/^AC_INIT([^,]*,[ \t]*\([^ \t,)]*\).*/\1/p' \
			configure.ac)
		[[ ${FULL_VERSION} ]] || die "Cannot determine current Emacs version"
		einfo "Emacs branch: ${EGIT_BRANCH}"
		einfo "Commit: ${EGIT_VERSION}"
		einfo "Emacs version number: ${FULL_VERSION}"
		[[ ${FULL_VERSION} =~ ^${PV%.*}(\..*)?$ ]] \
			|| die "Upstream version number changed to ${FULL_VERSION}"
	fi

	if use jit; then
		# These files ignore LDFLAGS. We assign the variable here, because
		# for live ebuilds FULL_VERSION doesn't exist in global scope
		QA_FLAGS_IGNORED="usr/$(get_libdir)/emacs/${FULL_VERSION}/native-lisp/.*"

		# gccjit doesn't play well with ccache #801580
		# For now, work around the problem with an explicit LIBRARY_PATH
		has ccache ${FEATURES} && tc-is-gcc \
			&& export LIBRARY_PATH=$("$(tc-getCC)" -print-search-dirs \
				| sed -n '/^libraries:/{s:^[^/]*::;p}')
	fi

	default

	# Fix filename reference in redirected man page
	sed -i -e "/^\\.so/s/etags/&-${EMACS_SUFFIX}/" doc/man/ctags.1 || die

	AT_M4DIR=m4 eautoreconf
}

src_configure() {
	strip-flags
	filter-flags -pie					#526948

	if use ia64; then
		replace-flags "-O[2-9]" -O1		#325373
	else
		replace-flags "-O[3-9]" -O2
	fi

	local myconf

	if use alsa; then
		if use sound; then		#;madhu 190704
		use sound || ewarn \
			"USE flag \"alsa\" overrides \"-sound\"; enabling sound support."
		myconf+=" --with-sound=alsa"
		fi
	else
		myconf+=" --with-sound=$(usex sound oss)"
	fi

	xconf() {
		myconf+=" --with-x --without-ns"
		myconf+=" $(use_with gconf)"
		myconf+=" $(use_with gsettings)"
		myconf+=" $(use_with toolkit-scroll-bars)"
		myconf+=" $(use_with gif)"
		myconf+=" $(use_with jpeg)"
		myconf+=" $(use_with png)"
		myconf+=" $(use_with svg rsvg)"
		myconf+=" $(use_with tiff)"
		myconf+=" $(use_with xpm)"
		myconf+=" $(use_with imagemagick)"

		if use xft; then
			myconf+=" --with-xft"
			myconf+=" $(use_with cairo)"
			myconf+=" $(use_with harfbuzz)"
			myconf+=" $(use_with m17n-lib libotf)"
			myconf+=" $(use_with m17n-lib m17n-flt)"
		else
			myconf+=" --without-xft"
			myconf+=" --without-cairo"
			myconf+=" --without-libotf --without-m17n-flt"
			use cairo && ewarn \
				"USE flag \"cairo\" has no effect if \"xft\" is not set."
			use m17n-lib && ewarn \
				"USE flag \"m17n-lib\" has no effect if \"xft\" is not set."
		fi
	}

	myconf+=" --includedir="${EPREFIX}"/usr/include/${EMACS_SUFFIX}"
	myconf+=" --infodir="${EPREFIX}"/usr/share/info/${EMACS_SUFFIX}"
	myconf+=" --localstatedir="${EPREFIX}"/var"
	myconf+=" --enable-locallisppath="${EPREFIX}/etc/emacs:${EPREFIX}${SITELISP}""
	myconf+=" --without-compress-install"
	myconf+=" --without-hesiod"
	myconf+=" --without-pop"
	myconf+=" --with-file-notification=$(usev inotify || usev gfile || echo no)"

	myconf+=" $(use_enable acl)"
	myconf+=" $(use_with dbus)"
	myconf+=" $(use_with dynamic-loading modules)"
	myconf+=" $(use_with games gameuser ":gamestat")"
	myconf+=" $(use_with gmp libgmp)"
	myconf+=" $(use_with gpm)"
	myconf+=" $(use_with jit native-compilation)"
	myconf+=" $(use_with json)"
	myconf+=" $(use_with kerberos)"
	myconf+=" $(use_with kerberos kerberos5)"
	myconf+=" $(use_with lcms lcms2)"
	myconf+=" $(use_with libxml2 xml2)"
	myconf+=" $(use_with mailutils)"
	myconf+=" $(use_with selinux)"
	myconf+=" $(use_with ssl gnutls)"
	myconf+=" $(use_with systemd libsystemd)"
	myconf+=" $(use_with threads)"
	myconf+=" $(use_with wide-int)"
	myconf+=" $(use_with zlib)"

	myconf+=" $(use_with webp)"
	myconf+=" $(use_with xinput2)"

if ! use multivariant; then
	if ! use gui; then
		einfo "Configuring to build without window system support"
		myconf+=" --without-x --without-ns"
	elif use aqua; then
		einfo "Configuring to build with Nextstep (Macintosh Cocoa) support"
		myconf+=" --with-ns --disable-ns-self-contained"
		myconf+=" --without-x"
	else
		xconf
		local f line
		if use gtk; then
			einfo "Configuring to build with GIMP Toolkit (GTK+)"
			while read line; do ewarn "${line}"; done <<-EOF
				Your version of GTK+ will have problems with closing open
				displays. This is no problem if you just use one display, but
				if you use more than one and close one of them Emacs may crash.
				See <https://gitlab.gnome.org/GNOME/gtk/-/issues/221> and
				<https://gitlab.gnome.org/GNOME/gtk/-/issues/2315>.
				If you intend to use more than one display, then it is strongly
				recommended that you compile Emacs with the Athena/Lucid or the
				Motif toolkit instead.
			EOF
			myconf+=" --with-x-toolkit=gtk3 $(use_with xwidgets)"
			for f in motif Xaw3d athena; do
				use ${f} && ewarn \
					"USE flag \"${f}\" has no effect if \"gtk\" is set."
			done
		elif use motif; then
			einfo "Configuring to build with Motif toolkit"
			myconf+=" --with-x-toolkit=motif"
			for f in Xaw3d athena; do
				use ${f} && ewarn \
					"USE flag \"${f}\" has no effect if \"motif\" is set."
			done
		elif use athena || use Xaw3d; then
			einfo "Configuring to build with Athena/Lucid toolkit"
			myconf+=" --with-x-toolkit=lucid $(use_with Xaw3d xaw3d)"
		else
			einfo "Configuring to build with no toolkit"
			myconf+=" --with-x-toolkit=no"
		fi
		! use gtk && use xwidgets && ewarn \
			"USE flag \"xwidgets\" has no effect if \"gtk\" is not set."
	fi

	if tc-is-cross-compiler; then
		# Configure a CBUILD directory when cross-compiling to make tools
		mkdir "${S}-build" && pushd "${S}-build" >/dev/null || die
		ECONF_SOURCE="${S}" econf_build --without-all --without-x-toolkit
		popd >/dev/null || die
		# Don't try to execute the binary for dumping during the build
		myconf+=" --with-dumping=none"
	else
		myconf+=" --with-dumping=pdumper"
	fi

	ECONF_SOURCE=${S} econf \
		--program-suffix="-${EMACS_SUFFIX}" \
		${myconf}
else
	if use gtk; then
		x_conf
		mkdir build-variant-gtk
		pushd build-variant-gtk
		ECONF_SOURCE=${S} econf \
					--program-suffix="-${EMACS_SUFFIX}-gtk" \
					--with-x-toolkit=gtk3 $(use_with xwidgets) \
					${myconf}
		popd
	fi

	if use motif; then
		x_conf
		mkdir build-variant-motif
		pushd build-variant-motif
		ECONF_SOURCE=${S} econf \
					--program-suffix="-${EMACS_SUFFIX}-motif" \
					--with-x-toolkit=motif \
					${myconf}
		popd
	fi

	if use athena; then
		x_conf
		mkdir build-variant-athena
		pushd build-variant-athena
		ECONF_SOURCE=${S} econf \
					--program-suffix="-${EMACS_SUFFIX}-athena" \
					--with-x-toolkit=lucid \
					$(use_with Xaw3d xaw3d) \
					${myconf}
		popd
	fi

	if use nox; then
		mkdir build-variant-nox
		pushd build-variant-nox
		ECONF_SOURCE=${S} econf \
					--program-suffix="-${EMACS_SUFFIX}-nox" \
					--without-x \
					${myconf}
		popd
	fi
fi #multivariant
}

src_compile() {
if ! use multivariant; then
	if tc-is-cross-compiler; then
		# Build native tools for compiling lisp etc.
		emake -C "${S}-build" src
		emake lib	   # Cross-compile dependencies first for timestamps
		# Save native build tools in the cross-directory
		cp "${S}-build"/lib-src/make-{docfile,fingerprint} lib-src || die
		# Specify the native Emacs to compile lisp
		emake -C lisp all EMACS="${S}-build/src/emacs"
	fi
	emake
else
	for i in build-variant-*; do
		pushd $i
		emake || die "Make failed"
		popd
	done
fi
}

src_install () {
	local ARCHLIBDIR=${ED}/usr/libexec/emacs/${FULL_VERSION}/${CHOST}

	setvariants
	for variant in "${primary_variant}" ${variants[@]} ; do

		if use multivariant; then
			pushd build-variant-${variant}
		fi

		if [ "${primary_variant}" = "${variant}" ]; then

			#primary build: do a regular install
			emake DESTDIR="${D}" NO_BIN_LINK=t BLESSMAIL_TARGET= install

			if use multivariant; then
				[ -n "${variant}" ] || die sanity
				# the pdmp should have the same name as emacs'
				# argv[0]. eselect emacs puts a symlink for /usr/bin/emacs
				# but argv[0] is just emacs and this uses the default
				# emacs.pdmp. If invoked as $EMACS_SUFFIX-$variant arrange
				# to have an EMACS_SUFFIX-$variant.pdmp file.
				cp -apfv -l ${ARCHLIBDIR}/{emacs.pdmp,${EMACS_SUFFIX}-${variant}.pdmp}
				mv -fv "${ED}"/usr/bin/{emacs-${FULL_VERSION}-,}${EMACS_SUFFIX}-${variant} \
					|| die "moving emacs executable failed"
			else
+				mv -fv "${ED}"/usr/bin/{emacs-${FULL_VERSION}-,}${EMACS_SUFFIX} || die "moving emacs executable failed"
			fi

		else
			# secondary multivariant builds: we would want to use
			# src/Makefile: install-arch-dep to install the executable
			# and the pdump files but that has install-arch-indep as a
			# prereq. Instead we just copy the files.
			[ -n "${variant}" ] || die sanity
			use multivariant || die sanity
			cp -apfv "src/emacs" "${ED}/usr/bin/${EMACS_SUFFIX}-${variant}"
			cp -apfv "src/emacs.pdmp" "${ARCHLIBDIR}/${EMACS_SUFFIX}-${variant}.pdmp"
			# ;madhu 211128 TODO also needs a symlink (cd /usr; ln -sv lib64/emacs/29.0.50/native-lisp)
			cp -apfvr "native-lisp/"* "${ED}/usr/$(get_libdir)/emacs/${FULL_VERSION}/native-lisp/"

		fi

		if use multivariant; then
			popd
		fi

	#SINGLE COMMON INSTALLATION
	#mv -fv "${ED}"/usr/share/man/man1/{emacs-,}${EMACS_SUFFIX}${variant:+-}${variant}.1 || die "moving emacs man page failed"
	#mv -fv "${ED}"/usr/share/metainfo/{emacs-,}${EMACS_SUFFIX}${variant:+-}${variant}.appdata.xml || die "moving emacs appdata.xml"

if [ "${primary_variant}" = "${variant}" ]; then
	mv -fv "${ED}"/usr/share/man/man1/{emacs-,}${EMACS_SUFFIX}${variant:+-}${variant}.1 || die

# ??? no appdata.xml in my builds
	mv -fv "${ED}"/usr/share/metainfo/{emacs-,}${EMACS_SUFFIX}${variant:+-}${variant}.appdata.xml

	# move info dir to avoid collisions with the dir file generated by portage
	mv -fv "${ED}"/usr/share/info/${EMACS_SUFFIX}/dir{,.orig} || die
	touch "${ED}"/usr/share/info/${EMACS_SUFFIX}/.keepinfodir
	docompress -x /usr/share/info/${EMACS_SUFFIX}/dir.orig

	# movemail must be setgid mail
	if ! use mailutils; then
		fowners root:mail /usr/libexec/emacs/${FULL_VERSION}/${CHOST}/movemail
		fperms 2751 /usr/libexec/emacs/${FULL_VERSION}/${CHOST}/movemail
	fi

	# avoid collision between slots, see bug #169033 e.g.
	rm "${ED}"/usr/share/emacs/site-lisp/subdirs.el || die
	rm -rf "${ED}"/usr/share/{applications,icons} || die
	rm -rf "${ED}/usr/$(get_libdir)/systemd" || die
	rm -rf "${ED}"/var || die

	# remove unused <version>/site-lisp dir
	rm -rf "${ED}"/usr/share/emacs/${FULL_VERSION}/site-lisp || die

	# remove COPYING file (except for etc/COPYING used by describe-copying)
	rm "${ED}"/usr/share/emacs/${FULL_VERSION}/lisp/COPYING || die

	if use systemd; then
		insinto /usr/lib/systemd/user
		sed -e "/^##/d" \
			-e "/^ExecStart/s,emacs,${EPREFIX}/usr/bin/${EMACS_SUFFIX}," \
			-e "/^ExecStop/s,emacsclient,${EPREFIX}/usr/bin/&-${EMACS_SUFFIX}," \
			etc/emacs.service | newins - ${EMACS_SUFFIX}.service
		assert
	fi

	if use gzip-el; then
		# compress .el files when a corresponding .elc exists
		find "${ED}"/usr/share/emacs/${FULL_VERSION}/lisp -type f \
			-name "*.elc" -print | sed 's/\.elc$/.el/' | xargs gzip -9n
		assert "gzip .el failed"
	fi

	local cdir
	if use source; then
		cdir="/usr/share/emacs/${FULL_VERSION}/src"
		insinto "${cdir}"
		# This is not meant to install all the source -- just the
		# C source you might find via find-function
		doins src/*.{c,h,m}
	elif has installsources ${FEATURES}; then
		cdir="/usr/src/debug/${CATEGORY}/${PF}/${S#"${WORKDIR}/"}/src"
	fi

	sed -e "${cdir:+#}/^Y/d" -e "s/^[XY]//" >"${T}/${SITEFILE}" <<-EOF || die
	X
	;;; ${EMACS_SUFFIX} site-lisp configuration
	X
	(when (string-match "\\\\\`${FULL_VERSION//./\\\\.}\\\\>" emacs-version)
	Y  (setq find-function-C-source-directory
	Y	"${EPREFIX}${cdir}")
	X  (let ((path (getenv "INFOPATH"))
	X	(dir "${EPREFIX}/usr/share/info/${EMACS_SUFFIX}")
	X	(re "\\\\\`${EPREFIX}/usr/share\\\\>"))
	X    (and path
	X	 ;; move Emacs Info dir before anything else in /usr/share
	X	 (let* ((p (cons nil (split-string path ":" t))) (q p))
	X	   (while (and (cdr q) (not (string-match re (cadr q))))
	X	     (setq q (cdr q)))
	X	   (setcdr q (cons dir (delete dir (cdr q))))
	X	   (setq Info-directory-list (prune-directory-list (cdr p)))))))
	EOF
	elisp-site-file-install "${T}/${SITEFILE}" || die

	dodoc README BUGS CONTRIBUTE

	if use gui && use aqua; then
		dodir /Applications/Gentoo
		rm -rf "${ED}"/Applications/Gentoo/${EMACS_SUFFIX^}.app || die
		mv nextstep/Emacs.app \
			"${ED}"/Applications/Gentoo/${EMACS_SUFFIX^}.app || die
	fi

	local DOC_CONTENTS="You can set the version to be started by
		/usr/bin/emacs through the Emacs eselect module, which also
		redirects man and info pages. Therefore, several Emacs versions can
		be installed at the same time. \"man emacs.eselect\" for details.
		\\n\\nIf you upgrade from a previous major version of Emacs, then
		it is strongly recommended that you use app-admin/emacs-updater
		to rebuild all byte-compiled elisp files of the installed Emacs
		packages."
	if use gui; then
		DOC_CONTENTS+="\\n\\nYou need to install some fonts for Emacs.
			Installing media-fonts/font-adobe-{75,100}dpi on the X server's
			machine would satisfy basic Emacs requirements under X11.
			See also https://wiki.gentoo.org/wiki/Xft_support_for_GNU_Emacs
			for how to enable anti-aliased fonts."
		use aqua && DOC_CONTENTS+="\\n\\n${EMACS_SUFFIX^}.app is in
			\"${EPREFIX}/Applications/Gentoo\". You may want to copy or
			symlink it into /Applications by yourself."
	fi
	tc-is-cross-compiler && DOC_CONTENTS+="\\n\\nEmacs did not write
		a portable dump file due to being cross-compiled.
		To create this file at run time, execute the following command:
		\\n${EMACS_SUFFIX} --batch -Q --eval='(dump-emacs-portable
		\"/usr/libexec/emacs/${FULL_VERSION}/${CHOST}/emacs.pdmp\")'"
	readme.gentoo_create_doc

fi #SINGLE COMMON INSTALLATION

	done
}

pkg_preinst() {
	# move Info dir file to correct name
	if [[ -d ${ED}/usr/share/info ]]; then
		mv "${ED}"/usr/share/info/${EMACS_SUFFIX}/dir{.orig,} || die
	fi
}

pkg_postinst() {
	elisp-site-regen
	readme.gentoo_print_elog

	local variant_suffix
	if use multivariant; then
		setvariants
		variant_suffix=${primary_variant:+-}${primary_variant}
	fi

	if use livecd; then
		# force an update of the emacs symlink for the livecd/dvd,
		# because some microemacs packages set it with USE=livecd
		eselect emacs update
	elif [[ $(readlink "${EROOT}"/usr/bin/emacs) = ${EMACS_SUFFIX}${variant_suffix} ]]; then
		# refresh symlinks in case any installed files have changed
		eselect emacs set ${EMACS_SUFFIX}${variant_suffix}
	else
		eselect emacs update ifunset
	fi
}

pkg_postrm() {
	elisp-site-regen
	eselect emacs update ifunset
}
