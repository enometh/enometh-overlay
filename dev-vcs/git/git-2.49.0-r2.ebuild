# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Sun Feb 23 11:24:28 2020 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2020-2022 Madhu.  All Rights Reserved.
#
# ;madhu 200223 2.25.0->2.25.1 # files tar files generated from git
# ;git-maint by git-archive -o TODO name these files differently, kill
# ;PLOCALES
#
# ;madhu 201129 2.29.2 - build from git. add USE=doc instead of futzing
# ;with the manual repo. EGIT_SUBMODULES=() or tries to fetch url =
# ;https://github.com/cr-marcstevens/sha1collisiondetection.git
#
# ;madhu 210321 2.31.0 - reinit on a new-workdir repo resets file
# ;mode. patch git-init
#
# ;madhu 210618 2.32.0
# ;madhu 220225 2.35.1
# ;madhu 230215 2.39.2
# ;madhu 230928 2.42.0
# ;madhu 240820 2.46.0
# ;madhu 250511 2.49.0 - no meson, fix adoc,perl installlocations, doc untested.
# ;madhu 250601 2.49.0 - fix doc.

EAPI=8

GENTOO_DEPEND_ON_PERL=no
USE_GIT=true
USE_EXPORTED_TARBALLS=false

# bug #329479: git-remote-testgit is not multiple-version aware
PYTHON_COMPAT=( python3_{10..13} )

inherit toolchain-funcs perl-module bash-completion-r1 optfeature plocale python-single-r1 systemd

#madhu PLOCALES="bg ca de es fr is it ko pt_PT ru sv vi zh_CN"
PLOCALES=""

if  ${USE_GIT} || [[ ${PV} == *9999 ]] ; then
	EGIT_REPO_URI="https://git.kernel.org/pub/scm/git/git.git"
	GITMAN_URI=https://git.kernel.org/pub/scm/git/git-manpages.git

	inherit git-r3

	if ${USE_GIT}; then
	EGIT_BRANCH="maint"
#	EGIT_COMMIT=d50a5e8939abfc07c2ff97ae72e9330939b36ee0
	EGIT_SUBMODULES=()
	EGIT_MANPAGES_COMMIT=39762a4d24093613f0246dcaaf08211e8abe554e
	EGIT_CLONE_TYPE=shallow

	KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~loong ~m68k ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86 ~x64-cygwin ~amd64-linux ~x86-linux ~ppc-macos ~x64-macos ~sparc-solaris ~sparc64-solaris ~x64-solaris ~x86-solaris"

	# Please ensure that all _four_ 9999 ebuilds get updated; they track the 4 upstream branches.
	# See https://git-scm.com/docs/gitworkflows#_graduation
	# In order of stability:
	# 9999-r0: maint
	# 9999-r1: master
	# 9999-r2: next
	# 9999-r3: seen
	case ${PVR} in
		9999) EGIT_BRANCH=maint ;;
		9999-r1) EGIT_BRANCH=master ;;
		9999-r2) EGIT_BRANCH=next;;
		9999-r3) EGIT_BRANCH=seen ;;
	esac
	fi
else

	SRC_URI_SUFFIX="xz"
	SRC_URI_KORG="https://www.kernel.org/pub/software/scm/git"

	[[ ${PV/rc} != ${PV} ]] && SRC_URI_KORG+='/testing'

if ${USE_EXPORTED_TARBALLS}; then
	:
#madhu exported via
# git config tar.tar.xz.command "xz -c"
#git archive -o /gentoo/local-portage/rodistdirs/git-manpages-2.25.1-exported.tar.xz --prefix=git-manpages-2.25.1/ remotes/origin/man
#git archive -o /gentoo/local-portage/rodistdirs/git-2.25.1-exported.tar.xz --prefix=git-2.25.1/ remotes/origin/maint
#for i in  /gentoo/local-portage/rodistdirs/git-*2.25.1-exported.tar.xz; do ln -sv $i /gentoo/distfiles/$(basename $i); done
#touch /gentoo/distfiles/git-htmldocs-2.25.1-exported.tar.xz
# Idelally USE=-doc for no html pages. but we don't want to fetch it
# at update-manifest time
	EXPORT_SUFFIX="-exported"

# done by hand not in Manifest
#	SRC_URI="${SRC_URI_KORG}/${MY_P}.tar.${SRC_URI_SUFFIX}"
#	SRC_URI+=" ${SRC_URI_KORG}/${PN}-manpages-${DOC_VER}.tar.${SRC_URI_SUFFIX}"
#	SRC_URI+=" doc? ( ${SRC_URI_KORG}/${PN}-htmldocs-${DOC_VER}.tar.${SRC_URI_SUFFIX} )"
else
	SRC_URI="${SRC_URI_KORG}/${MY_P}.tar.${SRC_URI_SUFFIX}"
	SRC_URI+=" ${SRC_URI_KORG}/${PN}-manpages-${DOC_VER}.tar.${SRC_URI_SUFFIX}"
	SRC_URI+=" doc? ( ${SRC_URI_KORG}/${PN}-htmldocs-${DOC_VER}.tar.${SRC_URI_SUFFIX} )"
fi
	if [[ ${PV} != *_rc* ]] ; then
		KEYWORDS="~alpha ~amd64 ~arm ~arm64 ~hppa ~ia64 ~loong ~m68k ~mips ~ppc ~ppc64 ~riscv ~s390 ~sparc ~x86 ~amd64-linux ~x86-linux ~arm64-macos ~ppc-macos ~x64-macos ~x64-solaris"
	fi
fi

MY_PV="${PV/_rc/.rc}"
MY_P="${PN}-${MY_PV}"

DOC_VER="${MY_PV}"

DESCRIPTION="stupid content tracker: distributed VCS designed for speed and efficiency"
HOMEPAGE="https://www.git-scm.com/"

S="${WORKDIR}"/${MY_P}

LICENSE="GPL-2"
SLOT="0"
IUSE="+blksha1 +curl cgi doc keyring +gpg highlight +iconv mediawiki +nls +pcre perforce +perl +safe-directory selinux subversion tk +webdav xinetd cvs test"

# Common to both DEPEND and RDEPEND
DEPEND="
	dev-libs/openssl:=
	sys-libs/zlib
	curl? (
		net-misc/curl
		webdav? ( dev-libs/expat )
	)
	keyring? (
		app-crypt/libsecret
		dev-libs/glib:2
	)
	iconv? ( virtual/libiconv )
	pcre? ( dev-libs/libpcre2:= )
	perl? ( dev-lang/perl:=[-build(-)] )
	tk? ( dev-lang/tk:= )
"
RDEPEND="
	${DEPEND}
	gpg? ( app-crypt/gnupg )
	perl? (
		dev-perl/Error
		dev-perl/MailTools
		dev-perl/Authen-SASL
		>=virtual/perl-libnet-3.110.0-r4[ssl]
		cgi? (
			dev-perl/CGI
			highlight? ( app-text/highlight )
		)
		cvs? (
			>=dev-vcs/cvsps-2.1:0
			dev-perl/DBI
			dev-perl/DBD-SQLite
		)
		mediawiki? (
			dev-perl/DateTime-Format-ISO8601
			dev-perl/HTML-Tree
			dev-perl/MediaWiki-API
		)
		subversion? (
			dev-vcs/subversion[-dso(-),perl]
			dev-perl/libwww-perl
			dev-perl/TermReadKey
		)
	)
	perforce? ( ${PYTHON_DEPS} )
	selinux? ( sec-policy/selinux-git )
"

# This is how info docs are created with Git:
#   .txt/asciidoc --(asciidoc)---------> .xml/docbook
#   .xml/docbook  --(docbook2texi.pl)--> .texi
#   .texi         --(makeinfo)---------> .info
BDEPEND="
	doc? (
		app-text/asciidoc
		app-text/docbook2X
		app-text/xmlto
		sys-apps/texinfo
	)
	keyring? ( virtual/pkgconfig )
	nls? ( sys-devel/gettext )
	test? (	app-crypt/gnupg	)
"

# Live ebuild builds man pages and HTML docs, additionally
if [[ ${PV} == *9999 ]]; then
	BDEPEND+=" app-text/asciidoc"
fi

SITEFILE="50${PN}-gentoo.el"

REQUIRED_USE="
	cgi? ( perl )
	cvs? ( perl )
	mediawiki? ( perl )
	perforce? ( ${PYTHON_REQUIRED_USE} )
	subversion? ( perl )
	webdav? ( curl )
"

RESTRICT="!test? ( test )"

PATCHES=(
	# Avoid automagic CVS, bug #350330
#	"${FILESDIR}"/git-2.37.0_rc1-optional-cvs.patch

	# Make submodule output quiet
#	"${FILESDIR}"/git-2.21.0-quiet-submodules-testcase.patch

	${FILESDIR}/git-2.39.2-git-instaweb-preserve-gitweb_config.perl-if-it-exist.patch
	${FILESDIR}/git-2.49.0-init-don-t-reset-core.filemode-on-git-new-workdirs.patch
)

pkg_setup() {
	if use subversion && has_version "dev-vcs/subversion[dso]" ; then
		ewarn "Per Gentoo bugs #223747, #238586, when subversion is built"
		ewarn "with USE=dso, there may be weird crashes in git-svn. You"
		ewarn "have been warned!"
	fi

	if use perforce ; then
		python-single-r1_pkg_setup
	fi
}

# This is needed because for some obscure reasons future calls to make don't
# pick up these exports if we export them in src_unpack()
exportmakeopts() {
	local extlibs myopts

	myopts=(
		ASCIIDOC_NO_ROFF=YesPlease

		$(usex perl 'INSTALLDIRS=vendor NO_PERL_CPAN_FALLBACKS=YesPlease' NO_PERL=YesPlease)

		$(usev elibc_musl NO_REGEX=NeedsStartEnd)
		$(usev !cvs NO_CVS=YesPlease)
		$(usev !iconv NO_ICONV=YesPlease)
		$(usev !nls NO_GETTEXT=YesPlease)
		$(usev !perforce NO_PYTHON=YesPlease)
		$(usev !subversion NO_SVN_TESTS=YesPlease)
		$(usev !tk NO_TCLTK=YesPlease)
	)

	if use blksha1 ; then
		myopts+=( BLK_SHA1=YesPlease )
	fi

	if use curl ; then
		use webdav || myopts+=( NO_EXPAT=YesPlease )
	else
		myopts+=( NO_CURL=YesPlease )
	fi

	# Broken assumptions because of static build system
	myopts+=(
		NO_FINK=YesPlease
		NO_DARWIN_PORTS=YesPlease
		INSTALL=install
		TAR=tar
		SHELL_PATH="${EPREFIX}/bin/sh"
		SANE_TOOL_PATH=
		OLD_ICONV=
		NO_EXTERNAL_GREP=
	)

	# Can't define this to null, since the entire makefile depends on it
	sed -i -e '/\/usr\/local/s/BASIC_/#BASIC_/' Makefile || die

	if use pcre; then
		myopts+=( USE_LIBPCRE2=YesPlease )
		extlibs+=( -lpcre2-8 )
	fi

	if [[ ${CHOST} == *-solaris* ]]; then
		myopts+=(
			NEEDS_LIBICONV=YesPlease
			HAVE_CLOCK_MONOTONIC=1
		)

		if grep -Fq getdelim "${ESYSROOT}"/usr/include/stdio.h ; then
			myopts+=( HAVE_GETDELIM=1 )
		fi
	fi

	myopts+=( ASCIIDOC8=YesPlease )

	export MY_MAKEOPTS="${myopts[@]}"
	export EXTLIBS="${extlibs[@]}"
}

src_unpack() {
	if [[ ${PV} != *9999 ]] && ! ${USE_GIT} ; then
if ${USE_EXPORTED_TARBALLS}; then
		unpack ${MY_P}${EXPORT_SUFFIX}.tar.${SRC_URI_SUFFIX}
else
			unpack ${MY_P}.tar.${SRC_URI_SUFFIX}
fi

		cd "${S}" || die
if ${USE_EXPORTED_TARBALLS}; then
		unpack ${PN}-manpages-${DOC_VER}${EXPORT_SUFFIX}.tar.${SRC_URI_SUFFIX}
else
		unpack ${PN}-manpages-${DOC_VER}.tar.${SRC_URI_SUFFIX}
fi

		if use doc ; then
			pushd "${S}"/Documentation &>/dev/null || die
if ${USE_EXPORTED_TARBALLS}; then
			unpack ${PN}-htmldocs-${DOC_VER}${EXPORT_SUFFIX}.tar.${SRC_URI_SUFFIX}
else
			unpack ${PN}-htmldocs-${DOC_VER}.tar.${SRC_URI_SUFFIX}
fi
			popd &>/dev/null || die
		fi
	else
		git-r3_src_unpack

#		cp "${FILESDIR}"/GIT-VERSION-GEN .

		# setting the remote ref to the ${EGIT_MANPAGES_COMMIT} does not
		# work because it still checks out EGIT_BRANCH. git-r3
		# unconditionally sets branch refs/heads/{branch_name} so it
		# can't be refs/remotes/origin/man.

		if ${USE_GIT} ; then
#;madhu 250601
			EGIT_BRANCH=master EGIT_COMMIT=${EGIT_MANPAGES_COMMIT} EGIT_REPO_URI=$GITMAN_URI EGIT_CHECKOUT_DIR=${WORKDIR}/${P}/manpages git-r3_src_unpack

# override  EGIT_OVERRIDE_REPO_GIT_GIT_MANPAGES
#   EGIT_OVERRIDE_BRANCH_GIT_GIT_MANPAGES
#   EGIT_OVERRIDE_COMMIT_GIT_GIT_MANPAGES
#   EGIT_OVERRIDE_COMMIT_DATE_GIT_GIT_MANPAGES

# this  was from when manpages were on a man branch on the main git repo.
#			unset EGIT_OVERRIDE_BRANCH_GIT_GIT
#			EGIT_BRANCH=man
#			git-r3_fetch ${EGIT_REPO_URI}  ${EGIT_MANPAGES_COMMIT} ${CATEGORY}/${PN}/manpages/${SLOT%/*}
#			git-r3_checkout ${EGIT_REPO_URI} ${WORKDIR}/${P}/manpages  ${CATEGORY}/${PN}/manpages/${SLOT%/*}
			(cd ${WORKDIR}/${P} ;
			 for i in manpages/man? ; do ln -sv $i; done)
		fi

	fi

}

src_prepare() {
	if ! use safe-directory ; then
		# This patch neuters the "safe directory" detection.
		# bugs #838271, #838223
		PATCHES+=(
			"${FILESDIR}"/git-2.46.2-unsafe-directory.patch
		)
	fi

	default

	if use prefix ; then
		# bug #757309
		sed -i \
			-e 's:/usr/local/opt/gettext/:/do/not/look/elsewhere/:g' \
			-e 's:/opt/homebrew/:/do/not/look/elsewhere/:g' \
			config.mak.uname || die
	fi

	sed -i \
		-e 's:^\(CFLAGS[[:space:]]*=\).*$:\1 $(OPTCFLAGS) -Wall:' \
		-e 's:^\(LDFLAGS[[:space:]]*=\).*$:\1 $(OPTLDFLAGS):' \
		-e 's:^\(CC[[:space:]]* =\).*$:\1$(OPTCC):' \
		-e 's:^\(AR[[:space:]]* =\).*$:\1$(OPTAR):' \
		-e "s:\(PYTHON_PATH[[:space:]]\+=[[:space:]]\+\)\(.*\)$:\1${EPREFIX}\2:" \
		-e "s:\(PERL_PATH[[:space:]]\+=[[:space:]]\+\)\(.*\)$:\1${EPREFIX}\2:" \
		Makefile || die

	# Fix docbook2texi command
	sed -r -i 's/DOCBOOK2X_TEXI[[:space:]]*=[[:space:]]*docbook2x-texi/DOCBOOK2X_TEXI = docbook2texi.pl/' \
		Documentation/Makefile || die

	# make the .dirty bit go away
	# ;madhu 250511 fix "build meson fixes" commit e40622a60b
	sed -i -e 's|--dirty||g' GIT-VERSION-GEN

}

git_emake() {
	# bug #320647: PYTHON_PATH
	local PYTHON_PATH=""
	use perforce && PYTHON_PATH="${PYTHON}"

	local mymakeargs=(
		${MY_MAKEOPTS}

		prefix="${EPREFIX}"/usr
		htmldir="${EPREFIX}"/usr/share/doc/${PF}/html
		perllibdir="$(use perl && perl_get_raw_vendorlib)"
		sysconfdir="${EPREFIX}"/etc

		CC="$(tc-getCC)"
		CFLAGS="${CFLAGS}"
		PKG_CONFIG="$(tc-getPKG_CONFIG)"

		# TODO: --verbose?
		GIT_TEST_OPTS="--no-color"

		OPTAR="$(tc-getAR)"
		OPTCC="$(tc-getCC)"
		OPTCFLAGS="${CFLAGS}"
		OPTLDFLAGS="${LDFLAGS}"

		PERL_PATH="${EPREFIX}/usr/bin/perl"
		PERL_MM_OPT=""

		PYTHON_PATH="${PYTHON_PATH}"

		V=1
		"$@"
	)

	emake "${mymakeargs[@]}"
}

src_configure() {
	exportmakeopts
}

src_compile() {
	git_emake

	if use perl && use cgi ; then
		git_emake gitweb
	fi

	if [[ ${CHOST} == *-darwin* ]] && tc-is-clang ; then
		git_emake -C contrib/credential/osxkeychain
	fi

	pushd Documentation &>/dev/null || die
	if [[ ${PV} == *9999 ]] ; then # XXX USE_GIT?
		git_emake man
		if use doc ; then
			git_emake info html
		fi
	else
		if use doc ; then
			git_emake info
		fi
	fi
	popd &>/dev/null || die

	if use keyring ; then
		git_emake -C contrib/credential/libsecret
	fi

	pushd contrib/subtree &>/dev/null || die
	git_emake git-subtree
	# git-subtree.1 requires the full USE=doc dependency stack
	use doc && git_emake git-subtree.html git-subtree.1
	popd &>/dev/null || die

	git_emake -C contrib/diff-highlight

	if use mediawiki ; then
		git_emake -C contrib/mw-to-git
	fi
}

src_test() {
	local disabled=(
		# We make safe-directory optional
		t0033-safe-directory.sh
	)

	local tests_cvs=(
		t9200-git-cvsexportcommit.sh
		t9400-git-cvsserver-server.sh
		t9401-git-cvsserver-crlf.sh
		t9402-git-cvsserver-refs.sh
		t9600-cvsimport.sh
		t9601-cvsimport-vendor-branch.sh
		t9602-cvsimport-branches-tags.sh
		t9603-cvsimport-patchsets.sh
		t9604-cvsimport-timestamps.sh
	)

	local tests_perl=(
		t3701-add-interactive.sh
		t5502-quickfetch.sh
		t5512-ls-remote.sh
		t5520-pull.sh
		t7106-reset-unborn-branch.sh
		t7501-commit.sh
	)
	# Bug #225601 - t0004 is not suitable for root perm
	# Bug #219839 - t1004 is not suitable for root perm
	# t0001-init.sh - check for init notices EPERM*  fails
	local tests_nonroot=(
		t0001-init.sh
		t0004-unwritable.sh
		t0070-fundamental.sh
		t1004-read-tree-m-u-wf.sh
		t3700-add.sh
		t7300-clean.sh
	)
	# t9100 still fails with symlinks in SVN 1.7
	local test_svn=( t9100-git-svn-basic.sh )

	# Unzip is used only for the testcase code, not by any normal parts of Git.
	if ! has_version app-arch/unzip ; then
		einfo "Disabling tar-tree tests"
		disabled+=( t5000-tar-tree.sh )
	fi

	local cvs=0
	use cvs && let cvs=${cvs}+1
	if [[ ${EUID} -eq 0 ]] ; then
		if [[ ${cvs} -eq 1 ]] ; then
			ewarn "Skipping CVS tests because CVS does not work as root!"
			ewarn "You should retest with FEATURES=userpriv!"
			disabled+=( ${tests_cvs[@]} )
		fi
		einfo "Skipping other tests that require being non-root"
		disabled+=( ${tests_nonroot[@]} )
	else
		if [[ ${cvs} -gt 0 ]] && has_version dev-vcs/cvs ; then
			let cvs=${cvs}+1
		fi

		if [[ ${cvs} -gt 1 ]] && has_version "dev-vcs/cvs[server]" ; then
			let cvs=${cvs}+1
		fi

		if [[ ${cvs} -lt 3 ]] ; then
			einfo "Disabling CVS tests (needs dev-vcs/cvs[USE=server])"
			disabled+=( ${tests_cvs[@]} )
		fi
	fi

	if ! use perl ; then
		einfo "Disabling tests that need Perl"
		disabled+=( ${tests_perl[@]} )
	fi

	einfo "Disabling tests that fail with SVN 1.7"
	disabled+=( ${test_svn[@]} )

	# Reset all previously disabled tests
	pushd t &>/dev/null || die
	local i
	for i in *.sh.DISABLED ; do
		[[ -f "${i}" ]] && mv -f "${i}" "${i%.DISABLED}"
	done
	einfo "Disabled tests:"
	for i in ${disabled[@]} ; do
		if [[ -f "${i}" ]] ; then
			mv -f "${i}" "${i}.DISABLED" && einfo "Disabled ${i}"
		fi
	done

	# Avoid the test system removing the results because we want them ourselves
	sed -e '/^[[:space:]]*$(MAKE) clean/s,^,#,g' -i Makefile || die

	# Clean old results first, must always run
	nonfatal git_emake clean
	popd &>/dev/null || die

	# Now run the tests, keep going if we hit an error, and don't terminate on
	# failure
	local rc
	einfo "Start test run"
	#MAKEOPTS=-j1
	nonfatal git_emake --keep-going test
	rc=$?

	# Display nice results, now print the results
	pushd t &>/dev/null || die
	nonfatal git_emake aggregate-results

	# And bail if there was a problem
	[[ ${rc} -eq 0 ]] || die "Tests failed. Please file a bug!"
}

src_install() {
	git_emake DESTDIR="${D}" install

	if [[ ${CHOST} == *-darwin* ]] && tc-is-clang ; then
		dobin contrib/credential/osxkeychain/git-credential-osxkeychain
	fi

	# Depending on the tarball and manual rebuild of the documentation, the
	# manpages may exist in either OR both of these directories.
	find man?/*.[157] >/dev/null 2>&1 && doman man?/*.[157]
	find Documentation/*.[157] >/dev/null 2>&1 && doman Documentation/*.[157]
	dodoc README* Documentation/{SubmittingPatches,CodingGuidelines}
	use doc && dodir /usr/share/doc/${PF}/html
	local d
	for d in / /howto/ /technical/ ; do
		docinto ${d}
		dodoc Documentation${d}*.adoc
		if use doc ; then
			docinto ${d}/html
			dodoc Documentation${d}*.html
		fi
	done
	docinto /
	# Upstream does not ship this pre-built :-(
	use doc && doinfo Documentation/{git,gitman}.info

	newbashcomp contrib/completion/git-completion.bash ${PN}
	bashcomp_alias git gitk
	# Not really a bash-completion file (bug #477920)
	# but still needed uncompressed (bug #507480)
	insinto /usr/share/${PN}
	doins contrib/completion/git-prompt.sh

	#dobin contrib/fast-import/git-p4 # Moved upstream
	#dodoc contrib/fast-import/git-p4.txt # Moved upstream
	newbin contrib/fast-import/import-tars.perl import-tars
	exeinto /usr/libexec/git-core/
	newexe contrib/git-resurrect.sh git-resurrect

	# git-subtree
	pushd contrib/subtree &>/dev/null || die
	git_emake DESTDIR="${D}" install
	if use doc ; then
		# Do not move git subtree install-man outside USE=doc!
		git_emake DESTDIR="${D}" install-man install-html
	fi
	newdoc README README.git-subtree
	dodoc git-subtree.adoc
	popd &>/dev/null || die

	if use mediawiki ; then
		git_emake -C contrib/mw-to-git DESTDIR="${D}" install
	fi

	# diff-highlight
	dobin contrib/diff-highlight/diff-highlight
	newdoc contrib/diff-highlight/README README.diff-highlight

	# git-jump
	exeinto /usr/libexec/git-core/
	doexe contrib/git-jump/git-jump
	newdoc contrib/git-jump/README git-jump.adoc

	# git-contacts
	exeinto /usr/libexec/git-core/
	doexe contrib/contacts/git-contacts
	dodoc contrib/contacts/git-contacts.adoc

	if use keyring ; then
		pushd contrib/credential/libsecret &>/dev/null || die
		dobin git-credential-libsecret
		popd &>/dev/null || die
	fi

	dodir /usr/share/${PN}/contrib
	# The following are excluded:
	# completion - installed above
	# diff-highlight - done above
	# emacs - removed upstream
	# examples - these are stuff that is not used in Git anymore actually
	# git-jump - done above
	# gitview - installed above
	# p4import - excluded because fast-import has a better one
	# patches - stuff the Git guys made to go upstream to other places
	# persistent-https - TODO
	# mw-to-git - TODO
	# subtree - built seperately
	# svnimport - use git-svn
	# thunderbird-patch-inline - fixes thunderbird
	local contrib_objects=(
		buildsystems
		fast-import
		hooks
		remotes2config.sh
		rerere-train.sh
		stats
		workdir
	)
	local i
	for i in "${contrib_objects[@]}" ; do
		cp -rf "${S}"/contrib/${i} "${ED}"/usr/share/${PN}/contrib || die "Failed contrib ${i}"
	done

	if use cgi ; then
		# We used to install in /usr/share/${PN}/gitweb
		# but upstream installs in /usr/share/gitweb
		# so we will install a symlink and use their location for compat with other
		# distros
		dosym ../gitweb /usr/share/${PN}/gitweb

		# INSTALL discusses configuration issues, not just installation
		docinto /
		newdoc  "${S}"/gitweb/INSTALL INSTALL.gitweb
		newdoc  "${S}"/gitweb/README README.gitweb

		for d in "${ED}"/usr/lib{,64}/perl5/ ; do
			if [[ -d "${d}" ]] ; then
				find "${d}" -name .packlist -delete || die
			fi
		done
	else
		rm -rf "${ED}"/usr/share/gitweb
	fi
	#;madhu 250511 - make install apparentlyinstalls to the right locatiion.
	if false; then
	if use perl ; then
		dodir "$(perl_get_vendorlib)"
		mv "${ED}"/usr/share/perl5/Git.pm "${ED}/$(perl_get_vendorlib)" || die
		mv "${ED}"/usr/share/perl5/Git "${ED}/$(perl_get_vendorlib)" || die
	fi
	fi

	if use mediawiki ; then
		git_emake -C contrib/mw-to-git DESTDIR="${D}" install
	fi

	if ! use subversion ; then
		rm -f "${ED}"/usr/libexec/git-core/git-svn \
			"${ED}"/usr/share/man/man1/git-svn.1*
	fi

	if use xinetd ; then
		insinto /etc/xinetd.d
		newins "${FILESDIR}"/git-daemon.xinetd git-daemon
	fi

	if ! use prefix ; then
		newinitd "${FILESDIR}"/git-daemon-r2.initd git-daemon
		newconfd "${FILESDIR}"/git-daemon.confd git-daemon
		systemd_newunit "${FILESDIR}/git-daemon_at-r1.service" "git-daemon@.service"
		systemd_dounit "${FILESDIR}/git-daemon.socket"
	fi

	perl_delete_localpod

	# Remove disabled linguas
	# we could remove sources in src_prepare, but install does not
	# handle missing locale dir well
	rm_loc() {
		if [[ -e "${ED}/usr/share/locale/${1}" ]] ; then
			rm -r "${ED}/usr/share/locale/${1}" || die
		fi
	}
	plocale_for_each_disabled_locale rm_loc
}

pkg_postinst() {
	if [[ -z ${REPLACING_VERSIONS} ]] ; then
		elog "Please read /usr/share/bash-completion/completions/git for Git bash command"
		elog "completion."
		elog "Please read /usr/share/git/git-prompt.sh for Git bash prompt"
		elog "Note that the prompt bash code is now in that separate script"
	fi

	optfeature_header "Some scripts require additional dependencies:"
	optfeature git-quiltimport dev-util/quilt
	optfeature git-instaweb www-servers/lighttpd www-servers/apache www-servers/nginx
}
