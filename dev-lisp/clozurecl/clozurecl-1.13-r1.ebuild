# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Wed Jan 30 21:43:07 2019 +0530 <enometh@meer.net>
#   Bugs-To: enometh@meer.net
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2019-2021 Madhu.  All Rights Reserved.
#
# ;madhu 190322 - 1.12_pre4
# ;madhu 191127 - 1.12_pre5
# ;madhu 200330 - use git
# ;madhu 210921 - 1.12.1 - amd64 only - use git, no asdf, bootstrap from
# ;               installed version, doc in dev-lisp/ccldoc, BP1
# ;               bootstrap patch
# ;
# ;madhu 230928 - 1.12.2 GIT  v1.12.2-10-g7c2ebad
# ;madhu 240918 - 1.13.0 GIT
# ;madhu 250807 - 1.13.1-r1  fix typos
#
#
# "eclass" notes
#
# USE_GIT - boolean static variable. like "9999" in `P' but lets us use
# a regular `PV' and a local git repository to build the package
# from. "static" means it has to be defined within the ebuild itself,
# and is needed before the Manifest file is generated.
#
# USE_BOOTSTRAP_INSTALLED - boolean static variable. if true use an
# installed version of ${PN} to bootstrap the new version instead of
# downloading a binary package.

EAPI=8

USE_GIT=true
USE_BOOTSTRAP_INSTALLED=true

MY_GIT_COMMIT="842515360d84b2f4703a27856ba94a7c93f6edc4"

inherit multilib toolchain-funcs vcs-clean

MY_PN=ccl
#MY_P=${MY_PN}-${PV} #not used

# support ebuilds named as dev-lisp/closurecl-1.12_pre5.ebuild:
#
#<gyakovlev#gentoo-dev-help[07:24:27]> ...: MY_PV="${PV//-dev./_pre}" and use MY_PV instead of PV in the ebuild, should work.
#
#the source package is at github.com/Clozure/ccl/archive/v1.12-dev.5.tar.gz
#the binary package is at github.com/Clozure/ccl/releases/download/v1.12-dev.5/linuxx86.tar.gz
#in upstream language i'm compiling ccl-1.12-dev.5 sources from ccl-1.12-dev.5 binaries

# USE_BOOTSTRAP_INSTALLED - copy x86-headers and xdump from the
# installed clozurecl version instead of fetching a binary snapshot.

x=${PV}
if [[ $x =~ pre ]]; then
x=$(ver_rs 2 - $x)
x=$(ver_rs 3 . $x)
x=${x/-pre./-dev.}
fi

my_base=${x%%.$(ver_cut 4)}
PLVL=$(ver_cut 4)
PLVL=${PLVL:+.${PLVL}}

DESCRIPTION="Common Lisp implementation, derived from Digitool's MCL product"
HOMEPAGE="https://ccl.clozure.com"

SRC_URI=""
if ${USE_GIT}; then
	inherit git-r3
#	EGIT_REPO_URI="https://github.com/Clozure/ccl.git"
	EGIT_REPO_URI="https://github.com/enometh/ccl.git"

	# control GIT variables:
	# EGIT_BRANCH=master
	# EGIT_SUBMODULES=()
	# EGIT_CLONE_TYPE=shallow

	# or override via etc/porage/{package.,env}
	# EGIT_OVERRIDE_REPO_CLOZURE_CCL=/build/git-mirror/ccl.git
	# EGIT_OVERRIDE_BRANCH_CLOZURE_CCL=madhu-tip
	# EGIT_OVERRIDE_BRANCH_CLOZURE_CCL
	# EGIT_OVERRIDE_COMMIT_CLOZURE_CCL

	EGIT_OVERRIDE_BRANCH_ENOMETH_CCL=madhu-tip
	if [ -n $MY_GIT_COMMIT]; then
		EGIT_COMMIT=$MY_GIT_COMMIT
	fi

	#;madhu 230928 TODO may need to manually copy shallow to $GIT_DIR
	#EGIT_OVERRIDE_CLONE_TYPE_ENOMETH_CCL=shallow

	# XXX set the git checkout dir to the ${S} directory that the source
	# tarball would unpack into
	EGIT_CHECKOUT_DIR="${WORKDIR}/${MY_PN}-${x}"
else
	SRC_URI+="
		x86? (
			https://github.com/Clozure/ccl/archive/v${my_base}.4.tar.gz -> ${P}.tar.gz
		)
		amd64? (
			https://github.com/Clozure/ccl/archive/v${my_base}.4.tar.gz -> ${P}.tar.gz
		)"
fi

if ! ${USE_BOOTSTRAP_INSTALLED}; then 	# use the binary snapshots
	SRC_URI+="
		x86? (
			https://github.com/Clozure/ccl/releases/download/v${my_base}$PLVL/linuxx86.tar.gz -> ${P}-linuxx86.tar.gz
		)
		amd64? (
			https://github.com/Clozure/ccl/releases/download/v${my_base}$PLVL/linuxx86.tar.gz -> ${P}-linuxx86.tar.gz
		)"
fi

# unsupported here but in gentoo
# 	"x86? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-linuxx86.tar.gz )
# 	amd64? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-linuxx86.tar.gz )
# 	arm? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-linuxarm.tar.gz )
# 	x86-macos? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-darwinx86.tar.gz )
# 	x64-macos? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-darwinx86.tar.gz )
#
# 	x86-solaris? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-solarisx86.tar.gz )
# 	x64-solaris? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-solarisx86.tar.gz )
#	x64-macos? ( https://github.com/Clozure/ccl/releases/download/v${PV}/${MY_P}-darwinx86.tar.gz )

# get doc via dev-lisp/ccldoc package
# doc? ( https://ccl.clozure.com/docs/ccl.html )

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="-* ~amd64 ~x86 ~amd64-linux ~x86-linux ~x64-macos"
IUSE="doc"

#;madhu 190130 - no asdf
#RDEPEND=">=dev-lisp/asdf-2.33-r3:="
#DEPEND="${RDEPEND}"

if ${USE_BOOTSTRAP_INSTALLED}; then
	DEPEND+=" dev-lisp/clozurecl "
	DEPEND+=" net-misc/rsync "	#  TODO use cp -r instead
# xattr
fi

# if use doc; then DEPEND+="dev-lisp/ccldoc"; fi

S="${WORKDIR}"/${MY_PN}-${x}
ENVD="${T}/50ccl"

PATCHES=(
)

# bootstrap patch
BP1=clozurecl-1.2.1-parse-macro-2.patch.lisp

src_unpack () {
	default_src_unpack
	if ${USE_GIT}; then
		git-r3_src_unpack
	fi
	if ! ${USE_BOOTSTRAP_INSTALLED}; then
	   cd "${S}"
	   mv -iv "${S}/../"* .
	fi
}

src_prepare() {
	default
# no asdf
#	cp "${EPREFIX}/usr/share/common-lisp/source/asdf/build/asdf.lisp" tools/ || die
	if ${USE_BOOTSTRAP_INSTALLED}; then
		if use x86; then
			rsync -aHivOJ ${EROOR}/usr/$(get_libdir)/${PN}/{x86-headers,lx86cl{,.image}} . || die
		fi
		if use amd64; then
			rsync -aHivOJ ${EROOT}/usr/$(get_libdir)/${PN}/{x86-headers64,lx86cl64{,.image}} . || die
		fi
	fi
	cp ${FILESDIR}/${BP1} . -apiv
}

src_configure() {
	if use x64-macos; then
		CCL_RUNTIME=dx86cl64; CCL_HEADERS=darwin-x86-headers64; CCL_KERNEL=darwinx8664
	elif use x86; then
		CCL_RUNTIME=lx86cl; CCL_HEADERS=x86-headers; CCL_KERNEL=linuxx8632
		if use amd64; then
			die "hey gentoo?- what about multilib with x86 and amd64?"
		fi
	elif use amd64; then
		CCL_RUNTIME=lx86cl64; CCL_HEADERS=x86-headers64; CCL_KERNEL=linuxx8664
	fi
}

src_compile() {
	emake -C lisp-kernel/${CCL_KERNEL} clean
	emake -C lisp-kernel/${CCL_KERNEL} all CC="$(tc-getCC)"

	unset CCL_DEFAULT_DIRECTORY
	./${CCL_RUNTIME} -n -b -Q -e \
	 "(ccl:set-development-environment)" -e \
	 "(load \"$BP1\")" -e \
	 '(ccl:rebuild-ccl :full t)' -e '(ccl:quit)' || die "Compilation failed"

	# remove non-owner write permissions on the full-image
	chmod go-w ${CCL_RUNTIME}{,.image} || die

	esvn_clean
}

src_install() {
	local target_dir="/usr/$(get_libdir)/${PN}"
	local prefix_dir="${EPREFIX}/${target_dir#/}"

	mkdir -p "${D}/${prefix_dir#/}"

	find . -type f -name '*fsl' -delete || die
	rm -f lisp-kernel/${CCL_KERNEL}/*.o || die
	cp -a compiler level-0 level-1 lib library lisp-kernel scripts \
		tools xdump ${CCL_HEADERS} ${CCL_RUNTIME} ${CCL_RUNTIME}.image \
		"${D}/${prefix_dir#/}" || die

	echo "CCL_DEFAULT_DIRECTORY=${prefix_dir}" > "${ENVD}"
	doenvd "${ENVD}"

	#;madhu 240918 prefix_dir instead of target_dir ?
	dosym "${prefix_dir}/${CCL_RUNTIME}" /usr/bin/ccl
	dodoc doc/release-notes.txt

	if use doc ; then
#		dodoc "${DISTDIR}/ccl.html"
		dodoc -r doc/manual
		dodoc -r examples
	fi
}
