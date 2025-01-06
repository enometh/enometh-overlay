# Copyright 1999-2025 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jan 06 15:41:07 2025 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2025 Madhu.  All Rights Reserved.
# ;madhu 250106 14.2.1_p20250104, from an earlier snapshot. vanilla build only.

EAPI=8

BASE_PLEVEL="14-20241221"
INCPATCHES="14-20241221-14-20241228 14-20241228-14-20250104"

TOOLCHAIN_PATCH_DEV="none"
TOOLCHAIN_HAS_TESTS="" # don't download python stuff
PATCH_GCC_VER="14.2.0"
PATCH_VER="" # 7, don't  download patch
MUSL_VER="" # 1, don't download musl stuff
PYTHON_COMPAT=( python3_{10..12} )

GCC_TARBALL_SRC_URI="bogus"
TOOLCHAIN_SET_S=no

inherit toolchain

SRC_URI="https://gcc.gnu.org/pub/gcc/snapshots/${BASE_PLEVEL}/gcc-${BASE_PLEVEL}.tar.xz"
for i in ${INCPATCHES}; do
	# gcc-14-${a%-*}-14-${a#*-}.diff.xz
	j="${i#*-}"
	SRC_URI+=" https://gcc.gnu.org/pub/gcc/snapshots/${j#*-}/diffs/gcc-${i}.diff.xz"
done
S=${WORKDIR}/gcc-${BASE_PLEVEL}

KEYWORDS="~alpha amd64 arm arm64 hppa ~loong ~mips ppc ppc64 ~riscv ~s390 sparc x86"

if [[ ${CATEGORY} != cross-* ]] ; then
	# Technically only if USE=hardened *too* right now, but no point in complicating it further.
	# If GCC is enabling CET by default, we need glibc to be built with support for it.
	# bug #830454
	RDEPEND="elibc_glibc? ( sys-libs/glibc[cet(-)?] )"
	DEPEND="${RDEPEND}"
fi

src_prepare() {
	eapply "${FILESDIR}"/${PN}-13-fix-cross-fixincludes.patch
	for i in ${INCPATCHES}; do
		eapply ${WORKDIR}/gcc-$i.diff
	done
	eapply_user

	# Make sure the pkg-config files install into multilib dirs.

	# (as gcc itself takes care of building multilibs). bug #435728
	find "${S}" -name Makefile.in \
		-exec sed -i '/^pkgconfigdir/s:=.*:=$(toolexeclibdir)/pkgconfig:' {} + || die

	setup_multilib_osdirnames
	local actual_version=$(< "${S}"/gcc/BASE-VER)
	if ! tc_is_live && [[ "${GCC_RELEASE_VER}" != "${actual_version}" ]] ; then
		eerror "'${S}/gcc/BASE-VER' contains '${actual_version}', expected '${GCC_RELEASE_VER}'"
		die "Please set 'TOOLCHAIN_GCC_PV' to '${actual_version}'"
	fi

	# Fixup libtool to correctly generate .la files with portage
	elibtoolize --portage --shallow --no-uclibc

	gnuconfig_update

	if ! is_crosscompile && ! use prefix-guest && [[ -n ${EPREFIX} ]] ; then
		einfo "Prefixifying dynamic linkers..."
		for f in gcc/config/*/*linux*.h ; do
			ebegin "  Updating ${f}"
			if [[ ${f} == gcc/config/rs6000/linux*.h ]]; then
				sed -i -r "s,(DYNAMIC_LINKER_PREFIX\s+)\"\",\1\"${EPREFIX}\",g" "${f}" || die
			else
				sed -i -r "/_DYNAMIC_LINKER/s,([\":])(/lib),\1${EPREFIX}\2,g" "${f}" || die
			fi
			eend $?
		done
	fi

	einfo "Touching generated files"
	./contrib/gcc_update --touch | \
		while read f ; do
			einfo "  ${f%%...}"
		done

}
