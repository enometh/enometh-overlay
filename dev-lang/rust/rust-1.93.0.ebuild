# Copyright 1999-2026 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
#   Time-stamp: <>
#   Touched: Mon Jan 26 18:41:44 2026 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2026 Madhu.  All Rights Reserved.
#
#  ;madhu 260126 1.93.0 assume system-llvm in /opt/llvm-LLVMVER. cannot
#  use existing RUSTC, so do a bootstrap build
EAPI=8

LLVM_COMPAT=( 21 )
PYTHON_COMPAT=( python3_{11..14} )
#LLVMVER=20.1.8
LLVMVER=21.1.8

RUST_MAX_VER=${PV%%_*}
RUST_PV=${PV%%_p*}
RUST_P=${PN}-${RUST_PV}

# bypass all rust checking
rust_check_deps() { return 1; }

#  llvm-r1
inherit check-reqs estack flag-o-matic multiprocessing optfeature
inherit multilib multilib-build python-any-r1 rust rust-toolchain toolchain-funcs
inherit verify-sig

BOOT_DATE=2025-12-11
BOOT_PV=1.92.0

# curlget.sh static.rust-lang.org/dist/channel-rust-1.92.0.toml
if true; then
	MY_P="rustc-${RUST_PV}"

	SRC_URI="
		https://static.rust-lang.org/dist/$BOOT_DATE/cargo-${BOOT_PV}-x86_64-unknown-linux-gnu.tar.xz
		https://static.rust-lang.org/dist/$BOOT_DATE/rust-std-${BOOT_PV}-x86_64-unknown-linux-gnu.tar.xz
		https://static.rust-lang.org/dist/$BOOT_DATE/rustc-${BOOT_PV}-x86_64-unknown-linux-gnu.tar.xz
		https://static.rust-lang.org/dist/rustc-${RUST_PV}-src.tar.xz
	"
	S="${WORKDIR}/${MY_P}-src"

	KEYWORDS="~amd64 ~arm ~arm64 ~loong ~mips ~ppc ~ppc64 ~riscv ~sparc ~x86"
fi

DESCRIPTION="Systems programming language originally developed by Mozilla"
HOMEPAGE="https://www.rust-lang.org/"

# keep in sync with llvm ebuild of the same version as bundled one.
ALL_LLVM_TARGETS=( AArch64 AMDGPU ARC ARM AVR BPF CSKY DirectX Hexagon Lanai )
ALL_LLVM_TARGETS+=( LoongArch M68k Mips MSP430 NVPTX PowerPC RISCV Sparc SPIRV )
ALL_LLVM_TARGETS+=( SystemZ VE WebAssembly X86 XCore Xtensa )
ALL_LLVM_TARGETS=( "${ALL_LLVM_TARGETS[@]/#/llvm_targets_}" )
LLVM_TARGET_USEDEPS=${ALL_LLVM_TARGETS[@]/%/(-)?}

# https://github.com/rust-lang/llvm-project/blob/rustc-1.87.0/llvm/CMakeLists.txt
_ALL_RUST_EXPERIMENTAL_TARGETS=( ARC CSKY DirectX M68k Xtensa )
declare -A ALL_RUST_EXPERIMENTAL_TARGETS
for _x in "${_ALL_RUST_EXPERIMENTAL_TARGETS[@]}"; do
	ALL_RUST_EXPERIMENTAL_TARGETS["llvm_targets_${_x}"]=0
done

# Bare metal targets which can be built on the host system and have no
# dependency on compiler runtime, libc and unwinder.
ALL_RUST_SYSROOTS=( bpf wasm )
ALL_RUST_SYSROOTS=( "${ALL_RUST_SYSROOTS[@]/#/rust_sysroots_}" )

LICENSE="|| ( MIT Apache-2.0 ) BSD BSD-1 BSD-2 BSD-4"
SLOT="${PV%%_*}" # Beta releases get to share the same SLOT as the eventual stable

IUSE="big-endian clippy cpu_flags_x86_sse2 debug dist doc llvm-libunwind lto"
IUSE+=" system-llvm"
IUSE+=" rustfmt rust-analyzer rust-src test"
IUSE+=" ${ALL_LLVM_TARGETS[*]} ${ALL_RUST_SYSROOTS[*]}"

LLVM_DEPEND=( dev-antichrist/llvmorg )

# dev-libs/oniguruma is used for documentation
# dev-libs/oniguruma use bundled?
BDEPEND="
	${PYTHON_DEPS}
	app-eselect/eselect-rust
"

DEPEND="
	>=app-arch/xz-utils-5.2
	net-misc/curl:=[http2,ssl]
	virtual/zlib:=
	dev-libs/openssl:0=
"

# 	dev-lang/rust-common
RDEPEND="
	${DEPEND}
	app-eselect/eselect-rust
	sys-apps/lsb-release
	!dev-lang/rust:stable
	!dev-lang/rust-bin:stable
"

REQUIRED_USE="
	|| ( ${ALL_LLVM_TARGETS[*]} )
	rust-analyzer? ( rust-src )
	test? ( ${ALL_LLVM_TARGETS[*]} )
	rust_sysroots_bpf? ( llvm_targets_BPF )
	rust_sysroots_wasm? ( llvm_targets_WebAssembly )
	x86? ( cpu_flags_x86_sse2 )
"

# we don't use cmake.eclass, but can get a warning
CMAKE_WARN_UNUSED_CLI=no

QA_FLAGS_IGNORED="
	usr/lib/${PN}/${SLOT}/bin/.*
	usr/lib/${PN}/${SLOT}/libexec/.*
	usr/lib/${PN}/${SLOT}/lib/lib.*.so
	usr/lib/${PN}/${SLOT}/lib/rustlib/.*/bin/.*
	usr/lib/${PN}/${SLOT}/lib/rustlib/.*/lib/lib.*.so
"

QA_SONAME="
	usr/lib/${PN}/${SLOT}/lib/lib.*.so.*
	usr/lib/${PN}/${SLOT}/lib/rustlib/.*/lib/lib.*.so
"

QA_PRESTRIPPED="
	usr/lib/${PN}/${SLOT}/lib/rustlib/.*/bin/rust-llvm-dwp
	usr/lib/${PN}/${SLOT}/lib/rustlib/.*/lib/self-contained/crtn.o
"

# An rmeta file is custom binary format that contains the metadata for the crate.
# rmeta files do not support linking, since they do not contain compiled object files.
# so we can safely silence the warning for this QA check.
QA_EXECSTACK="usr/lib/${PN}/${SLOT}/lib/rustlib/*/lib*.rlib:lib.rmeta"

# causes double bootstrap
RESTRICT="test"

clear_vendor_checksums() {
	sed -i 's/\("files":{\)[^}]*/\1/' "vendor/${1}/.cargo-checksum.json" || die
}

toml_usex() {
	usex "${1}" true false
}

pre_build_checks() {
	local M=9216
	# multiply requirements by 1.3 if we are doing x86-multilib
	if use amd64; then
		M=$(( $(usex abi_x86_32 13 10) * ${M} / 10 ))
	fi
	M=$(( $(usex clippy 128 0) + ${M} ))
	if [[ ${PV} == *9999* ]]; then
		M=$(( $(usex miri 128 0) + ${M} ))
	fi
	M=$(( $(usex rustfmt 256 0) + ${M} ))
	# add 2G if we compile llvm and 256M per llvm_target
	if true; then
		M=$(( 2048 + ${M} ))
		local ltarget
		for ltarget in ${ALL_LLVM_TARGETS[@]}; do
			M=$(( $(usex ${ltarget} 256 0) + ${M} ))
		done
	fi
	M=$(( $(usex rust_sysroots_bpf 256 0) + ${M} ))
	M=$(( $(usex rust_sysroots_wasm 256 0) + ${M} ))
	M=$(( $(usex debug 2 1) * ${M} ))
	eshopts_push -s extglob
	if is-flagq '-g?(gdb)?([1-9])'; then
		M=$(( 15 * ${M} / 10 ))
	fi
	eshopts_pop
	M=$(( $(usex doc 256 0) + ${M} ))
	CHECKREQS_DISK_BUILD=${M}M check-reqs_pkg_${EBUILD_PHASE}
}

# Is LLVM being linked against libc++?
is_libcxx_linked() {
	local code='#include <ciso646>
#if defined(_LIBCPP_VERSION)
	HAVE_LIBCXX
#endif
'
	local out=$($(tc-getCXX) ${CXXFLAGS} ${CPPFLAGS} -x c++ -E -P - <<<"${code}") || return 1
	[[ ${out} == *HAVE_LIBCXX* ]]
}

pkg_pretend() {
	pre_build_checks
}

pkg_setup() {
	pre_build_checks
	python-any-r1_pkg_setup

	export LIBGIT2_NO_PKG_CONFIG=1 #749381

# skip rust_pkg_setup, llvm_pkg_setup
#	rust_pkg_setup

	local llvm_config=${EROOT}/opt/llvm-${LLVMVER}/bin/llvm-config
	export LLVM_LINK_SHARED=1
	export RUSTFLAGS="${RUSTFLAGS} -Lnative=$("${llvm_config}" --libdir)"
}

src_unpack() {
	unpack "rustc-${RUST_PV}-src.tar.xz"
	mkdir -pv ${WORKDIR}/cache/${BOOT_DATE}
	cp -apfv ${DISTDIR}/{cargo,rust-std,rustc}-${BOOT_PV}-x86_64-unknown-linux-gnu.tar.xz ${WORKDIR}/cache/${BOOT_DATE}
}

src_prepare() {
	#madhu
	default
	export PATH=${ED}/opt/llvm-${LLVMVER}/bin:$PATH
}

src_configure() {
	filter-lto # https://bugs.gentoo.org/862109 https://bugs.gentoo.org/866231

	local rust_target="" rust_targets="" arch_cflags
	for v in $(multilib_get_enabled_abi_pairs); do
		rust_targets+=",\"$(rust_abi $(get_abi_CHOST ${v##*.}))\""
	done
	if use rust_sysroots_bpf; then
		rust_targets+=",\"bpfeb-unknown-none\",\"bpfel-unknown-none\""
	fi
	if use rust_sysroots_wasm; then
		rust_targets+=",\"wasm32-unknown-unknown\""
		if system-llvm; then
			# un-hardcode rust-lld linker for this target
			# https://bugs.gentoo.org/715348
			sed -i '/linker:/ s/rust-lld/wasm-ld/' compiler/rustc_target/src/spec/base/wasm.rs || die
		fi
	fi
	rust_targets="${rust_targets#,}"

	# cargo and rustdoc are mandatory and should always be included
	local tools='"cargo","rustdoc"'
	use clippy && tools+=',"clippy"'
	use rustfmt && tools+=',"rustfmt"'
	use rust-analyzer && tools+=',"rust-analyzer","rust-analyzer-proc-macro-srv"'
	use rust-src && tools+=',"src"'

# skip sysroot stage0_root
if false; then
	local rust_stage0_root="$(${RUSTC} --print sysroot || die "Can't determine rust's sysroot")"
	# in case of prefix it will be already prefixed, as --print sysroot returns full path
	[[ -d ${rust_stage0_root} ]] || die "${rust_stage0_root} is not a directory"
fi

	rust_target="$(rust_abi)"
	rust_build="$(rust_abi "${CBUILD}")"
	rust_host="$(rust_abi "${CHOST}")"

	RUST_EXPERIMENTAL_TARGETS=()
	for _x in "${!ALL_RUST_EXPERIMENTAL_TARGETS[@]}"; do
		if [[ ${ALL_RUST_EXPERIMENTAL_TARGETS[${_x}]} == 1 ]] && use ${_x} ; then
			RUST_EXPERIMENTAL_TARGETS+=( ${_x#llvm_targets_} )
		fi
	done
	RUST_EXPERIMENTAL_TARGETS=${RUST_EXPERIMENTAL_TARGETS[@]}

	local cm_btype="$(usex debug DEBUG RELEASE)"
	local build_channel=stable

	# TODO: Add optimized-compiler-builtins for system-llvm to avoid
	# building bundled compiler-rt.
	cat <<- _EOF_ > "${S}"/bootstrap.toml
		# Suppresses a warning about tracking changes which we don't care about.
		change-id = "ignore"
		# https://github.com/rust-lang/rust/issues/135358 (bug #947897)
		profile = "dist"
		[llvm]
		download-ci-llvm = false
		optimize = $(toml_usex !debug)
		release-debuginfo = $(toml_usex debug)
		assertions = $(toml_usex debug)
		ninja = true
		targets = "${LLVM_TARGETS// /;}"
		experimental-targets = "${RUST_EXPERIMENTAL_TARGETS// /;}"
		link-shared = true # system-llvm
		$(if is_libcxx_linked; then
			# https://bugs.gentoo.org/732632
			echo "use-libcxx = true"
			echo "static-libstdcpp = false"
		fi)
		$(case "${rust_target}" in
			i586-*-linux-*)
				# https://github.com/rust-lang/rust/issues/93059
				echo 'cflags = "-fcf-protection=none"'
				echo 'cxxflags = "-fcf-protection=none"'
				echo 'ldflags = "-fcf-protection=none"'
				;;
			*)
				;;
		esac)
		enable-warnings = false
		[llvm.build-config]
		CMAKE_VERBOSE_MAKEFILE = "ON"
		[build]
		build-stage = 2
		test-stage = 2
		build = "${rust_build}"
		host = ["${rust_host}"]
		target = [${rust_targets}]
		bootstrap-cache-path="${WORKDIR}/cache"
#		cargo = "$CARGO"
#		rustc = "$RUSTC"
#		rustfmt = "$RUSTFMT"
		description = "gentoo"
		docs = $(toml_usex doc)
		compiler-docs = false
		submodules = false
		python = "${EPYTHON}"
		locked-deps = true
		vendor = true
		extended = true
		tools = [${tools}]
		verbose = 2
		sanitizers = false
		profiler = true
		cargo-native-static = false
		[install]
		prefix = "${EPREFIX}/usr/lib/${PN}/${SLOT}"
		sysconfdir = "etc"
		docdir = "share/doc/rust"
		bindir = "bin"
		libdir = "lib"
		mandir = "share/man"
		[rust]
		# https://github.com/rust-lang/rust/issues/54872
		codegen-units-std = 1
		optimize = true
		debug = $(toml_usex debug)
		debug-assertions = $(toml_usex debug)
		debug-assertions-std = $(toml_usex debug)
		debuginfo-level = $(usex debug 2 0)
		debuginfo-level-rustc = $(usex debug 2 0)
		debuginfo-level-std = $(usex debug 2 0)
		debuginfo-level-tools = $(usex debug 2 0)
		debuginfo-level-tests = 0
		backtrace = true
		incremental = false
		$(if ! tc-is-cross-compiler; then
			echo "default-linker = \"${CHOST}-cc\""
		fi)
		channel = "${build_channel}"
		rpath = true
		verbose-tests = true
		optimize-tests = $(toml_usex !debug)
		codegen-tests = true
		omit-git-hash = false
		dist-src = false
		remap-debuginfo = true
		lld = $(usex system-llvm false $(toml_usex rust_sysroots_wasm))
		$(if use lto && tc-is-clang && ! tc-ld-is-mold; then
			echo "use-lld = true"
		fi)
		# only deny warnings if doc+wasm are NOT requested, documenting stage0 wasm std fails without it
		# https://github.com/rust-lang/rust/issues/74976
		# https://github.com/rust-lang/rust/issues/76526
		deny-warnings = $(usex rust_sysroots_wasm $(usex doc false true) true)
		backtrace-on-ice = true
		jemalloc = false
		# See https://github.com/rust-lang/rust/issues/121124
		lto = "$(usex lto thin off)"
		[dist]
		src-tarball = false
		compression-formats = ["xz"]
		compression-profile = "balanced"
	_EOF_

	for v in $(multilib_get_enabled_abi_pairs); do
		rust_target=$(rust_abi $(get_abi_CHOST ${v##*.}))
		arch_cflags="$(get_abi_CFLAGS ${v##*.})"

		export CFLAGS_${rust_target//-/_}="${arch_cflags}"

		cat <<- _EOF_ >> "${S}"/bootstrap.toml
			[target.${rust_target}]
			ar = "$(tc-getAR)"
			cc = "$(tc-getCC)"
			cxx = "$(tc-getCXX)"
			linker = "$(tc-getCC)"
			ranlib = "$(tc-getRANLIB)"
			llvm-libunwind = "$(usex llvm-libunwind $(usex system-llvm system in-tree) no)"
		_EOF_
		if use system-llvm; then
			cat <<- _EOF_ >> "${S}"/bootstrap.toml
				llvm-config = "${EROOT}/opt/llvm-${LLVMVER}/bin/llvm-config"
			_EOF_
		fi
		# by default librustc_target/spec/linux_musl_base.rs sets base.crt_static_default = true;
		# but we patch it and set to false here as well
		if use elibc_musl; then
			cat <<- _EOF_ >> "${S}"/bootstrap.toml
				crt-static = false
				musl-root = "$($(tc-getCC) -print-sysroot)/usr"
			_EOF_
		fi
	done
	if use rust_sysroots_wasm; then
		wasm_target="wasm32-unknown-unknown"
		if tc-is-clang; then
			local wasm_cc=$(tc-getCC)
			local wasm_cxx=$(tc-getCXX)
		else
			local wasm_cc=${CHOST}-clang
			local wasm_cxx=${CHOST}-clang++
		fi
		export CFLAGS_${wasm_target//-/_}="$(
			CC="${wasm_cc} --target=wasm32-unknown-unknown"
			filter-flags '-mcpu*' '-march*' '-mtune*'
			strip-unsupported-flags
			echo "${CFLAGS}"
		)"
		cat <<- _EOF_ >> "${S}"/bootstrap.toml
			[target.wasm32-unknown-unknown]
			cc = "${wasm_cc}"
			cxx = "${wasm_cxx}"
			linker = "$(usex system-llvm lld rust-lld)"
			# wasm target does not have profiler_builtins https://bugs.gentoo.org/848483
			profiler = false
		_EOF_
	fi

	einfo "Rust configured with the following flags:"
	echo
	echo RUSTFLAGS="\"${RUSTFLAGS}\""
	echo RUSTFLAGS_BOOTSTRAP="\"${RUSTFLAGS_BOOTSTRAP}\""
	echo RUSTFLAGS_NOT_BOOTSTRAP="\"${RUSTFLAGS_NOT_BOOTSTRAP}\""
	echo MAGIC_EXTRA_RUSTFLAGS="\"${MAGIC_EXTRA_RUSTFLAGS}\""
	env | grep "CARGO_TARGET_.*_RUSTFLAGS="
	env | grep "CFLAGS_.*"
	echo
	einfo "bootstrap.toml contents:"
	cat "${S}"/bootstrap.toml || die
	echo
}

src_compile() {
	# -v will show invocations, -vv "very verbose" is overkill, -vvv "very very verbose" is insane
	RUST_BACKTRACE=1 "${EPYTHON}" ./x.py build -v --config="${S}"/bootstrap.toml -j$(makeopts_jobs) || die
}

src_test() {
	# https://rustc-dev-guide.rust-lang.org/tests/intro.html

	# those are basic and codegen tests.
	local tests=(
		codegen
		codegen-units
		compile-fail
		incremental
		mir-opt
		pretty
		run-make
	)

	# fails if llvm is not built with ALL targets.
	# and known to fail with system llvm sometimes.
	use system-llvm || tests+=( assembly )

	# fragile/expensive/less important tests
	# or tests that require extra builds
	# TODO: instead of skipping, just make some nonfatal.
	if [[ ${ERUST_RUN_EXTRA_TESTS:-no} != no ]]; then
		tests+=(
			rustdoc
			rustdoc-js
			rustdoc-js-std
			rustdoc-ui
			run-make-fulldeps
			ui
			ui-fulldeps
		)
	fi

	local i failed=()
	einfo "rust_src_test: enabled tests ${tests[@]/#/src/test/}"
	for i in "${tests[@]}"; do
		local t="src/test/${i}"
		einfo "rust_src_test: running ${t}"
		if ! RUST_BACKTRACE=1 "${EPYTHON}" ./x.py test -vv --config="${S}"/bootstrap.toml \
				-j$(makeopts_jobs) --no-doc --no-fail-fast "${t}"
		then
				failed+=( "${t}" )
				eerror "rust_src_test: ${t} failed"
		fi
	done

	if [[ ${#failed[@]} -ne 0 ]]; then
		eerror "rust_src_test: failure summary: ${failed[@]}"
		die "aborting due to test failures"
	fi
}

src_install() {
	DESTDIR="${D}" "${EPYTHON}" ./x.py install -v --config="${S}"/bootstrap.toml -j$(makeopts_jobs) || die

	docompress /usr/lib/${PN}/${SLOT}/share/man/

	# bash-completion files are installed by dev-lang/rust-common instead
	# bug #689562, #689160.
	rm -v "${ED}/usr/lib/${PN}/${SLOT}/etc/bash_completion.d/cargo" || die
	rmdir -v "${ED}/usr/lib/${PN}/${SLOT}/etc/bash_completion.d" || die

	local symlinks=(
		cargo
		rustc
		rustdoc
		rust-gdb
		rust-gdbgui
		rust-lldb
	)

	use clippy && symlinks+=( clippy-driver cargo-clippy )
	if [[ ${PV} = *9999* ]]; then
		use miri && symlinks+=( miri cargo-miri )
	fi
	use rustfmt && symlinks+=( rustfmt cargo-fmt )
	use rust-analyzer && symlinks+=( rust-analyzer )

	einfo "installing eselect-rust symlinks and paths: ${symlinks[@]}"
	local i
	for i in "${symlinks[@]}"; do
		# we need realpath on /usr/bin/* symlink return version-appended binary path.
		# so /usr/bin/rustc should point to /usr/lib/rust/<ver>/bin/rustc-<ver>
		# need to fix eselect-rust to remove this hack.
		local ver_i="${i}-${RUST_PV%%_*}"
		if [[ -f "${ED}/usr/lib/${PN}/${SLOT}/bin/${i}" ]]; then
			einfo "Installing ${i} symlink"
			ln -v "${ED}/usr/lib/${PN}/${SLOT}/bin/${i}" "${ED}/usr/lib/${PN}/${SLOT}/bin/${ver_i}" || die
		else
			ewarn "${i} symlink requested, but source file not found"
			ewarn "please report this"
		fi
		dosym "../lib/${PN}/${SLOT}/bin/${ver_i}" "/usr/bin/${ver_i}"
	done

	# symlinks to switch components to active rust in eselect
	dosym "${SLOT}/lib" "/usr/lib/${PN}/lib-${SLOT}"
	use rust-analyzer && dosym "${SLOT}/libexec" "/usr/lib/${PN}/libexec-${SLOT}"
	dosym "${SLOT}/share/man" "/usr/lib/${PN}/man-${SLOT}"
	dosym "rust/${SLOT}/lib/rustlib" "/usr/lib/rustlib-${SLOT}"
	dosym "../../lib/${PN}/${SLOT}/share/doc/rust" "/usr/share/doc/${RUST_P}"

	newenvd - "50${RUST_P}" <<-_EOF_
		MANPATH="${EPREFIX}/usr/lib/rust/man-${SLOT}"
	_EOF_

	rm -rf "${ED}/usr/lib/${PN}/${SLOT}"/*.old || die
	rm -rf "${ED}/usr/lib/${PN}/${SLOT}/bin"/*.old || die
	rm -rf "${ED}/usr/lib/${PN}/${SLOT}/doc"/*.old || die

	# note: eselect-rust adds EROOT to all paths below
	cat <<-_EOF_ > "${T}/provider-${PN}-${SLOT}"
		/usr/bin/cargo
		/usr/bin/rustdoc
		/usr/bin/rust-gdb
		/usr/bin/rust-gdbgui
		/usr/bin/rust-lldb
		/usr/lib/rustlib
		/usr/lib/rust/lib
		/usr/lib/rust/man
		/usr/share/doc/rust
	_EOF_

	if use clippy; then
		echo /usr/bin/clippy-driver >> "${T}/provider-${RUST_P}"
		echo /usr/bin/cargo-clippy >> "${T}/provider-${RUST_P}"
	fi
	if [[ ${SLOT} == *9999* ]] && use miri; then
		echo /usr/bin/miri >> "${T}/provider-${RUST_P}"
		echo /usr/bin/cargo-miri >> "${T}/provider-${RUST_P}"
	fi
	if use rustfmt; then
		echo /usr/bin/rustfmt >> "${T}/provider-${RUST_P}"
		echo /usr/bin/cargo-fmt >> "${T}/provider-${RUST_P}"
	fi
	if use rust-analyzer; then
		echo /usr/lib/rust/libexec >> "${T}/provider-${RUST_P}"
		echo /usr/bin/rust-analyzer >> "${T}/provider-${RUST_P}"
	fi

	insinto /etc/env.d/rust
	doins "${T}/provider-${PN}-${SLOT}"

	if use dist; then
		"${EPYTHON}" ./x.py dist -v --config="${S}"/bootstrap.toml -j$(makeopts_jobs) || die
		insinto "/usr/lib/${PN}/${SLOT}/dist"
		doins -r "${S}/build/dist/."
	fi
}

pkg_postinst() {
	eselect rust update

	if has_version dev-debug/gdb || has_version llvm-core/lldb; then
		elog "Rust installs helper scripts for calling GDB and LLDB,"
		elog "for convenience they are installed under /usr/bin/rust-{gdb,lldb}-${RUST_PV}."
	fi

	if has_version app-editors/emacs; then
		optfeature "emacs support for rust" app-emacs/rust-mode
	fi

	if has_version app-editors/gvim || has_version app-editors/vim; then
		optfeature "vim support for rust" app-vim/rust-vim
	fi
}

pkg_postrm() {
	eselect rust cleanup
}
