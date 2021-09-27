# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
# openrc init files from pentoo/app-admin/opensnitch
#
#   Time-stamp: <>
#   Touched: Sat Mar 27 22:19:08 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021 Madhu.  All Rights Reserved.
#
# ;madhu 210327 1.3.6 USE_GIT. ebuild from pentoo converted to use
# ;EGO_SUM instead of EGO_VENDOR. (fix keepdir for rules_path)
# ;madhu 210328 doesn't work yet - ui_process hangs in some tight loop
# ;madhu 210926  1.3.6-r1 - python 3.9
# ;madhu 210927  1.4.0 - v1.4.0-13-g4ea0904

EAPI=8

USE_GIT=true

PYTHON_COMPAT=( python3_{8..10} )
inherit distutils-r1 go-module linux-info

DESCRIPTION="Desktop application firewall"
HOMEPAGE="https://github.com/evilsocket/opensnitch"

# echo "EGO_SUM=(" ; awk '{print $1 " " $2}' go.sum | sed -e 's/^/   "/g' -e 's/$/"/g'; echo ")"

EGO_SUM=(
	"cloud.google.com/go v0.26.0/go.mod"
	"github.com/BurntSushi/toml v0.3.1/go.mod"
	"github.com/census-instrumentation/opencensus-proto v0.2.1/go.mod"
	"github.com/client9/misspell v0.3.4/go.mod"
	"github.com/envoyproxy/go-control-plane v0.9.1-0.20191026205805-5f8ba28d4473/go.mod"
	"github.com/envoyproxy/protoc-gen-validate v0.1.0/go.mod"
	"github.com/evilsocket/ftrace v1.2.0"
	"github.com/evilsocket/ftrace v1.2.0/go.mod"
	"github.com/fsnotify/fsnotify v1.4.7"
	"github.com/fsnotify/fsnotify v1.4.7/go.mod"
	"github.com/golang/glog v0.0.0-20160126235308-23def4e6c14b"
	"github.com/golang/glog v0.0.0-20160126235308-23def4e6c14b/go.mod"
	"github.com/golang/mock v1.1.1/go.mod"
	"github.com/golang/protobuf v1.2.0/go.mod"
	"github.com/golang/protobuf v1.3.2/go.mod"
	"github.com/golang/protobuf v1.5.0"
	"github.com/golang/protobuf v1.5.0/go.mod"
	"github.com/google/go-cmp v0.2.0/go.mod"
	"github.com/google/go-cmp v0.3.1/go.mod"
	"github.com/google/go-cmp v0.5.5"
	"github.com/google/go-cmp v0.5.5/go.mod"
	"github.com/google/gopacket v1.1.14"
	"github.com/google/gopacket v1.1.14/go.mod"
	"github.com/google/nftables v0.0.0-20210514154851-a285acebcad3"
	"github.com/google/nftables v0.0.0-20210514154851-a285acebcad3/go.mod"
	"github.com/iovisor/gobpf v0.2.0"
	"github.com/iovisor/gobpf v0.2.0/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20190606172950-9527aa82566a"
	"github.com/jsimonetti/rtnetlink v0.0.0-20190606172950-9527aa82566a/go.mod"
	"github.com/koneu/natend v0.0.0-20150829182554-ec0926ea948d"
	"github.com/koneu/natend v0.0.0-20150829182554-ec0926ea948d/go.mod"
	"github.com/mdlayher/netlink v0.0.0-20190409211403-11939a169225/go.mod"
	"github.com/mdlayher/netlink v0.0.0-20191009155606-de872b0d824b"
	"github.com/mdlayher/netlink v0.0.0-20191009155606-de872b0d824b/go.mod"
	"github.com/prometheus/client_model v0.0.0-20190812154241-14fe0d1b01d4/go.mod"
	"github.com/vishvananda/netlink v0.0.0-20210811191823-e1a867c6b452"
	"github.com/vishvananda/netlink v0.0.0-20210811191823-e1a867c6b452/go.mod"
	"github.com/vishvananda/netns v0.0.0-20180720170159-13995c7128cc/go.mod"
	"github.com/vishvananda/netns v0.0.0-20200728191858-db3c7e526aae"
	"github.com/vishvananda/netns v0.0.0-20200728191858-db3c7e526aae/go.mod"
	"golang.org/x/crypto v0.0.0-20190308221718-c2843e01d9a2/go.mod"
	"golang.org/x/exp v0.0.0-20190121172915-509febef88a4/go.mod"
	"golang.org/x/lint v0.0.0-20181026193005-c67002cb31c3/go.mod"
	"golang.org/x/lint v0.0.0-20190227174305-5b3e6a55c961/go.mod"
	"golang.org/x/lint v0.0.0-20190313153728-d0100b6bd8b3/go.mod"
	"golang.org/x/net v0.0.0-20180724234803-3673e40ba225/go.mod"
	"golang.org/x/net v0.0.0-20180826012351-8a410e7b638d/go.mod"
	"golang.org/x/net v0.0.0-20190213061140-3a22650c66bd/go.mod"
	"golang.org/x/net v0.0.0-20190311183353-d8887717615a/go.mod"
	"golang.org/x/net v0.0.0-20190827160401-ba9fcec4b297/go.mod"
	"golang.org/x/net v0.0.0-20191028085509-fe3aa8a45271"
	"golang.org/x/net v0.0.0-20191028085509-fe3aa8a45271/go.mod"
	"golang.org/x/oauth2 v0.0.0-20180821212333-d2e6202438be/go.mod"
	"golang.org/x/sync v0.0.0-20180314180146-1d60e4601c6f/go.mod"
	"golang.org/x/sync v0.0.0-20181108010431-42b317875d0f/go.mod"
	"golang.org/x/sync v0.0.0-20190423024810-112230192c58/go.mod"
	"golang.org/x/sync v0.0.0-20200625203802-6e8e738ad208/go.mod"
	"golang.org/x/sys v0.0.0-20180830151530-49385e6e1522/go.mod"
	"golang.org/x/sys v0.0.0-20190215142949-d0b11bdaac8a/go.mod"
	"golang.org/x/sys v0.0.0-20190312061237-fead79001313/go.mod"
	"golang.org/x/sys v0.0.0-20190411185658-b44545bcd369/go.mod"
	"golang.org/x/sys v0.0.0-20190826190057-c7b8b68b1456/go.mod"
	"golang.org/x/sys v0.0.0-20191029155521-f43be2a4598c/go.mod"
	"golang.org/x/sys v0.0.0-20200217220822-9197077df867/go.mod"
	"golang.org/x/sys v0.0.0-20200728102440-3e129f6d46b1"
	"golang.org/x/sys v0.0.0-20200728102440-3e129f6d46b1/go.mod"
	"golang.org/x/text v0.3.0"
	"golang.org/x/text v0.3.0/go.mod"
	"golang.org/x/tools v0.0.0-20190114222345-bf090417da8b/go.mod"
	"golang.org/x/tools v0.0.0-20190226205152-f727befe758c/go.mod"
	"golang.org/x/tools v0.0.0-20190311212946-11955173bddd/go.mod"
	"golang.org/x/tools v0.0.0-20190524140312-2c0ae7006135/go.mod"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543/go.mod"
	"google.golang.org/appengine v1.1.0/go.mod"
	"google.golang.org/appengine v1.4.0/go.mod"
	"google.golang.org/genproto v0.0.0-20180817151627-c66870c02cf8/go.mod"
	"google.golang.org/genproto v0.0.0-20190819201941-24fa4b261c55"
	"google.golang.org/genproto v0.0.0-20190819201941-24fa4b261c55/go.mod"
	"google.golang.org/grpc v1.19.0/go.mod"
	"google.golang.org/grpc v1.23.0/go.mod"
	"google.golang.org/grpc v1.27.0"
	"google.golang.org/grpc v1.27.0/go.mod"
	"google.golang.org/protobuf v1.26.0-rc.1/go.mod"
	"google.golang.org/protobuf v1.26.0"
	"google.golang.org/protobuf v1.26.0/go.mod"
	"honnef.co/go/tools v0.0.0-20190102054323-c2f93a96b099/go.mod"
	"honnef.co/go/tools v0.0.0-20190523083050-ea95bdfd59fc/go.mod"
	)

go-module_set_globals

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI="${EGO_SUM_SRC_URI}"
	EGIT_REPO_URI="https://github.com/evilsocket/opensnitch"
	#EGIT_COMMIT=00c02855ff2f39a271fefc8fa13b558d4f7dd5bb #1.4.0
else
SRC_URI="https://github.com/evilsocket/opensnitch/archive/v${PV}.tar.gz -> ${P}.tar.gz
	${EGO_SUM_SRC_URI}"
fi

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~amd64 ~x86"

RESTRICT="mirror test"

DEPEND="net-libs/libnetfilter_queue"
RDEPEND="
	dev-python/grpcio-tools[${PYTHON_USEDEP}]
	dev-python/python-slugify[${PYTHON_USEDEP}]
	dev-python/pyinotify[${PYTHON_USEDEP}]
	dev-python/PyQt5[sql,${PYTHON_USEDEP}]
"

pkg_setup() {
	CONFIG_CHECK="
		~NETFILTER_XT_MATCH_CONNTRACK
	"
	check_extra_config
}

src_unpack() {
	if ${USE_GIT} ; then
		git-r3_src_unpack
	else
		default
	fi
	go-module_setup_proxy

	#ship missing go.sum with the ebuild
	cp -apv  ${FILESDIR}/${P}.go.sum ${S}/daemon/go.sum
#	cp -apv  ${FILESDIR}/${P}.ui.pb.go ${S}/daemon/ui/protocol

	OLD_S=${S}
	S=${S}/daemon
	go-module_src_unpack
	S=${OLD_S}

	# ??? directory permissions shouldn't be affected!
	[ -n ${HOME} -a -d ${HOME} ] && find ${HOME} -type d \! -perm 755 -print0 | xargs -0 chmod -v 755
}


src_compile() {

# use shipped generated protocol file because go-proto isn't there
	pushd proto
	emake
	popd

	pushd daemon >/dev/null || die
	go build -v -x -o opensnitchd || die
	popd >/dev/null || die

	pushd ui >/dev/null || die
	pyrcc5 -o opensnitch/resources_rc.py opensnitch/res/resources.qrc
	sed -i 's/^import ui_pb2/from . import ui_pb2/' opensnitch/ui_pb2*
	distutils-r1_src_compile
	popd >/dev/null || die
}

src_install(){
	pushd daemon >/dev/null || die
	dobin opensnitchd
	popd >/dev/null || die

	pushd ui >/dev/null || die
	distutils-r1_src_install
	popd >/dev/null || die

	pushd daemon >/dev/null || die
	keepdir /etc/opensnitchd/rules
	insinto /etc/opensnitchd/
#	@cp opensnitchd.service /etc/systemd/system/
	doins default-config.json
	doins system-fw.json
	popd >/dev/null || die

	newinitd "${FILESDIR}"/opensnitch.initd ${PN}
}
