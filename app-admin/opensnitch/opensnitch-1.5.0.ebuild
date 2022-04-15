# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2
#
# openrc init files from pentoo/app-admin/opensnitch
#
#   Time-stamp: <>
#   Touched: Sat Mar 27 22:19:08 2021 +0530 <enometh@net.meer>
#   Bugs-To: enometh@net.meer
#   Status: Experimental.  Do not redistribute
#   Copyright (C) 2021-2022 Madhu.  All Rights Reserved.
#
# ;madhu 210327 1.3.6 USE_GIT. ebuild from pentoo converted to use
# ;EGO_SUM instead of EGO_VENDOR. (fix keepdir for rules_path)
# ;madhu 210328 doesn't work yet - ui_process hangs in some tight loop
# ;madhu 210926  1.3.6-r1 - python 3.9
# ;madhu 210927  1.4.0 - v1.4.0-13-g4ea0904
# ;madhu 220410  1.5.0 - v1.5.0-21-gb35985f

EAPI=8

USE_GIT=true

#;madhu 220414 go mod tidy still wants to contact big-brother-ship?
#export GOFLAGS="-v -x -mod=readonly"
#;madhu 220415 have to call go mod tidy -e to update go.sum because of
# errors when compiling with go1.18

PYTHON_COMPAT=( python3_{8..10} )
inherit distutils-r1 go-module linux-info xdg

DESCRIPTION="Desktop application firewall"
HOMEPAGE="https://github.com/evilsocket/opensnitch"

# echo "EGO_SUM=(" ; awk '{print $1 " " $2}' go.sum | sed -e 's/^/   "/g' -e 's/$/"/g'; echo ")"

EGO_SUM=(
	"cloud.google.com/go v0.26.0/go.mod"
	"github.com/BurntSushi/toml v0.3.1/go.mod"
	"github.com/BurntSushi/toml v0.4.1"
	"github.com/BurntSushi/toml v0.4.1/go.mod"
	"github.com/census-instrumentation/opencensus-proto v0.2.1/go.mod"
	"github.com/cilium/ebpf v0.5.0/go.mod"
	"github.com/cilium/ebpf v0.7.0/go.mod"
	"github.com/client9/misspell v0.3.4/go.mod"
	"github.com/cncf/udpa/go v0.0.0-20191209042840-269d4d468f6f/go.mod"
	"github.com/davecgh/go-spew v1.1.1/go.mod"
	"github.com/envoyproxy/go-control-plane v0.9.0/go.mod"
	"github.com/envoyproxy/go-control-plane v0.9.4/go.mod"
	"github.com/envoyproxy/protoc-gen-validate v0.1.0/go.mod"
	"github.com/evilsocket/ftrace v1.2.0"
	"github.com/evilsocket/ftrace v1.2.0/go.mod"
	"github.com/frankban/quicktest v1.11.3/go.mod"
	"github.com/fsnotify/fsnotify v1.4.7"
	"github.com/fsnotify/fsnotify v1.4.7/go.mod"
	"github.com/golang/glog v0.0.0-20160126235308-23def4e6c14b/go.mod"
	"github.com/golang/mock v1.1.1/go.mod"
	"github.com/golang/protobuf v1.2.0/go.mod"
	"github.com/golang/protobuf v1.3.2/go.mod"
	"github.com/golang/protobuf v1.3.3/go.mod"
	"github.com/golang/protobuf v1.5.0"
	"github.com/golang/protobuf v1.5.0/go.mod"
	"github.com/google/go-cmp v0.2.0/go.mod"
	"github.com/google/go-cmp v0.3.1/go.mod"
	"github.com/google/go-cmp v0.4.0/go.mod"
	"github.com/google/go-cmp v0.5.2/go.mod"
	"github.com/google/go-cmp v0.5.4/go.mod"
	"github.com/google/go-cmp v0.5.5/go.mod"
	"github.com/google/go-cmp v0.5.6"
	"github.com/google/go-cmp v0.5.6/go.mod"
	"github.com/google/gopacket v1.1.14"
	"github.com/google/gopacket v1.1.14/go.mod"
	"github.com/google/nftables v0.0.0-20220210072902-edf9fe8cd04f"
	"github.com/google/nftables v0.0.0-20220210072902-edf9fe8cd04f/go.mod"
	"github.com/iovisor/gobpf v0.2.0"
	"github.com/iovisor/gobpf v0.2.0/go.mod"
	"github.com/josharian/native v0.0.0-20200817173448-b6b71def0850"
	"github.com/josharian/native v0.0.0-20200817173448-b6b71def0850/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20190606172950-9527aa82566a/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20200117123717-f846d4f6c1f4/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20201009170750-9c6f07d100c1/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20201216134343-bde56ed16391/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20201220180245-69540ac93943/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20210122163228-8d122574c736/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20210212075122-66c871082f2b/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20210525051524-4cc836578190/go.mod"
	"github.com/jsimonetti/rtnetlink v0.0.0-20211022192332-93da33804786/go.mod"
	"github.com/kr/pretty v0.2.1/go.mod"
	"github.com/kr/pty v1.1.1/go.mod"
	"github.com/kr/text v0.1.0/go.mod"
	"github.com/mdlayher/ethtool v0.0.0-20210210192532-2b88debcdd43/go.mod"
	"github.com/mdlayher/ethtool v0.0.0-20211028163843-288d040e9d60/go.mod"
	"github.com/mdlayher/genetlink v1.0.0/go.mod"
	"github.com/mdlayher/netlink v0.0.0-20190409211403-11939a169225/go.mod"
	"github.com/mdlayher/netlink v1.0.0/go.mod"
	"github.com/mdlayher/netlink v1.1.0/go.mod"
	"github.com/mdlayher/netlink v1.1.1/go.mod"
	"github.com/mdlayher/netlink v1.2.0/go.mod"
	"github.com/mdlayher/netlink v1.2.1/go.mod"
	"github.com/mdlayher/netlink v1.2.2-0.20210123213345-5cc92139ae3e/go.mod"
	"github.com/mdlayher/netlink v1.3.0/go.mod"
	"github.com/mdlayher/netlink v1.4.0/go.mod"
	"github.com/mdlayher/netlink v1.4.1/go.mod"
	"github.com/mdlayher/netlink v1.4.2"
	"github.com/mdlayher/netlink v1.4.2/go.mod"
	"github.com/mdlayher/socket v0.0.0-20210307095302-262dc9984e00/go.mod"
	"github.com/mdlayher/socket v0.0.0-20211007213009-516dcbdf0267/go.mod"
	"github.com/mdlayher/socket v0.0.0-20211102153432-57e3fa563ecb"
	"github.com/mdlayher/socket v0.0.0-20211102153432-57e3fa563ecb/go.mod"
	"github.com/prometheus/client_model v0.0.0-20190812154241-14fe0d1b01d4/go.mod"
	"github.com/vishvananda/netlink v0.0.0-20210811191823-e1a867c6b452"
	"github.com/vishvananda/netlink v0.0.0-20210811191823-e1a867c6b452/go.mod"
	"github.com/vishvananda/netns v0.0.0-20180720170159-13995c7128cc/go.mod"
	"github.com/vishvananda/netns v0.0.0-20200728191858-db3c7e526aae"
	"github.com/vishvananda/netns v0.0.0-20200728191858-db3c7e526aae/go.mod"
	"github.com/yuin/goldmark v1.2.1/go.mod"
	"github.com/yuin/goldmark v1.4.0/go.mod"
	"github.com/yuin/goldmark v1.4.1/go.mod"
	"golang.org/x/crypto v0.0.0-20190308221718-c2843e01d9a2/go.mod"
	"golang.org/x/crypto v0.0.0-20191011191535-87dc89f01550/go.mod"
	"golang.org/x/crypto v0.0.0-20200622213623-75b288015ac9/go.mod"
	"golang.org/x/exp v0.0.0-20190121172915-509febef88a4/go.mod"
	"golang.org/x/lint v0.0.0-20181026193005-c67002cb31c3/go.mod"
	"golang.org/x/lint v0.0.0-20190227174305-5b3e6a55c961/go.mod"
	"golang.org/x/lint v0.0.0-20190313153728-d0100b6bd8b3/go.mod"
	"golang.org/x/mod v0.3.0/go.mod"
	"golang.org/x/mod v0.4.2/go.mod"
	"golang.org/x/mod v0.5.1"
	"golang.org/x/mod v0.5.1/go.mod"
	"golang.org/x/net v0.0.0-20180724234803-3673e40ba225/go.mod"
	"golang.org/x/net v0.0.0-20180826012351-8a410e7b638d/go.mod"
	"golang.org/x/net v0.0.0-20190213061140-3a22650c66bd/go.mod"
	"golang.org/x/net v0.0.0-20190311183353-d8887717615a/go.mod"
	"golang.org/x/net v0.0.0-20190404232315-eb5bcb51f2a3/go.mod"
	"golang.org/x/net v0.0.0-20190620200207-3b0461eec859/go.mod"
	"golang.org/x/net v0.0.0-20190827160401-ba9fcec4b297/go.mod"
	"golang.org/x/net v0.0.0-20191007182048-72f939374954/go.mod"
	"golang.org/x/net v0.0.0-20200202094626-16171245cfb2/go.mod"
	"golang.org/x/net v0.0.0-20201010224723-4f7140c49acb/go.mod"
	"golang.org/x/net v0.0.0-20201021035429-f5854403a974/go.mod"
	"golang.org/x/net v0.0.0-20201110031124-69a78807bb2b/go.mod"
	"golang.org/x/net v0.0.0-20201216054612-986b41b23924/go.mod"
	"golang.org/x/net v0.0.0-20201224014010-6772e930b67b/go.mod"
	"golang.org/x/net v0.0.0-20210119194325-5f4716e94777/go.mod"
	"golang.org/x/net v0.0.0-20210226172049-e18ecbb05110/go.mod"
	"golang.org/x/net v0.0.0-20210525063256-abc453219eb5/go.mod"
	"golang.org/x/net v0.0.0-20210805182204-aaa1db679c0d/go.mod"
	"golang.org/x/net v0.0.0-20210928044308-7d9f5e0b762b/go.mod"
	"golang.org/x/net v0.0.0-20211015210444-4f30a5c0130f/go.mod"
	"golang.org/x/net v0.0.0-20211020060615-d418f374d309/go.mod"
	"golang.org/x/net v0.0.0-20211201190559-0a0e4e1bb54c/go.mod"
	"golang.org/x/net v0.0.0-20211209124913-491a49abca63"
	"golang.org/x/net v0.0.0-20211209124913-491a49abca63/go.mod"
	"golang.org/x/oauth2 v0.0.0-20180821212333-d2e6202438be/go.mod"
	"golang.org/x/sync v0.0.0-20180314180146-1d60e4601c6f/go.mod"
	"golang.org/x/sync v0.0.0-20181108010431-42b317875d0f/go.mod"
	"golang.org/x/sync v0.0.0-20190423024810-112230192c58/go.mod"
	"golang.org/x/sync v0.0.0-20201020160332-67f06af15bc9/go.mod"
	"golang.org/x/sync v0.0.0-20210220032951-036812b2e83c/go.mod"
	"golang.org/x/sys v0.0.0-20180830151530-49385e6e1522/go.mod"
	"golang.org/x/sys v0.0.0-20190215142949-d0b11bdaac8a/go.mod"
	"golang.org/x/sys v0.0.0-20190312061237-fead79001313/go.mod"
	"golang.org/x/sys v0.0.0-20190411185658-b44545bcd369/go.mod"
	"golang.org/x/sys v0.0.0-20190412213103-97732733099d/go.mod"
	"golang.org/x/sys v0.0.0-20190826190057-c7b8b68b1456/go.mod"
	"golang.org/x/sys v0.0.0-20191008105621-543471e840be/go.mod"
	"golang.org/x/sys v0.0.0-20200202164722-d101bd2416d5/go.mod"
	"golang.org/x/sys v0.0.0-20200217220822-9197077df867/go.mod"
	"golang.org/x/sys v0.0.0-20200728102440-3e129f6d46b1/go.mod"
	"golang.org/x/sys v0.0.0-20200930185726-fdedc70b468f/go.mod"
	"golang.org/x/sys v0.0.0-20201009025420-dfb3f7c4e634/go.mod"
	"golang.org/x/sys v0.0.0-20201118182958-a01c418693c7/go.mod"
	"golang.org/x/sys v0.0.0-20201119102817-f84b799fce68/go.mod"
	"golang.org/x/sys v0.0.0-20201218084310-7d0127a74742/go.mod"
	"golang.org/x/sys v0.0.0-20210110051926-789bb1bd4061/go.mod"
	"golang.org/x/sys v0.0.0-20210119212857-b64e53b001e4/go.mod"
	"golang.org/x/sys v0.0.0-20210123111255-9b0068b26619/go.mod"
	"golang.org/x/sys v0.0.0-20210124154548-22da62e12c0c/go.mod"
	"golang.org/x/sys v0.0.0-20210216163648-f7da38b97c65/go.mod"
	"golang.org/x/sys v0.0.0-20210305230114-8fe3ee5dd75b/go.mod"
	"golang.org/x/sys v0.0.0-20210423082822-04245dca01da/go.mod"
	"golang.org/x/sys v0.0.0-20210525143221-35b2ab0089ea/go.mod"
	"golang.org/x/sys v0.0.0-20210809222454-d867a43fc93e/go.mod"
	"golang.org/x/sys v0.0.0-20210906170528-6f6e22806c34/go.mod"
	"golang.org/x/sys v0.0.0-20210927094055-39ccf1dd6fa6/go.mod"
	"golang.org/x/sys v0.0.0-20211019181941-9d821ace8654/go.mod"
	"golang.org/x/sys v0.0.0-20211025201205-69cdffdb9359/go.mod"
	"golang.org/x/sys v0.0.0-20211124211545-fe61309f8881/go.mod"
	"golang.org/x/sys v0.0.0-20211205182925-97ca703d548d"
	"golang.org/x/sys v0.0.0-20211205182925-97ca703d548d/go.mod"
	"golang.org/x/term v0.0.0-20201126162022-7de9c90e9dd1/go.mod"
	"golang.org/x/text v0.3.0/go.mod"
	"golang.org/x/text v0.3.3/go.mod"
	"golang.org/x/text v0.3.6/go.mod"
	"golang.org/x/text v0.3.7"
	"golang.org/x/text v0.3.7/go.mod"
	"golang.org/x/tools v0.0.0-20180917221912-90fa682c2a6e/go.mod"
	"golang.org/x/tools v0.0.0-20190114222345-bf090417da8b/go.mod"
	"golang.org/x/tools v0.0.0-20190226205152-f727befe758c/go.mod"
	"golang.org/x/tools v0.0.0-20190311212946-11955173bddd/go.mod"
	"golang.org/x/tools v0.0.0-20190524140312-2c0ae7006135/go.mod"
	"golang.org/x/tools v0.0.0-20191119224855-298f0cb1881e/go.mod"
	"golang.org/x/tools v0.1.0/go.mod"
	"golang.org/x/tools v0.1.7/go.mod"
	"golang.org/x/tools v0.1.8"
	"golang.org/x/tools v0.1.8/go.mod"
	"golang.org/x/xerrors v0.0.0-20190717185122-a985d3407aa7/go.mod"
	"golang.org/x/xerrors v0.0.0-20191011141410-1b5146add898/go.mod"
	"golang.org/x/xerrors v0.0.0-20191204190536-9bdfabe68543/go.mod"
	"golang.org/x/xerrors v0.0.0-20200804184101-5ec99f83aff1"
	"golang.org/x/xerrors v0.0.0-20200804184101-5ec99f83aff1/go.mod"
	"google.golang.org/appengine v1.1.0/go.mod"
	"google.golang.org/appengine v1.4.0/go.mod"
	"google.golang.org/genproto v0.0.0-20180817151627-c66870c02cf8/go.mod"
	"google.golang.org/genproto v0.0.0-20190819201941-24fa4b261c55"
	"google.golang.org/genproto v0.0.0-20190819201941-24fa4b261c55/go.mod"
	"google.golang.org/grpc v1.19.0/go.mod"
	"google.golang.org/grpc v1.23.0/go.mod"
	"google.golang.org/grpc v1.25.1/go.mod"
	"google.golang.org/grpc v1.32.0"
	"google.golang.org/grpc v1.32.0/go.mod"
	"google.golang.org/protobuf v1.26.0-rc.1/go.mod"
	"google.golang.org/protobuf v1.26.0"
	"google.golang.org/protobuf v1.26.0/go.mod"
	"honnef.co/go/tools v0.0.0-20190102054323-c2f93a96b099/go.mod"
	"honnef.co/go/tools v0.0.0-20190523083050-ea95bdfd59fc/go.mod"
	"honnef.co/go/tools v0.2.1/go.mod"
	"honnef.co/go/tools v0.2.2"
	"honnef.co/go/tools v0.2.2/go.mod"
	)
go-module_set_globals

if ${USE_GIT}; then
	inherit git-r3
	SRC_URI="${EGO_SUM_SRC_URI}"
	EGIT_REPO_URI="https://github.com/evilsocket/opensnitch"
	#EGIT_COMMIT=aef656c42c1a39280017892c0a16d62717b8575 #v1.5.0-19-g0aef656
else
SRC_URI="https://github.com/evilsocket/opensnitch/archive/v${PV}.tar.gz -> ${P}.tar.gz
	${EGO_SUM_SRC_URI}"
fi

LICENSE="Apache-2.0"
SLOT="0"
KEYWORDS="~amd64 ~x86"

RESTRICT="mirror test"

BDEPEND="dev-go/protoc-gen-go
dev-go/protoc-gen-go-grpc
"

DEPEND="net-libs/libnetfilter_queue"

RDEPEND="
	dev-python/grpcio-tools[${PYTHON_USEDEP}]
	dev-python/python-slugify[${PYTHON_USEDEP}]
	dev-python/pyinotify[${PYTHON_USEDEP}]
	dev-python/PyQt5[sql,${PYTHON_USEDEP}]

	dev-python/notify2[${PYTHON_USEDEP}]
"

# ;madhu 220415 - TODO rdep "dev-python/pyasn[${PYTHON_USEDEP}]" not in
# gentoo. upstream pyasn-1.6.1 https://pypi.org/project/pyasn/
# https://github.com/hadiasghari/pyasn. used for looking up Autonomous
# System Numbers from IP address. see use in ui/opensnitch/utils.py


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

	# ship missing go.sum
	cp -apv  ${FILESDIR}/${P}.go.sum ${S}/daemon/go.sum
	# ship updated go.mod
	cp -apv  ${FILESDIR}/${P}.go.mod ${S}/daemon/go.mod

	# don't ship generated pg.go files if protoc-gen-{,grpc}-go are
	# installed
	#cp -apv ${FILESDIR}/${P}.ui.pb.go ${S}/daemon/ui/protocol

	OLD_S=${S}
	S=${S}/daemon
	go-module_src_unpack
	go-module_setup_proxy
	S=${OLD_S}

	# ??? directory permissions shouldn't be affected!
#	[ -n "${HOME}" -a -d "${HOME}" ] && (find "${HOME}" -type d \! -perm 755 -print0 | xargs -0 chmod -v 755 )
#	[ -d "${HOME}" ] && (find ${WORKDIR}/go-mod -type d \! -perm 755 -print0 | xargs -0 chmod -v 755)

	rm -rfv ${S}/ui/tests
}


src_compile() {
	# generate the protocol buffer file in ${S}/daemon/ui/protocol
	pushd proto >/dev/null || die
	emake
	popd >/dev/null || die

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

pkg_postrm() {
	xdg_desktop_database_update
	xdg_icon_cache_update
}

pkg_postinst() {
	xdg_desktop_database_update
	xdg_icon_cache_update
}
