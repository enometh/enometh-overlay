module github.com/evilsocket/opensnitch/daemon

go 1.14

require (
	github.com/evilsocket/ftrace v1.2.0
	github.com/fsnotify/fsnotify v1.4.7
	github.com/google/gopacket v1.1.14
	github.com/google/nftables v0.0.0-20220210072902-edf9fe8cd04f
	github.com/iovisor/gobpf v0.2.0
	github.com/vishvananda/netlink v0.0.0-20210811191823-e1a867c6b452
	github.com/vishvananda/netns v0.0.0-20200728191858-db3c7e526aae // indirect
	golang.org/x/net v0.0.0-20211209124913-491a49abca63
	golang.org/x/sys v0.0.0-20211205182925-97ca703d548d
	google.golang.org/grpc v1.32.0
	google.golang.org/protobuf v1.26.0
)
