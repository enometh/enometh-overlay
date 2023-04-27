#!/bin/sh
BASENAME=${0##*/}
INTERFACE=$1
DEVICE=$2
SPEED=$3
LOCALIP=$4
REMOTEIP=$5
IPPARAM=$6
EXPORT=`date +%Y%m%d%H%M%S`.$INTERFACE
#EXPORT=darkstat.db
if [ -p /dev/xconsole ] ; then
    export LOG=/dev/xconsole
else
    export LOG=/dev/null
fi
logger "MADHU: INTERFACE=$1 DEVICE=$2 EXPORT=$EXPORT" LOG=$LOG "3=$3 4=$4 5=$5 6=$6"
start-stop-daemon --stop --exec /usr/sbin/darkstat --pidfile /run/darkstat-ppp-$INTERFACE.pid
start-stop-daemon --start --exec /usr/sbin/darkstat -- --pidfile /run/darkstat-ppp-$INTERFACE.pid --user darkstat -i $INTERFACE -b 127.0.0.1 --chroot /var/log/darkstat --export $EXPORT --import $EXPORT  >> $LOG 2>&1
