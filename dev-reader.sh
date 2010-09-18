#!/bin/sh

RDIR=`dirname $0`
CLASSPATH=`echo $RDIR/target/scala_2.8.0/classes \
    $RDIR/lib_managed/scala_2.8.0/compile/*.jar \
    $RDIR/project/boot/scala-2.8.0/lib/scala-*.jar \
    /usr/local/java/lib/tools.jar | sed 's/ /:/g'`
exec java -classpath $CLASSPATH coreen.java.Main "$@"
