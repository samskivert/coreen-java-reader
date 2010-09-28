#!/bin/sh

RDIR=`dirname $0`
CLASSPATH=`echo $RDIR/project/boot/scala-2.8.0/lib/scala-library.jar \
    $RDIR/lib_managed/scala_2.8.0/compile/* \
    $RDIR/target/scala_2.8.0/coreen-java-reader_2.8.0-0.1.jar \
    /usr/local/java/lib/tools.jar | sed 's/ /:/g'`
exec java -classpath $CLASSPATH coreen.java.Main "$@"
