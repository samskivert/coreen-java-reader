#!/bin/sh

RDIR=`dirname $0`
CLASSPATH=`echo $RDIR/target/scala_2.8.0/coreen-java-reader_2.8.0-0.1.min.jar \
    /usr/local/java/lib/tools.jar | sed 's/ /:/g'`
exec java -classpath $CLASSPATH coreen.java.Main "$@"
