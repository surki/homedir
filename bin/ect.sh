#!/bin/bash -l

SERVERNAME=surki
#Attempt to connect to an existing server
emacsclient -c -s $SERVERNAME -t $*
if [ $? -ne 0 ]
then
#Start a new emacs server and connect
preload_emacs.sh $SERVERNAME 0
emacsclient -c -s $SERVERNAME $*
fi
