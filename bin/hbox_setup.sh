#! /bin/bash
# vboxhbox.sh
# Start or stop the virtual machines for hbox dev environment testing


vbnames="GW1 GW2 H1C1 H1C2 H2C1 H2C2 RendServ"
whattodo=$1

usage="Usage: $(basename $0) command
Commands:
       start - Start all the virtual machines($vbnames)
       save - Save all the running virtual machines($vbnames)
       shutdown - ACPI shutdown the all running virtual machines($vbnames)
       poweroff - Force poweroff the all running virtual machines($vbnames)
       screen - Start a screen session and ssh into all running virtual machines($vbnames)"

# Parameters
if [ -z "$1" ]; then
    echo "$usage"
    exit
fi

if [ "$whattodo" = "start" ]; then
    echo "Starting all the virtual machines ( $vbnames )"
    for i in $vbnames;
    do
        echo "Starting $i"
        VBoxHeadless -startvm $i -vrdp off &
        sleep 1
    done;
elif [ "$whattodo" = "save" ]; then
    echo "Saving all the virtual machines ( $vbnames )"
    for i in $vbnames;
    do
        echo "Saving $i"
        VBoxManage controlvm $i savestate
    done;
elif [ "$whattodo" = "shutdown" ]; then
    echo "Saving all the virtual machines ( $vbnames )"
    for i in $vbnames;
    do
        echo "Shutting down $i"
        VBoxManage controlvm $i acpipowerbutton
    done;
elif [ "$whattodo" = "poweroff" ]; then
    echo "Powering off all the virtual machines ( $vbnames )"
    for i in $vbnames;
    do
        echo "Powering off $i"
        VBoxManage controlvm $i poweroff
    done;
elif [ "$whattodo" = "screen" ]; then
    echo "Trying to start a screen session and sshing into all the machines ( $vbnames ). Use \"screen -r -S vboxsession\" to connect to it"
    screen -d -m -S vboxsession -c ~/.screenrc.vbox
else
    echo "$usage"
    exit
fi

exit
