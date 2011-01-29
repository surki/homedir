host=`hostname -s`
if [ $host == "surki-tml" -a ! "$(pidof autossh)" ]
then
    autossh -f -N -R 2222:localhost:22 home -S "none"
fi

