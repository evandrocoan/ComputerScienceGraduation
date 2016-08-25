


# Import the helper functions.
. ./../timer_calculator.sh


# sync files, usage example: sshpass -p 123 rsync -rvu ../../TeachingEpos/* user.name@so.user.name.vms.ufsc.br:~/2016-2_OperatingSystems/TeachingEpos
# Use this on your '~/.bashrc' file:
# export EPOS_COMPILER_MACHINE_PASS=123
# export EPOS_COMPILER_MACHINE_ADDRESS=user.name@so.user.name.vms.ufsc.br
#
sshpass -p $EPOS_COMPILER_MACHINE_PASS rsync -rvu ../../TeachingEpos/* $EPOS_COMPILER_MACHINE_ADDRESS:~/2016-2_OperatingSystems/TeachingEpos


# Get the application name
APPLICATION_TO_RUN=$(echo $1 | cut -d'.' -f 1)
export APPLICATION=$APPLICATION_TO_RUN


REMOTE_COMMAND_TO_RUN="cd /home/evandro.coan/2016-2_OperatingSystems/TeachingEpos;
pwd;
sh compile_and_run.sh;"

sshpass -p $EPOS_COMPILER_MACHINE_PASS ssh $EPOS_COMPILER_MACHINE_ADDRESS $REMOTE_COMMAND_TO_RUN;



showTheElapsedSeconds "$0"













