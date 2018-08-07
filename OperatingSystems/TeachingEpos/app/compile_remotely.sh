#!/bin/sh


# Import the helper functions.
. ./../timer_calculator.sh

pushd `dirname $0` > /dev/null
SCRIPT_FOLDER_PATH=`pwd`
popd > /dev/null

# sync files, usage example: sshpass -p 123 rsync -rvu ../../TeachingEpos/* user.name@so.user.name.vms.ufsc.br:~/2016-2_OperatingSystems/TeachingEpos
# Use this on your '~/.bashrc' file:
# export EPOS_COMPILER_MACHINE_PASS=123
# export EPOS_COMPILER_MACHINE_ADDRESS=user.name@so.user.name.vms.ufsc.br
#
sshpass -p $EPOS_COMPILER_MACHINE_PASS rsync -rvu $SCRIPT_FOLDER_PATH/../../TeachingEpos/* $EPOS_COMPILER_MACHINE_ADDRESS:~/OperatingSystems/TeachingEpos

# sshpass -p $EPOS_COMPILER_MACHINE_PASS unison ../../TeachingEpos ssh://$EPOS_COMPILER_MACHINE_ADDRESS//home/evandro.coan/OperatingSystems/TeachingEpos


# Get the application name
APPLICATION_TO_RUN=$(echo $1 | cut -d'.' -f 1)


REMOTE_COMMAND_TO_RUN="cd /home/evandro.coan/OperatingSystems/TeachingEpos;
pwd;
sh compile_and_run.sh $APPLICATION_TO_RUN;"


sshpass -p $EPOS_COMPILER_MACHINE_PASS ssh $EPOS_COMPILER_MACHINE_ADDRESS $REMOTE_COMMAND_TO_RUN;

showTheElapsedSeconds "$0"













