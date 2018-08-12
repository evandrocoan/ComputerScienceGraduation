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
# EPOS_COMPILER_MACHINE_PASS=admin123
# EPOS_COMPILER_MACHINE_ADDRESS=linux@127.0.0.1

TARGET_DIRECTORY='~/OperatingSystems/TeachingEpos'
sshpass -p $EPOS_COMPILER_MACHINE_PASS ssh $EPOS_COMPILER_MACHINE_ADDRESS mkdir -p $TARGET_DIRECTORY
sshpass -p $EPOS_COMPILER_MACHINE_PASS rsync -rvu $SCRIPT_FOLDER_PATH/../../TeachingEpos/* $EPOS_COMPILER_MACHINE_ADDRESS:$TARGET_DIRECTORY

# https://tech.tiq.cc/2016/04/how-to-use-unison-for-automated-two-way-file-synchronization-on-linux-ubuntu-and-windows-and-android/
# sshpass -p $EPOS_COMPILER_MACHINE_PASS unison $SCRIPT_FOLDER_PATH/../../TeachingEpos ssh://$EPOS_COMPILER_MACHINE_ADDRESS//home/evandro.coan/OperatingSystems/TeachingEpos -auto


# Get the application name
APPLICATION_TO_RUN=$(echo $1 | cut -d'.' -f 1)


# REMOTE_COMMAND_TO_RUN="cd /home/evandro.coan/OperatingSystems/TeachingEpos;
REMOTE_COMMAND_TO_RUN="cd $TARGET_DIRECTORY;
pwd;
sh compile_and_run.sh $APPLICATION_TO_RUN $2;"


sshpass -p $EPOS_COMPILER_MACHINE_PASS ssh $EPOS_COMPILER_MACHINE_ADDRESS $REMOTE_COMMAND_TO_RUN;

showTheElapsedSeconds "$0"













