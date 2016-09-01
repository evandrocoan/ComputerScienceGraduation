

CURRENT_PATH=$(pwd)
COMMAND_TO_RUN="pwd; echo '$CURRENT_PATH/app';cd $CURRENT_PATH/app; sh compile_remotely.sh $1"

echo $CURRENT_PATH


# Declare an array variable
# You can access them using echo "${arr[0]}", "${arr[1]}"
declare -a terminal_list=( "mintty" "xfce4-terminal" "konsole" "gnome-terminal" "xterm" )

# Now loop through the above array
for current_terminal in "${terminal_list[@]}"
do
    if command -v $current_terminal >/dev/null 2>&1; then
        /bin/$current_terminal -w max -h always -e /bin/bash --login -i -c "$COMMAND_TO_RUN"
        exit 0
    fi
done















