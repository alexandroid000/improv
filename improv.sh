#! /usr/bin/env bash


export TURTLE=turtle.launch
export GAZEBO=turtlebot_world.launch
export IMPROV_DIR=${PWD}

# to switch simulators, should only need to change the SIM variable
export SIM=$GAZEBO

roslaunch $SIM &
if [ "$SIM" = "$GAZEBO" ]; then
    sleep 15s
else
    sleep 2s
fi

./monitor ./user_instructions.txt ./mkDance
