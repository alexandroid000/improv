#! /usr/bin/env bash

export IMPROV_DIR=${PWD}

roslaunch turtlebot_world.launch &
sleep 15s

./monitor ./user_instructions.txt ./mkDance
