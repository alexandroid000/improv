#! /usr/bin/env bash

export IMPROV_DIR=${PWD}

roslaunch turtlebot_world.launch &
sleep 7s

./monitor src/test.imp ./mkDance
