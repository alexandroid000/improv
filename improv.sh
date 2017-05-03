#! /usr/bin/sh

roslaunch turtle.launch &

monitor file src/Demo.hs ./mkDance
