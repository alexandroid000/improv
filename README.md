A domain-specific language for live-coding robot motion.

Heavily inspired by / uses code from:
[Dance](http://haskell.cs.yale.edu/?post_type=publication&p=168),
[TidalCycles](https://tidalcycles.org/), and
[roshask](https://github.com/acowley/roshask).

Dependencies
-------------

-   [Stack (for managing Haskell dependencies and installation). Click here for installation instructions.](https://docs.haskellstack.org/en/stable/README/)
-   ROS + TurtleSim or Gazebo (tested with indigo and kinetic)
-   some kind of shell (tested with zsh and bash)

Install Instructions
-------------------

0. On a Linux computer (ideally Ubuntu distro), install dependencies above.

In a command line:

1. Clone repository (`git clone https://github.com/alexandroid000/improv.git`)
2. Install everything with `stack build` (will probably take a while)

The `master` branch uses TurtleSim currently. To use Gazebo, run `git checkout
gazebo-integration`.

Getting Started
---------------

Run the script `improv.sh` in a terminal. If all goes well, this will launch
roscore, the simulator, and start monitoring the user input file.

Edit the file `user_instructions.txt` in whatever text editor you prefer. When
this file is saved, the monitoring script will detect a change and interpret the
code, creating a ROS node which publishes commands to the simulator. You should
see the effects in the simulator immediately.

[Now you are ready to follow the tutorial to learn features of the improv
language!](https://github.com/alexandroid000/improv/wiki/Tutorial)
