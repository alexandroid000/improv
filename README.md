A domain-specific language for live-coding robot motion.

Heavily inspired by / uses code from:
[Dance](http://haskell.cs.yale.edu/?post_type=publication&p=168),
[TidalCycles](https://tidalcycles.org/), and
[roshask](https://github.com/acowley/roshask).

Dependencies
-------------

-   [Stack (for managing Haskell dependencies and installation). Click here for installation instructions.](https://docs.haskellstack.org/en/stable/README/)
-   ROS (Robot Operating System). [Click here for install instructions](http://wiki.ros.org/ROS/Installation). We recommend installing Kinetic version or Indigo.
-   TurtleSim (will come with a full desktop ROS installation) or Gazebo ([click here for install instructions](http://gazebosim.org/tutorials?cat=install))
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
a ROS server, the simulator, and start monitoring the user input file.

If all does not go well, please check that you've installed all dependencies
and submit a bug report!

Edit the file `user_instructions.txt` in whatever text editor you prefer. When
this file is saved, the monitoring script will detect a change and interpret the
code, creating a ROS node which publishes commands to the simulator. You should
see the effects in the simulator immediately.

[Now you are ready to follow the tutorial to learn features of the improv
language!](https://github.com/alexandroid000/improv/wiki/Tutorial)
