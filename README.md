A domain-specific language for live-coding robot motion.

Heavily inspired by / uses code from:
[Dance](http://haskell.cs.yale.edu/?post_type=publication&p=168),
[TidalCycles](https://tidalcycles.org/), and
[roshask](https://github.com/acowley/roshask).

Dependencies
-------------

-   [Stack (for installation)](https://docs.haskellstack.org/en/stable/README/)
-   ROS + TurtleSim (can change to use with any ROS-message compatible simulator) (tested with kinetic)
-   some kind of shell (tested with zsh and bash)

Install Instructions
-------------------

1. Clone repository
2. Run `stack build`

Getting Started
---------------

Run the script `improv.sh` in a terminal. If all goes well, this will launch
roscore, turtle simulator, and launch the monitoring script.

Edit the file `src/test.imp`. When this file is saved, the monitoring script will
detect a change and interpret the code, creating a ROS node which publishes commands
to the simulator. You should see the effects in the simulator immediately.
