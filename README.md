A domain-specific language for live-coding robot motion.

Heavily inspired by / uses code from:
[Dance](http://haskell.cs.yale.edu/?post_type=publication&p=168),
[TidalCycles](https://tidalcycles.org/), and
[roshask](https://github.com/acowley/roshask).

Dependencies
-------------

-   [Stack (for installation)](https://docs.haskellstack.org/en/stable/README/)
-   ROS + TurtleSim (can change to use with any ROS-message compatible simulato
    r) (tested with kinetic)
-   some kind of shell (tested with zsh)

Install Instructions
-------------------

1. Clone repository
2. Run `stack build`

Getting Started
---------------

For now, this is an embedded DSL in Haskell - so we must compile the whole
library to run user-created code.

Run the script `improv.sh` in a terminal. If all goes well, this will launch
roscore, turtle simulator (can edit this file to change simulators), and launch
the monitoring script.

Edit the file `src/Demo.hs`. When this file is saved, the monitoring script will
detect a change and compile the code into a ROS node which publishes commands.
