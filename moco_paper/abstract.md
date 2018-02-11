We introduce a new software tool, Improv, for high-level description of
robotic motion, along with immediate simulation of the resulting movement.

The system is composed of a domain-specific language, which compiles to
instructions in a
Haskell client library for ROS, the "Robot Operating System," which controls the robot.

ROS is an open-source control architecture for robotics which is widely used in
academia and supported by many commercially available robots.

The domain-specific language is inspired by and using
abstractions from choreographic terminology. It allows for several ways of
composing and transforming movement primitives.

The DSL is compiled
to a ROS node which publishes messages to any simulator compatible with ROS.

The compilation step is performed whenever the user saves changes to their program
file, creating a "live-coding" interface which works with any text editor and
provides immediate visual feedback in the robot simulator.

We hope this tool will be of interest to people looking for an accessible way to
generate robot motion, such as educators, artists, and researchers. We discuss
plans for upcoming user studies, to measure the effect of the immediate feedback
and accessible interface on usability and user satisfaction.
