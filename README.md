# Rubik's Cube Simulator - coded in Haskell, using Reflex, 

A simulator for a Rubik's Cube.


Click here to see it in action:  http://dc25.github.io/rubiksCubeReflex/


Although this is still work in progress, I'll try to keep the above link pointing to a working version.

This branch (eventsTest) contains experiments with disabling events.  The hope was that animation performance would improve.

On chrome, the best I could do with events enabled was the top face rotating at 10 degrees per step at 0.09 seconds per time step.  Decreasing the time step made the animation choppy.  Turning off selection altogether ( except for a button to trigger face rotation ) made no difference.

On firefox, the results were the same except that I started to see the bad performance at 0.15 seconds per time step.

In both cases, I tried this both by returning a Reflex "never" event from the arrow drawing function and by removing the returned event altogether from the arrow drawing function.
