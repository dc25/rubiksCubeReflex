# Rubik's Cube Simulator - coded in Haskell, using Reflex, 

A simulator for a Rubik's Cube.


Click here to see it in action:  http://dc25.github.io/rubiksCubeReflex/


Although this is still work in progress, I'll try to keep the above link pointing to a working version.

The version on the branch you are now looking at, failedAlternateFaceOrientation, is an experiment that did not pan out.

The idea was to draw the sides of the cube with orientations that are relative to the top.  The intention was to eliminate the need for the turn count and assembly matrix in the model view computation.  This almost worked but failed because I overlooked the fact that the top of the model gets changed every time a selection takes place and this method didn't correctly update the orientation of the model to account for that.

Maybe there's a way to make this work but I'm putting it on hold.
