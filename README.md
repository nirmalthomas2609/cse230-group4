# Flappy Bird (Using Brick)

## Proposal

For the project, we will be trying to replicate a basic version of the popular game Flappy Bird (https://flappybird.io/) using the brick library in haskell. Following are the features we plan on implementing during the project:

### Score Update
- Keep track of the score which increments linearly with number of pipes passed

### Bird Movement
- Bird position on the X-axis remains constant
- When no input has been provided, bird position falls on the Y-axis non-linearly (to try and simulate gravity)
- When an input is provided, the bird position moves up by certain number of pixels over N screens and then status of the bird changes to "No Input" where in the bird continues to drop. Also, upward jump happens in a series of non-linear upward movements over the next set of screen renderings.

### Pipes
- Pipes are spawned at a given position with a constant gap from the previous pipe rendered on the screen. 
- Every pipe moves it's position on the X-Axis to the left by a certain number of pixels every time the screen is re-rendered
- A random position on the Y-Axis is determines as the start of the gap's top on the pipe and the height of the gap remains constant throughout 

### Game Ending Conditions
- If the position of the bird reaches a particular point on the Y-Axis (determined to be the ground)
- If the position of the pipe on the X-Axis is the same as that of the bird and the bird's position on the Y-Axis is not part of the pipe's gap, then a collision is determined and the game ends

### Leaderboard
- Keep track of history of highest scores

### Wishlist of features
- Gap on the pipe moves within the Y-Axis dynamically every screen rendering