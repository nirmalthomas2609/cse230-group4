# Spaceforce

#### Terminal interface for the Spaceforce game. Built using the haskell [brick](https://hackage.haskell.org/package/brick-0.18) library.

<p align="center">
  <img src="./docs/img/gameGif.gif"/>
</p>

## Project Architecture
The main components of the projects are
### 1. Ship module
  This module has the major function related to the functioning to the game. It implements the functionalities listed below and more.
  * Initialization of the game object
  * Random generation of the rocks.
  * Movement of Ship and the rocks.
  * Collision of Ship with rocks.
  * Updation of the Score.
  * Controlling the speed of the game.
### 2. UI module
  This module takes care of the game display on the terminal. The Ship module is imported into the UI module and used display the states on the UI. It also implements the function to bind keyboard inputs to the game functions.



## Installation
For installation from source follow these steps
1. Clone the repository using git clone.
```shell
git clone url
```
2. Install the required dependencies by running stack install
```shell
stack install
```

## Testing
The unit testing for game are in the test directory. They can run using
```shell
stack test
```
The result should show all tests passed.

## Playing the game

Launch the game using
```shell
stack run
```

After launching the game, follow the instructions on the menu. It should something like below.

<p align="center">
  <img src="./docs/img/gameMenu.jpg"/>
</p>

Press 'q' to quit at anytime, 'r' to restart the game.
