**Conway's Game of Life Animation using R**

This repository contains a script in R language that simulates the famous cellular automaton, Conway's Game of Life. The script generates a GIF animation of the game's evolution over a specified number of iterations.

**How to use**

1. Clone the repository onto your local machine using `git clone https://github.com/your-username/gptconway.R.git`
2. Navigate to the repository directory using `cd gptconway.R`
3. Run the script using `Rscript gptconway.R` (make sure you have R and Rscript installed on your system)
4. The script will generate a GIF animation named `conways_game_of_life.gif` in a directory named `outputs` in your current working directory

**Technical Details**

* The script uses the `dplyr`, `ggplot2`, `gganimate`, and `gifski` packages for data manipulation, visualization, and animation.
* The game is initialized with a random grid of 50x50 cells, with 20% chance of starting alive.
* The `next_gen` function implements the rules of Conway's Game of Life, where a live cell dies if it has less than 2 or more than 3 live neighbors, and a dead cell becomes live if it has exactly 3 live neighbors.
* The script collects the grid data over a specified number of iterations (100 in this case) and creates a data frame for each iteration.
* The data frames are then combined and used to create a ggplot animation using `gganimate`.
* The animation is saved as a GIF file using `gifski`.

**Future Improvements**

* Implement user input for customizing the grid size, starting conditions, and number of iterations.
* Modify the animation to include additional visualizations, such as grid edges or grid boundaries.
* Experiment with different game rules or variations to create new and interesting patterns.

**Acknowledgments**

Conway's Game of Life is a well-known cellular automaton first proposed by British mathematician John Horton Conway in the 1970s. This script is inspired by the original concept and implementation, and is intended for educational and recreational purposes only.