## R Browser-Based Animation & Visualization Workspace

This project is all about creating a browser-accessible workspace for R animations and visualizations. It runs inside a containerized environment using Docker and Docker Compose, making it reproducible and easy to set up anywhere. The goal is to have a smooth experience for developing and sharing R-based graphics, animations, and data visualizations without dealing with dependency headaches.

### What's Inside?
It's built on top of `rocker/rstudio`, so you get RStudio in the browser with all the necessary libraries for animation and graphics. It includes `ggplot2`, `gganimate`, `gifski`, and more, making it easy to experiment with dynamic and interactive visualizations. The stack also includes Rust and Cargo, in case you want to optimize performance with compiled code.

### Getting Started
To spin up the environment, just clone the repo and run:

```sh
docker-compose up --build
```

Then, head to `http://localhost:8787` in your browser, and you're good to go. Authentication is disabled for convenience in local development.

### Why Docker?
Reproducibility. No more fighting with system dependencies. If it works on your machine, it'll work on anyone else's with just a `docker-compose up`.

### Example: Conway’s Game of Life
One of the animations in this project is an implementation of Conway’s Game of Life using `ggplot2` and `gganimate`. It generates a grid-based simulation, animating how cells evolve over time according to the classic rules.

Once you're inside the container, you can run the script to generate a GIF of the simulation, saved in the `outputs` folder.

---

Built with love for reproducibility and creative R visualizations.

