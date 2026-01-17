# Assignment2


# Project Overview
This project applies automated data collection techniques and interactive visualisation to analyse street-level crime patterns in London. Using the UK Police API and OpenStreetMap (OSM), the project collects, processes crime data, and presents the results through an interactive R Shiny dashboard.

This project has social science significance by examining how crime is unevenly distributed across space, time, and crime categories in London. By integrating crime data with police station locations, it provides insight into spatial inequality in urban safety and the distribution of policing resources. The interactive visualisations further support social science analysis by making complex crime patterns accessible and interpretable.


# Project Structure
The compressed project folder contains:
- R scripts (`.R`)  
- Datasets (`.csv`)  
- README file(`.md`)  
- The main report (`.html`)  
- Shiny App (`.R`)
- Assignment coversheet
- The project (`.Rproj`)

# Data sources
- UK Police API
- OpenStreetMap (OSM)

Data were collected programmatically using API requests. Due to API rate limits and long runtimes (more than 1 hour), the processed datasets are included in the data/ directory to ensure reproducibility and ease of assessment.


# How users can get started with the project
The project is organised as an RStudio Project (.Rproj), which automatically sets the working directory to the project root. All scripts and the Shiny application use relative file paths. All required R packages are explicitly loaded at the beginning of the scripts.

The R version used for this project is 4.5.1 (2025-06-13).

# How users can reproduce the analysis

- Download and unzip the project folder.

- Open `Assignment2.Rproj` in RStudio.

- Run the R scripts to reproduce the data processing, and use the included datasets in `data/`.

- To launch the Shiny app, open `app.R` and click the "Run App" button.

# Where users can get help with your project
If you encounter issues or have questions, please email the UCL address.

# Who maintains and contributes to the project
This project is maintained by the candidate TZHM6 as part of Assessment 2.
