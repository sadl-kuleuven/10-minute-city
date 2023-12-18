# The '10-minute City'

This repository contains documentation of the model produced within the framework of the project: “Scientific support for the refinement of the concept of the "10-minute city" that in the context of the municipal plan for sustainable development of the City of Brussels.” The tutorial guides the user through the implementation and the execution of the model to construct maps indicating the levels of accessibility within the Administration of the City of Brussels. 
![alt text](https://github.com/sadl-kuleuven/10-minute-city/blob/main/assets/V10_pratique_brouillon.png)


The workflow is split in three parts: data preparation, isochrones and heatmaps. A background knowledge of FME is necessary. An overview of the workflow is given below, with the more detailed tutorial supplied [here](https://github.com/sadl-kuleuven/10-minute-city/blob/main/Tutorial%201.0.pdf)

## Data Preparation
Before calculating the isochrones or heatmaps, the input data needs be preprocessed. There are two types of input data: the supply variables and the roadmap. A roadmap is required to calculate isochrones around the collection of datapoints of a supply variable. 
![alt text](https://github.com/sadl-kuleuven/10-minute-city/blob/main/assets/isochrones%20calc.png)


## Isochrones
For each of the supply variables, isochrone maps depicting the areas accessible from all datapoints within a certain time threshold are generated. In order to optimize and automate this process, an R script is integrated in FME. The FME workflow is different depending on the spatial element, points or polygons. 
![alt text](https://github.com/sadl-kuleuven/10-minute-city/blob/main/assets/isochrones%20piscines.jpg)

## Heatmaps
In order to measure the spatial-temporal accessibility for different (sub)themes, heatmaps were created. A heatmap is constructed by overlaying the isochrone maps of each variable within one (sub)theme. The resulting intersecting areas receive a score, calculated as the sum of each variable’s score multiplied by the variable’s weight. The score of a variable equals its weight if the variable is present in the area and equals zero if the variable is not present in the area. The weights are assigned by the user. The level of accessibility of an area is expressed in % and equals its score times 100. The calculations are automated using the software FME. The user can specify the mode of transport (by bike or by foot) and the (sub)theme(s) for which the model should run heatmaps. Also the file location of the supply variables and the destination folder of the resulting heatmaps can be set by the user. 
| ![Accessibility in percentage, on foot](https://github.com/sadl-kuleuven/10-minute-city/blob/main/assets/heatmap%20analyse.jpg) | 
|:--:| 
| *Accessibility(%) on foot* |
