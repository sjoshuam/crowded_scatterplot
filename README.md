### Description
Scatterplot are a ubiquitous data visualization format.  However, they become ineffective or even inaccurate if there are too many points – the top layer of points hides the other points, creating a misleading picture of the data.  This project describes three solutions to the “crowded scatterplot” problem, using the geographic distribution of the United States population to illustrate each solution.

The US population demonstrates the crowded scatterplot problem well because a large share of the population lives in the tiny strips of land that hold America’s largest cities.  If you plot a point for each person at the approximate location of their residence, hundreds of millions of points are invisible beneath other points.

This project generates a poster-sized visualization that depicts the problem and three solutions to it.  The solutions are:

1.	Lump the population into a small number of groups.  Display the groups as dots sized according to the amount of people lumped into the group.  To generate these groups, I use k-means clustering to lump the population into 10,000 groups and then use hierarchical clustering (Ward’s minimum distance) to those groups to a few hundred.  Each group represents the population in about a 100-mile swath of land.  This solution is the least computationally expensive of the three solutions, but can make it difficult to tell where the major population centers lie within those swaths of land.
2.	Make points darker if at least a million people live within a 25-mile radius of the point.  This solution best emphasizes the location of populous places but double counts people in a way that can be more challenging to explain to non-technical audiences.
3.	Move all points that would have been under other points, so that all points are visible.  This solution most accurately depicts the size of the population in populous areas, but least accurately depicts the location of highly-populated cities.

This project relies on data from the United States Census Bureau.  The first script pulls population data via the Census Bureau’s API and GIS data via FTP file request.  The exact URLs are listed in the script.

### Repository Layout

This repository contains 3-4 directories.  For convenience, each directory name has a unique letter prefix, such as "X_".

+ **A_Inputs** - Holds all source data files.  For coding purposes, I treat this directory as read only.
+ **B_Intermediates** – Holds data files created as byproducts of the computations performed.  This can include cached API pulls, temporary data caches for large computations, and data objects to be passed from one script to the next.
+ **C_Outputs** – Holds the final products for the project.
+ **D_Misc** – If needed, this directory will contain any other useful materials for the project, such as source data documentation.

The scripts containing executable code are in the top level of the repository. Each script has a unique numeric prefix, such as "2_", which indicates the order in which to execute the scripts.

### Scripts (Actions, Inputs, and Outputs)

This section describes each the code in each script in this project.

**1_pull_data.R** - Downloads data from the US Census Bureau, compiles it into a single dataset, and generates an alternative version of that data necessary for one of the solution.  

Inputs: Retrieves all inputs from the Internet.  

Outputs (all in B_Intermediates):
+ population.RData - The total population and coordinates for each census tract in the contiguous United States.
+ pop_points.RData - A dataset of coordinates, where each point represents 2000 people in a census tract.  This is an alternative form of the data in population.RData.

**2_calculate_solutions.R** - Declares a collection of functions that parallel process chunks of a dataset, but store the results to storage memory instead of RAM.  This enables a laptop to conduct calculations involving a fine geographic mesh in hours instead of days.  Script uses these functions to calculate three sets of coordinates that represent the United States population more accurately than literally plotting points for each person living in a census tract.  Two of the solutions require some of the same computations - a geographic mesh and distances between mesh and each population center. Those computations are done once and then read twice to improve efficiency.

Inputs: B_Intermediates/population.RData and B_Intermediates/pop_points.RData, as described for the previous script.

Outputs (all in B_Intermediates):
+ cluster_solution.RData - A dataset of groups of census tracts.  For each group, it contains the coordinates of the population-weighted centroid of those tracts, the total population of those tracts, and the names of the tracts.
+ proximity_solution.RData - A dataset of regularly spaced geographic points within the contiguous United states (IE, a mesh) and the total population within 25 miles of each point.
+ spacing_solution.RData – A dataset of coordinates.  Each coordinate represents 2,000 people, but the coordinates are spaced to avoid overlapping each other.

**3_render_poster.R** – Renders a poster-like visualization of this project.  Describes the crowded scatterplot problem (top three panels) and explains three solutions to it (bottom three panels).

Inputs (all in the B_Intermediates directory):
+ From script #1: population.RData and pop_points.RData
+ From script #2: cluster_solution.RData, proximity_solution.RData, and spacing_solution.RData

Outputs (in C_Outputs):
+ crowded_scatterplot_poster.pdf - poster-sized visualization illustrating the crowded scatterplot problem and three solutions to it.  PDF file is formatted such that it could be printed as a 3ft x 2 ft wall poster.
+ crowded_scatterplot.png - PNG version of the PDF visualization.  This file is smaller and loads faster than the PDF, so it is useful for quick checks.  Provides the thumbnail for this project in the project gallery (https://sjoshuam.github.io/project_gallery/).

### Project Status and To-Dos

This project is still in progress.  I have written code to generate three solutions to the scatterplot problem for the United States population and have completed about half of the poster visualizing the results.  Tasks remaining:

- [ ] Write code to generate the top-right panel, which will depict the visible and obscured points in a geographic depiction of the US population.

- [ ] Write explanatory paragraphs for each of the solutions' poster panels

- [ ] Make sure the poster, README, and gallery text are fully aligned.

- [ ] Proof-read and improve the text on the poster, gallery text, and this README file.
