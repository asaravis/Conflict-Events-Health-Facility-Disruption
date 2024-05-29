# Exploring Predictive Composite Measures for Global Health Facility Disruptions In Conflict Zones: an Analysis of Armed Conflict and Location Event Data Project (ACLED) Indicators for Health Facility Disruption Status
This project explores predictive composite measures for health facility disruptions in conflict zones based on conflict intensity (measured through repeat conflict events in close proximity) over a defined period of time and type of conflict event. This analysis, in R, emplores an Inverse Distance Weighting Model to quantify conflict intensity for each health facility by conflict event type (battles, explosions/remote violence, riots, violent protests, and violence against civilians). Conflict intensity per conflict event type then serves as the exposure for a multivariate logistic regression model with health facility disruption status (physical, functional, physical & functional, or no disruptions) as the outcome.

The purpose of this analysis is to contribute to the strengthening of predictive models for health service coverage in areas affected by conflict. By gaining a deeper understanding of how different types of violent conflict impact the functionality and structure of health facilities, we can better understand how to target emergency health response and functionality status surveillence. 

## Instructions for Use
### Data Sources
The ACLED database is used for geolocated conflict events with descriptive variables (such as: date of event, latutude, longitude, event type, sub event type, fatailties, etc.). The data can be downloaded here: https://acleddata.com/data-export-tool/ 

This analysis is generalizable to input a desired data source for health facility disruption status. In this code, a simulated dataset is created for the disruption status and mimics the variables that are expected to be in such a dataset.

### Useage Instructions
The analysis is provided in the repository as: Conflict_Analysis.R
The R file includes all packages needed. Step-by-step code walkthrough is provided in: Code_Documentation.doc 

## Analysis: Visual Model
![Screen Shot 2024-05-19 at 3 06 38 PM](https://github.com/asaravis/Conflict-Events-Health-Facility-Disruption/assets/131823982/2f7bb9ff-1607-4caf-b056-58b3ee6a5595)

## Code Documentation: 
More detailed information can be found in Code_Documentation.doc which provides detailed background/rationale, codebook, methodology, analysis model, and code walkthrough. 

## Contact Information
For questions or suggestions, please contact Arden Saravis at ardens514@gmail.com.
