# Distance to a Location

MoveApps

Github repository: *github.com/movestore/Distance-To-Loc*


## Description
Calculates the distance to a reference location and plots it for all individuals.

## Documentation
After downsampling your data, this App calculates the Vincenty ellipsoid distances in metres (R package geosphere) of all positions to the user-provided location. These distances are then plotted over time as lines (one per individual) in one plot. The reference location can be changed interactively.

### Input data
moveStack in Movebank format

### Output data
Shiny user interface (UI)

### Artefacts
`distance_table.csv`: csv-file with Table of all individuals' distances to the specified location (incl. individual ID, timestamp, longitude, latitude). The file is only generated for the initial parameter settings.

### Settings 
`Longitude of Reference Location`: longitude of the location to which the distance for each data position is calculated.

`Latitude of Reference Location`: latitude of the position to which the distance for each data position is calculated.

`Update!`: click on this button to update the calculation after changing the coordinates of the reference position.

### Null or error handling:
**Setting `Longitude of Reference Location`:** This parameter defaults to 0, which is the Prime Meridian. It can be interactively changed in the UI.

**Setting `Latitude of Reference Location`:** This parameter defaults to 0, which is the Equator. It can be interactively changed in the UI.

**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned.

