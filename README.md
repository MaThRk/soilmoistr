# Package `soilmoistr`

- This package is intended to facilitate the processing of the soilmoisture data. 

- More information about the underlying data can be found [here](https://www.mdpi.com/2072-4292/10/8/1285).

## Usage: 

- The main function is called `get_sm_data`

### In order for this function to work you need:
  
* A folder with all the soilmoisture-tiffs. Each tiff should be in the format <SMCS1_YYYYMMDD_HHMMSS_TR#_(A/D)>
  
    * Where:
    
      + YYYYMMDD is the <year><month><day>
      
      + HHMMSS is the <hour><minute><second>
      
      + TR is a three digit number of the track (e.g. `117`)
      
      + `A` or `D` desribes if the satellite was in ascending or descending track
  
   
* A vector-geometry **with a column called `date` that actuall holds the dates**:
    * **Point**: If you use points (like landslide-points) you have the options to just use the points or to apply a spatial buffer. 
    
    * **Point with buffer**: You need to set the `point_buffer`- argument in the units of your vector geometry. As we are using polygons now, the tiff will be read into an object of type `raster` and the `exactextractr`-package will be used for extracting the raster values in that polyon. You can provide a vector to the `aggre_fun`-argument with the aggregation functions you want to apply on the raster-values intersecting with the buffered region. If no function is provided, all cell-values will be returned
    
    * **Polygons**: Behaves very similar to the points with buffers. Just don't need to set a `point_buffer argument`. Use again the `aggre_fun`-parameter to specify how you want to summarize the data intersecting with the polygons.
    
    
## Example
  
- An example call of the function can be made like this: 

```r
# In case the path points to a point-dataset, this will create a buffer of 200 m (if using a projected crs) arounf the points and extract the mean value of the intersection in case of a temoral match
# We consider the date of the landslide and 6 days before for potential intersections

res = get_sm_data(
  path_sm = <path_to_soilmoisture_data>,
  days_before_window = 6,
  landsld = <path_to_vector_geometry>/<or_object_of_type_sf>,
  point_buffer = 200,
  aggre_fun = c("mean")
)
```

- This will return a dataframe with that as many rows as in the original vector-geometry that had no `NA` in the `date`-column

- The soimoisture data is in a list-column called `sm_data`. Here for each landslide (each row), or any other vector, the soilmoisture data is split up into the actual values for the extraction, the path, track and time of the acquisition.

- This looks more or less like this: 

```r
 sm_values       date track swath     time
1         0 2018-03-31     D   326  5_17_22
2         0 2018-03-31     A   326 17_14_43
3         0 2018-04-01     A   326  17_5_39
4         0 2018-04-02     A   326 16_58_30
```
