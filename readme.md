
## Readme



### Data Cleaning / Merging

#### Merge data sets

- Merge sets on `component_id`
- Merge `test_set.csv` with
   - `bill_of_materials.csv`
   - `components.csv`
   - `tube.csv`
   - `spec.csv`
   - `tube_end_form.csv`
   - `tube_end_form.csv`

#### Deal with Missing Data

- Remove all columns with more than 10% missing data.
- For everything else:
   - Impute the median for all other data.
   - Consider NA as data type for categorical variables.

#### Categorical

- Count the number of factors for each variable.
- If a column has more than X factors, we will consider every factor category greater than X to be the same factor, assigned to factor number X.  
