# Agg -- aggregating data

Agg reads space separated input data and aggregates it based on a
script describing the aggregation.

The script file is based on a DSL which is tailored to make it easy
to work on nested data sets.


## Example use

Suppose we have a data file with information about commits from a CVS:

    2010-02-04   John   4   3   src/foo.sh
    2010-02-04   John   1   6   src/bar.sh
    2010-02-04   Bob    0   2   src/foo.sh
    2010-05-01   John   4   7   src/bar.sh
    2010-05-01   Bob    1   1   src/foo.sh

Each line is a commit with date, author, number of inserts, number of
deletes and file name.

Now we can easily write agg scripts to aggregate this data in various
ways.

Say we want to see total number of inserts per date. The script might
look like this:

    # First we declare names for each input column:
    columns [D,A,I,R,F]
    
    # We group the data based on the date field (D):
    group by [D] {
        
        # We want to aggregate inside each group, by summing it's changes:
        aggregate [sum(I), sum(R)]
    }
    
    # Finally, we can flatten the groups back into tsv again:
    flatten

Lines starting with `#` are comments.

The above program will output:

    2010-02-04	5	11
    2010-05-01	5	8


## Environment variables

If environment variable `IFS` is set, then it is used for input field
separator. Otherwise, spaces are used to separate fields.

If environment variable `OFS` is set, then it is used for output field
separator. Otherwise, tab character is used.


## Commands

These are the different commands which can be stringed to
together to form the script:

 - `aggregate`
 - `columns`
 - `cross join` 
 - `flatten`
 - `group by`
 - `project`
 - `sort by`
 - `take`


`columns [` _name_ ... `]`

Defines names for the columns (in that order), to be referenced in
later commands. Must be the first command in an aggregation script.

`flatten` 

Flattens nested datasets (groups) into a single, flat dataset.

`group by [` _column_ ... `]` `{` _command_ ... `}`

Splits the data set into groups (subsets) based in the given input
columns.

`project [` _column_ [ `as` _name_ ] ... `]`

Rename and/or move columns.

`aggregate [` _agg-function_ `(` _column_ `)` ... `]`

Aggregate all records in the data set into a single record.

Available aggregation functions:

 - `count`
 - `max`
 - `min`
 - `product`
 - `sum`

`sort by [` _column_ [ `desc` ] ... `]`

Sort the data set in the order based on the listed columns. Sort order
is either ascending (default) or descending if `desc` is added after
the column name.

`take` _number_

Keep the first _number_ records in the data set. Throw away the rest.

`cross join {` _command_ ... `}`

Input if cross joined with the result if applying the inner commands
to the input.
