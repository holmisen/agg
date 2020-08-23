# Agg -- aggregating csv data

Agg is a tool which reads character separated input data and
aggregates it according to a script. The script is written in a DSL
which is tailored to make it easy to work on nested datasets.

The DSL provides constructs (commands) similar to SQL, but with some
added benefits:

 - It provides commands to work on nested data sets.

 - Commands are stringed together to describe the desired
   transformations.

Nested data sets are created using the `group by` command. (See
reference below.) This command takes a set of columns to group on, and
a block of commands describing how to aggregate the nested data sets.

Nested data sets can in turn be nested and so on.


## Example use

Suppose we have a data file with information about commits from a CVS:

    2010-02-04   John   4   3   src/foo.sh
    2010-02-04   John   1   6   src/bar.sh
    2010-02-04   Bob    3   2   src/foo.sh
    2010-05-01   John   4   7   src/bar.sh
    2010-05-01   Bob    1   1   src/foo.sh

Each line is a commit with date, author, number of inserts, number of
deletes and file name.

Now we can easily write agg scripts to aggregate this data in various
ways.

### Example 1

Say we want to see total number of inserts and deletes per date. The
script might look like this:

    # First we declare names for each input column:
    columns [D,A,I,R,F]
    
    # Now group the data based on the date field (D):
    group by [D] {
        
        # Aggregate each group, by summing it's changes:
        aggregate [sum(I), sum(R)]
    }
    
    # Finally, flatten the groups back into flat records:
    flatten

Lines starting with `#` are comments.

The above script will produce:

    2010-02-04	8	11
    2010-05-01	5	8

### Example 2

Now we want to see what file has the most number of inserts per
day. The following script would provide:

    columns [D,A,I,R,F]
    
    group by [D] {
       group by [F] {
          aggregate [sum(I)]
       }
       flatten
       sort by [I desc]
       take 1
    }
    flatten

This will produce:

    2010-02-04	src/foo.sh	7
    2010-05-01	src/bar.sh	4


## Field separators

If environment variable `IFS` is set, then it is used for input field
separator. Otherwise, spaces are used to separate fields.

If environment variable `OFS` is set, then it is used for output field
separator. Otherwise, tab character is used.


## Command reference

These are the commands which can be stringed to together to form the
script:

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

Flattens nested data sets (groups) into a single, flat data set.

`group by [` _column_ ... `]` `{` _command_ ... `}`

Splits the data set into groups (subsets) based in the given input
columns.

Commands inside the braces act on the groups.

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


## Building

 1. Install the [stack](haskellstack.org) build tool.

 2. Run `stack install` to build and install.

