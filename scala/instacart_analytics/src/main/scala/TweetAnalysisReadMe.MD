### Problem 1

Loaded raw tsv data into dataframe using sqlContext csv reader by providing appropriate options like delimeter, multiLine, escape, quote 
Once Dataframe is loaded, used .chache() to cache it in memory so that more computations can be done on top of it

### Problem 2

Registered DataFrame as temp table with name tweets
Using sqpark sql query `select wordle_id, count(*) cnt from tweets group by wordle_id limit 1`, extracted top tweeted wordle_id in a DataFrame
Get the only row from the DataFrame using head and then print the output

### Problem 3

Created rdd from DataFrame
Defined class `WordCount` with 3 mutable parameters playCount, theCount and wordleCount and a function `getFinalList` which can return list of word and count as tupple with descending sort on count
Function `wordsCount` which takes each tweet text as input and return object of `WordCount` with all 3 counts filled
Used map function on top of rdd to convert each row tweet text into object of `WordCount` by calling `wordsCount` function
used reduce funtion on top of above rdd to reduce all the rows into single `WordCount` object by calling `red` function which takes 2 `WordCount` objects and merges into single `WordCount` object by adding corresponding counts
Finally call `getFinalList` function on top of final object `WordCount` to get final result and print it

### Problem 4

Defined a function `convertDateToweek` which takes input date format as input and return Day of the week. This uses `DateTimeFormatter` and `LocalDateTime` to achive the result
select only `tweet_date` on top of DataFrame and convert it to RDD
used map function on top of RDD to transform the raw date to tupple of (Week of day, 1). value of tupple is constant
used `reduceByKey` function on top of above RDD to aggregate all rows on top of Week of day
Sort above RDD using `sortBy` on count (descending order) and take top element from it and print the end result
