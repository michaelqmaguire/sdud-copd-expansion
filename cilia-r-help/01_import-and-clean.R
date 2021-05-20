library(car)
library(nlme)

# ----------------------------------------------------------------------------------------------------------------#
# R is rooted in object-oriented programming                                                                      #
# You create 'objects' that for all intents and purposes are your datasets or elements of your data.              #
# It takes the form of object <- what you want to put in your object.                                             #
# The arrow assigns whatever you want to your object name.                                                        #
# You can also use an = to assign objects, but it's not recommended. People do it, though.                        #
# ----------------------------------------------------------------------------------------------------------------#

# My general approach to make code more consistent is to freely use carriage returns/enter between lines of code.
# I always assign my object on the top line, and then enter the commands below that line, like this:
# object <-
#   what_i_want_in_my_object.

# -------------------- #
# QUESTION 1 BEGINNING #
# -------------------- #


# Read in the file.

base <-
  read.csv(
    # specify file in your local directory.
    file = "./cilia-r-help/aggregate-by-state-with-rurality-treatment-flags.csv", # put the directory to your file here. note that R does not like backslashes.
    # base R converts strings to factors which isn't recommended.
    stringsAsFactors = FALSE 
  )

# Examine the contents. head() looks at the first 10 observations.

head(base)

# Examine the contents. tail() looks at the last 10 observations.

tail(base)

# Remove the suppression records.

baseNoSuppression <-
  base[base$suppression_used == "FALSE", ]

baseNoSuppression

# Aggregate your file.

baseAgg <-
  # with tells R that you want to take an object you've already created and use it in the following calculation
  with( 
    # this is your object that you want to conduct calculations on.
    baseNoSuppression,
    # The 'aggregate' function is one way of aggregating in R
      aggregate(
        # Since we're creating two summed up variables, we need to enclose these variables in a list. Lists can hold multiple elements.
        list(
          chipMedicaidEnroll_agg = chipMedicaidEnroll, # Here we're creating aggregate variables based on the old variables.
          total_prescriptions_agg = total_prescriptions # Here we're creating aggregate variables based on the old variables.
        ),
        # now we want to split these summations by groups of year, half-year, and group. Specify a list again.
        by = list(
          year = year,
          half_year = half_year,
          group = group
        ),
        # Then call on how we want to aggregate - we want to sum the chipMedicaidEnroll and total_prescription variables.
        FUN = sum
      )
    )

# Create the rx_rate variable.

baseAgg$rx_rate_per_hundred <-
  (baseAgg$total_prescriptions_agg / baseAgg$chipMedicaidEnroll_agg) * 100

# View the file

baseAgg

# Ordering dataset is necessary at this point.

baseAgg <-
  baseAgg[order(-baseAgg$group, baseAgg$year), ]

# We need to add our flags: "halfYear' time', 'level', 'trend', 'grouptime', 'grouplevel', and 'grouptrend'.

# First, we'll remove those records for 2014,1
# Notice the exclamation point before the statement - it means to remove those records.

baseAggFlags <-
  baseAgg

# baseAggFlags <-
#   baseAgg[!(baseAgg$year == "2014" & baseAgg$half_year == "1"), ]

# Create yearHalfYear flag

baseAggFlags$yearHalfYear <-
  # Paste0 concatenates or pieces together elements without spaces. Here we want '2011,1'.
  paste0(
    baseAggFlags$year, # first element we want is year
    ",", # then our comma
    baseAggFlags$half_year # then our half year.
  )

# See it in the output.

baseAggFlags

# Create the time flag.

baseAggFlags$time <- 
  # 'ave' is a function that allows you to do sequence operations
  ave( 
    baseAggFlags$year, #First group variable, our year
    baseAggFlags$group, # Second group variable, our group
    FUN = seq_along # seq_along() then creates a sequence pattern based on these groupings.
  )

# Create the level flag.

baseAggFlags$level <-
  ifelse( #ifelse is a pretty useful tool in R
    baseAggFlags$year >= 2014, # This says, if baseAggFlags$year is greater than or equal to 2014, 
    1, # Then set it to 1.
    0 # otherwise set it to 0.
  )

# Create trend flag. This isn't a very programmatic way to do it, but other usual functions struggled.

baseAggFlags$trend <-
  ifelse(
    baseAggFlags$level == 1,
    baseAggFlags$time - 6, # Here I'm just subtracting from another sequence to get the numbers that were needed.
    0
  )

# Create the group time variable.

baseAggFlags$grouptime <-
  ifelse(
    baseAggFlags$group == 1,
    baseAggFlags$time,
    0
  )

# Create group level variable.

baseAggFlags$grouplevel <-
  ifelse(
    baseAggFlags$group == 1,
    baseAggFlags$level,
    0
  )

# Create group trend variable.

baseAggFlags$grouptrend <-
  ifelse(
    baseAggFlags$group == 1,
    baseAggFlags$trend,
    0
  )

# Put treatment group into their own object.

baseAggFlagsTreat <- 
  baseAggFlags[baseAggFlags$group == 1, ]

# Put control group into their own object.


baseAggFlagsControl <-
  baseAggFlags[baseAggFlags$group == 0, ]

# Make plotting area have space for 1 row and 2 columns.

par(mfrow=c(1, 2))

# Make the plot of the treatment group.

plot(baseAggFlagsTreat$time,
     baseAggFlagsTreat$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="red",
     xaxt="n")

abline(v=6.5,lty=2)

points(
  baseAggFlagsTreat$time,
  baseAggFlagsTreat$rx_rate_per_hundred,
  col = "red"
)

# Give it a title.

title("Treatment Group")

# Make the plot of the control group.

plot(baseAggFlagsControl$time,
     baseAggFlagsControl$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="blue",
     xaxt="n")

abline(v=6.5,lty=2)
axis(1, at=1:12, labels=baseAggFlagsControl$yearHalfYear, )

points(
  baseAggFlagsControl$time,
  baseAggFlagsControl$rx_rate_per_hundred,
  col = "blue"
)

# Give it a title.

title("Control Group")

# Putting them in the same plot and saving them out.

png("./cilia-r-help/one-plot.png")
plot(baseAggFlagsTreat$time,
     baseAggFlagsTreat$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="red",
     xaxt="n")


points(
  baseAggFlagsTreat$time,
  baseAggFlagsTreat$rx_rate_per_hundred,
  col = "red"
)

par(new = TRUE)

plot(baseAggFlagsControl$time,
     baseAggFlagsControl$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="blue",
     xaxt="n")

points(
  baseAggFlagsControl$time,
  baseAggFlagsControl$rx_rate_per_hundred,
  col = "blue"
)
dev.off()

# End of plotting and file is saved out.

model <-
  lm(
    formula = rx_rate_per_hundred ~ time + group + grouptime + level + trend + grouplevel + grouptrend,
    data = baseAggFlags
  )

summary(model)

# -------------------- #
# QUESTION 1 END       #
# -------------------- #

# -------------------- #
# QUESTION 2 BEGINNING #
# -------------------- #

## Remove those with 2014,1.
## You have to re-run all your computations, here. There are more efficient ways to do this, but I think it's not worth delving into it now.

baseAggFlagsRm20141 <-
  baseAggFlags[baseAggFlags$yearHalfYear != "2014,1", ]

baseAggFlagsRm20141$yearHalfYear <-
  # Paste0 concatenates or pieces together elements without spaces. Here we want '2011,1'.
  paste0(
    baseAggFlagsRm20141$year, # first element we want is year
    ",", # then our comma
    baseAggFlagsRm20141$half_year # then our half year.
  )

# Create the time flag.

baseAggFlagsRm20141$time <- 
  # 'ave' is a function that allows you to do sequence operations
  ave( 
    baseAggFlagsRm20141$year, #First group variable, our year
    baseAggFlagsRm20141$group, # Second group variable, our group
    FUN = seq_along # seq_along() then creates a sequence pattern based on these groupings.
  )

# Create the level flag.

baseAggFlagsRm20141$level <-
  ifelse( #ifelse is a pretty useful tool in R
    baseAggFlagsRm20141$year >= 2014, # This says, if baseAggFlags$year is greater than or equal to 2014, 
    1, # Then set it to 1.
    0 # otherwise set it to 0.
  )

# Create trend flag. This isn't a very programmatic way to do it, but other usual functions struggled.

baseAggFlagsRm20141$trend <-
  ifelse(
    baseAggFlagsRm20141$level == 1,
    baseAggFlagsRm20141$time - 6, # Here I'm just subtracting from another sequence to get the numbers that were needed.
    0
  )

# Create the group time variable.

baseAggFlagsRm20141$grouptime <-
  ifelse(
    baseAggFlagsRm20141$group == 1,
    baseAggFlagsRm20141$time,
    0
  )

# Create group level variable.

baseAggFlagsRm20141$grouplevel <-
  ifelse(
    baseAggFlagsRm20141$group == 1,
    baseAggFlagsRm20141$level,
    0
  )

# Create group trend variable.

baseAggFlagsRm20141$grouptrend <-
  ifelse(
    baseAggFlagsRm20141$group == 1,
    baseAggFlagsRm20141$trend,
    0
  )

# Put treatment group into their own object.

baseAggFlagsTreatRm20141 <- 
  baseAggFlagsRm20141[baseAggFlagsRm20141$group == 1, ]

# Put control group into their own object.


baseAggFlagsControlRm20141 <-
  baseAggFlagsRm20141[baseAggFlagsRm20141$group == 0, ]

# Make plotting area have space for 1 row and 2 columns.

par(mfrow=c(1, 2))

# Make the plot of the treatment group.

plot(baseAggFlagsTreatRm20141$time,
     baseAggFlagsTreatRm20141$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="red",
     xaxt="n")

points(
  baseAggFlagsTreatRm20141$time,
  baseAggFlagsTreatRm20141$rx_rate_per_hundred,
  col = "red"
)

# Give it a title.

title("Treatment Group")

# Make the plot of the control group.

plot(baseAggFlagsControlRm20141$time,
     baseAggFlagsControlRm20141$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="blue",
     xaxt="n")

points(
  baseAggFlagsControlRm20141$time,
  baseAggFlagsControlRm20141$rx_rate_per_hundred,
  col = "blue"
)

# Give it a title.

title("Control Group")

# Putting them in the same plot and saving them out.

png("./cilia-r-help/one-plot.png")
plot(baseAggFlagsTreatRm20141$time,
     baseAggFlagsTreatRm20141$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="red",
     xaxt="n")

points(
  baseAggFlagsTreatRm20141$time,
  baseAggFlagsTreatRm20141$rx_rate_per_hundred,
  col = "red"
)

par(new = TRUE)

plot(baseAggFlagsControlRm20141$time,
     baseAggFlagsControlRm20141$rx_rate_per_hundred,
     ylab="COPD Rx Rate: per 100",
     ylim=c(0,5),
     xlab="Year HalfYear",
     type="l",
     col="blue",
     xaxt="n")

points(
  baseAggFlagsControlRm20141$time,
  baseAggFlagsControlRm20141$rx_rate_per_hundred,
  col = "blue"
)
dev.off()

# End of plotting and file is saved out.

model_rm20141 <-
  lm(
    formula = rx_rate_per_hundred ~ time + group + grouptime + level + trend + grouplevel + grouptrend,
    data = baseAggFlagsRm20141
  )

summary(model_rm20141)

  