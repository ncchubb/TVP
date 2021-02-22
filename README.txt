.README
# This is a small set of code to recode The Violence Project 
# Database Excel file into a streamlined, easily accessible 
# and interchangeable set of data that is easier to work with.
#
# (1)
#
# First, take the "TVP-Mass-Shooter-Batabse-Version-2.xlsx" 
# and remove the entire first line. All of the information 
# on the first line is descriptive of categories that are 
# already described efficiently in the Codebook. This is 
# helpful for doing swifter data analysis by eliminating an 
# extra header when we read it into R or other data analysis
# tool other than Excel.
#
# I also renamed the 'Case #' variable to 'case_num'. This
# parses easier in UTF-8 and would be difficult to work with
# in most programs.
#
# (2)
#
# I had trouble wrangling this date from the 'Full.Date' column.
# There is a function at the end of the 'frcode' and 'append' files to
# ensure that there is a separate date class object (date_col) from 
# the day, month, and year column.
#
# This is especially useful if you want to use the Lubridate 
# package in the future or parse x-axis as.Date() in ggplot2.
#
#
# (3)
#
# Save the file as a CSV UTF-8 file. 
#
#
# (4)
#
# Your headers should be parsed in the form used by this
# script in R or RStudio.
#
#
# -----------------
#     The Codes
# -----------------
#
#
# (5) TVP_recoding_append.R
#
# This is a code that appends the dataset by adding a .txt column 
# for every variable.
#
# These new columns will parse according to the values set
# within the codebook and then print out the text in the cells.
# 
# There are two issues: the pairs of integer categorical variables
# are only appended at the end of the other dataset.
#
# The second issue is that the pairs do not match exact in their text.
# For consistency across the dataset, all _txt variables appended
# were all lower cased with _underscores_ for spaces, as the .csv
# as parsed by R inserted .periods. 
# 
#
# 
# (6) TVP_rename.R
# 
# This script renames all the column names to shorter names that match
# their _txt counterparts and neatly makes every variable easy to access
# in further analysis in R as the titles have both regular and regular_txt
# variables.
# 
# (7) TVP_frcode.R
#
# frcode() is a useful function in a package written by github.com/ryanburge.
# See the .readme here: https://ryanburge.github.io/socsci/index.html
# for details.
#
# It is useful to "recode and keep all the factor variables."
#
# Install the needed packages.
#
# The frcode() script here will only work with the renamed columns.
# Make sure that TVP_rename.R was run
#
# (8) 4_TVP_rest.R script.
# 
# This script demonstrates just why the frcode() is so handy.
#
#
# -------------------------
# ncchubb@eiu.edu
# Noah Chubb / Noelle Chubb
# @NoelleNope
# --------------------------