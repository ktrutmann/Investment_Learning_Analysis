
# Analysis of the Context sensitive Learning Project

The files should be run in the order indicated by the file names.

If you source the `make_codebook.R` file, you should then be able to call the objects `cb_dat_main_long` and `cb_complete_table`.
	These objects have the same names as the columns in `dat_main_long` and `complete_table` but contain a sort description of the respective variable.

## General Notes:

The `vroom` package which is used to read in the data gets tripped up by double quotes followed by a comma in the data.
Otree saves the list of pages as "[""Page1"", ""Page2""]" with the double quotes indicating a single quote that should not end the string.
While this usually works fine for `vroom` for some reason it interprets the double quotes as end of string if it is followed by a comma.
I therefore manually removed these characters as I don't need that list anyway.