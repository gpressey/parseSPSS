# parseSPSS
The package parses SPSS syntax files describing fixed width text files.

Often datafiles are provided by institutions such as Statistics Canada in a fixed width text file, with variables described in an SPSS syntax file (`.sps` format). This package is designed to avoid the tedious, manual work of 

# status

This is currently a work in progress. All comments and advice are appreciated.

# use

To load a file, pass the location of a `.sps` script and the `.txt` fixed-width file to the function `read_sps_fwf`.

Currently, it returns a `list` with two objects: the processed datafile, and a data dictionary with all variables, labels, and levels.
