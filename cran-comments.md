## Test environments
* local MRAN install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

On Windows, the only NOTE is regarding the new submission.
On ubuntu, an additional NOTE is raised regarding the package size.
I seek an exemption from the data limits. 

- This is a data package using data from the 2016 Australian Census so is unlikely to be updated.
- The data is less than the limit on Windows machine, and exceeds the limit by less than 0.5 MB on ubuntu.
- The data has been optimally compressed: `tools::resaveRdaFiles()` was run over the `.rda` files. 
- All numbers in the data sets have been cast to integers if possible.  

