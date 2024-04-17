# This is the README of Adrien's final project for class "ECON2020 Spring24 S01 Applied Economics Analysis"

## Foreword
The project proposal associated to this project is located in "0_proposal". Be aware that the nature of the project was changed past the proposal submission. Consequently, the present project proposal is irrelated to the work presented in this repository.

## Data
A unique .dta file is necessary to replicate the project. It is located in "1_data".

## Code
A unique .r script needs to be run by the user to replicate the project anaylses. It is located in "2_analyses" and it is called "0_main.r". This script nets all the other scripts located in this directory, such that it should not be necessary to run or edit the latter. Note that the variables called in any of the other scripts are not necessarily defined in that script, therefore only advanced users could be expected to edit them or to run them separately in a relevant manner.

A snapshot of the environment necessary to run the script is provided in the main directory (files ".Rprofile" and "renv.lock"). The associated "renv" folder includes a ".gitignore" file due to its size. The environment can be set up using R package "renv" and following the usual procedure (setting this repository as working directory).

A series of unit tests are called at the end of the replication to check if the compilation outputs are as expected. In case of failed tests, advanced users can correct the scripts.

## Outputs
All .tex table outputs are located in "3_table". All .pdf vectorial figure outputs are located in "4_figures".

## Publishables
The documents used for the submission of this project are located in "5_submission".

## Comments
Should you run in an issue, please email Adrien Foutelet (adrien_foutelet@brown.edu).
