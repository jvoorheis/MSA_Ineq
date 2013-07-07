#!/bin/bash

cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code"
stata-se -b ACS_read_in.do
R CMD BATCH ACS_cleaning.R
R CMD BATCH Census_cleaning.R
