#!/bin/bash
cd "/media/john/Shared Linux_Windows Files/MORG/CPS_demo"
/usr/local/stata12/stata-se -b CPS_demo.do
cd "/media/john/Shared Linux_Windows Files/MSA Level Inequality/Code"
Rscript MORG_education.R
