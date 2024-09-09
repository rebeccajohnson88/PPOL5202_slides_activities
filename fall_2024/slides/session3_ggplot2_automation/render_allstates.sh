#!/bin/bash
for state in "IL" "IN" "MI" "OH" "WI";
do
	cp state_template.qmd "${state}_template.qmd";
	quarto render "${state}_template.qmd" -o "cl_demreport_${state}.html" -P state:"$state";
done 
