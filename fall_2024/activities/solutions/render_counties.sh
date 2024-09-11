#!/bin/bash
for county in "COOK" "CHAMPAIGN" "LAKE";
do
	cp 01_county_template_solutions.qmd "${county}_template.qmd";
	quarto render "${county}_template.qmd" -o "cl_edreport_${county}.html" -P state:"IL" -P county:"$county";
done 
