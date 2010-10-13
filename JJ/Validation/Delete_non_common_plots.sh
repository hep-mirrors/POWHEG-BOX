#!/bin/bash
##########################################################
# What this does:
# Takes two topdrawer files and deletes any plots in them
# which they do not have in common.
#
# How it works:
# It identifies all plots in a topdrawer file according
# to their title strings then it loops over the first 
# file's list of titles, for each one it checks if the
# list of titles for the second file contains the same
# string. If the check fails it deletes the histogram in
# the first file. The process is then repeated the other
# way around checking the first file's titles against 
# those in the second file...
#
# To run:
# ./Delete_non_common_plots.sh first_file.top second_file.top
##########################################################
#

file_a=$1
file_b=$2
rm -f temp_?
grep TITLE $file_a  | grep -v "INT\|ENT\|OFL\|UFL\|LEFT\|SET\|BOTTOM" > temp_a
grep TITLE $file_b  | grep -v "INT\|ENT\|OFL\|UFL\|LEFT\|SET\|BOTTOM" > temp_b
number_a_titles=`sed -n '$ =' temp_a`
number_b_titles=`sed -n '$ =' temp_b`
file_a_titles=`cat temp_a`
file_b_titles=`cat temp_b`
file_a_counter=0
file_b_counter=0

echo "Found $number_a_titles in $file_a"
echo "Found $number_b_titles in $file_b"

IFS=""

for (( i=1 ; i<=$number_a_titles ; i++ ))
do
    ith_title=`sed -n "$i p" temp_a`
    ith_name=`echo $ith_title | sed "s/\(.*\)\"\(.*\)\"\(.*\)/\2/"`
    ith_name_no_spaces=`echo $ith_name | sed "s/\ //g"`
    found_title="false"
    for (( j=1 ; j<=$number_b_titles ; j++ ))
    do
	jth_title=`sed -n "$j p" temp_b`
	jth_name=`echo $jth_title | sed "s/\(.*\)\"\(.*\)\"\(.*\)/\2/"`
	jth_name_no_spaces=`echo $jth_name | sed 's/\ //g'`
	if [ "$ith_name_no_spaces" = "$jth_name_no_spaces" ]
	then
	    found_title="true"
	fi
    done
    if [ "$found_title" = "false" ]
    then
	echo "Deleting plot $ith_name from $file_a" 
	./Delete_plot.sh $file_a "$ith_name" > blahblah
	rm -f blahblah
	file_a_counter=$((file_a_counter+1))
    fi
done

unset $IFS
echo "Deleted $file_a_counter files from $file_a"
echo " "

for (( i=1 ; i<=$number_b_titles ; i++ ))
do
    ith_title=`sed -n "$i p" temp_b`
    ith_name=`echo $ith_title | sed "s/\(.*\)\"\(.*\)\"\(.*\)/\2/"`
    ith_name_no_spaces=`echo $ith_name | sed "s/\ //g"`
    found_title="false"
    for (( j=1 ; j<=$number_a_titles ; j++ ))
    do
	jth_title=`sed -n "$j p" temp_a`
	jth_name=`echo $jth_title | sed "s/\(.*\)\"\(.*\)\"\(.*\)/\2/"`
	jth_name_no_spaces=`echo $jth_name | sed 's/\ //g'`
	if [ "$ith_name_no_spaces" = "$jth_name_no_spaces" ]
	then
	    found_title="true"
	fi
    done
    if [ "$found_title" = "false" ]
    then
	echo "Deleting plot $ith_name from $file_b" 
	./Delete_plot.sh $file_b "$ith_name" > blahblah
	rm -f blahblah
	file_b_counter=$((file_b_counter+1))
    fi
done

rm -f temp_?

echo "Deleted $file_a_counter files from $file_a"
echo "Deleted $file_b_counter files from $file_b"
echo "All done ..."
