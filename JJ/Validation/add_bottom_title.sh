#!/bin/bash
#################################################
#                                               #
# What this does:                               #
# Takes a topdrawer file and looks for all the  #
# TITLE LEFT "..."'s and makes                  #
# TITLE BOTTOM "..."'s out of that              #
# including the necessary CASE "..." bit.       #
# More info in the comments below.              #
#                                               #
#################################################

in_file=$1

# Find all of the "TITLE LEFT ..." lines and store "..." in temp
sed -n '/TITLE LEFT \".*\"/ p' $in_file | grep -v FDIFF | grep -v CHI2 | sed 's/.*LEFT\ \"// ; s/(Mb.*// ; s/dS\/d// ' > temp
no_lines=`sed -n '$ =' temp`
echo "Number of titles found =" $no_lines
# Now move all of the "..." title strings into array top_titles:
for (( i=1; i<=$no_lines; i++ ))
do 
  top_titles[$((no_lines+1-i))]=`sed -n "$i p" temp`
done

# Find all of the "CASE ..." following the "TITLE LEFT ..."'s and put in temp
sed -n '/TITLE LEFT \"/{n;p;}' $in_file | grep -v FDIFF | grep -v CHI2 > temp
no_lines=`sed -n '$ =' temp`
echo "Number of corresponding case strings found =" $no_lines
# Now move all of the "..." case strings into array top_cases:
for (( i=1; i<=$no_lines; i++ ))
do 
  top_cases[$((no_lines+1-i))]=`sed -n "$i p" temp | sed 's/\" G  /\"/'`
done

# Find all of the "TITLE BOTTOM ..." line *numbers* and put in temp
sed -n '/TITLE BOTTOM \"\"/ =' $in_file > temp
no_lines=`sed -n '$ =' temp`
echo "Number of TITLE BOTTOMs found =" $no_lines
for (( i=1; i<=$no_lines; i++ ))
# Now move all of the bottom line numbers into array bottom_line_no:
do 
  bottom_line_no[$((no_lines+1-i))]=`sed -n "$i p" temp`
done

# Go through all the bottom line numbers and replace the title with the
# "TITLE LEFT ..." title, putting the "CASE ..." underneath.
for (( i=1; i<=$no_lines; i++ ))
do 
  title=`echo ${top_titles[$i]}`
  echo $i ${bottom_line_no[$i]} $title ${top_cases[$i]}
  sed -i -e "${bottom_line_no[$i]} a ${top_cases[$i]} ( CHI2" $in_file
  sed -i -e "${bottom_line_no[$i]} a TITLE BOTTOM \"`echo $title`\" ( CHI2" $in_file
  sed -i -e "${bottom_line_no[$i]} d" $in_file
done

# 

exit

