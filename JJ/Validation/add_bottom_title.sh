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

# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o- #
# Next line, IFS="", stops bash deleting extra  #
# white space from strings - PHEW!              #
IFS=""
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o- #

in_file=$1

# Find all of the "TITLE LEFT ..." lines and store "..." in temp
sed -n '/TITLE LEFT \".*\"/ p' $in_file | grep -v FDIFF | grep -v CHI2 | sed 's/.*LEFT\ \"// ; s/(Mb.*// ; s/dS\/d// ' > temp
no_lines=`sed -n '$ =' temp`
echo "Number of left titles found =" $no_lines
# Now move all of the "..." title strings into array left_titles:
for (( i=1; i<=$no_lines; i++ ))
do 
  left_titles[$((no_lines+1-i))]=`sed -n "$i p" temp`
done

# Find all of the "CASE ..." following the "TITLE LEFT ..."'s and put in temp
sed -n '/TITLE LEFT \"/{n;p;}' $in_file | grep -v FDIFF | grep -v CHI2 > temp
no_lines=`sed -n '$ =' temp`
echo "Number of corresponding case strings found =" $no_lines
# Now move all of the "..." case strings into array left_cases:
for (( i=1; i<=$no_lines; i++ ))
do 
  left_cases[$((no_lines+1-i))]=`sed -n "$i p" temp | sed 's/\" G  /\"/'`
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
  title=`echo ${left_titles[$i]}`
#  echo $i ${bottom_line_no[$i]} $title ${left_cases[$i]}
  sed -i -e "${bottom_line_no[$i]} a ${left_cases[$i]} ( CHI2" $in_file
  sed -i -e "${bottom_line_no[$i]} a TITLE BOTTOM \"`echo $title`\" ( CHI2" $in_file
  sed -i -e "${bottom_line_no[$i]} d" $in_file
done

# 

# Find all of the "TITLE TOP ..." lines and store "..." in temp
sed -n '/TITLE TOP \".*\"/ p' $in_file | grep -v FDIFF | grep -v CHI2 | sed 's/.*\"\(.*\)\"/\1/' > temp
no_lines=`sed -n '$ =' temp`
echo "Number of top titles found =" $no_lines
# Now move all of the "..." title strings into array top_titles:
for (( i=1; i<=$no_lines; i++ ))
do 
  top_titles[$((no_lines+1-i))]=`sed -n "$i p" temp`
done

# Find all of the "CASE ..." following the "TITLE TOP ..."'s and put in temp
sed -n '/TITLE TOP \"/{n;p;}' $in_file | grep -v FDIFF | grep -v CHI2 > temp
no_lines=`sed -n '$ =' temp`
echo "Number of corresponding case strings found =" $no_lines
# Now move all of the "..." case strings into array top_cases:
for (( i=1; i<=$no_lines; i++ ))
do 
  top_cases[$((no_lines+1-i))]=`sed -n "$i p" temp | sed 's/\" G  /\"/'`
done

# Find all of the "TITLE TOP ..." line *numbers* and put in temp
sed -n '/TITLE TOP \".*\"/ =' $in_file > temp
no_lines=`sed -n '$ =' temp`
echo "Number of TITLE TOPs found =" $no_lines
for (( i=1; i<=$no_lines; i++ ))
# Now move all of the top line numbers into array top_line_no:
do
  top_line_no[$((no_lines+1-i))]=`sed -n "$i p" temp`
done

# Go through all the top line numbers and replace the title with the
# "TITLE TOP ..." title, putting the "CASE ..." underneath.
for (( i=1; i<=$no_lines; i++ ))
do
  title=`echo ${top_titles[$i]}`
  title_string=`echo $title | sed 's/.*\"\(.*\)\"/\1/'`
  first_bit=`echo $title_string | sed "s/\ .*/\ /"`
  second_bit=`echo $title_string | sed "s/$first_bit//"`
  case_string=`echo ${top_cases[$i]} | sed 's/.*\"\(.*\)\"/\1/'`
  length_first_bit=${#first_bit}
  new_case_string=`echo "CASE " \"${case_string:length_first_bit}\"`
  if [ "$second_bit" = "" ] 
  then
      second_bit=$title_string
      new_case_string=`echo "CASE " \"$case_string\"`
  fi
  if [ "`echo $title | sed -n '/H01\/21/ p' `" != "" ] 
  then
      title_string=`echo $title | sed 's/.*\"\(.*\)\"/\1/'`
      first_bit="H01\/21 "
      second_bit=`echo $title_string | sed "s/.* E0T,1/E0T,1/"`
#      second_bit=$title_string
      length_first_bit=7
      new_case_string=`echo "CASE " \"${case_string:length_first_bit}\"`
  fi
  dummy_variable=$((${top_line_no[$i]}+1))
  sed -i -e "$dummy_variable d" $in_file
  dummy_variable=$((${top_line_no[$i]}))
  sed -i -e "$dummy_variable a $new_case_string " $in_file
  sed -i -e "$dummy_variable a TITLE TOP \"$second_bit\"" $in_file
  sed -i -e "$dummy_variable d" $in_file
done

unset $IFS

exit

