#!/bin/bash
##########################################################
# What this does:
# It is supposed to combine all histograms found in a 
# file for the same quantity evaluated at different
# cuts and combine them into one plot, which it sticks
# on the end of the file.
#
# IT IS SUPPOSED TO WORK ON OUTPUT FROM merge_plots.f
# AND 1up_plot_converter.sh (BEFORE OR AFTER RUNNING
# make_titles_nice.sh). IT IS NOT GOING TO WORK ON THE
# DIRECT OUTPUT OF pwhg_analysis.f
#
# How it is supposed to work:
# (see comments below).
#
# To run:
# ./mJJ_combination_pT_20.sh the_file.top
#
# Scope:
# 90% of the code is not specific to mJJ and could
# easily be adapted for other distributions (in progress).

##########################################################
# Delete all instances of the following commented out
# lines as they confuse the following code:
sed -i -e '/( NEW PLOT/d' $1

##########################################################
# Find all of the TITLE TOP "blah blah" lines numbers 
# with sed and put them in an array ...
echo "Finding all of the TITLE TOP \"M0JJ1.*20 ..."
title_lines=`sed -n '/TITLE TOP.*M0JJ1.*20/=' $1`
title_array_size=0
for i in $title_lines
do 
    title_array_size=$((title_array_size+1))
    title_array[$((title_array_size))]=$i
done
if [ "$title_array_size" -eq 0 ] 
then
    echo "Didn't find any TITLE TOP \"M0JJ1 ..."
    echo "Trying TITLE TOP \"m0JJ1 ... instead ..."
    title_lines=`sed -n '/TITLE TOP.*m0JJ1.*20/=' $1`
    title_array_size=0
    for i in $title_lines
    do 
	title_array_size=$((title_array_size+1))
	title_array[$((title_array_size))]=$i
    done
fi

##########################################################
# Find the position of each NEW PLOT command just ABOVE
# the corresponding TITLE TOP "blah blah" line and, you
# guessed it, put it in an array ... these are obviously
# the line numbers where each of the plots we want to
# combine starts! 
new_plot_lines=`sed -n '/NEW PLOT/=' $1`
new_plot_array_size=1
previous_value=
for i in $title_lines
do
    for j in $new_plot_lines
    do 
	if [ $((j)) -gt $((i)) ]
	then
	    new_plot_array[$((new_plot_array_size))]=$previous_value
	    new_plot_array_size=$((new_plot_array_size+1))
	    break
	fi
	previous_value=$j
    done
done
new_plot_array_size=$((new_plot_array_size-1))

##########################################################
# Here we want to basically find the positions of the
# end of each of the plot headers, so we do the following.  
# Find all of the ( INT= line numbers which ** directly
# following** each of the line numbers of TITLE TOP "blah
# blah", just found above, and put them in an array ...
# If it can't find any ( INT= lines. Alternatively
# , if the file has been merged using merge_plots.f
# it looks for TITLE LEFT "dS/d(blah blah)" instead
# because that can sometimes also be the thing that
# appears at the start of the numbers (which are the
# positions we are really looking for).
merged_flag=`sed -n '/SET WINDOW Y.*2.8/p' $1`
if [ "$merged_flag" = "" ] 
then
    int_eq_lines=`sed -n '/( INT=/=' $1`
    int_eq_array_size=1
    for i in $title_lines
    do
	for j in $int_eq_lines
	do 
	    if [ $((j)) -gt $((i)) ]
	    then
		int_eq_array[$((int_eq_array_size))]=$j
		int_eq_array_size=$((int_eq_array_size+1))
		break
	    fi
	done
    done
    int_eq_array_size=$((int_eq_array_size-1))
else
    int_eq_lines=`sed -n '/TITLE LEFT \"dS\/d/=' $1`
    int_eq_array_size=1
    for i in $title_lines
    do
	for j in $int_eq_lines
	do 
	    if [ $((j)) -gt $((i)) ]
	    then
		int_eq_array[$((int_eq_array_size))]=$((j+2))
		int_eq_array_size=$((int_eq_array_size+1))
		break
	    fi
	done
    done
    int_eq_array_size=$((int_eq_array_size-1))
fi

##########################################################
# Now we want to find the positions where each chunk of
# data for each plot ends. This means looking for the
# next "NEW PLOT" command if the file has not been made
# with merge_plots.f, or, if the file has been merged,
# it means we look for the position where the commands
# to set up the subplot window (fractional difference /
# chi^2) start. Of course, these go in arrays.
merged_flag=`sed -n '/SET WINDOW Y.*2.8/p' $1`
if [ "$merged_flag" = "" ] 
then
    new_plot_2_lines=`sed -n '/NEW PLOT/=' $1`
    new_plot_2_array_size=1
    for i in $title_lines
    do
	for j in $new_plot_2_lines
	do 
	    if [ $((j)) -gt $((i)) ]
	    then
		new_plot_2_array[$((new_plot_2_array_size))]=$((j-1))
		new_plot_2_array_size=$((new_plot_2_array_size+1))
		break
	    fi
	done
    done
else
    new_plot_2_lines=`sed -n '/SET WINDOW Y.*2.8/=' $1`
    new_plot_2_array_size=1
    for i in $title_lines
    do
	for j in $new_plot_2_lines
	do 
	    if [ $((j)) -gt $((i)) ]
	    then
		new_plot_2_array[$((new_plot_2_array_size))]=$((j-1))
		new_plot_2_array_size=$((new_plot_2_array_size+1))
		break
	    fi
	done
    done
fi
new_plot_2_array_size=$((new_plot_2_array_size-1))


##########################################################
# Now we should have 4 arrays. An array containing the
# position of the top title (title_array), an array 
# containing line numbers where each plot starts
# (new_plot_array) an array containing the line numbers
# where each of those plot headers ends (int_eq_array),
# and finally an array containing the positions of 
# the ends of each corresponding data chunk in the file.
echo "new_plot_array_size " $new_plot_array_size
echo "title_array_size " $title_array_size
echo "int_eq_array_size " $int_eq_array_size
echo "new_plot_2_array_size " $new_plot_2_array_size
# Write out the line numbers in each array so we can see
# they make sense
for (( i=1 ; i<=$((title_array_size)); i++ ))
do 
    echo $((new_plot_array[${i}])) \
         $((title_array[${i}])) \
         $((int_eq_array[${i}])) \
         $((new_plot_2_array[${i}]))
done

##########################################################
# So we have all the relevant line numbers in hand now.
# Now we are going to try cutting and pasting the data on
# to the end of the file to create a new plot

# First a bit of a header taken from the first of the plots
# we are combining and modified a little bit:
sed -n "$((new_plot_array[1])),$((int_eq_array[1])) p" $1 > temp
sed -i -e 's/TITLE TOP.*\"/TITLE TOP \"m0JJ1 p0T1>20\"/g' temp
sed -i -e '/TITLE TOP.*\"/ a CASE \" X  X  X X   \" ' temp
sed -i -e '/(/!s/TITLE BOTTOM.*\"/TITLE BOTTOM \"m0JJ1 p0T1>20\"/g' temp
sed -i -e '/^  TITLE BOTTOM.*\"/ a CASE \" X  X  X X   \" ' temp
sed -i -e 's/TITLE LEFT.*\"/TITLE LEFT \"dS\/d(m0JJ1 p0T1>20) (mb\/bin)\"/g' temp
sed -i -e '/TITLE LEFT.*\"/ a CASE \" G    X  X  X X     S      S\" ' temp
echo "SET WINDOW Y 1.6 TO 9." >> temp
echo "SET SCALE  Y LOG" >> temp
echo "SET LIMITS Y 1E-3 1E15" >> temp
echo "SET AXES BOTTOM ON" >> temp

# Check if the TITLE LEFT command is present in this header (needed later)
title_left_flag=`grep -v "( TITLE LEFT" temp | grep "TITLE LEFT"`

# OK now we join the header on to the end of the file it came from
cat temp >> $1
rm temp

# Now we loop over the arrays of line numbers cutting and pasting
# the non-header bits in i.e. data and data footers into a temporary
# file "temp" which then gets tidied up a little bit with a couple
# of sed calls after which it gets appended to the original file. 
for (( i=1 ; i<=$((int_eq_array_size)); i++ ))
do
    # Extract the title of the histogram currently being processed:
    the_label=`sed -n "$((title_array[$((i))])) p" $1`
    the_label=`echo $the_label | sed 's/\(.*\)\"\(.*\)\"/\2/g'`
    the_label=`echo $the_label | sed 's/.*pT>20//g'`
    the_label=`echo $the_label | sed 's/.*p0T1>20//g'`
    # Calculate the vertical position to write it at:
    ypos=`echo 6.5+0.2*$((i-1)) | bc `
    echo "SET TITLE SIZE 1.2" > temp
    echo "TITLE  7.50 "$ypos" \""$the_label" (x102"$((i-1))"3)\"" >> temp
    # In the case of the mJJ plots the various histos are scaled
    # up by factors which we calculate here according to the loop 
    # index
    if [ "$i" -eq 1 ] 
    then
        echo "CASE \"  X   X          X X \"" >> temp
	echo "SET ORDER X Y 2.5E6 DY 2.5E6" >> temp
    else
        echo "CASE \"      X   X          X X \"" >> temp
	echo "SET ORDER X Y 2.5E"$((6+(i-1)))" DY 2.5E"$((6+(i-1))) >> temp
    fi
    # Here we paste in the corresponding big data chunk
    sed -n "$((int_eq_array[$((i))]+1)), \
            $((new_plot_2_array[$((i))])) p" $1 >> temp
    # Here we are doing a bit of tidying up, deleting any stray TITLE LEFT
    # and TAG lines, for all but the first instance of the loop since, the
    # other instances just repeat those commands and make a mess (titles on
    # top of each other etc.
    if [ -z "$title_left_flag" ] 
    then
	if [ "$i" -ne 1 ] 
	then
	    sed -i -e "/TITLE LEFT/d" temp
	    sed -i -e "/TAG/d" temp
	else
	    echo "Getting left title from the first histogram" 
	fi
    else
	sed -i -e "/TITLE LEFT/d" temp
    fi
    # This next bit of code checks for lines saying CASE " ... "
    # which have no text above them and removes them: to keep
    # topdrawer happy.
    case_lines=`sed -n '/CASE/=' temp`
    for j in $case_lines
    do
	above_case_line=$((j-1))
	above_case_line=`sed -n "$above_case_line p" temp`
	above_case_line=`echo $above_case_line | grep \" `
	if [ -z "$above_case_line" ]
	then
	    sed -i -e "$j d" temp
	fi
    done
    # Now we join our data chunk plus footers in "temp" onto
    # the original file...
    cat temp >> $1
    # and clean up:
    rm temp
done

######################################################################
echo "all done ..."




exit
