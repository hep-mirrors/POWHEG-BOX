#!/bin/bash
##########################################################
# What this does:
# Deletes plots from a topdrawer file so that several
# topdrawer files can be standardised so as to contain
# the same histogram content.
#
# IT HAS ONLY BEEN DEVELOPED AND TESTED ON FILES COMING
# DIRECTLY OUT OF pwhg_analysis.f .
#
# How it works:
# You give a string / substring in appearing in one or
# many TITLE BOTTOM commands in the topdrawer file and
# the corresponding parts of the plot above and below
# each of these commands will be deleted.
#
# To run:
# ./Delete_plot.sh the_file.top "the string or substring"
# or, for all files in the directory,
# for i in ls *.top ; do ./Delete_plot.sh $i "the string or substring" ; done
#
#

##########################################################
echo ""
echo "Deleting plots containing string / substring "
echo "$2 "
echo "in the title in file "$1

##########################################################
# Delete all instances of the following commented out
# lines as they confuse the following code:
sed -i -e '/( NEW PLOT/d' $1

##########################################################
# Find all of the TITLE BOTTOM.*\".*$2.*\"" lines numbers 
# with sed and put them in an array ...
# Note that if you want to adapt 
echo "Finding all of the TITLE BOTTOM.*\".*$2.*\""
title_lines=`sed -n "/TITLE BOTTOM.*\".*$2.*\"/=" $1`
title_array_size=0
for i in $title_lines
do 
    title_array_size=$((title_array_size+1))
    title_array[$((title_array_size))]=$i
done

##########################################################
# Find the position of each PLOT command just ABOVE
# the corresponding TITLE BOTTOM "blah blah" line and, you
# guessed it, put it in an array ... these are obviously
# the line numbers where each of the plots we want to
# combine starts! 
new_plot_lines=`sed -n '/ PLOT/=' $1`
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
# If it can't find any ( INT= lines then it tries
# looking for TITLE LEFT "dS/d(blah blah)" instead
# because that can sometimes also be the thing that
# appears at the start of the numbers (which are the
# positions we are really looking for).
int_eq_lines=`sed -n '/(\ INT=/=' $1`
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

##########################################################
# Now we want to find the positions where each chunk of
# data for each plot ends. This means looking for the
# next "HIST" command. Of course, these go in arrays.
new_plot_2_lines=`sed -n '/HIST/=' $1`
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
echo "Forwards arrays:"
j=0
for (( i=$((title_array_size)); i>=1; i-- ))
do 
    j=$((j+1))
    bwd_new_plot_array[$j]=$((new_plot_array[${i}]))
    bwd_title_array[$j]=$((title_array[${i}]))
    bwd_int_eq_array[$j]=$((int_eq_array[${i}]))
    bwd_new_plot_2_array[$j]=$((new_plot_2_array[${i}]))
    echo $((new_plot_array[${i}])) \
         $((title_array[${i}])) \
         $((int_eq_array[${i}])) \
         $((new_plot_2_array[${i}]))
done
echo "Backwards arrays:"
for (( i=1 ; i<=$((title_array_size)); i++ ))
do 
    echo $((bwd_new_plot_array[${i}])) \
         $((bwd_title_array[${i}])) \
         $((bwd_int_eq_array[${i}])) \
         $((bwd_new_plot_2_array[${i}]))
done

##########################################################
# Now we loop over the INT= line numbers. Each time we 
# find one we save the INT= value so that we can then 
# divide the Y values and their associated errors by
# this number. With the number in hand we proceed to
# loop over all data points from INT= down to the next
# HIST command extracting the Y and DY values on each
# line, dividing them by INT=, and then replacing the
# line with a new line containing the new values.
for (( i=1 ; i<=$((int_eq_array_size)); i++ ))
do
    echo ""
    echo "Deleting $((bwd_new_plot_array[$((i))])) to $((bwd_new_plot_2_array[$((i))]))"
    from=$((bwd_new_plot_array[$((i))]+1))
    to=$((bwd_new_plot_2_array[$((i))]+2))
#    sed -n "$from,$to p" $1
    sed -i -e "$from,$to d" $1
done

######################################################################

##########################################################
#
#
echo "all done ..."


exit
