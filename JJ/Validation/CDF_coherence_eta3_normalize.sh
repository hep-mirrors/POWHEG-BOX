#!/bin/bash
##########################################################
# What this does:
# Normalizes selective groups of histograms to one.
#
# IT HAS ONLY BEEN DEVELOPED AND TESTED ON FILES COMING
# DIRECTLY OUT OF pwhg_analysis.f . IT ALMOST CERTAINLY
# DOES NOT WORK ON merge_plots.f OUTPUT. IT -MAY- WORK
# ON pwhg_analysis.f OUTPUT WHICH HAS GONE THROUGH 
# 1up_plot_converter.sh 
#
# How it works:
# It identifies all histograms in a topdrawer file according
# to a common string appearing in the header (this may include
# wild cards, obviously, so it can be applied to a group of
# histograms differing only by cuts say). This string should
# only appear once per header!! The line numbers of these 
# strings are stored. The script goes on to associate to
# each of those line numbers the corresponding line numbers
# for the start of the header, the end of the header and
# the end of the data. This is why the string must appear
# only once in a given header.
# The end of the header is supposed to contain the histogram
# integral "INT=...". This number is extracted and all y 
# values in the underlying data are divided by it, as are
# their errors.
#
# To run:
# ./unit_normalize_dphiJJ.sh the_file.top
# or, for all files in the directory,
# for i in ls *.top ; do ./unit_normalize_dphiJJ.sh $i ; done
#
# Scope:
# To get this to work on other histograms with titles
# different to delta phi, you need only modify the regexp
# search pattern in setting the variable "title_lines" 
# just below here.
#

##########################################################
echo "Normalizing the delta phi plots "
echo "in file "$1

##########################################################
# Delete all instances of the following commented out
# lines as they confuse the following code:
sed -i -e '/( NEW PLOT/d' $1

##########################################################
# Find all of the TITLE X Y "blah blah" lines numbers 
# with sed and put them in an array ...
# Note that if you want to adapt 
echo "Finding all of the TITLE.*[0-9].*\" H0J31   .*\" ..."
title_lines=`sed -n '/TITLE.*[0-9].*\" H0J31   .*\"/=' $1`
title_array_size=0
for i in $title_lines
do 
    title_array_size=$((title_array_size+1))
    title_array[$((title_array_size))]=$i
done

##########################################################
# Find the position of each PLOT command just ABOVE
# the corresponding TITLE X Y "blah blah" line and, you
# guessed it, put it in an array ... these are obviously
# the line numbers where each of the plots we want to
# combine starts! 
new_plot_lines=`sed -n '/   PLOT/=' $1`
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
for (( i=1 ; i<=$((title_array_size)); i++ ))
do 
    echo $((new_plot_array[${i}])) \
         $((title_array[${i}])) \
         $((int_eq_array[${i}])) \
         $((new_plot_2_array[${i}]))
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
    # Get the histogram integral in the range it is filled
    # i.e. omitting under & overflow
    the_factor=`sed -n "$((int_eq_array[$((i))])) p" $1`
    the_factor=`echo $the_factor | sed 's/.*INT=\(.*\)\ .*ENTRIES.*/\1/g'`
    the_factor_mant=`echo $the_factor | sed 's/E.*//g' `
    the_factor_exp=`echo $the_factor | sed 's/.*E//g' `
    the_factor_exp=`echo $the_factor_exp | sed 's/+//g' `
    the_factor=`echo $the_factor | sed 's/E/\*10\^/g' `
    sed -n "$((int_eq_array[$((i))]+1)), \
            $((new_plot_2_array[$((i))])) p" $1 > temp
    sed -i -e "$((int_eq_array[$((i))]+1)), \
            $((new_plot_2_array[$((i))])) d" $1
    no_lines=`sed -n '$ =' temp`
    binwidth=0
    prev_x=0
    for (( j=1 ; j<=$((no_lines)); j++ ))
    do
	jth_line=`sed -n "$j p" temp`
	jth_x=`echo $jth_line | sed 's/\(.*\) \(.*\) \(.*\)/\1/g'`
	binwidth=`echo "scale=8 ; $jth_x - $prev_x " | bc `
	prev_x=$jth_x
    done
    for (( j=1 ; j<=$((no_lines)); j++ ))
    do
	jth_line=`sed -n "$j p" temp`
	jth_x=`echo $jth_line | sed 's/\(.*\) \(.*\) \(.*\)/\1/g'`
	jth_y=`echo $jth_line | sed 's/\(.*\) \(.*\) \(.*\)/\2/g'`
	jth_dy=`echo $jth_line | sed 's/\(.*\) \(.*\) \(.*\)/\3/g'`
	jth_y_mant=`echo $jth_y | sed 's/E.*//g' `
	jth_y_exp=`echo $jth_y | sed 's/.*E//g' `
	jth_y_exp=`echo $jth_y_exp | sed 's/+//g' `
	jth_y=`echo $jth_y | sed 's/E/\*10\^/g' `
	jth_dy_mant=`echo $jth_dy | sed 's/E.*//g' `
	jth_dy_exp=`echo $jth_dy | sed 's/.*E//g' `
	jth_dy_exp=`echo $jth_dy_exp | sed 's/+//g' `
	jth_dy=`echo $jth_dy | sed 's/E/\*10\^/g' `
	if [ -z "$jth_y_exp" ]
	then
	    echo "Warning!"
	    echo "Did not find exponent for $j th data entry."
	    echo "Proceeding with bash calc anyway, however "
	    echo "precision is truncated to 10 decimal places"
	    echo "and no scientific formatting of numbers, so"
	    echo "rounding errors may occur which may be severe."
	    jth_y=`echo "scale=8 ; ( $jth_y ) / ( $the_factor ) " | bc `
#	    jth_y=`echo "scale=8 ; ( $jth_y ) / ( $binwidth ) " | bc `
	    jth_dy=`echo "scale=8 ; ( $jth_dy ) / ( $the_factor ) " | bc `
#	    jth_dy=`echo "scale=8 ; ( $jth_dy ) / ( $binwidth ) " | bc `
	else
	    jth_y_mant=`echo "scale=8 ; $jth_y_mant / $the_factor_mant " | bc `
#	    jth_y_mant=`echo "scale=8 ; $jth_y_mant / $binwidth " | bc `
	    jth_y_exp=`echo "scale=8 ; $jth_y_exp - $the_factor_exp " | bc `
	    if [ "$jth_y_mant" = "0" ]
	    then
		jth_y_exp="0"
	    fi
	    jth_y=`echo $jth_y_mant"E"$jth_y_exp`
	    jth_dy_mant=`echo "scale=8 ; $jth_dy_mant / $the_factor_mant " | bc `
#	    jth_dy_mant=`echo "scale=8 ; $jth_dy_mant / $binwidth " | bc `
	    jth_dy_exp=`echo "scale=8 ; $jth_dy_exp - $the_factor_exp " | bc `
	    if [ "$jth_dy_mant" = "0" ]
	    then
		jth_dy_exp="0"
	    fi
	    jth_dy=`echo $jth_dy_mant"E"$jth_dy_exp`
	fi
	new_jth_line=`echo $jth_x $jth_y $jth_dy`
	sed -i -e "$j a $new_jth_line" temp
	sed -i -e "$j d" temp
	sed -i -e "$((int_eq_array[$((i))]+j-1)) a $new_jth_line" $1
    done
    sum_y=0
    for (( j=1 ; j<=$((no_lines)); j++ ))
    do
	jth_line=`sed -n "$j p" temp`
	jth_y=`echo $jth_line | sed 's/\(.*\) \(.*\) \(.*\)/\2/g'`
	jth_y=`echo $jth_y | sed 's/E/\*10\^/g' `
	sum_y=`echo "scale=20 ; $sum_y + $jth_y " | bc `
    done
    echo "Now sum of y values = " `echo "scale=20 ; $sum_y" | bc `
    echo "N.B. The CDF analysis does not divide by the bin width so nor do we."
done

######################################################################

##########################################################
# Rename the relevant titles so that it is clear that
# this plot is now normalised to one:
sed -i -e "s/\".*H0J31        .*\"/\"H0J31 Unit Norm\"/g" $1

echo "all done ..."
echo "N.B. The y limits in the plots header(s) will need to be"
echo "     modified by hand to make the data visible, unless"
echo "     the output is to be given to merge_plots.f which"
echo "     will correct for this automatically."



exit
