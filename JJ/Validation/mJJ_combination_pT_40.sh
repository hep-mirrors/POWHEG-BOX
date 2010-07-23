#!/bin/bash
######################################################################
# What this does:

######################################################################
sed -i -e '/( NEW PLOT/d' $1

# Find all of the m0JJ1 top titles and put them in an array ...
echo "Finding TITLE TOP \"M0JJ1.*40 ..."
title_lines=`sed -n '/TITLE TOP.*M0JJ1.*40/=' $1`
title_array_size=0
for i in $title_lines
do 
    title_array_size=$((title_array_size+1))
    title_fwd_array[$((title_array_size))]=$i
done
if [ "$title_array_size" -eq 0 ] 
then
    echo "Didn't find any TITLE TOP \"M0JJ1 ..."
    echo "Trying TITLE TOP \"m0JJ1 ... instead ..."
    title_lines=`sed -n '/TITLE TOP.*m0JJ1.*40/=' $1`
    title_array_size=0
    for i in $title_lines
    do 
	title_array_size=$((title_array_size+1))
	title_fwd_array[$((title_array_size))]=$i
    done
fi

# Find all of the ( INT= entries and put the ones corresponding to
# each M0JJ1 in an array ...
int_eq_lines=`sed -n '/( INT=/=' $1`
int_eq_array_size=1
for i in $title_lines
do
    for j in $int_eq_lines
    do 
	if [ $((j)) -gt $((i)) ]
	then
	    int_eq_fwd_array[$((int_eq_array_size))]=$j
	    int_eq_array_size=$((int_eq_array_size+1))
	    break
	fi
    done
done
int_eq_array_size=$((int_eq_array_size-1))
if [ "$int_eq_array_size" -eq 0 ] 
then
    echo "Didn't find any ( INT= ..."
    echo "Trying TITLE LEFT ds/d... instead ..."
    int_eq_lines=`sed -n '/TITLE LEFT \"dS\/d/=' $1`
    int_eq_array_size=1
    for i in $title_lines
    do
	for j in $int_eq_lines
	do 
	    if [ $((j)) -gt $((i)) ]
	    then
		int_eq_fwd_array[$((int_eq_array_size))]=$((j+2))
		int_eq_array_size=$((int_eq_array_size+1))
	    break
	    fi
	done
    done
    int_eq_array_size=$((int_eq_array_size-1))
fi

# Find the next NEW PLOT command following the data corresponding to
# each M0JJ1 in an array ...
new_plot_lines=`sed -n '/NEW PLOT/=' $1`
new_plot_array_size=1
previous_value=
for i in $title_lines
do
    for j in $new_plot_lines
    do 
	if [ $((j)) -gt $((i)) ]
	then
	    new_plot_fwd_array[$((new_plot_array_size))]=$previous_value
	    new_plot_array_size=$((new_plot_array_size+1))
	    break
	fi
	previous_value=$j
    done
done


# Find the next NEW PLOT command following the data corresponding to
# each M0JJ1 in an array ...
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
		new_plot_2_fwd_array[$((new_plot_2_array_size))]=$((j-1))
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
		new_plot_2_fwd_array[$((new_plot_2_array_size))]=$((j-1))
		new_plot_2_array_size=$((new_plot_2_array_size+1))
		break
	    fi
	done
    done
fi
new_plot_2_array_size=$((new_plot_2_array_size-1))

echo "title_array_size " $title_array_size
echo "int_eq_array_size " $int_eq_array_size
echo "new_plot_array_size " $new_plot_2_array_size
echo "new_plot_2_array_size " $new_plot_2_array_size

for (( i=1 ; i<=$((title_array_size)); i++ ))
do 
    echo $((new_plot_fwd_array[${i}])) $((title_fwd_array[${i}])) $((int_eq_fwd_array[${i}])) $((new_plot_2_fwd_array[${i}]))
done

##########################################################
# Now we are going to try cutting and pasting the data on
# to the end of the file to create a new plot
sed -n "$((new_plot_fwd_array[1])),$((int_eq_fwd_array[1])) p" $1 > temp
sed -i -e 's/TITLE TOP.*\"/TITLE TOP \"m0JJ1 p0T1>40\"/g' temp
sed -i -e '/TITLE TOP.*\"/ a CASE \" X  X  X X   \" ' temp
sed -i -e '/(/!s/TITLE BOTTOM.*\"/TITLE BOTTOM \"m0JJ1 p0T1>40\"/g' temp
sed -i -e '/^  TITLE BOTTOM.*\"/ a CASE \" X  X  X X   \" ' temp
sed -i -e 's/TITLE LEFT.*\"/TITLE LEFT \"dS\/d(m0JJ1 p0T1>40) (mb\/bin)\"/g' temp
sed -i -e '/TITLE LEFT.*\"/ a CASE \" G    X  X  X X     S      S\" ' temp
title_left_flag=`grep -v "( TITLE LEFT" temp | grep "TITLE LEFT"`
cat temp >> $1
rm temp
eof_holder=`sed -n "$ =" $1`
echo "SET WINDOW Y 1.6 TO 9." >> $1
echo "SET SCALE  Y LOG" >> $1
echo "SET LIMITS Y 1E-3 1E15" >> $1
echo "SET AXES BOTTOM ON" >> $1
for (( i=1 ; i<=$((int_eq_array_size)); i++ ))
do
    the_label=`sed -n "$((title_fwd_array[$((i))])) p" $1`
    the_label=`echo $the_label | sed 's/\(.*\)\"\(.*\)\"/\2/g'`
    the_label=`echo $the_label | sed 's/.*pT>40//g'`
    the_label=`echo $the_label | sed 's/.*p0T1>40//g'`
    ypos=`echo 6.5+0.2*$((i-1)) | bc `
    echo "SET TITLE SIZE 1.2" > temp
    echo "TITLE  7.50 "$ypos" \""$the_label" (x10^"$((i-1))")\"" >> temp
    if [ "$i" -eq 1 ] 
    then
        echo "CASE \"  X   X \"" >> temp
	echo "SET ORDER X Y 2.5E6 DY 2.5E6" >> temp
    else
        echo "CASE \"      X   X \"" >> temp
	echo "SET ORDER X Y 2.5E"$((6+(i-1)))" DY 2.5E"$((6+(i-1))) >> temp
    fi
    sed -n "$((int_eq_fwd_array[$((i))]+1)), \
            $((new_plot_2_fwd_array[$((i))])) p" $1 >> temp
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
	sed -i -e "s/TITLE LEFT/d" temp
    fi
    sed -i -e 's/TITLE LEFT.*\"/TITLE LEFT \"dS\/d(m0JJ1 p0T1>40) (mb\/bin)\"/g' temp
    sed -i -e '/(mb\/bin)/ a CASE \" G    X  X  X X     S      S\"' temp
    sed -i -e '/CASE       \" G                                 S      S\"/d' temp
    sed -i -e '/CASE \" X  X  X X   \"/d' temp
    cat temp >> $1
    rm temp
done

######################################################################
echo "all done ..."




exit
