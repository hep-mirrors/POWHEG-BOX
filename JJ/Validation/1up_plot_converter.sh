#!/bin/bash
######################################################################
# What this does:
# It takes a topdrawer file from the output of the JJ or
# Frixione-Ridolfi programs and flattens it i.e. instead of
# having 6 plots on one postscript page, there is just one
# per page. This was primarily intended to help standardise
# the output of these two programs and to make them more
# amenable to manipulation and combination using bash and
# grep and sed. In particular it was foreseen to be very tricky 
# to combine the results for a given observable, for different
# incremental cuts, on a single plot, if the plots which needed
# combining lived on different pages of the postscript file. 
# A more solution specific to that problem could have been 
# attempted but I decided to go for something more general
# / widely usable, as no doubt there will be other, unforeseen,
# problems which will also benefit from this.
#
# To use type:
# my_shell>./1up_plot_converter.sh the_plot.top
# and you will find the_plot_1up.top comes out.

######################################################################
# Backup the original
echo "Temporarily backing up the original ..."
cp -f $1 $1_backup_file

######################################################################
# Delete trailing white space
echo "Deleting trailing white space ..."
sed -i -e 's/[ \t]*$//' $1

######################################################################
# Delete leading and trailing white space
echo "Deleting leading and trailing white space ..."
sed -i -e 's/^[ \t]*//;s/[ \t]*$//' $1

######################################################################
# Delete all current NEW PLOT lines
echo "Deleting NEW PLOT lines ..."
sed -i -e '/NEW PLOT/d' $1

######################################################################
# Delete all current TITLE TOP and LEFT and BOTTOM.*".* lines
echo "Deleting TITLE TOP/LEFT/BOTTOM lines ..."
sed -i -e '/TITLE TOP.*\".*/d' $1
sed -i -e '/TITLE BOTTOM.*\".*/d' $1
sed -i -e '/TITLE LEFT.*\".*/d' $1

######################################################################
# Delete all lines including TITLE.*MLM and the two lines before that!
echo "Getting MLM title lines ..."
mlm_lines=`sed -n '/TITLE.*MLM/=' $1`
mlm_array_size=0
# First get the array of lines Nos containing MLM and its length 
for i in $mlm_lines ;
do 
    mlm_array_size=$((mlm_array_size+1))
    tmp_array[${mlm_array_size}]=$i
done
# Reverse this array and insert the line Nos of the preceding 2 lines
echo "Building inverted MLM line number array ..."
j=1
for (( i=1; i<=$mlm_array_size; i++ ))
do 
    mlm_array[$((j))]=$((tmp_array[$((mlm_array_size-i+1))]))
    mlm_array[$((j+1))]=$((tmp_array[$((mlm_array_size-i+1))]-1))
    mlm_array[$((j+2))]=$((tmp_array[$((mlm_array_size-i+1))]-2))
    j=$((j+3))
done
# Now feed those line numbers to sed for deletion
echo "Deleting MLM title lines ..."
for (( i=1 ; i<=$((3*mlm_array_size)); i++ ))
do 
    sed -i -e "${mlm_array[${i}]} d" $1
done

######################################################################
# Insert NEW PLOT commands everytimes a 'SET FONT.*PLEX' is seen
echo "Inserting fresh NEW PLOT commands for every histogram ..."
sed -i -e '/SET FONT.*PLEX/ i NEW PLOT' $1

######################################################################
# Change all window X and Y coordinates
echo "Fixing window coordinates ..."
sed -i -e 's/SET WINDOW X   .* TO  .*$/SET WINDOW X 2.5 TO 10.0/g' $1
sed -i -e 's/SET WINDOW Y   .* TO  .*$/SET WINDOW Y 1.6 TO  9.0/g' $1

######################################################################
# Move the box INT ENT UFL OFL contents
echo "Moving the statistics ..."
sed -i -e 's/TITLE\(.*\)" INT=\(.*\)/TITLE   8.7750   8.8500 " INT=\2/g' $1
sed -i -e 's/TITLE\(.*\)" ENT=\(.*\)/TITLE   8.7750   8.7000 " ENT=\2/g' $1
sed -i -e 's/TITLE\(.*\)" OFL=\(.*\)/TITLE   8.7750   8.5500 " OFL=\2/g' $1
sed -i -e 's/TITLE\(.*\)" UFL=\(.*\)/TITLE   8.7750   8.4000 " UFL=\2/g' $1

######################################################################
# Find all lines reading JOIN TEXT and modify the 2 lines before that!
echo "Moving the statistics box ..."
the_lines=`sed -n '/JOIN TEXT/=' $1`
counter=0
for i in $the_lines ;
do 
    counter=$((counter+1))
    l=$((counter%2))
    if [ "$l" -eq 1 ]
    then
	sed_command="$sed_command $((i-1))""s/[0-9].*/8.73 8.2/g;"
	sed_command="$sed_command $((i-2))""s/[0-9].*/10.0 8.2/g;"
    elif [ "$l" -eq 0 ]
    then
	sed_command="$sed_command $((i-1))""s/[0-9].*/8.73 8.2/g;"
	sed_command="$sed_command $((i-2))""s/[0-9].*/8.73 9.0/g;"
    fi
done ;
sed -i -e "$sed_command" $1

######################################################################
# Find all top TITLES and make them say TITLE TOP ...
echo "Fixing the top title ..."
the_line_nos=`sed -n '/TITLE/=' $1`
for i in $the_line_nos ;
do
    the_line=`sed -n "$i p" $1` 
    the_line_number=`sed -n "$i =" $1` 
    the_line=`echo $the_line | grep -v "INT\|OFL\|UFL\|ENT" `
    the_line=`echo $the_line | grep -v "BOTTOM\|LEFT\|SIZE\|ANGLE"`
    the_title=`echo $the_line | sed 's/\(.*\)"\(.*\)"/\2/g' `
    the_title=`echo $the_title | sed 's/\//\,/g' `
    the_title=`echo $the_title | sed 's/\&/\\\&/g' `
    if [ "$the_line" != "" ]
    then
	current_contents=`sed -n "$the_line_number p" $1`
	current_contents=`echo $current_contents | sed 's/TOP \".*/TOP \"/'`
	sed -i -e "$the_line_number s/TITLE.*/TITLE TOP \"$the_title\"/g" $1
    fi
done

######################################################################
# Add matching TITLE BOTTOM
echo "Fixing the bottom title ..."
sed -i -e '/TITLE TOP \".*\"/ i TITLE BOTTOM \"blahblah\"' $1
j=0
for i in `sed -n '/TITLE TOP/=' $1`
do
    the_title=`sed -n "$i p" $1`
    the_title=`echo $the_title | sed 's/TITLE TOP \"//g'` 
    the_title=`echo $the_title | sed 's/\"$//g'`
    the_title=`echo $the_title | sed 's/\//\,/g' `
    the_title=`echo $the_title | sed 's/\&/\\\&/g' `
    j=$((i-1))
    sed -i -e "$j s/blahblah/$the_title/g" $1
done


######################################################################
# Backup the original
echo "Renaming the backup and the output file ..."
new_name=`echo "$1" | sed 's/.top//g'`
new_name=$new_name"_1up.top"
cp -f $1 $new_name
mv $1_backup_file $1

######################################################################
echo "all done ..."
exit
