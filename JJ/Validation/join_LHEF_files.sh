#!/bin/bash
##########################################################
# What this does:
# It joins a bunch of Les Houches Event files together
# deleting the header and initialization business of
# all but the first file in the process. The number of
# events in the final header is updated to reflect the
# new number of events in the merged output file. Also
# the names of the files used in creating the merged
# file are appended to it i.e. to see these just type
# tail the_output_file.lhe
#
# How it is supposed to work:
# (see comments below).
#
# To run:
# ./join_LHEF_files.sh "a.lhe b.lhe c.lhe" abc.lhe
# where the " ... " is a list of input files and
# abc.lhe specifies the name of the resulting output
# file.
# ./join_LHEF_files.sh "`ls *.lhe`" merged.lhe
# merges all .lhe files in the directory to merged.lhe
##########################################################
input_files=$1
output_file=$2

##########################################################
# First we just write out a few reassuring messages:
no_files=0
for i in $input_files
do
    no_files=$((no_files+1))
done
echo
echo "Merging the following $no_files files:"
for i in $input_files
do
    echo $i
done
echo

##########################################################
# Loop over all of the input files and first count the
# number of events in each one (for debugging).
j=0
for i in $input_files
do
    n_events=`grep -wc '</event>' $i`
    j=$((j=j+1))
    if [ "$j" = "1" ]
    then
	# If we are dealing with the first file then we want
	# the whole thing minus the last two lines which
	# should indicate the end of the events and the next
	# random seeds.
	echo "Merging file $i with $n_events into $output_file"
	echo ""
	cp $i $output_file
	sed -i -e '$ d' $output_file
	sed -i -e '$ d' $output_file
    else
	# If we are dealing with other than  the first file
        # then we want the whole thing minus the last two lines
        # and also minus the header and initialization stuff.
	echo "Merging file $i with $n_events events into $output_file"
	begin_events=`sed -n '/\/extra-info-true-sigma/=' $i`
	begin_events=$((begin_events+1))
	sed -n "$begin_events,$ p" $i > temp
	sed -i -e '$ d' temp
	sed -i -e '$ d' temp
	initial_line_count=`sed -n '$ =' $i`
	final_line_count=`sed -n '$ =' temp`
	deleted_lines=$((initial_line_count-final_line_count))
	echo "Deleted $deleted_lines lines and merged in the $n_events events."
	cat temp >> $output_file
	echo
    fi
done

##########################################################
# Finally we add the tags signalling the end of the 
# events to the output file and also append a list
# of the input files used to construct it.
echo "</LesHouchesEvents>" >> $output_file
echo " #End of merged file" >> $output_file
echo " #This file is merged file was made from:" >> $output_file
for i in $input_files
do
    echo " #$i" >> $output_file
done

##########################################################
# Now count the number of events in the merged file
n_events=`grep -wc '</event>' $output_file`

##########################################################
# Sanity check:
# Test the beginning of every event but the first event
# is preceded by the end of another event?
# THIS IS FAR TOO SLOW SO I COMMENTED IT OUT
#sed -n '/<event>/=' $output_file > temp
#temp_file_length=`sed -n '$ =' temp`
#for (( i=2 ; i<=$temp_file_length ; i++ ))
#do
#    counter=$((i%10000))
#    if [ "$counter" = "0" ] 
#    then
#	echo "Checked $i events ..."
#    fi
#    begin_event_line_no=`sed -n "$i p" temp`
#    previous_line_no=$((begin_event_line_no-1))
#    previous_line=`sed -n "$previous_line_no p" $output_file`
#    previous_line=`echo $previous_line | sed 's/<\/extra-info-previous-event>/OK/g'`
#    if [ "$previous_line" != "OK" ] 
#    then
#	echo "File integrity compromised at line $begin_event_line_no ..."
#	exit
#    fi
#done
#rm temp

##########################################################
# Now we swap the number of events in the header for the
# total we got from merging & write out a good bye message.
echo
sed -i -e "s/numevts .*/numevts $n_events/" $output_file
echo "Created file $output_file containing $n_events events."
echo ""
