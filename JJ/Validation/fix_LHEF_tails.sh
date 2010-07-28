#!/bin/bash
##########################################################
# What this does:
# It fixes the ends of Les Houches files for which the 
# last end was not written out fully due to e.g. a job
# terminating unexpectedly. It leaves Les Houches files
# that completed normally untouched.
#
# How it is supposed to work:
# (see comments below).
#
# To run:
# ./fix_LHEF_tails.sh a_lhef_file.lhe
# or, for all LHEF files in the current dir
# for i in `ls *.lhe` ; do ./fix_LHEF_tails.sh $i ; done

the_file=$1

##########################################################
# If all went well then the last line should say #Random ..
# but the integrity will also be fine if the last line
# is what is normally the penultimate line i.e. </LesHouchesEvents>
# in which case, so that everything is more standard (to 
# make life easier in subsequent scripts) we patch on 
#Random number generator exit values: ----- -------- -
last_line_number=`sed -n '$ =' $the_file`
last_line_ok=`sed -n '$ p' $the_file`
last_line_ok=`echo $last_line | sed 's/<\/LesHouchesEvents>/OK/g'`
if [ "$last_line_ok" == "OK" ]
then
    echo "File $the_file ended correctly except for the #Random ... bit."
    echo "This is now fixed. The file had $event_count events."    
    echo " #Random number generator exit values: ----- -------- -" >> $the_file
    event_count=`grep -wc '<\/event>' $the_file`
    exit
fi
##########################################################
# Now apart from that special circumstance above what we 
# want is for all files to end in a complete event followed
# by </LesHouchesEvents> and #Random ... So we look at the
# penultimate line and check if it says </LesHouchesEvents>.
penultimate_line_number=$((last_line_number-1))
penultimate_line_ok=`sed -n "$penultimate_line_number p" $the_file`
penultimate_line_ok=`echo $penultimate_line_ok | sed 's/<\/LesHouchesEvents>/OK/g'`
# If the 2nd last line said </LesHouchesEvents> then we
# just issue a nice message and proceed.
if [ "$penultimate_line_ok" == "OK" ]
then
    event_count=`grep -wc '<\/event>' $the_file`
    echo "File $the_file is OK with $event_count events."
# Otherwise we warn the user that we need to operate on
# his file. 
else
    echo "File $the_file is NOT OK"
    # Count the initial number of lines in the file (for debugging).
    initial_lines=`sed -n '$ =' $the_file`
    # Get all of the line numbers of lines containing 
    # <\/extra-info-previous-event> (signalling the end of an
    # event) and put them in file "temp."
    sed -n "/<\/extra-info-previous-event>/=" $the_file > temp
    # the line number corresponding to the end of the last event
    # can be read of the last line in "temp".
    end_of_last_event=`sed -n '$ p' temp`
    end_of_last_event=$((end_of_last_event+1))
    # We use that line number to delete from that position to the
    # end of the file.
    sed "$end_of_last_event,$ d" $the_file > temp 
    # Then we patch on tags signalling the end of the event file
    # and, purely to make life simpler in other scripts, a dummy 
    # line which would normally display useful random number seeds.
    echo "</LesHouchesEvents>" >> temp
    echo " #Random number generator exit values: ----- -------- -" >> temp
    # Count the initial number of lines in the file (for debugging).
    final_lines=`sed -n '$ =' temp`
    # Count the number of events in the patched file (for debugging).
    event_count=`grep -wc '<\/event>' temp`
    # Say something about what we did.
    echo "Found $event_count events, deleted last $((initial_lines-final_lines)) lines."
    # Remember to copy the patched file in "temp" over the original!
    mv temp $the_file
fi
