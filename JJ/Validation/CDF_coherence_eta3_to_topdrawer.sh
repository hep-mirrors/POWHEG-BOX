#!/bin/bash
##########################################################
# What this does:
# Reads the text format of in the Durham reaction data base
# in the format z xlow xhigh blah blah ... and turns that
# into something topdrawer understands.
#
# IT IS REALLY ONLY A TEMPLATE. DIFFERENT ANALYSES IN HEPDATA
# HAVE DIFFERENT CONVENTIONS: SOME ONLY HAVE E.G. ONE SET
# OF DY+ AND DY- OTHERS HAVE TWO. SO YOU NEED TO CHECK THIS
# AND MODIFY THE SCRIPT ACCORDINGLY BEFORE RUNNING. CORRECTING
# FOR THESE DIFFERENCES SHOULD BE TRIVIAL.
#
# How it works:
# It reads each line of the text file into an array.
# It scans each array element for the numerical values
# it wants and it stores these in other arrays with 
# obvious names. Then it manipulates those arrays a
# little bit to get the bin widths right and adds the
# statistical and systematic errors in quadrature.
#
# To run:
# ./hepdata_to_topdrawer.sh the_text_file_from_durham
#
#
# CAUTIONARY NOTE:
# Any floating point arithmetic is done with "bc" which 
# if used carelessly can have significant rounding errors.
# If rounding errors are observed one try to fix it by 
# increasing "bc"s precision, by increasing the value 
# of scale= in the bc calls below. 
#

####################################################################
# First we read in the file and extract the header to the "header" 
# array and each data entry to array "array". Then from the each
# individual data entry we extract the numbers into arrays
# X (x) XL (xlow) XH (xhigh) Y (y) DYSTATP (dy+) DYSTATM (dy-)
# DYSYSP (other dy+) DYSYSM (other dy-) :
h=0
j=0
for (( i=1 ; i<=`sed -n '$=' $1` ; i++ ))
do 
    line=`sed -n "$i p" $1`
    line=`echo $line | sed 's/.*[a-z].*//g' | sed 's/.*[ABCDFGHIKLMNOPQRSTUVWXYZ].*//g' `
    if [ -z "$line" ]
    then 
	h=$((h+1))
        header[$((h))]=`sed -n "$i p" $1`
        header[$((h))]="( ${header[$((h))]}"
	continue
    fi
    j=$((j+1))
    k=0
    line=`echo $line | sed 's/ +/ /g'`
    for val in $line
    do 
	k=$((k+1))
	array[$((k))]=$val
	X[$((j))]=${array[1]}
	XL[$((j))]=${array[2]}
	XH[$((j))]=${array[3]}
	Y[$((j))]=${array[4]}
	DYSTATP[$((j))]=${array[5]}
	DYSTATM[$((j))]=${array[6]}
    done
done
####################################################################
# Now we write out the numbers we got: this should basically be
# exactly the same as in the input text file. This is just some
# for the purposes of letting you know what is going on. 
echo "x	xlow	xhigh	y	dy+	dy-  "
for (( i=1 ; i<=j ; i++ ))
do 
    echo ${X[i]} ${XL[i]} ${XH[i]} ${Y[i]} ${DYSTATP[i]} ${DYSTATM[i]} 
done

####################################################################
# Now we calculate topdrawers DX and NDX as DX=xhigh-x and
# NDX=x-xlow respectively. Equally DY and NDY are calculated
# assuming that DYSTATP and DYSYSP add in quadrature to give
# DY and likewise DYSTATM and DYSYSM to give NDY.
for (( i=1 ; i<=j ; i++ ))
do 
    # First turn the scientific number notation to floating point
    # so that bc doesn't complain or give crap back
    X[$((i))]=`echo ${X[$((i))]} | sed 's/E+/\*10\^/g' `
    X[$((i))]=`echo ${X[$((i))]} | sed 's/E-/\/10\^/g' `
    X[$((i))]=`echo "scale=10; ${X[$i]}" | bc `
    XL[$((i))]=`echo ${XL[$((i))]} | sed 's/E+/\*10\^/g' `
    XL[$((i))]=`echo ${XL[$((i))]} | sed 's/E-/\/10\^/g' `
    XL[$((i))]=`echo "scale=10; ${XL[$i]}" | bc `
    XH[$((i))]=`echo ${XH[$((i))]} | sed 's/E+/\*10\^/g' `
    XH[$((i))]=`echo ${XH[$((i))]} | sed 's/E-/\/10\^/g' `
    XH[$((i))]=`echo "scale=10; ${XH[$i]}" | bc `
    Y[$((i))]=`echo ${Y[$((i))]} | sed 's/E+/\*10\^/g' `
    Y[$((i))]=`echo ${Y[$((i))]} | sed 's/E-/\/10\^/g' `
    Y[$((i))]=`echo "scale=10; ${Y[$i]}" | bc `
    DYSTATP[$((i))]=`echo ${DYSTATP[$((i))]} | sed 's/E+/\*10\^/g' `
    DYSTATP[$((i))]=`echo ${DYSTATP[$((i))]} | sed 's/E-/\/10\^/g' `
    DYSTATP[$((i))]=`echo "scale=10; ${DYSTATP[$i]}" | bc `
    DYSTATM[$((i))]=`echo ${DYSTATM[$((i))]} | sed 's/E+/\*10\^/g' `
    DYSTATM[$((i))]=`echo ${DYSTATM[$((i))]} | sed 's/E-/\/10\^/g' `
    DYSTATM[$((i))]=`echo "scale=10; ${DYSTATM[$i]}" | bc `
    # Now we actually compute the things topdrawer needs:
    if [ `echo "${X[i]}>0" | bc ` -eq 1 ]
    then
	DX[$((i))]=`echo "scale=10; ${XH[i]}-${X[i]} " | bc`
    else
	DX[$((i))]=`echo "scale=10; ${XH[i]}+(-1*${X[i]}) " | bc`
    fi
    if [ `echo "${XL[i]}>0" | bc ` -eq 1 ]
    then
	NDX[$((i))]=`echo "scale=10; ${X[i]}-${XL[i]} " | bc`
    else
	NDX[$((i))]=`echo "scale=10; ${X[i]}+(-1*${XL[i]}) " | bc`
    fi
    DY[$((i))]=${DYSTATP[$((i))]}
    NDY[$((i))]=${DYSTATM[$((i))]}
    echo ${X[i]} ${DX[i]} ${NDX[i]} ${Y[i]} ${DY[i]} ${NDY[i]}
done

####################################################################
# Now we want to basically write out a topdrawer file with this
# stuff in it. First we regurgitate the hepdata header to keep
# keep track of what the numbers corresponded to:
rm $1.top
touch $1.top
for (( i=1 ; i<=h ; i++ ))
do
    echo ${header[i]} >> $1.top
done

####################################################################
# Now we write out a title (the top line of the hepdata header)
# followed by the SET ORDER command and then the data themselves:
echo "TITLE TOP \"${header[1]}\"" >> $1.top
echo "SET ORDER X DX NDX Y DY NDY" >> $1.top
for (( i=1 ; i<=j ; i++ ))
do
    echo ${X[i]} ${DX[i]} ${NDX[i]} ${Y[i]} ${DY[i]} ${NDY[i]} >> $1.top
done
echo "PLOT" >> $1.top
