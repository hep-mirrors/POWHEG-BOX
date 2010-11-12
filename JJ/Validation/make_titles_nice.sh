#!/bin/bash
######################################################################
#
# What this does:
# Mainly it adds CASE statements for the titles in files produced
# by JJ/pwhg_analysis.f . To work on the raw JJ/pwhg_analysis you
# need to first convert the .top file to 1up format using the 
# 1up_plot_converter.sh . Alternatively it works directly on files
# produced using the merge_plots.f program. It does not work directly
# on pwhg_analysis.f (6-up) output because it searches for calls to
# TITLE TOP, TITLE BOTTOM and TITLE LEFT in order to insert the new
# CASE statements.
#
# How it works:
# ./make_plots_nice.sh the_plots.top
# and the_plots.top should now be 'nice(r)'.

######################################################################
# Delete trailing white space
sed -i -e 's/[ \t]*$//' $1

######################################################################
# Delete leading and trailing white space
sed -i -e 's/^[ \t]*//;s/[ \t]*$//' $1

######################################################################
# Delete all current NEW PLOT lines
sed -i -e '/( TITLE/d' $1

######################################################################
# Replace ?b/bin by Mb/bin everywhere
sed -i -e 's/?b\/bin/Mb\/bin/g' $1

######################################################################
# Replace "Total ET" by a sum of ET 
sed -i -e 's/Total E0T1/SE0T1/g' $1
# Add case statements for the total ET plot
sed -i -e '/TITLE TOP.*SE0T1/ a CASE "F X X"' $1
sed -i -e '/TITLE BOTTOM.*SE0T1/ a CASE "F X X"' $1
#sed -i -e '/TITLE LEFT.*SE0T1/ a\
#             CASE " G   F X X  SG     S"' $1
sed -i -e '/TITLE LEFT.*SE0T1/ a\
CASE " G  F X X SG     S"' $1
sed -i -e 's/TITLE LEFT.*SE0T1.* (/TITLE LEFT \"dS\/dSE0T1 (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)SE0T1.*/ a CASE \"F X X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)SE0T1.*/TITLE BOTTOM \"SE0T1 (GeV)\"/g' $1

######################################################################
# Replace "pT of UB"
sed -i -e 's/TITLE\(.*\)pT of UB\"/TITLE\1Underlying p0T1\"/g' $1
sed -i -e 's/TITLE\(.*\)pT of UB)\"/TITLE\1Underlying p0T1)\"/g' $1
sed -i -e '/TITLE\(.*\)Underlying p0T1\"/ a CASE "            X X\"' $1
sed -i -e 's/TITLE\(.*\)pT of UB E0T11 >/TITLE\1Underlying p0T1 E0T,11 >/g' $1
sed -i -e '/TITLE\(.*\)Underlying p0T1 E0T,11 >/ a CASE "            X X  X   X"' $1
sed -i -e 's/TITLE\(.*\)pT of UB E0T11 \& E0T21 >/TITLE\1Underlying p0T1 E0T,11 \& E0T,21 >/g' $1
sed -i -e '/TITLE\(.*\)Underlying p0T1 E0T,11 \& E0T,21 >/ a CASE "            X X  X   X    X   X"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)Underlying p0T1.*\">/TITLE BOTTOM\1Underlying p0T1\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)Underlying p0T1\">/ a CASE "            X X"' $1
                                   
# New left titles (without cuts info):
sed -i -e 's/TITLE LEFT\(.*\)(Underlying p0T1.*/TITLE LEFT\1p0T1 (Mb\/bin)/g' $1
sed -i -e '/TITLE LEFT\(.*\)p0T1 (Mb\/bin)/ a \
CASE      " G   X X SG     S"' $1
sed -i -e '/TITLE LEFT\(.*\)(pT of UB.*/ a CASE \" G   X X SG     S\"' $1
sed -i -e 's/TITLE LEFT\(.*\)(pT of UB.*/TITLE LEFT\1p0T1 (Mb\/bin)/g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)Underlying p0T1.*/ a CASE \"            X X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)Underlying p0T1.*/TITLE BOTTOM \"Underlying p0T1 (GeV)\"/g' $1


######################################################################
# Replace "njets"
sed -i -e 's/TITLE\(.*\)njets ET1 >/TITLE\1n0jets1 E0T,11 >/g' $1
sed -i -e '/TITLE\(.*\)n0jets1 E0T,11 >/ a CASE " X    X  X   X"' $1
sed -i -e 's/TITLE\(.*\)njets ET1 \& ET2 >/TITLE\1n0jets1 E0T,11 \& E0T,21 >/g' $1
sed -i -e '/TITLE\(.*\)n0jets1 E0T,11 \& E0T,21 >/ a CASE " X    X  X   X    X   X"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)n0jets1.*\">/TITLE BOTTOM\1n0jets1\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)n0jets1\">/ a CASE " X    X"' $1
                                   
# New left titles (without cuts info):
sed -i -e 's/TITLE LEFT\(.*\)(n0jets1.*/TITLE LEFT\1n0jets1 (Mb\/bin)/g' $1
sed -i -e '/TITLE LEFT\(.*\)n0jets1 (Mb\/bin)/ a \
CASE      " G   X    X SG     S"' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)n0jets1.*/ a CASE \" X    X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)n0jets1.*/TITLE BOTTOM \"n0jets1\"/g' $1

######################################################################
# Replace "H0J1/J21" by H01/21
sed -i -e 's/H0J1\/J21/H01\/21/g' $1
sed -i -e 's/H0J1\,J21/H01\/21/g' $1
sed -i -e 's/H01\/21 E0T11 \& E0T21/H01\/21 E0T,11 \& E0T,21/g' $1
sed -i -e 's/H01\/21 E0T11 >/H01\/21 E0T,11 >/g' $1
sed -i -e 's/H01\/21 E0T11 \& 2E0T21/H01\/21 E0T,11 \& 2E0T,21/g' $1
                                   
# Add case statements for H01/21
sed -i -e '/TITLE TOP.*H01\/21.* E0T,11 \& E/ a CASE "GX   X  X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*H01\/21.* E0T,11 \& E/ a CASE "GX   X  X   X    X   X"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*H01\/21 E0T,11 >/ a CASE "GX   X  X   X"' $1
sed -i -e '/TITLE BOTTOM.*H01\/21 E0T,11 >/ a CASE "GX   X  X   X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*H01\/21.*20.* / a \
CASE      " G  GX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*H01\/21.*20.* /TITLE LEFT \"dS\/dH01\/21 /g' $1
sed -i -e '/TITLE LEFT.*H01\/21.*40.* / a \
CASE      " G  GX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*H01\/21.*40.* /TITLE LEFT \"dS\/dH01\/21 /g' $1
sed -i -e '/TITLE LEFT.*H01\/21.*100.* / a \
CASE      " G  GX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*H01\/21.*100.* /TITLE LEFT \"dS\/dH01\/21 /g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)H01\/21 .*/ a CASE \"GX   X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)H01\/21 .*/TITLE BOTTOM \"H01\/21\"/g' $1

######################################################################
# Replace abs(DH01,21) by |DH01,21|
sed -i -e 's/abs(DH01,21)/\|DH0121\|/g' $1
sed -i -e 's/DH0121\(.*\)E0T11 \& E0T21/DH0121\1E0T,11 \& E0T,21/g' $1
sed -i -e 's/DH0121\(.*\)E0T11 >/DH0121\1E0T,11 >/g' $1
sed -i -e 's/DH0121\(.*\)E0T11 \& 2E0T21/DH0121\1E0T,11 \& 2E0T,21/g' $1

# Add case statements for |DH01,21|
sed -i -e '/TITLE TOP.*DH0121.* E0T,11 \& E/ a CASE " FGX  X   X   X    X   X "' $1
sed -i -e '/TITLE BOTTOM.*DH0121.* E0T,11 \& E/ a CASE " FGX  X   X   X    X   X "' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*DH0121.*E0T,11 >/ a CASE " FGX  X   X   X"' $1
sed -i -e '/TITLE BOTTOM.*DH0121.*E0T,11 >/ a CASE " FGX  X   X   X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*DH0121.*20.* / a \
CASE      " G   FGX  X  SG     S"' $1
sed -i -e 's/TITLE LEFT.*DH0121.*20.* /TITLE LEFT \"dS\/d|DH0121| /g' $1
sed -i -e '/TITLE LEFT.*DH0121.*40.* / a \
CASE      " G   FGX  X  SG     S"' $1
sed -i -e 's/TITLE LEFT.*DH0121.*40.* /TITLE LEFT \"dS\/d|DH0121| /g' $1
sed -i -e '/TITLE LEFT.*DH0121.*100.* / a \
CASE      " G   FGX  X  SG     S"' $1
sed -i -e 's/TITLE LEFT.*DH0121.*100.* /TITLE LEFT \"dS\/d|DH0121| /g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)DH0121.*/ a CASE \" FGX  X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)DH0121.*/TITLE BOTTOM \"|DH0121|\"/g' $1

######################################################################
# Replace "S(D)"
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>20 E0T21>20+D/ a CASE "G F   X  X     X  X    F"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>20 E0T21>20+D/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>20 E0T21>20+D.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>40 E0T21>40+D/ a CASE "G F   X  X     X  X    F"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>40 E0T21>40+D/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>40 E0T21>40+D.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>100 E0T21>100+D/ a CASE "G F   X  X      X  X     F"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>100 E0T21>100+D/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>100 E0T21>100+D.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)S(D) E0T11>.*E0T21>.*D/ a CASE \"F S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)S(D) E0T11>.*E0T21>.*D.*/TITLE BOTTOM \"D (GeV)\"/g' $1

sed -i -e '/TITLE\(.*\)\"S(D) E0T11>20+D E0T21>20/ a CASE "G F   X  X    F  X  X"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>20+D E0T21>20/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>20+D E0T21>20.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>40+D E0T21>40/ a CASE "G F   X  X    F  X  X"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>40+D E0T21>40/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>40+D E0T21>40.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>100+D E0T21>100/ a CASE "G F   X  X     F  X  X"' $1
sed -i -e '/TITLE LEFT\(.*\)S(D) E0T11>100+D E0T21>100/ a CASE "G F  SG S"' $1
sed -i -e 's/TITLE LEFT\(.*\)S(D) E0T11>100+D E0T21>100.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)S(D) E0T11>.*D E0T21>.*/ a CASE \"F S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)S(D) E0T11>.*D E0T21>.*/TITLE BOTTOM \"D (GeV)\"/g' $1

                                   
sed -i -e '/TITLE\(.*\)\"S(D) E0T11>D/ a CASE "G F   X  X F"' $1
sed -i -e '/TITLE LEFT\(.*\)(S(D) E0T11>D.*/ a CASE \"G F  SG S\"' $1
sed -i -e 's/TITLE LEFT\(.*\)(S(D) E0T11>D.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)S(D) E0T11>D.*/ a CASE \"F S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)S(D) E0T11>D.*/TITLE BOTTOM \"D (GeV)\"/g' $1

sed -i -e '/TITLE\(.*\)\"S(D) E0T11 \& E0T21>D/ a CASE "G F   X  X    X  X F"' $1
sed -i -e '/TITLE LEFT\(.*\)(S(D) E0T11 \& E0T21>D.*/ a CASE \"G F  SG S\"' $1
sed -i -e 's/TITLE LEFT\(.*\)(S(D) E0T11 \& E0T21>D.*/TITLE LEFT \"S(D) (Mb)\"/g' $1
sed -i -e '/TITLE BOTTOM\(.*\)S(D) E0T11 \& E0T21>D.*/ a CASE \"F S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)S(D) E0T11 \& E0T21>D.*/TITLE BOTTOM \"D (GeV)\"/g' $1

######################################################################
# Replace DF01,21 by DF0121
sed -i -e 's/DF01,21/DF0121/g' $1
sed -i -e 's/DF0121 E0T11 \& E0T21/DF0121 E0T,11 \& E0T,21/g' $1
sed -i -e 's/DF0121 E0T11 >/DF0121 E0T,11 >/g' $1
sed -i -e 's/DF0121 E0T11 \& 2E0T21/DF0121 E0T,11 \& 2E0T,21/g' $1

# Add case statements for DF01,21
sed -i -e '/TITLE TOP.*DF0121.* E0T,11 \& E/ a CASE "FGX  X  X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*DF0121.* E0T,11 \& E/ a CASE "FGX  X  X   X    X   X"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*DF0121 E0T,11 >/ a CASE "FGX  X  X   X"' $1
sed -i -e '/TITLE BOTTOM.*DF0121 E0T,11 >/ a CASE "FGX  X  X   X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*DF0121.*20.* / a \
CASE      " G  FGX  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DF0121.*20.* /TITLE LEFT \"dS\/dDF0121 /g' $1
sed -i -e '/TITLE LEFT.*DF0121.*40.* / a \
CASE      " G  FGX  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DF0121.*40.* /TITLE LEFT \"dS\/dDF0121 /g' $1
sed -i -e '/TITLE LEFT.*DF0121.*100.* / a \
CASE      " G  FGX  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DF0121.*100.* /TITLE LEFT \"dS\/dDF0121 /g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)DF0121.*/ a CASE \"FGX  X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)DF0121.*/TITLE BOTTOM \"DF0121\"/g' $1

######################################################################
# Replace DR01,21 by DR0121
sed -i -e 's/DR01,21/DR0121/g' $1
sed -i -e 's/DR0121\(.*\)E0T11/DR0121\1E0T,11/g' $1
sed -i -e 's/DR0121\(.*\)E0T21/DR0121\1E0T,21/g' $1
sed -i -e 's/DR0121\(.*\)E0T12/DR0121\1E0T,21/g' $1

# Add case statements for DF01,21
sed -i -e '/TITLE TOP.*DR0121.* E0T,11 \& E/ a CASE "F X  X  X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*DR0121.* E0T,11 \& E/ a CASE "F X  X  X   X    X   X"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*DR0121 E0T,11 > / a CASE "F X  X  X   X"' $1
sed -i -e '/TITLE BOTTOM.*DR0121 E0T,11 > / a CASE "F X  X  X   X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*DR0121.*20.* / a \
CASE      " G  F X  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DR0121.*20.* /TITLE LEFT \"dS\/dDR0121 /g' $1
sed -i -e '/TITLE LEFT.*DR0121.*40.* / a \
CASE      " G  F X  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DR0121.*40.* /TITLE LEFT \"dS\/dDR0121 /g' $1
sed -i -e '/TITLE LEFT.*DR0121.*100.* / a \
CASE      " G  F X  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*DR0121.*100.* /TITLE LEFT \"dS\/dDR0121 /g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)DR0121.*/ a CASE \"F X  X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)DR0121.*/TITLE BOTTOM \"DR0121\"/g' $1

######################################################################
# Replace p0T,J31 by p0T,31
sed -i -e 's/\"p0T,J31/\"p0T,31/g' $1
sed -i -e 's/d(p0T,J31/d(p0T,31/g' $1
sed -i -e 's/p0T,31, p0T11 \& p0T21/p0T,31, p0T,11 \& p0T,21/g' $1
sed -i -e 's/p0T,31, p0T11 >/p0T,31, p0T,11 >/g' $1

# Add case statements for p0T,31
sed -i -e '/TITLE TOP.*\"p0T,31.* \& p/ a CASE " X   X   X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*\"p0T,31.* \& p/ a CASE " X   X   X   X    X   X"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*\"p0T,31.*p0T11 >/ a CASE " X   X   X   X"' $1
sed -i -e '/TITLE BOTTOM.*\"p0T,31.*p0T11 >/ a CASE " X   X   X   X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*(p0T,31.* (Mb/ a \
CASE      " G   X   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*(p0T,31.* (Mb/TITLE LEFT \"dS\/dp0T,31 (Mb/g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)p0T,31, p0T,11 \&.*/ a CASE \" X   X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)p0T,31, p0T,11 \&.*/TITLE BOTTOM \"p0T,31 (GeV)\"/g' $1

######################################################################
# Replace H0J31, p0T,J31 by H031, p0T,31
sed -i -e 's/\"H0J31, p0T,J31\(.*\)p0T1\(.*\)/\"H031, p0T,31\1p0T,11 \& p0T,21 > 40 GeV\"/g' $1
sed -i -e 's/d(H0J31, p0T,J31\(.*\)p0T1\(.*\)/d(H031, p0T,31\1p0T,11 \& p0T,21 > 40 GeV) (Mb\/bin)\"/g' $1
#TITLE TOP "H031, p0T,31>10, E0T11 & E0T,11 & E0T,21 > 40 GeV
#     CASE "GX X   X   X      X  X    X   X"
# Add case statements for H031 plots
sed -i -e '/TITLE TOP.*\"H031, p0T,31.*>10,/ a \
CASE "GX X   X   X      X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*\"H031, p0T,31.*>10,/ a \
CASE "GX X   X   X      X   X    X   X"' $1
sed -i -e '/TITLE TOP.*\"H031, p0T,31.*>100,/ a \
CASE "GX X   X   X       X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*\"H031, p0T,31.*>100,/ a \
CASE "GX X   X   X       X   X    X   X"' $1

sed -i -e '/TITLE LEFT.*d(H031, p0T,31.*>10,/ a \
CASE " G  GX X SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(H031, p0T,31.*>10,.* (/TITLE LEFT \"dS\/dH031 (/' $1

sed -i -e '/TITLE LEFT.*d(H031, p0T,31.*>100,/ a \
CASE " G  GX X SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(H031, p0T,31.*>100,.* (/TITLE LEFT \"dS\/dH031 (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)H031, p0T,31>.*/ a CASE \"GX X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)H031, p0T,31>.*/TITLE BOTTOM \"H031\"/g' $1

######################################################################
# Replace Y0J31, p0T,J31 by Y031, p0T,31
sed -i -e 's/\"Y0J31, p0T,J31/\"y031, p0T,31/g' $1
sed -i -e 's/d(Y0J31, p0T,J31/d(y031, p0T,31/g' $1

# Add case statements for Y031 plots
sed -i -e '/TITLE TOP.*\"y031, p0T,31.*>10,.* \& p/ a \
CASE " X X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031, p0T,31.*>10,.* \& p/ a \
CASE " X X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE TOP.*\"y031, p0T,31.*>100,.* \& p/ a \
CASE " X X   X   X       X  X    X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031, p0T,31.*>100,.* \& p/ a \
CASE " X X   X   X       X  X    X  X"' $1
sed -i -e '/TITLE LEFT.*d(y031, p0T,31.*>10,.* \& p/ a \
CASE " G    X X   X   X      X  X    X  X         SG     S"' $1
sed -i -e '/TITLE LEFT.*d(y031, p0T,31.*>100,.* \& p/ a \
CASE " G    X X   X   X       X  X    X  X         SG     S"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*\"y031, p0T,31.*>10,.* \& 2E/ a \
CASE " X X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031, p0T,31.*>10,.* \& 2E/ a \
CASE " X X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE TOP.*\"y031, p0T,31.*>100,.* \& 2E/ a \
CASE " X X   X   X       X  X     X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031, p0T,31.*>100,.* \& 2E/ a \
CASE " X X   X   X       X  X     X  X"' $1

sed -i -e '/TITLE LEFT.*d(y031, p0T,31.*>10,/ a \
CASE " G   X X SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(y031, p0T,31.*>10,.* (/TITLE LEFT \"dS\/dy031 (/' $1

sed -i -e '/TITLE LEFT.*d(y031, p0T,31.*>100,/ a \
CASE " G   X X SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(y031, p0T,31.*>100,.* (/TITLE LEFT \"dS\/dy031 (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)y031, p0T,31>.*/ a CASE \" X X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)y031, p0T,31>.*/TITLE BOTTOM \"y031\"/g' $1

######################################################################
# Replace Y0J31-Y0J121, p0T,J31 by Y031, p0T,31
sed -i -e 's/\"Y0J31-Y0J121, p0T,J31/\"y031-y0121, p0T,31/g' $1
sed -i -e 's/d(Y0J31-Y0J121, p0T,J31/d(y031-y0121, p0T,31/g' $1

# Add case statements for y031-y0121 plots
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>10,.* \& p/ a \
CASE " X X  X  X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>10,.* \& p/ a \
CASE " X X  X  X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>50,.* \& p/ a \
CASE " X X  X  X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>50,.* \& p/ a \
CASE " X X  X  X   X   X      X  X    X  X"' $1
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>100,.* \& p/ a \
CASE " X X  X  X   X   X       X  X    X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>100,.* \& p/ a \
CASE " X X  X  X   X   X       X  X    X  X"' $1
# Ditto but for asymmetric case
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>10,.* \& 2E/ a \
CASE " X X  X  X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>10,.* \& 2E/ a \
CASE " X X  X  X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>50,.* \& 2E/ a \
CASE " X X  X  X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>50,.* \& 2E/ a \
CASE " X X  X  X   X   X      X  X     X  X"' $1
sed -i -e '/TITLE TOP.*\"y031-y0121, p0T,31.*>100,.* \& 2E/ a \
CASE " X X  X  X   X   X       X  X     X  X"' $1
sed -i -e '/TITLE BOTTOM.*\"y031-y0121, p0T,31.*>100,.* \& 2E/ a \
CASE " X X  X  X   X   X       X  X     X  X"' $1

# New left titles (without cuts info):
sed -i -e '/TITLE LEFT.*d(y031-y0121, p0T,31.*>10.* (/ a \
CASE      " G    X X  X  X  SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(y031-y0121, p0T,31.*>10.* (/TITLE LEFT \"dS\/d(y031-y0121) (/' $1
sed -i -e '/TITLE LEFT.*d(y031-y0121, p0T,31.*>50.* (/ a \
CASE      " G    X X  X  X  SG     S"' $1
sed -i -e 's/TITLE LEFT.*d(y031-y0121, p0T,31.*>50.* (/TITLE LEFT \"dS\/d(y031-y0121) (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)y031-y0121, p0T,31>.*/ a CASE \" X X  X  X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)y031-y0121, p0T,31>.*/TITLE BOTTOM \"y031-y0121\"/g' $1

######################################################################
# Replace pT by p0T1
sed -i -e 's/pT/p0T1/g' $1
# Replace M0JJ1 by m0JJ1
sed -i -e 's/M0JJ1/m0JJ1/g' $1
# Add case statements for m0JJ1 plots y_max<0.4
sed -i -e '/TITLE TOP.*m0JJ1.*<0.4/ a CASE " X  X  X X      X   X"' $1
sed -i -e '/TITLE BOTTOM.*m0JJ1.*<0.4/ a CASE " X  X  X X      X   X"' $1
# sed -i -e '/TITLE LEFT.*m0JJ1.*<0.4/ a\
# CASE      " G   X  X SG     S"' $1
# sed -i -e 's/TITLE LEFT.*m0JJ1.*<0.4.* (/TITLE LEFT \"dS/dm0JJ1 (/' $1
# Add case statements for the other m0JJ1 plots
sed -i -e '/TITLE TOP.*m0JJ1.*<.*</ a CASE " X  X  X X          X   X"' $1
sed -i -e '/TITLE BOTTOM.*m0JJ1.*<.*</ a CASE " X  X  X X          X   X"' $1

sed -i -e '/TITLE LEFT.*(m0JJ1.* (/ a\
CASE      " G   X  X SG     S"' $1
sed -i -e 's/TITLE LEFT.*(m0JJ1.* (/TITLE LEFT \"dS\/dm0JJ1 (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)m0JJ1 p0T1>.*/ a CASE \" X  X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)m0JJ1 p0T1>.*/TITLE BOTTOM \"m0JJ1 (GeV)\"/g' $1


######################################################################
# Add case statements for DF plots 75 < pT1^max < 100
sed -i -e '/TITLE TOP.*DF 75.*100/ a CASE "FG       X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF 75.*100/ a CASE "FG       X XX   X"' $1
sed -i -e '/TITLE TOP.*DF  75.*100/ a CASE "FG        X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF  75.*100/ a CASE "FG        X XX   X"' $1
unit_norm=`sed -n "/TITLE TOP.*DF.*75.*100.*Unit Norm/p" $1`
if [ -z "$unit_norm" ]
then
    sed -i -e '/TITLE LEFT.*DF 75.*100/ a \
CASE " G   FG       X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF  75.*100/ a \
CASE " G   FG        X XX   X            SG     S"' $1
#    sed -i -e '/TITLE LEFT.*DF  75.*100/ a \
#CASE " G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF  75.*100/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF  75.*100.* (/TITLE LEFT \"dS\/dDF (/' $1
else
    sed -i -e '/TITLE TOP.*DF.*75.*100/ s/ Unit Norm//g' $1
    sed -i -e '/TITLE LEFT.*DF.*75.*100/ s/\".*dS\/d(/\"1\/SdS\/d(/g' $1
    sed -i -e '/TITLE LEFT.*DF.*75.*100/ s/ Unit Norm//g' $1
    sed -i -e '/TITLE LEFT.*DF 75.*100/ a \
CASE "  G G   FG       X XX   X            SG     S"' $1
#    sed -i -e '/TITLE LEFT.*DF  75.*100/ a \
#CASE "  G G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF  75.*100/ a \
CASE "  G G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF  75.*100.* (/TITLE LEFT \"1\/SdS\/dDF (/' $1
fi

# Add case statements for DF plots 100 < pT1^max < 130
sed -i -e '/TITLE TOP.*DF.*100.*130/ a CASE "FG        X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF.*100.*130/ a CASE "FG        X XX   X"' $1
unit_norm=`sed -n "/TITLE TOP.*DF.*100.*130.*Unit Norm/p" $1`
if [ -z "$unit_norm" ]
then
#    sed -i -e '/TITLE LEFT.*DF.*100.*130/ a \
#CASE " G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF.*100.*130/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF.*100.*130.* (/TITLE LEFT \"dS\/dDF (/' $1
else
    sed -i -e '/TITLE TOP.*DF.*100.*130/ s/ Unit Norm//g' $1
    sed -i -e '/TITLE LEFT.*DF.*100.*130/ s/\".*dS\/d(/\"1\/SdS\/d(/g' $1
    sed -i -e '/TITLE LEFT.*DF.*100.*130/ s/ Unit Norm//g' $1
#    sed -i -e '/TITLE LEFT.*DF.*100.*130/ a \
#CASE "  G G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF.*100.*130/ a \
CASE "  G G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF.*100.*130.* (/TITLE LEFT \"1\/SdS\/dDF (/' $1
fi

# Add case statements for DF plots 130 < pT1^max < 180
sed -i -e '/TITLE TOP.*DF.*130.*180/ a CASE "FG        X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF.*130.*180/ a CASE "FG        X XX   X"' $1
unit_norm=`sed -n "/TITLE TOP.*DF.*130.*180.*Unit Norm/p" $1`
if [ -z "$unit_norm" ]
then
#    sed -i -e '/TITLE LEFT.*DF.*130.*180/ a \
#CASE " G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF.*130.*180/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF.*130.*180.* (/TITLE LEFT \"dS\/dDF (/' $1
else
    sed -i -e '/TITLE TOP.*DF.*130.*180/ s/ Unit Norm//g' $1
    sed -i -e '/TITLE LEFT.*DF.*130.*180/ s/\".*dS\/d(/\"1\/SdS\/d(/g' $1
    sed -i -e '/TITLE LEFT.*DF.*130.*180/ s/ Unit Norm//g' $1
#    sed -i -e '/TITLE LEFT.*DF.*130.*180/ a \
#CASE "  G G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF.*130.*180/ a \
CASE "  G G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF.*130.*180.* (/TITLE LEFT \"1\/SdS\/dDF (/' $1
fi

# Add case statements for DF plots pT1^max > 180
sed -i -e '/TITLE TOP.*DF p.*>.*180/ a CASE "FG  X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF p.*>.*180/ a CASE "FG  X XX   X"' $1
sed -i -e '/TITLE TOP.*DF       p.*>.*180/ a CASE "FG        X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*DF       p.*>.*180/ a CASE "FG        X XX   X"' $1
unit_norm=`sed -n "/TITLE TOP.*DF[ ]*p.*>.*180.*Unit Norm/p" $1`
if [ -z "$unit_norm" ]
then
#    sed -i -e '/TITLE LEFT.*DF p.*>.*180/ a \
#CASE " G   FG  X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF p.*>.*180/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF p.*>.*180.* (/TITLE LEFT \"dS\/dDF (/' $1
#    sed -i -e '/TITLE LEFT.*DF       p.*>.*180/ a \
#CASE " G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF       p.*>.*180/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF       p.*>.*180.* (/TITLE LEFT \"dS\/dDF (/' $1
else
    sed -i -e '/TITLE TOP.*DF.*p.*>.*180/ s/ Unit Norm//g' $1
    sed -i -e '/TITLE LEFT.*DF.*p.*>.*180/ s/\".*dS\/d(/\"1\/SdS\/d(/g' $1
    sed -i -e '/TITLE LEFT.*DF.*p.*>.*180/ s/ Unit Norm//g' $1
#    sed -i -e '/TITLE LEFT.*DF p.*>.*180/ a \
#CASE "  G G   FG  X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF.*p.*>.*180/ a \
CASE " G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF.*p.*>.*180.* (/TITLE LEFT \"dS\/dDF (/' $1
#    sed -i -e '/TITLE LEFT.*DF       p.*>.*180/ a \
#CASE "  G G   FG        X XX   X            SG     S"' $1
    sed -i -e '/TITLE LEFT.*DF       p.*>.*180/ a \
CASE "  G G  FG SG     S"' $1
    sed -i -e 's/TITLE LEFT.*DF       p.*>.*180.* (/TITLE LEFT \"1\/SdS\/dDF (/' $1
fi

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)DF.*p0T12max3/ a CASE \"FG\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)DF.*p0T12max3.*/TITLE BOTTOM \"DF\"/g' $1

######################################################################
# Add case statements p0T12JET3 only generation cuts
sed -i -e '/TITLE TOP.*p0T12JET3 only/ a \
CASE " X XX   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 only/ a \
CASE " X XX   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 only/ a \
#CASE " G    X XX   X                       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 only/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 only.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Add case statements p0T12JET3 |y0jet1|<0.1
sed -i -e '/TITLE TOP.*p0T12JET3 |y0jet1|<0.1/ a \
CASE " X XX   X   X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 |y0jet1|<0.1/ a \
CASE " X XX   X   X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 |y0jet1|<0.1/ a \
#CASE " G    X XX   X   X   X       SG     S"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 |y0jet1|<0.1/ a \
#CASE " G    X XX   X   X   X       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 |y0jet1|<0.1/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 |y0jet1|<0.1.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Add case statements p0T12JET3 0.1<|y0jet1|<0.7
sed -i -e '/TITLE TOP.*p0T12JET3 0.1<|y0jet1|<0.7/ a \
CASE " X XX   X       X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 0.1<|y0jet1|<0.7/ a \
CASE " X XX   X       X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 0.1<|y0jet1|<0.7/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 0.1<|y0jet1|<0.7/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 0.1<|y0jet1|<0.7/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 0.1<|y0jet1|<0.7.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Add case statements p0T12JET3 0.7<|y0jet1|<1.1
sed -i -e '/TITLE TOP.*p0T12JET3 0.7<|y0jet1|<1.1/ a \
CASE " X XX   X       X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 0.7<|y0jet1|<1.1/ a \
CASE " X XX   X       X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 0.7<|y0jet1|<1.1/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 0.7<|y0jet1|<1.1/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 0.7<|y0jet1|<1.1/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 0.7<|y0jet1|<1.1.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Add case statements p0T12JET3 1.1<|y0jet1|<1.6
sed -i -e '/TITLE TOP.*p0T12JET3 1.1<|y0jet1|<1.6/ a \
CASE " X XX   X       X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 1.1<|y0jet1|<1.6/ a \
CASE " X XX   X       X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 1.1<|y0jet1|<1.6/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 1.1<|y0jet1|<1.6/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 1.1<|y0jet1|<1.6/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 1.1<|y0jet1|<1.6.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Add case statements p0T12JET3 1.6<|y0jet1|<2.1
sed -i -e '/TITLE TOP.*p0T12JET3 1.6<|y0jet1|<2.1/ a \
CASE " X XX   X       X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12JET3 1.6<|y0jet1|<2.1/ a \
CASE " X XX   X       X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 1.6<|y0jet1|<2.1/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
#sed -i -e '/TITLE LEFT.*p0T12JET3 1.6<|y0jet1|<2.1/ a \
#CASE " G    X XX   X       X   X       SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12JET3 1.6<|y0jet1|<2.1/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12JET3 1.6<|y0jet1|<2.1.* (/TITLE LEFT \"dS\/dp0T12JET3 (/' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)p0T12JET3 .*/ a CASE \" X XX   X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)p0T12JET3 .*/TITLE BOTTOM \"p0T12jet3 (GeV)\"/g' $1

######################################################################
# Replace "Y2JET3 everywhere by y2JET3
sed -i -e 's/\"Y2JET3/\"y2JET3/g' $1
sed -i -e 's/d(Y2JET3/d(y2JET3/g' $1

# Add case statements for Y2JET3 |p0T1|> 10/20/50 plots
sed -i -e '/TITLE TOP.*y2JET3.*|p0T1|> / a \
CASE " X   X              X X     "' $1
sed -i -e '/TITLE BOTTOM.*y2JET3.*|p0T1|> / a \
CASE " X   X              X X     "' $1
#sed -i -e '/TITLE LEFT.*y2JET3.*|p0T1|> / a \
#CASE " G    X   X              X X       SG     S"' $1
sed -i -e '/TITLE LEFT.*y2JET3.*|p0T1|> / a \
CASE " G   X   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*y2JET3.*|p0T1|> .* (/TITLE LEFT \"dS\/dy2JET3 (/g' $1

# Add case statements for Y2JET3 |p0T1|> 100 plots
sed -i -e '/TITLE TOP.*y2JET3.*|p0T1|>100/ a \
CASE " X   X              X X     "' $1
sed -i -e '/TITLE BOTTOM.*y2JET3.*|p0T1|>100/ a \
CASE " X   X              X X     "' $1
#sed -i -e '/TITLE LEFT.*y2JET3.*|p0T1|>100/ a \
#CASE " G    X   X              X X       SG     S"' $1
sed -i -e '/TITLE LEFT.*y2JET3.*|p0T1|>100/ a \
CASE " G   X   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*y2JET3.*|p0T1|>100.* (/TITLE LEFT \"dS\/dy2JET3 (/g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)y2JET3 (1-3).*/ a CASE \" X   X\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)y2JET3 (1-3).*/TITLE BOTTOM \"y2jet3\"/g' $1

######################################################################
# Add case statements for p0T12rel3 J1, ET1 & ET2 > 40 GeV
sed -i -e 's/p0T12rel3 J1, E0T11 \& E0T21/p0T12rel3 J1, E0T,11 \& E0T,21/g' $1
sed -i -e '/TITLE TOP.*p0T12rel3 J1, E0T,11 & E0T,21 > 40 GeV/ a \
CASE " X XX   X      X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12rel3 J1, E0T,11 & E0T,21 > 40 GeV/ a \
CASE " X XX   X      X   X    X   X"' $1
sed -i -e '/TITLE LEFT.*p0T12rel3 J1, E0T,11 & E0T,21 > 40 GeV/ a \
CASE " G    X XX   X      X   X    X   X           SG     S"' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)p0T12rel3 J1.*/ a CASE \" X XX   X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)p0T12rel3 J1.*/TITLE BOTTOM \"p0T12rel3 (GeV)\"/g' $1

######################################################################
# Add case statements for p0T12rel3 J1 and J2, E0T1>40 GeV
sed -i -e 's/p0T12rel3 J1 and J2, E0T11 \& E0T21/p0T12rel3 J1 and J2, E0T,11 \& E0T,21/g' $1
sed -i -e '/TITLE TOP.*p0T12rel3 J1 and J2, E0T,11 \& E0T,21/ a \
CASE " X XX   X             X   X    X   X"' $1
sed -i -e '/TITLE BOTTOM.*p0T12rel3 J1 and J2, E0T,11 \& E0T,21/ a \
CASE " X XX   X             X   X    X   X"' $1
#sed -i -e '/TITLE LEFT.*p0T12rel3 J1 and J2, E0T,11 \& E0T,21/ a \
#CASE " G    X XX   X             X   X    X   X           SG     S"' $1
sed -i -e '/TITLE LEFT.*p0T12rel3 J1.*/ a \
CASE " G   X XX   X SG     S"' $1
sed -i -e 's/TITLE LEFT.*p0T12rel3 J1.* (/TITLE LEFT \"dS\/dp0T12rel3 (/g' $1

# Improved bottom title:
sed -i -e '/TITLE BOTTOM\(.*\)p0T12rel3 J1.*/ a CASE \" X XX   X S   S\"' $1
sed -i -e 's/TITLE BOTTOM\(.*\)p0T12rel3 J1.*/TITLE BOTTOM \"p0T12rel3 (GeV)\"/g' $1

######################################################################
# Add case statements for H0J31 at the end
unit_norm=`sed -n "/TITLE TOP \"H0J31 Unit Norm\"/p" $1`
sed -i -e 's/\"H0J31 \"/\"H031\"/g' $1
sed -i -e 's/\"H0J31\"/\"H031\"/g' $1
sed -i -e 's/\"H0J31 Unit Norm\"/\"H031\"/g' $1
sed -i -e '/TITLE TOP.*\"H031\"/ a\
            CASE "GX X"' $1
sed -i -e '/TITLE BOTTOM.*\"H031\"/ a\
            CASE "GX X"' $1
if [ "$unit_norm" = "" ]
then
    sed -i -e 's/d(H0J31)/d(H031)/g' $1
#    sed -i -e '/TITLE LEFT.*d(H031)/ a \
#CASE " G   GX X  SG     S"' $1
    sed -i -e '/TITLE LEFT.*d(H031)/ a \
CASE " G  GX X SG     S"' $1
    sed -i -e 's/TITLE LEFT.*d(H031).* (/TITLE LEFT \"dS\/dH031 (/' $1
else
    sed -i -e 's/\".*dS\/d(H0J31.*Unit.*)/\"1\/SdS\/d(H031)/g' $1
#    sed -i -e '/TITLE LEFT.*d(H031)/ a \
#CASE "  G G   GX X  SG     S"' $1
    sed -i -e '/TITLE LEFT.*d(H031)/ a \
CASE "  G G  GX X SG     S"' $1
    sed -i -e 's/TITLE LEFT.*d(H031)/TITLE LEFT \"1\/SdS\/dH031/' $1 
fi


######################################################################
# Delete all DS/DDS left titles and replace with nicer chi!
sed -i -e '/LEFT.*DS\/DDS.*CHI2/{n;d;}' $1
sed -i -e '/LEFT.*DS\/DDS.*CHI2/ a CASE \"G\" \( CHI2' $1
sed -i -e 's/\(.*\)DS\/DDS\(.*\)CHI2/\1C\2CHI2/' $1

######################################################################
# Delete all "PLOT" commands
sed -i -e '/^PLOT/d' $1
sed -i -e 's/HIST\(.*\)/HIST\1 INTENSITY 7/g' $1
sed -i -e 's/HIST\(.*\)(\(.*\)/HIST\1 INTENSITY 7 (\2/g' $1
sed -i -e 's/TITLE TEXT\(.*\)\"\(.*\)\".*( TAG/TITLE TEXT\1\"\2\" ( TAG/g' $1
sed -i -e 's/2JET3/2jet3/g' $1 
# Change all HISTs to JOINs?
# sed -i -e 's/HIST\ \(.*\)/JOIN \1/g' $1