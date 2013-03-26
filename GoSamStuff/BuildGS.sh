#!/bin/sh

# ####################################################################
# To successfully employ this script, make sure that your bash is in #
# the bin - directory or adjust accordingly.                         #
######################################################################

RUNDIR=${PWD}
GOSAMSTUFFDIR=${PWD}/../GoSamStuff
GOSAMDIR=${PWD}/GoSam_POWHEG
GOSAMLIBDIR=${PWD}/GoSamlib
VIRDIR=$GOSAMDIR/Virtual
CONTRIBDIR=$GOSAMSTUFFDIR/GoSam-contrib

# If input is 'HELP':
if [ "$1" = "help" ] || [ "$1" = "" ]
then
    echo "******************************************************************************************"
    echo "-- HELP menu for the script buildvirt --                                                  "
    echo "Possible running options:                                                                 "
    echo "./BuildGS virtual       : generates the full virtual code,                                "
    echo "./BuildGS newfiles      : creates the new files needed to run the virtual amplitude,      "
    echo "./BuildGS standalone    : makes standalone virtual code,                                  "
    echo "./BuildGS cleanvirt     : removes the created virtual code,                               "
    echo "./BuildGS cleannewfiles : removes the files created to run the virtual amplitude          "
    echo "./BuildGS veryclean     : restores initial conditions,                                    "
    echo "./BuildGS help          : shows this menu.                                                "
    echo "******************************************************************************************"
    exit
fi

# # If input is 'CLEANVIRT':
# if [ "$1" = "cleanvirt" ]
# then
#     echo "This will delete the files for the virtual amplitude."
#     echo "Are you sure you want to proceed? (Yes/No)"
#     read ANS
#     if [ "$ANS" = "Yes" ]
#     then
# 	echo "---> Cleaning virtual part ..."
# 	cd $GOSAMDIR
# 	rm -f gosam.crashed
# 	rm -fr Virtual
# 	rm -fr orderfile.olc
# 	rm -f libgolem_olp.so
	
# 	echo "---> Recreating dummy libgolem_olp.so ..."
# 	cd $GOSAMDIR
# 	gfortran -fPIC -DPIC -o golem_olp.o -c golem_olp.f
# 	gfortran -shared -o libgolem_olp.so golem_olp.o
# 	rm -f golem_olp.o    
# 	exit
#     else
# 	echo "Aborted!"
#     fi
# fi

# # If input is 'CLEANNEWFILES':
# if [ "$1" = "cleannewfiles" ]
# then
#     if [ $WRITEPWHGFL = 1 ]
#     then
# 	echo "---> Deleting new files and restoring initial setup..."
# 	cd $RUNDIR    
# 	if [ -e $RUNDIR/virtual.f.dummy ]
# 	then    
# 	    mv virtual.f.dummy virtual.f
# 	fi
# 	if [ -e $RUNDIR/init_couplings.f.old ]
# 	then    
# 	    mv init_couplings.f.old init_couplings.f
# 	fi
# 	if [ -e $RUNDIR/init_processes.f.old ]
# 	then    
# 	    mv init_processes.f.old init_processes.f
# 	fi
# 	cd $GOSAMDIR
# 	rm write_pwhg_files
# 	rm virtual_new.f
# 	rm init_couplings_new.f
# 	rm init_processes_new.f

# 	echo "*********************************************"
# 	echo "The files:                                   "
# 	echo "- virtual.f                                  "
# 	echo "- init_couplings.f                           "
# 	echo "- init_processes.f                           "
# 	echo "were replaced with their original versions.  "
# 	echo "*********************************************"
# 	exit
#     else
# 	echo "*********************************************"
# 	echo "This command is not applicable to your setup."
# 	echo "*********************************************"
#     fi
    
# fi

# # VERY CLEAN:
# if [ "$1" = "veryclean" ]
# then
#     echo "This will delete ALL the files for the virtual amplitude."
#     echo "Are you really sure you want to proceed? (Yes/No)"
#     read ANS
#     if [ "$ANS" = "Yes" ]
#     then
# 	echo "---> Deleting all files and restoring initial conditions ..."
# 	cd $RUNDIR
# 	make clean
# 	if [ -e $RUNDIR/virtual.f.dummy ]
# 	then    
# 	    mv virtual.f.dummy virtual.f
# 	fi
# 	if [ -e $RUNDIR/init_couplings.f.old ]
# 	then    
# 	    mv init_couplings.f.old init_couplings.f
# 	fi
# 	if [ -e $RUNDIR/init_processes.f.old ]
# 	then    
# 	    mv init_processes.f.old init_processes.f
# 	fi

# 	cd $GOSAMDIR
# 	rm -fr Virtual
# 	rm -f gosam.crashed
# 	rm -f orderfile.*
# 	rm -f write_pwhg_files
# 	rm -f virtual_new.f
# 	rm -f init_couplings_new.f
# 	rm -f init_processes_new.f
# 	rm -f libgolem_olp.so
# 	echo "*********************************************"
# 	echo "All GoSam files have been deleted.           "
# 	echo "*********************************************"
# 	exit
#     else
# 	echo "Aborted!"
#     fi
# fi

# # LIB_GOLEM_OLP
# if [ "$1" = "libgolem" ]
# then
#     if [ $LIBGOLEMOLP = 0 ]
#     then
# 	echo "---> Creating dummy $GOSAMDIR/libgolem_olp.so ..."
# 	cd $GOSAMDIR
# 	gfortran -fPIC -DPIC -o golem_olp.o -c golem_olp.f
# 	gfortran -shared -o libgolem_olp.so golem_olp.o
# 	rm -f golem_olp.o
# 	exit
#     else
# 	echo "*********************************************"
# 	echo "Dummy libgolem_olp.so already exists         "
# 	echo "*********************************************"
#     fi
# fi

# # ORDERFILE
# if [ "$1" = "orderfile" ]
# then
#     # call to gosam_flst_born in init_processes.f
#     cd $RUNDIR
#     if grep -q "gosam_flst_born" "$RUNDIR/init_processes.f"
#     then
# 	# do nothing
# 	:
#     else
# 	echo "*********************************************"
# 	echo "A call to gosam_flst_born is missing.        "
# 	echo "Please add a call to gosam_flst_born to the  "
# 	echo "file 'init_processes.f' with the proper      "
# 	echo "powers of alpha_s and alpha and then re-run  "
# 	echo "the script.                                  "
# 	echo "*********************************************"
# 	exit
#     fi
#     if [ $LIBGOLEMOLP = 0 ]
#     then
# 	echo "---> Creating dummy $GOSAMDIR/libgolem_olp.so ..."
# 	cd $GOSAMDIR
# 	gfortran -fPIC -DPIC -o golem_olp.o -c golem_olp.f
# 	gfortran -shared -o libgolem_olp.so golem_olp.o
# 	rm -f golem_olp.o
# 	if [ -f $GOSAMDIR/libgolem_olp.so ]
# 	then
# 	    LIBGOLEMOLP=1
# 	fi	
#     fi
#     if [ $ORDERFILELH = 0 ]
#     then
# 	echo "---> Creating order file ..."
	
# 	cd $RUNDIR
# 	export LD_LIBRARY_PATH=$GOSAMDIR:$LD_LIBRARY_PATH
# 	make 
	
# 	cd $GOSAMDIR
# 	$RUNDIR/pwhg_main
# 	exit
#     else
# 	echo "*********************************************"
# 	echo "Orderfile already exists!                    "
# 	echo "*********************************************"
#     fi
# fi

# GENVIRT
if [ "$1" = "virtual" ]
then
    if [ ! -d $GOSAMDIR ]
    then
	mkdir $GOSAMDIR
    fi 
    
    if [ -f $RUNDIR/MadGraph_POWHEG/my_proc/SubProcesses/orderfile.lh ]
    then
	cp $RUNDIR/MadGraph_POWHEG/my_proc/SubProcesses/orderfile.lh $GOSAMDIR/orderfile.lh
    else
	echo "No orderfile found!!"
	exit
    fi
    
    if [ -f $RUNDIR/gosam.rc ]
    then
	:
    else
	echo "------------------------------------------"
	echo "File 'gosam.rc' not found."
	read -p "Take the one in '../GoSamStuff/gosam.rc'? [Y/n]: " REPLY
	if [ $REPLY = "Y" ]
	then
	    cp $GOSAMSTUFFDIR/Templates/gosam.rc $RUNDIR/gosam.rc
	else
	    echo "Please provide a 'gosam.rc' file in "${PWD}
	    exit
	fi
    fi
    echo "---> GoSam is writing the code for virtual part ..."
    cd $RUNDIR
    gosam.py --olp --mc=powhegbox --config=gosam.rc --ignore-unknown --force --destination=$VIRDIR $GOSAMDIR/orderfile.lh
    cp $RUNDIR/gosam.rc $VIRDIR
    
    cd $VIRDIR
    echo "---> Generating code for virtual amplitudes ..."
    make source
    exit
fi


# NEWFILES
if [ "$1" = "newfiles" ]
then
    if [ ! -f write_pwhg_files.f ]
    then
	cp $GOSAMSTUFFDIR/Templates/write_pwhg_files.f $GOSAMDIR/write_pwhg_files.f
    fi

    echo "---> Generating new files to run the virtual amplitude ..."
    
    cd $GOSAMDIR
    gfortran -o write_pwhg_files write_pwhg_files.f
    ./write_pwhg_files
    
    cd $RUNDIR
    mv $RUNDIR/virtual.f $RUNDIR/virtual.f.dummy
    mv $RUNDIR/init_couplings.f $RUNDIR/init_couplings.f.old
    mv $RUNDIR/init_processes.f $RUNDIR/init_processes.f.old
    cp $GOSAMDIR/virtual_new.f $RUNDIR/virtual.f
    cp $GOSAMDIR/init_couplings_new.f $RUNDIR/init_couplings.f
    #cp $GOSAMDIR/init_processes_new.f $RUNDIR/init_processes.f
    cp $GOSAMSTUFFDIR/Templates/PhysPars.h $RUNDIR/PhysPars.h
    cp $GOSAMSTUFFDIR/Templates/pwhg_gosam.f $RUNDIR/pwhg_gosam.f

    echo "*******************************************************"
    echo "The new files:                                         "
    echo "- virtual.f                                            "
    echo "- init_couplings.f                                     "
    #echo "- init_processes.f                                     "
    echo "were created. The old files were renamed to:           "
    echo "- virtual.f.dummy                                      "
    echo "- init_couplings.f.old                                 "
    #echo "- init_processes.f.old                                 "
    echo "                                                       "
    echo "The new file                                           "
    echo "-PhysPars.h                                            "
    echo "-pwhg_gosam.f                                          "
    echo "was copied from the 'GoSamStuff/Templates' folder      "
    echo "                                                       "
    echo "Please before compile generate a standalone version of "
    echo "the virtual code.                                      "
    echo "*******************************************************"
    exit
fi

# STANDALONE
if [ "$1" = "standalone" ]
then
    MAKEDEPF90=$(command -v makedepf90 | gawk '{ if($1=="") printf "0"; else printf "present"}'); 
    if [ $MAKEDEPF90 = "present" ]
    then
	if [ ! -d $RUNDIR/GoSamlib ]
	then
	    mkdir $RUNDIR/GoSamlib
	fi	    
	for file in `find $GOSAMDIR -path "*.f90"`
	do    
	    nmodules=`grep -i '^ *end *module ' $file | wc -l`  
	    if [ "$nmodules" = "1" ]
		echo -n "."
	    then
		modname=`grep -i '^ *end *module ' $file | sed 's/^ *end *module //' | tr -d ' '`.f90
		\cp $file $GOSAMLIBDIR/$modname
	    elif  [ "$nmodules" = "0" ]
	    then
		:
	    else
		echo $file
		echo 'more than 1 module per file! exiting'
		exit -1
	    fi
	done

        # copy all needed fortran and include files from the contrib directory
        # to the GoSamlib subdirectory
	for file in  `find $CONTRIBDIR -path "*.f.in"`  `find $CONTRIBDIR -path "*.f90.in"`
	do
	    filename="${file##*/}"
	    
	    sed -e 's/\@DATADIR\@/GoSamlib/g' \
		-e 's/\@PACKAGE\@/gosam-contrib/g' \
		-e 's/\@VERSION\@/2.1.1/g' \
		-e 's/\@case_with_lt\@/!AC!/g' \
		-e 's/\@case_with_golem\@/!AC!/g' \
		-e 's/\@case_with_avh\@/    /g' \
		-e 's/\@case_with_ql\@/    /g' \
		-e 's/\@case_wout_lt\@/    /g' \
		-e 's/\@case_wout_golem\@/    /g' \
		-e 's/\@case_wout_avh\@/!AC!/g' \
		-e 's/\@case_wout_ql\@/!AC!/g' \
		-e 's/\@lt_real_kind\@/kind(1.0d0)/g' \
		-e 's/\@fortran_real_kind\@/kind(1.0d0)/g' $file > $GOSAMLIBDIR/"${filename%.*}"
	    
	done

	for file in  `find $CONTRIBDIR -path "*.[hf]"`  `find $CONTRIBDIR -path "*.[hf]90"`
	do
	    \cp $file $GOSAMLIBDIR/"${file##*/}"
	done

        # create the dependencies file
	cd $GOSAMLIBDIR
	
	makedepf90 *.f90 > deps.txt
	
	cat $GOSAMSTUFFDIR/StandAlone/Makefile.nodeps deps.txt > $GOSAMLIBDIR/Makefile.virt.dep
	
	echo "done"
    else
	echo "*******************************************************"	
	echo "The program 'makedepf90' was not found on your machine."
	echo "You need to install it to produce a working standalone "
	echo "version for the virtual amplitude code. A tarball with "
	echo "the code can be found in the folder:                   "
	echo $GOSAMSTUFFDIR/StandAlone
	echo "*******************************************************"
    fi
fi

exit

