#/bin/sh
cat ../btilde.f | sed -e "s/include '/include '..\//g;
s/         call allborn/c set the mt dependency in Born cross section\n\
         call setbornmassdep\n\
c evaluate Born cross section with the chosen top mass\n\
         call allborn/;
s/         call btildeborn(resborn)/         call btildeborn(resborn)\n\
c re-evaluate Born amplitudes in large top mass limit, if needed\n\
         call setbornmass2inf/" >  btilde.f