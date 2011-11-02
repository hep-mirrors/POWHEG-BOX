#/bin/sh
cat ../btilde.f | perl -pe 's/         call allborn/c set the mt dependency in Born cross section\n\
         call setbornmassdep\n\
c evaluate Born cross section with the chosen top mass\n\
         call allborn/;
s/         call btildeborn\(resborn\)/         call btildeborn\(resborn\)\n\
c re-evaluate Born amplitudes in large top mass limit, if needed\n\
         call setbornmass2inf/;
s/www=www0\*hc2/real \*8 finitemtcorr\n\
      external finitemtcorr\n\
      www=www0\*hc2*finitemtcorr\(\)/gi' >  btilde_ggH.f

mv btilde_ggH.f tmpfile
echo "c###################################################### " >btilde_ggH.f  
echo "c# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT!" >>btilde_ggH.f
echo "c###################################################### " >>btilde_ggH.f
cat tmpfile >> btilde_ggH.f
rm tmpfile