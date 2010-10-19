#/bin/sh
# add a warning at the beginning of the file
echo "c     !: This should be as the sigreal.f file in the main directory. The only
c     !: difference is the presence here of the genrad variable and the
c     !: corresponding common block." > pippo.f

# change all the include statements
cat ../sigreal.f | sed -e "s/include '/include '..\//g" >> pippo.f

# add genrad variable and correpsponding common blocks in proper places
sed '/subroutine sigreal_rad(/,/subroutine sigreal_btl(/s/call realgr(/genrad=.true. !:\n\t\t call realgr(/g' pippo.f > pippo2.f; mv pippo2.f pippo.f

sed '/subroutine sigreal_rad(/,/include/s/implicit none/implicit none\n      logical genrad !:\
      common\/cgenrad\/genrad !:/g' pippo.f > pippo2.f; mv pippo2.f pippo.f

sed '/subroutine sigreal_btl0(/,/subroutine fillmomenta(/s/call realgr(/genrad=.false. !:\n\t\t call realgr(/g' pippo.f > pippo2.f; mv pippo2.f pippo.f

sed '/subroutine sigreal_btl0(/,/include/s/implicit none/implicit none\n      logical genrad !:\
      common\/cgenrad\/genrad !:/g' pippo.f > pippo2.f; mv pippo2.f pippo.f

# rename file
mv pippo.f sigreal.f

# delete (subsitute with null) every occurrence of abc 
# between lines containing aaa and xxx
# 2 positional expression separated by comma
# sed '/aaa/,/xxx/s/abc//g' file > file.mod
