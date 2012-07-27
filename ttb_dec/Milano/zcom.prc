#procedure zcom
Antisymmetrize,za,1,2;
Antisymmetrize,zb,1,2;
Antisymmetrize,iza,1,2;
Antisymmetrize,izb,1,2;
Id,za(k1?,k2?)*iza(k1?,k2?)=1;
Id,zb(k1?,k2?)*izb(k1?,k2?)=1;
.sort
#endprocedure
