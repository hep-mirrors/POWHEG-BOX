#procedure zfix
Id,bra(k1?,+1)*ket(k1?,-1)=0;
Id,bra(k1?,-1)*ket(k1?,+1)=0;
Id,bra(k1?,+1)*ket(k2?,+1)=0;
Id,bra(k1?,-1)*ket(k2?,-1)=0;
.sort
Id,bra(k1?,-1)*ket(k2?,+1)=za(k1,k2);
Id,bra(k1?,+1)*ket(k2?,-1)=zb(k1,k2);
Id,bra(k1?,1)*g_(j?,MU?)*ket(k2?,-1)=0;
Id,bra(k1?,-1)*g_(j?,MU?)*ket(k2?,1)=0;
Id,bra(k1?,1)*g_(j?,MU1?)*g_(j?,MU2?)*ket(k2?,1)=0;
Id,bra(k1?,-1)*g_(j?,MU1?)*g_(j?,MU2?)*ket(k2?,-1)=0;
Id,bra(k1?,-1)*g_(j?,MU1?)*g_(j?,MU2?)*g_(j?,MU3?)*ket(k2?,+1)=0;
Id,bra(k1?,1)*g_(j?,MU1?)*g_(j?,MU2?)*g_(j?,MU3?)*ket(k2?,-1)=0;
Id,bra(k1?,-1)*g_(j?,MU?)*ket(k2?,-1)=zab(k1,MU,k2);
Id,bra(k1?,+1)*g_(j?,MU?)*ket(k2?,+1)=zab(k2,MU,k1);
.sort
Id,bra(k1?,h1?)*g_(j?,k1?)=0;
Id,g_(j?,k1?)*ket(k1?,h1?)=0;
#endprocedure


