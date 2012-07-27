#procedure polinsert1
#+ 
Id,Ub(pt,-1)=bra(q4,+1)*(g_(j,pt)+mt);
Id,Ub(pt,+1)=bra(q4,-1)*(g_(j,pt)+mt);
Id,V(ptb,-1)=(g_(j,ptb)-mt)*ket(q7,-1);
Id,V(ptb,+1)=(g_(j,ptb)-mt)*ket(q7,+1);


Id,e1p.px?=bra(p1,+1)*g_(j,px)*ket(p2,+1)*rt2/2*iza(p2,p1);
Id,e1m.px?=bra(p1,-1)*g_(j,px)*ket(p2,-1)*rt2/2*izb(p1,p2);
Id,e2p.px?=bra(p2,+1)*g_(j,px)*ket(p1,+1)*rt2/2*iza(p1,p2);
Id,e2m.px?=bra(p2,-1)*g_(j,px)*ket(p1,-1)*rt2/2*izb(p2,p1);
Id,g_(j?,e1p)=rt2*iza(p2,p1)*(+ket(p1,-1)*bra(p2,-1)+ket(p2,+1)*bra(p1,+1));
Id,g_(j?,e1m)=rt2*izb(p1,p2)*(+ket(p1,+1)*bra(p2,+1)+ket(p2,-1)*bra(p1,-1));
Id,g_(j?,e2p)=rt2*iza(p1,p2)*(+ket(p2,-1)*bra(p1,-1)+ket(p1,+1)*bra(p2,+1));
Id,g_(j?,e2m)=rt2*izb(p2,p1)*(+ket(p2,+1)*bra(p1,+1)+ket(p1,-1)*bra(p2,-1));
Id,rt2^2=2;
#endprocedure


