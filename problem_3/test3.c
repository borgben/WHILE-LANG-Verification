   Pre(x==x0 & y==y0);
   if (x>y) {
      t = x-y;
      while (t>0) {
         Inv( t==x-y0 & x+y==x0+y0 & t>=0);
         x=x-1; y=y+1;
         t=t-1;
      }
   }else{

   }
   Post( !(x0>y0) | (y==x0 & x==y0) );
