// sample file
   Pre( y==y0 & k==k0 & t==y0-k0 ); 

   while(t>0){
		Inv( t==y-k0 & k==k0 );
		y=y-1;
		t=t-1;
	 }

   Post( y <= k0 );
