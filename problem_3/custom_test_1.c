// sample file
Pre( y==y0 & k==k0 & t==y0-k0 ); 
k=y-k0;
y=k0+1;
t=t-1;
Post( y <= k0 );
