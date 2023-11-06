Pre( !(_i < n) |  A[_i] == 0 );
i = 0; 
while(i < n){
Inv( i<=n & (!(_i < i) | A[_i] == _i) );
A[i] = i;
}      
Post( !(_i < n) |  A[_i] == _i );
