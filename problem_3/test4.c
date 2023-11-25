// sample file
Pre( (A[x]==x0) & (A[y]==y0)); 
x    = 5;
t    = A[x];
A[x] = A[y];
A[y] = t;
Post( \x.\y.((A[x]==y0) & (A[y]==x0)));
