// sample file
Pre( (A[x]==x0) & (A[y]==y0)); 
t    = 1;
A[x] = A[y];
A[y] = t;
Post( (A[x]==y0) & (A[y]==x0));