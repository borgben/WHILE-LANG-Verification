Pre(\x.((x>=1 & x<=n)=>(R[x]==R_[x])));
j = 1;
while(2*j <= n){
    Inv(2*j <= n+1);
    t    = R[j];
    idx  = n+1-j;
    R[j] = R[idx];
    R[idx] = t;
    j = j+1;
}
Post(\x.((x>=1) & (x<=n)) => (R[x]==R_[n+1-x]));