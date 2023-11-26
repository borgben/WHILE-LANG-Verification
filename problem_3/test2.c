Pre(a>=0);
sum=0;
m=a;
  while (m>=1) {
    Inv( m+m+sum == a+a & a>=0);
    count = 0;
    while (count<2) {
      Inv( m+m+sum==a+a+count & m>=1 & count<=2 & a>=0);
      sum = sum + 1; 
      count = count + 1;
    }
    m = m - 1;
  }
Post(sum==a+a);
