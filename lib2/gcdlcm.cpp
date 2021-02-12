long long gcd(long long a, long long b)
{
  long long x = abs(a);
  long long y = abs(b);
  while (y > 0){
    long long r = x % y;
    x = y;
    y = r;
  }
  return x;
}

long long lcm(long long x, long long y)
{
  if (x * y == 0) return 0;
  else return x * y / gcd(x, y);
}


