void extgcd(ll a, ll b, ll& x, ll& y)
{
  if (b == 0){
    x = 1;
    y = 0;
    return;
  }

  ll s, t;
  extgcd(b, a % b, s, t);
  x = t;
  y = s - a / b * t;
}
