

class Racional {
  private int n,d;
  Racional(int n, int d) {
    this.n = n;
    this.d = d;
  }

  Racional somar(Racional q) {
    int
      n = this.n * q.d + q.n * this.d,
      d =  this.d * q.d;
    return new Racional(n,d);
  }

  public String toString() {
    return n + "/" + d;
  }

}//
