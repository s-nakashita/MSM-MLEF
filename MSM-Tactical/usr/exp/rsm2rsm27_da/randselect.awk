function randint(n) {
  return int(n * rand()) + 1
}

function choose(x, k, n, i) {
  for (i=1;n>0;i++) {
    if (rand() < k/n--) {
      print x[i]; k--
    }
  }
}

{
  srand()
  x[NR]=$0
}END{ choose(x, 10, 72, 1) }
