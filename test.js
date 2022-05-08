function fib(x) {
  if (x<3) {
      return 1
  } else {
      return fib(x-1)+fib(x-2)
  }
}
console.log(fib(40))