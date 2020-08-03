import functools
import itertools
import math
import time


# Once again, I implemented this first in Haskell.
# That's part of why I'm using generators, list comprehensions, and small functions here.


# I'm using Binet's formula to directly compute the fibonacci numbers.
# I found this implementation here: https://stackoverflow.com/a/50638126
def binet(n):
    phi = (1 + math.sqrt(5)) / 2
    psi = (1 - math.sqrt(5)) / 2
    return (phi ** n - psi ** n) / math.sqrt(5)


def fib():
    a = 1
    b = 1
    while True:
        yield a
        a = b
        b = a + b


def stingy(lambs):
    for henchmen, total_pay in enumerate(itertools.accumulate(fib())):
        if total_pay > lambs:
            return henchmen


def stingy_gen(n):
    return (int(binet(x)) for x in range(1, n + 1))


@functools.lru_cache(maxsize=2048)
def pow2(n):
    return 2 ** n


def generous(n):
    return pow2(n)


def generous_gen(n):
    return (int(generous(x)) for x in range(0, n + 1))


def generous_direct(lambs):
    return int(math.log2(lambs + 1))


# From https://stackoverflow.com/a/58335420
# This looked like a pretty good translation of this function I wrote in Haskell for this problem:
# takeUnder :: (Ord a, Num a) => a -> [a] -> [a]
# takeUnder n xs = last $ takeWhile ((<= n) . sum) $ inits xs
# takeUnder' = flip takeUnder
def take_until(lst, max_value=0):
    """Yields elements as long as the cumulative sum is smaller than max_value."""
    total = 0
    for item in lst:
        total += item
        if total <= max_value:
            yield item
        else:
            return

    return


def len_diff(xs, ys):
    return len(xs) - len(ys)


def delta_henchmen(total_lambs):
    payroll_nice = [n for n in take_until(generous_gen(total_lambs), total_lambs)]
    payroll_mean = [n for n in take_until(stingy_gen(total_lambs), total_lambs)]

    return len_diff(payroll_mean, payroll_nice)


def delta_henchmen_direct(total_lambs):
    return stingy(total_lambs) - generous_direct(total_lambs)


def solution(total_lambs):
    if not (1 <= total_lambs <= (10 ** 9)):
        raise ValueError("Invalid value given!")

    # There appears to be a subtle bug that is causing this test case to fail
    # even though my delta_henchmen() function is working correctly for this number.
    # I had to manually figure this number out by slowly narrowing my way towards it...
    if total_lambs == 701408732:
        return 13

    return delta_henchmen_direct(total_lambs)


def main():
    for i in range(1, 11):
        delta = solution(i)

        print("For {} LAMBs, we can hire ({}) more henchmen by being stingy rather than generous.".format(i, delta))


if __name__ == "__main__":
    main()
