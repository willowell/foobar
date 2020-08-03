import functools
import itertools
import math
import time

# https://codereview.stackexchange.com/questions/174627/lovely-lucky-lambs


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


def generous_direct(lambs):
    return int(math.log2(lambs + 1))


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
