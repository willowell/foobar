import functools
import itertools
import math
import time

# I first implemented my solution in Haskell because I have been learning Haskell
# and having a lot of fun with it, and I have found mathematical problems like this one
# to be easier to solve in Haskell than in an imperative style in another language.
# I then translated my solution into Python and made a few adjustments to it,
# such as adding memoization via @functools.lru_cache
# and accounting for negative input since Python does not have an unsigned integer type
# by default. My solution in Haskell uses Numeric.Natural to ensure the functions will
# never receive a negative value.

# Stores the settings for the LRU cache, which is also the limit of the range
# and the number of times solution() should be run to test its execution performance.
config = dict(cache_size=4096, times=100)


# A number is a perfect square if it is equal to
# the product of its square root with its square root.
def is_square(num):
    if num < 0:
        raise ValueError("A negative number cannot be passed to math.sqrt()!")

    sq = math.floor(math.sqrt(num))

    return sq * sq == num


# Finds the largest square in a number.
# If the number is already a perfect square,
# then return just that number.
# Otherwise, find which number in [1, num], when squared,
# is the largest square less than num.
def largest_square(num):
    if is_square(num):
        return num
    else:
        # maximum $ takeWhile (< n) $ map (^2) [1..n]
        return max(
            itertools.takewhile(lambda x: x < num,
                                map(lambda x: x ** 2,
                                    range(1, num + 1))))


# Recursively builds a list of the largest squares
@functools.lru_cache(maxsize=config["cache_size"])
def largest_squares(num):
    # Translation of:
    # go' n
    #     | n == 0 = []
    #     | n == 1 = [1]
    #     | otherwise =
    #         let m = largestSquare n
    #         in m : go' (n - m)

    # if we have zero left, then we are done
    if num == 0:
        return []
    # short circuit for 1 left
    elif num == 1:
        return [1]
    # short circuit for perfect squares
    elif is_square(num):
        return [num]
    # otherwise, find the largest square.
    else:
        m = largest_square(num)
        return [m] + largest_squares(num - m)


def solution(area):
    if area not in range(1, config["cache_size"] + 1):
        raise ValueError("Invalid value given!")

    return largest_squares(area)


def domain_test():
    print("Start:")
    try:
        solution(-9)
    except ValueError:
        print("OK, solution does not work for negative numbers.")

    try:
        solution(9000000)
    except ValueError:
        print("OK, solution does not work for very large numbers.")

    try:
        solution(354)
    except ValueError:
        print("Should not appear!")

    print("Done.")


def execution_time_test(test_number=0):
    start_time = time.time()

    for i in range(1, config["cache_size"] + 1):
        solution(i)

    end_time = time.time() - start_time

    cache_info = largest_squares.cache_info()

    summary_str = f'Execution #{test_number}\n' \
                  f'Execution took {end_time} seconds\n' \
                  f'{cache_info}'

    print(bordered(summary_str))

    return end_time


# Yes, this is from Stack Overflow... https://stackoverflow.com/a/20757491
def bordered(text):
    lines = text.splitlines()
    width = max(len(s) for s in lines)
    res = ['┌' + '─' * width + '┐']

    for s in lines:
        res.append('│' + (s + ' ' * width)[:width] + '│')

    res.append('└' + '─' * width + '┘')

    return '\n'.join(res)


def main():
    domain_test()

    acc = 0
    for i in range(1, config["times"] + 1):
        acc += execution_time_test(i)

    cache_info = largest_squares.cache_info()

    summary_str = f'Execution Summary\n' \
                  f'LRU Cache Hits: {cache_info.hits}\n' \
                  f'LRU Cache Misses: {cache_info.misses}\n' \
                  f'LRU Cache Maximum Size: {cache_info.maxsize}\n' \
                  f'LRU Cache Current Size: {cache_info.currsize} \n' \
                  f'Average execution time: {acc / config["times"]} seconds\n'
    print(bordered(summary_str))


if __name__ == "__main__":
    main()
