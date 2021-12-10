import subprocess
import sys


def run_train(times):
    for i in range(times):
        print(f"Runing {i} times")
        subprocess.run("swipl -f train.pl", shell=True)


def main():
    args = sys.argv
    run_train(int(args[1]))


main()
