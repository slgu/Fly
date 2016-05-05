import os
import sys

ok_cnt = 0
fail_cnt = 0

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def bunnytesting_fail(test_dir, ans_dir, test_list):
    print bcolors.HEADER + 'Testing failure cases' + bcolors.ENDC
    global ok_cnt
    global fail_cnt
    counter = 0
    for filename in test_list:
        counter = counter + 1
        print "Test " + str(counter) + ": " + filename
        os.system('rm -f tmp.cc')
        cmd_flyc = './fly < ' + test_dir + '/' + filename + ' &> result.txt'
        os.system(cmd_flyc);
        if os.path.isfile('tmp.cc'):
            print bcolors.FAIL + 'No Error ' + cmd_flyc + bcolors.ENDC
            fail_cnt += 1
            continue
        result = open('result.txt', 'r').read()
        ans_file = ans_dir + '/' + filename + '.txt'
        if not os.path.isfile(ans_file):
            print bcolors.FAIL + 'Answer file not found: ' + ans_file + bcolors.ENDC
            fail_cnt += 1
            continue
        ans = open(ans_dir + '/' + filename + '.txt','r').read()
        if result == ans:
            print bcolors.OKGREEN + 'OK' + bcolors.ENDC
            ok_cnt += 1
        else:
            print bcolors.FAIL + 'FAIL: wrong answer' + bcolors.ENDC
            fail_cnt += 1
            continue

def bunnytesting(test_dir, ans_dir, test_list):
    print bcolors.HEADER + 'Testing success cases' + bcolors.ENDC
    global ok_cnt
    global fail_cnt
    counter = 0
    for filename in test_list:
        counter = counter + 1
        print "Test " + str(counter) + ": " + filename
        os.system('rm -f tmp.cc')
        cmd_flyc = './fly < ' + test_dir + '/' + filename + ' > /dev/null 2>&1'
        os.system(cmd_flyc);
        if not os.path.isfile('tmp.cc'):
            print bcolors.FAIL + 'Compile failure: ' + cmd_flyc + bcolors.ENDC
            fail_cnt += 1
            continue
        cmd_gpp = 'rm -f test_build; g++ -pthread -o test_build -std=c++11 tmp.cc > /dev/null 2>&1'
        os.system(cmd_gpp)
        if not os.path.isfile('test_build'):
            print bcolors.FAIL + 'Compile failure: ' + cmd_gpp + bcolors.ENDC
            fail_cnt += 1
            continue
        os.system('./test_build > result.txt');
        result = open('result.txt', 'r').read()
        ans_file = ans_dir + '/' + filename + '.txt'
        if not os.path.isfile(ans_file):
            print bcolors.FAIL + 'Answer file not found: ' + ans_file + bcolors.ENDC
            fail_cnt += 1
            continue
        ans = open(ans_dir + '/' + filename + '.txt','r').read()
        if result == ans:
            print bcolors.OKGREEN + 'OK' + bcolors.ENDC
            ok_cnt += 1
        else:
            print bcolors.FAIL + 'FAIL: wrong answer' + bcolors.ENDC
            fail_cnt += 1
            continue

if __name__ == "__main__":
    test_success_dir = 'success'
    test_success_answer_dir = 'success_ans'
    test_fail_dir = 'fail'
    test_fail_answer_dir = 'fail_ans'
    test_success_list = os.listdir(test_success_dir)
    test_fail_list = os.listdir(test_fail_dir)

    print "Compiling fly......"
    os.system('rm -f fly')
    os.system('cd ..; make clean > /dev/null 2>&1; make  > /dev/null 2>&1  && cd - > /dev/null 2>&1 && cp ../fly ./')
    if os.path.isfile('fly'):
        print "Compile Done"
    else:
        print "Compile Failed"
        sys.exit(0)

    bunnytesting_fail(test_fail_dir, test_fail_answer_dir, test_fail_list)
    bunnytesting(test_success_dir, test_success_answer_dir, test_success_list)

    print bcolors.HEADER + "Done testing!" + bcolors.ENDC
    print bcolors.OKGREEN + 'Passed: ' + str(ok_cnt) + bcolors.ENDC
    print bcolors.FAIL + 'Failed: ' + str(fail_cnt) + bcolors.ENDC
