import os
import sys

test_success_dir = 'success'
test_fail_dir = 'fail'
test_answer_dir = 'success_ans'
fly_results = 'fly_results.txt'
comma = ', '
test_success_list = os.listdir(test_success_dir)
test_fail_list = os.listdir(test_fail_dir)

#command1 = 'make'
#command2 = './Fly < sucess/'
#command3 = ' > test.cpp'
command4 = 'g++ -std=c++11 test.cpp'
command5 = './a.out > ' + fly_results #put the results from running fly code into 'fly_result.txt'
#if len(sys.argv) > 1:
#	test_list = sys.argv[1:]
#print comma.join(test_list)

#test each file in the given directory
for filename in test_list:
	bunnytesting(filename, test_success_dir)

	os.remove('test.cpp')
	os.remove(fly_results)
	os.remove('a.out')
print "Done testing!"

def bunnytesting(filename, test_dir):
	print "testing file: " + filename + '/n'
	os.system('make')
	os.system('./Fly < ' + test_dir + '/' + filename + ' > test.cpp')

	#check if there is an a.out file
	if not ( 'a.out' in os.listdir(test_dir) )
		print filename + ': fail' + '/n'
	else
		os.system(command4)
		os.system(command5)

		file_fly_results = open(fly_results, 'r')
		list_fly_results = file_fly_results.read().split()

		filename_answer_results = test_answer_dir + '/' + filename
		file_answer_results = open(filename_answer_results, 'r')
		list_answer_results = file_answer_results.read().split()

		if(list_fly_results == list_answer_results):
			print filename, ': sucess. Correct result'
		else:
			print filename, ': incorrect result'
			print '\nfly_results: \n', comma.join(list_fly_results)
			print '\nanswer: ', comma.join(list_answer_results)
		print '\n\n'
