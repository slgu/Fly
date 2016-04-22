import os
import sys

test_dir = 'test'
test_answer_dir = 'test_answer'
fly_results = 'fly_results.txt'
comma = ', '

command1 = 'make'
command2 = './Fly < test/'
command3 = ' > test.cpp'
command4 = 'g++ -std=c++11 test.cpp'
command5 = './a.out > ' + fly_results #put the results from running fly code into 'fly_result.txt'

test_list = os.listdir(test_dir)
if len(sys.argv) > 1:
	test_list = sys.argv[1:]
#print comma.join(test_list)

for filename in test_list:
	print filename
	os.system(command1)
	os.system(command2 + filename + command3)
	os.system(command4)
	os.system(command5)

	file_fly_results = open(fly_results, 'r')
	list_fly_results = file_fly_results.read().split()

	filename_answer_results = test_answer_dir + '/' + filename
	file_answer_results = open(filename_answer_results, 'r')
	list_answer_results = file_answer_results.read().split()

	if(list_fly_results == list_answer_results):
		print filename, ': correct result'
	else:
		print filename, ': incorrect result'
		print '\nfly_results: \n', comma.join(list_fly_results)
		print '\nanswer: \n', comma.join(list_answer_results)
	print '\n'

os.remove('test.cpp')
os.remove(fly_results)








