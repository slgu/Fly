import os

test_dir = 'test'
cpp_results_dir = 'cpp_test_results'
fly_results = 'fly_results.txt'
##cpp_results = 'cpp_results.txt'

command1 = 'make'
command2 = './Fly < test/'
command3 = ' > test.cpp'
command4 = 'g++ test.cpp'
command5 = './a.out > ' + fly_results #put the results from running fly code into 'fly_result.txt'

for filename in os.listdir(test_dir):
	print filename
	os.system(command1)
	os.system(command2 + filename + command3)
	os.system(command4)
	os.system(command5)

	file_fly_results = open(fly_results, 'r')
	list_fly_results = file_fly_results.read().split()
	print list_fly_results

	list_cpp_results = open()......???????

	if(list_fly_results == list_cpp_results):
		print filename, 'correct result'
	else:
		print filename, 'incorrect result'
	print ''








