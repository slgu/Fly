#include <cstdio>
#include <iostream>
#include <thread>
#include <mutex>
class Signal {

}
int add(int a, int b) {
    return a + b;
}

int wrapper(int a, int b) {
    int result = add(a, b);
    std::cout  << result << std::endl;
    return result;
    //add to result queue signal
}


int main() {
    //create a signal
    std::thread t(wrapper, 3, 4);
    t.join();
    return 0;
}
