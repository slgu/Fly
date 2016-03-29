#include <cstdio>
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>

template <typename T> class Signal {
public:
    std::condition_variable data_cond;
    std::mutex data_mutex;
    std::queue <std::shared_ptr <T>> data_queue;
    std::shared_ptr <T> wait() {
        std::unique_lock<std::mutex> lk(data_mutex);
        data_cond.wait(lk, [this]{return !this->data_queue.empty();});
        lk.unlock();
        auto result = data_queue.front();
        return result;
    }
    void notify(std::shared_ptr <T> res) {
        std::lock_guard<std::mutex> lk(data_mutex);
        data_queue.push(res);
        data_cond.notify_one();
    }
};

int add(int a, int b) {
    return a + b;
}

int wrapper(int a, int b, std::shared_ptr <Signal<int>> s) {
    int result = add(a, b);
    std::cout  << result << std::endl;
    std::shared_ptr <int> shared_result(new int(result));
    //add to result queue signal
    s->notify(shared_result);
    return result;
}

void print_out(int a) {
    printf("%d\n", a);
}

void register_wrapper(std::shared_ptr <Signal<int>> s) {
    auto result = s->wait();
    print_out(*result);
}


int main() {
    //create a signal
    std::shared_ptr <Signal<int>> s(new Signal<int>());
    std::thread t(wrapper, 3, 4, s);
    t.join();
    //regiter this signal for another function
    for (int i = 0; i < 10; ++i) {
        std::thread w(register_wrapper, s);
        w.join();
    }
    return 0;
}
