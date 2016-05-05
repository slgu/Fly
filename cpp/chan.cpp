#include <iostream>
#include <thread>
#include <condition_variable>
#include <queue>
#include <mutex>
#include <vector>
using namespace std;
//TODO implement a blocking queue template
template <typename T> class Chan {
private:
    mutable std::mutex mut;
    std::queue <std::shared_ptr <T>> data_queue;
    std::condition_variable data_cond;
public:
    Chan(){

    }
    std::shared_ptr <T> wait_and_pop() {
        std::unique_lock <std::mutex> lk(mut);
        data_cond.wait(lk, [this]{return !data_queue.empty();});
        std::shared_ptr <T> res = data_queue.front();
        data_queue.pop();
        return res;
    }
    void push(std::shared_ptr <T> tmp) {
        std::lock_guard <std::mutex> lk(mut);
        data_queue.push(tmp);
        data_cond.notify_one();
    }
};
int main() {
    Chan<int> a;
    std::shared_ptr <int> b(new int(3));
    a.push(b);
    Chan < vector <int> > c;
    return 0;
}
