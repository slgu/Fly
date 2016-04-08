#include <iostream>
#include <string>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
using namespace std;
template <typename T> class Signal {
public:
   condition_variable data_cond;
   mutex data_mutex;
   queue <std::shared_ptr <T>> data_queue;
   shared_ptr <T> wait() {
       std::unique_lock<std::mutex> lk(data_mutex);
       data_cond.wait(lk, [this]{return !this->data_queue.empty();});
       lk.unlock();
       auto result = data_queue.front();
       data_queue.pop();
       return result;
   }
   void notify(std::shared_ptr <T> res) {
       std::lock_guard<std::mutex> lk(data_mutex);
       data_queue.push(res);
       data_cond.notify_one();
   }
};
int main () 
{
int a =  3 ; 
int b =  4 ; 
cout << ( a + b ) <<endl  ; 
for ( int i =  1 ; ( i < 10 ) ;  i =  ( i + 1 ) ) 
{
cout << i <<endl  ; 
}
return 0 ; 
}
