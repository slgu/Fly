#include <iostream>
#include <algorithm>
#include <future>
#include <list>
//parallel implementation of quicksort
template <typename T>
std::list <T> para_quick_sort(std::list <T> input) {
    if (input.empty()) {
        return input;
    }
    std::list <T> result;
    //transfer the first element
    result.splice(result.begin(), input, input.begin());
    T const& pivot=*result.begin();
    auto divide_point = std::partition(input.begin(), input.end(), [&](T const & t) {return t < pivot;});
    //same like Fly caputure value or caputure referrence
    std::list <T> lower_part;
    lower_part.splice(lower_part.end(),input, input.begin(), divide_point);
    std::future <std::list <T>> new_lower = std::async(&para_quick_sort<T>, std::move(lower_part));
    auto new_higher = para_quick_sort(std::move(input));
    result.splice(result.end(), new_higher);
    result.splice(result.begin(), new_lower.get());
    /*
    auto new_lower = para_quick_sort(std::move(lower_part));
    auto new_higher = para_quick_sort(std::move(input));
    result.splice(result.end(), new_higher);
    result.splice(result.begin(), new_lower);
    */
    return result;
}

int main() {
    std::list <int> input;
    for (int i = 10000; i > 0; --i)
        input.push_back(i);
    auto res = para_quick_sort(input);
    puts("done");
    return 0;
}
