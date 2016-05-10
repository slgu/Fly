int ___argc;
char ** ___argv;
#include <string>
std::string get_argv(int i) {
    if (i <= ___argc) {
        return ___argv[i];
    }
    else {
        return "";
    }
}
