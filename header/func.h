bool str_is_int(const string & s)
{
   if(s.empty() || ((!isdigit(s[0])) && (s[0] != '-') && (s[0] != '+'))) return false ;

   char * p ;
   strtol(s.c_str(), &p, 10) ;

   return (*p == 0) ;
}

int len(string a) {
    return a.length();
}

void print_bool(bool a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(int a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(size_t a) {
    print((int)a);
}
void print(string a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(float a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
float _float(int a){
	return float(a);
}
float _float(string a) {
	return stof(a);
}

int _int(string a){
    char * p;
    return strtol(a.c_str(), &p, 10);
}

string _string(int a ){
	return to_string(a);
}
string _string(float a ){
	return to_string(a);
}
string _string(string a) {
    return a;
}
void _sleep(int seconds){
	std::chrono::seconds duration(seconds);
	std::this_thread::sleep_for(duration);
}

void _exit(int exit_code){
	exit(exit_code);
}
