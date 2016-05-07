
    #include <sstream>
    #include <iostream>
    #include <iterator>
    #include <string>
    #include <string.h>     /* for memset() */
    #include <thread>
    #include <vector>
    #include <map>
    #include <mutex>
    #include <condition_variable>
    #include <queue>
    #include <sys/socket.h>
    #include <arpa/inet.h>  /* for sockaddr_in and inet_ntoa() */
    #include <stdlib.h>     /* for atoi() and exit() */
    #include <unistd.h>     /* for close() */
    #include <signal.h>     /* for signal() */

    using namespace std;


bool str_is_int(const string & s)
{
   if(s.empty() || ((!isdigit(s[0])) && (s[0] != '-') && (s[0] != '+'))) return false ;

   char * p ;
   strtol(s.c_str(), &p, 10) ;

   return (*p == 0) ;
}

shared_ptr<vector<string>> str_split(string s) {
    stringstream ss(s);
    istream_iterator<string> begin(ss);
    istream_iterator<string> end;
    return shared_ptr<vector<string>>( new vector<string>(begin, end));
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



template <typename T> class Signal {
    public:
        condition_variable data_cond;
        mutex data_mutex;
        queue <std::shared_ptr <T>> data_queue;
        shared_ptr <T> wait() {
            std::unique_lock<std::mutex> lk(data_mutex);
            data_cond.wait(lk, [this]{return !this->data_queue.empty();});
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

template<class K, class V>
class flymap
{
    std::map<K,V> m;
public:
    std::recursive_mutex m_mutex;

    int size() {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m.size();
    }

    V& operator[] (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m[k];
    }

    void erase (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        m.erase(k);
    }

    typename std::map<K,V>::iterator find (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m.find(k);
    }

    typename std::map<K,V>::iterator end (void) {
        return m.end();
    }

    typename std::map<K,V>::iterator begin (void) {
        return m.begin();
    }

    void insert (const K& k, const V& v) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        m[k] = v;
    }
};

class connection {
private:
    int c_sock = -1;
    FILE *c_fp = NULL;
    bool alive;
public:
    connection(int c, bool al): c_sock(c), alive(al) {};
    string recv();
    bool send(string s);
    void close();
    bool is_alive();
};

bool connection::is_alive() {
    return alive;
}

bool connection::send(string msg) {
    if (c_sock < 0) {
        cout << "connection::send wrong socket " << c_sock << endl;
        alive = false;
        return false;
    }

    msg += "\n";

    int len = msg.length();
    if (::send(c_sock, msg.c_str(), len, 0) != len) {
        cout << "connection::send fail " << endl;
        alive = false;
        return false;
    }

    return true;
}

string connection::recv() {

    string rmsg;
    char requestLine[1024] = {0};

    if (!c_fp) {
        if (c_sock < 0) {
            cout << "connection::recv wrong socket " << c_sock << endl;
            return rmsg;
        }
        c_fp = fdopen(c_sock, "r");
    }

    if (c_fp == NULL) {
        alive = false;
        return rmsg;
    }

    if (!fgets(requestLine, sizeof(requestLine), c_fp)) {
        alive = false;
        return rmsg;
    }

    rmsg = string(requestLine);
    rmsg.pop_back();

    return rmsg;
}

void connection::close() {
    fclose(c_fp);
    c_fp = NULL;
    c_sock = -1;
    alive = false;
}

class server {
private:
    int create_server_socket(unsigned short port);
    int serv_sock = 0;
public:
    void listen(int port);
    shared_ptr<connection> accept(void);
};

int server::create_server_socket(unsigned short port)
{
    int servSock;
    struct sockaddr_in servAddr;

    /* Create socket for incoming connections */
    if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        cout << "socket() failed" << endl;
        exit(1);
    }

    /* Construct local address structure */
    memset(&servAddr, 0, sizeof(servAddr));       /* Zero out structure */
    servAddr.sin_family = AF_INET;                /* Internet address family */
    servAddr.sin_addr.s_addr = htonl(INADDR_ANY); /* Any incoming interface */
    servAddr.sin_port = htons(port);              /* Local port */

    /* Bind to the local address */
    if (bind(servSock, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
        cout << "bind() failed" << endl;
        exit(1);
    }

    /* Mark the socket so it will listen for incoming connections */
    if (::listen(servSock, 5) < 0) {
        cout << "listen() failed" << endl;
    }

    return servSock;
}

void server::listen(int port) {
    signal(SIGPIPE, SIG_IGN);
    serv_sock = create_server_socket(port);
}

shared_ptr<connection> server::accept(void) {

    struct sockaddr_in clntAddr;
    unsigned int clntLen = sizeof(clntAddr);

    int c_sock = ::accept(serv_sock, (struct sockaddr *)&clntAddr, &clntLen);

    return shared_ptr<connection>(new connection(c_sock, true));
}

class client {
public:
    shared_ptr<connection> connect(string server_ip, int port) {

        int sockfd;
        struct sockaddr_in serv_addr;

        shared_ptr<connection> fail(new connection(sockfd, false));

        if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        {
            printf("Error : Could not create socket ");
            return fail;
        }

        memset(&serv_addr, '0', sizeof(serv_addr));
        serv_addr.sin_family = AF_INET;
        serv_addr.sin_port = htons((short)port);

        if (inet_pton(AF_INET, server_ip.c_str(), &serv_addr.sin_addr)<=0)
        {
            printf(" inet_pton error occured");
            return fail;
        }

        if ( ::connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
        {
            printf(" Error : Connect Failed ");
            return fail;
        }

        return shared_ptr<connection> (new connection(sockfd, true));
    }
};



int main () ;
void send_back ( shared_ptr <connection> con, string msg) ;
void handle_req ( shared_ptr <connection> con) ;
string process ( string msg) ;
void
handle_req_class_connection_signal_void
( shared_ptr <connection> con, shared_ptr <Signal<void>> handle_req_class_connection_signal_void_sig)
{
handle_req(con);
}
void
process_string_signal_string
( string msg, shared_ptr <Signal<string>> process_string_signal_string_sig)
{
string process_string_signal_string_var = process(msg);
process_string_signal_string_sig->notify(shared_ptr<string>(new string(process_string_signal_string_var)));
}
void
send_back_class_connection_signal_string
( shared_ptr <connection> con, shared_ptr <Signal<string>> send_back_sig)
{
string msg = *send_back_sig->wait();
send_back(con,msg);
}
string process ( string msg) 
{
return ( "Done: " + msg )  ; 
}
void send_back ( shared_ptr <connection> con, string msg) 
{
con->send ( msg )  ; 
}

const char split_var = '\x02';
const char split_type = '\x01';
vector <string> split(string str, char split_c) {
    int l = int(str.length());
    int last = -1;
    vector <string> res;
    for (int i = 0; i <= l; ++i) {
        if (i == l || str[i] == split_c) {
            if(i - last -1 > 0) {
                res.push_back(str.substr(last+1, i - last - 1));
            }
            last = i;
        }
    }
    return res;
}
void exec(string str) {
    vector <string> func_and_param = split(str, split_var);
    vector <string> func = split(func_and_param[0], split_type);
    string main_func = "int main(){";
    int idx = 0;
    int l = func_and_param.size();
    vector <string> assigns;
    for (int i = 1; i < l; ++i) {
        vector <string> type_and_content = split(func_and_param[i], split_type);
        if(type_and_content[0] == "int") {
            string tmp = "int ";
            tmp += idx + 'a';
            tmp += " = ";
            tmp += type_and_content[1];
            tmp += ";";
            assigns.push_back(tmp);
        }
        else {
        }
    }
    std::cout << func[1] << std::endl;
    std::cout << main_func << std::endl;
    for (int i = 0; i < assigns.size(); ++i)
        std::cout << "\t" << assigns[i] << std::endl;
    //exec
    std::cout << func[0] << "(";
    for (int i = 1; i < l; ++i) {
        if(i == 1) {
            std::cout << char(i + 'a');
        }
        else {
            std::cout << "," << char(i + 'a');
        }
    }
    std::cout << ");" << std::endl;
    std::cout << "}" << std::endl;
}
void handle_req ( shared_ptr <connection> con) 
{
while ( con->is_alive (  )  ) 
{
string msg =  con->recv (  )  ; 
exec(msg);
print ( msg )  ; 
shared_ptr <Signal<string>> s = shared_ptr <Signal<string>>(new Signal<string>()); thread( process_string_signal_string , msg , s ).detach() ; 
thread( send_back_class_connection_signal_string , con , s ).detach() ; 
}
}
int main () 
{
shared_ptr <server> a =  shared_ptr <server>(new server()) ; 
a->listen ( 5566 )  ; 
while ( true ) 
{
shared_ptr <connection> con =  a->accept (  )  ; 
thread( handle_req_class_connection_signal_void , con , shared_ptr <Signal<void>> (new Signal<void>()) ).detach() ; 
}
return 0 ; 
}
