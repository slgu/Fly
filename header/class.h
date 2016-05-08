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

template<class T>
class flyvector
{
    std::vector<T> v;
public:
    std::recursive_mutex v_mutex;

    flyvector () {
    }

    flyvector (istream_iterator<T> first, istream_iterator<T> last) {
        v.assign(first, last);
    }

    int size() {
        std::unique_lock<std::recursive_mutex> lk(v_mutex);
        return v.size();
    }

    T& get_at (const int& n) {
        std::unique_lock<std::recursive_mutex> lk(v_mutex);
        return v[n];
    }

    void set_at (const int& n, const T& val) {
        std::unique_lock<std::recursive_mutex> lk(v_mutex);
        v[n] = val;
    }

    void push_back (const T& val) {
        std::unique_lock<std::recursive_mutex> lk(v_mutex);
        v.push_back(val);
    }
};

shared_ptr<flyvector<string>> str_split(string s) {
    stringstream ss(s);
    istream_iterator<string> begin(ss);
    istream_iterator<string> end;
    return shared_ptr<flyvector<string>>( new flyvector<string>(begin, end));
}


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
