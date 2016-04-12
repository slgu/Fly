#include <iostream>
#include <thread>
#include <stdio.h>      /* for printf() and fprintf() */
#include <sys/socket.h> /* for socket(), bind(), and connect() */
#include <arpa/inet.h>  /* for sockaddr_in and inet_ntoa() */
#include <stdlib.h>     /* for atoi() and exit() */
#include <string.h>     /* for memset() */
#include <unistd.h>     /* for close() */
#include <time.h>       /* for time() */
#include <netdb.h>      /* for gethostbyname() */
#include <signal.h>     /* for signal() */
#include <sys/stat.h>   /* for stat() */
#include <pthread.h>   /* for threading */
#include <cstdio>
#include <mutex>
#include <condition_variable>
#include <queue>

using namespace std;

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

class connection {
private:
    int c_sock = -1;
    FILE *c_fp = NULL;
public:
    connection(int c): c_sock(c) {};
    string recv();
    void close();
};

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
        return rmsg;
    }
            
    fgets(requestLine, sizeof(requestLine), c_fp);

    rmsg = string(requestLine);

    return rmsg;
}

void connection::close() {
    fclose(c_fp);
    c_fp = NULL;
    c_sock = -1;
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
    serv_sock = create_server_socket(port);
}

shared_ptr<connection> server::accept(void) {
        
    struct sockaddr_in clntAddr;
    unsigned int clntLen = sizeof(clntAddr);
        
    int c_sock = ::accept(serv_sock, (struct sockaddr *)&clntAddr, &clntLen);

    return shared_ptr<connection>(new connection(c_sock));
}

void handle_req(shared_ptr<connection> con) {
    while(true) {
        auto msg = con->recv();
        cout << "handle_req " << msg << endl;
        /*
        s  =   fly calculate(msg);
        register s sendback(con);
        */
    }
}

void handle_req_wrapper(shared_ptr<connection> con, shared_ptr<Signal<void>> sig) {
    handle_req(con);
}

int main() {
    shared_ptr <server> a = shared_ptr <server> (new server());
    a->listen(5566);
    while(true) {
        shared_ptr<connection> con = a->accept();
        thread(handle_req_wrapper, con, shared_ptr<Signal<void>> (new Signal<void>)).detach();
    }
    return 0;
}

/*

func main() {
    a = @server;
    a.listen(5566); 
    while(true) {
        con = a.accept();
        if (con >= 0) {
            fly handle_req(con);
        }
    }
}

func handle_req(con) {
    while(true) {
        msg = con.recv();
        s  =   fly calculate(msg);
        register s sendback(con);
    }
}

Built-In
    server::listen 
        param int
        return void

    server::accept
        param void
        return connection

    connection::recv
        param int
        return shared_ptr<string>
 
*/
