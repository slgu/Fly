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

class connection {
private:
    int c_sock = -1;
    FILE *c_fp = NULL;
public:
    connection(int c): c_sock(c) {};
    string recv();
    void send(string s);
    void close();
};

void connection::send(string msg) {
    if (c_sock < 0) {
        cout << "connection::send wrong socket " << c_sock << endl;
        return;
    }

    msg += "\n";

    int len = msg.length();
    if (::write(c_sock, msg.c_str(), len) != len) {
        cout << "connection::send fail " << endl;
    }
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

class client {
public:
    shared_ptr<connection> connect(string server_ip, short port) {

        int sockfd;
        struct sockaddr_in serv_addr;

        shared_ptr<connection> ret;

        if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        {
            printf("\n Error : Could not create socket \n");
            return ret;
        }

        memset(&serv_addr, '0', sizeof(serv_addr));
        serv_addr.sin_family = AF_INET;
        serv_addr.sin_port = htons(port);

        if (inet_pton(AF_INET, server_ip.c_str(), &serv_addr.sin_addr)<=0)
        {
            printf("\n inet_pton error occured\n");
            return ret;
        }

        if ( ::connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
        {
            printf("\n Error : Connect Failed \n");
            return ret;
        }

        return shared_ptr<connection> (new connection(sockfd));
    }
};

int main(int argc, char *argv[]) {
    shared_ptr<client> clnt = shared_ptr<client>(new client());
    shared_ptr<connection> con = clnt->connect("192.168.1.5", 5567);
    cout << "connected " << endl;
    if (con != nullptr) {
        con->send("ping");
        cout << "sent " << endl;
        auto str = con->recv();
        cout << str << endl;
    }
    return 0;
}
